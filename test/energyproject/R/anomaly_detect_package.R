#' anomaly_detect_package
#'
#' This package judges whether or not the power consumption per hour is abnomaly.
#' @param selected_data The data variable should be composed of the household number (id), date (date), and power usage (usage).
#' @param weather_data It is the data extracted by Korean weather (koreaweather) R package. The variables in the data are date, average temperature, relative humidity, and wind.
#' @param training_start_date Determine the start date of training data to use in setting the prediction model in the R package.
#' @param training_finish_date  Determine the end date of the training data to use in setting the prediction model in the R package.
#' @param test_start_date Set the start date of test data to detect anomaly.
#' @param test_finish_date Set the finsih date of test data to detect anomaly.
#' @keywords anomaly detection, power consumption
#' @export
#' @examples
#'  energy_anomaly_detect(example_household, weather_information,
#'  "2016-01-03 23:00:00","2016-01-21 23:00:00",
#'  "2016-01-22 00:00:00", "2016-01-22 05:00:00")

energy_anomaly_detect <- function(selected_data, weather_data, training_start_date, training_finish_date,test_start_date,test_finish_date) {
  
  # date 날짜 형식으로 변경
  
  selected_data$date <- lubridate::ymd_hms(selected_data$date)
  
  #년월일-시간 변수 생성
  
  selected_data <- selected_data %>% 
    dplyr::mutate(date_hour = make_datetime(year(date),month(date), day(date),hour(date))) %>%
    dplyr::arrange(date)
  
  ## 1. 15분 간격 데이터를 시간당 평균 전력 사용량으로 변경
  
  hour_mean_data <- selected_data %>% 
    dplyr::group_by(id, date_hour) %>% 
    dplyr::summarise(hour_usage = mean(usage, na.rm = T))
  
  # 집계되지 않은 결측값 추가
  
  add_na_data <- hour_mean_data %>% 
    dplyr::arrange(date_hour) %>% 
    tidyr::complete(date_hour = seq.POSIXt(min(date_hour),max(date_hour), by = "1 hour"))
  
  ## 2. 결측값 대체
  
  # 변수 생성
  # 변수 의미 time interval : 시간 / day_type: 요일명 /date : 년월일/week_type:주말 & 평일
  
  filling_na_data <- add_na_data %>% 
    dplyr::mutate(time_interval = hour(date_hour),
                  day_type = wday(date_hour), 
                  date = make_date(year(date_hour),month(date_hour), day(date_hour))) %>% 
    dplyr::mutate(week_type = if_else(day_type %in% c(1, 7), "weekend", "weekday"))
  
  
  
  
  # 월(month) & 시간(hour) 전력 사용량 중앙값(median)
  mh_median_usage <- add_na_data %>% 
    dplyr::group_by(month(date_hour), hour(date_hour)) %>% 
    dplyr::summarise(month_hour_median_usage = median(hour_usage, na.rm = T)) %>%
    dplyr::ungroup()
  
  colnames(mh_median_usage) <- c("usage_month", "usage_hour", "median_usage")
  
  # NA 대체 과정
  na_occur_time <- which(is.na(filling_na_data$hour_usage))
  
  if (length(na_occur_time) > 0) {
    
    for (i in 1:length(na_occur_time)) {
      
      na_time <- filling_na_data[na_occur_time[i], ]
      
      na_imputation <- as.numeric(mh_median_usage %>% 
                                    dplyr::filter(usage_month == month(na_time$date_hour) & usage_hour == hour(na_time$date_hour)) %>% 
                                    dplyr::select(median_usage))
      
      filling_na_data[na_occur_time[i], "hour_usage"] <- na_imputation
      
    }
  } else {
    
    filling_na_data <- filling_na_data
  }
  
  ## outlier 제거
  
  before_replace_outlier <- filling_na_data %>% 
    dplyr::mutate(week_number = week(date))
  
  n_week <- unique(before_replace_outlier$week_number)
  week_usage <- list()
  
  
  for(j in 1:length(n_week)) {
    
    # 해당하는 주(week)의 시간대별(hour) 전력사용량 중앙값
    
    wh_median_usage <- before_replace_outlier %>%
      dplyr::filter(week_number == n_week[j]) %>% 
      dplyr::group_by(hour(date_hour)) %>% 
      dplyr::summarise(hour_median_usage = median(hour_usage, na.rm = T))  %>%
      dplyr::ungroup()
    
    colnames(wh_median_usage)<- c("usage_hour", "median_usage")
    
    # 주(week)별 추출
    
    week_usage[[j]] <- before_replace_outlier %>%
      dplyr::filter(week_number == n_week[j])
    
    
    #outlier 존재하는 row number
    
    upper_outlier_threshold <- boxplot(week_usage[[j]]$hour_usage, plot = F)$stats[5]
    lower_outlier_threshold <- boxplot(week_usage[[j]]$hour_usage, plot = F)$stats[1]
    outlier_occur_time <- which(week_usage[[j]]$hour_usage > upper_outlier_threshold |
                                  week_usage[[j]]$hour_usage < lower_outlier_threshold)
    
    if (length(outlier_occur_time) > 0) {
      
      for(i in 1:length(outlier_occur_time)) {
        
        outlier_time <- week_usage[[j]][outlier_occur_time[i], ]
        
        outlier_imputation <- as.numeric(wh_median_usage %>% 
                                           dplyr::filter(usage_hour == hour(outlier_time$date_hour)) %>%
                                           dplyr::select(median_usage))
        
        week_usage[[j]][outlier_occur_time[i], "hour_usage"] <- outlier_imputation
        
      }
    } else {
      
      week_usage[[j]] <- week_usage[[j]]
    }
    
  }
  
  replaced_na_outlier_data <- data.table::rbindlist(week_usage)
  
  # weather_data 결합
  
  preprocessed_data <- merge(x = replaced_na_outlier_data, 
                             y = weather_data,
                             by.x = "date_hour", 
                             by.y = "date", 
                             all.x = TRUE)
  
  ########Energy Anomaly Detect###################
  
  # 하루전 전 시간당 평균 전력사용량을 변수로 넣는 과정
  
  unique_day <- unique(preprocessed_data$date)
  min_day <- min(unique_day)
  max_day <- max(unique_day)
  
  delete_first_day <- preprocessed_data %>% 
    dplyr::filter(!date %in% min_day)
  
  delete_last_day <- preprocessed_data %>% 
    dplyr::filter(!date %in% max_day)
  
  delete_first_day$before_day_usage <- delete_last_day$hour_usage
  
  # 바로 전 시간 평균 사용량을 변수로 넣는 과정
  
  unique_date_hour <- unique(delete_first_day$date_hour)
  min_date_hour <- min(unique_date_hour)
  max_date_hour <- max(unique_date_hour)
  
  
  delete_first_hour <- delete_first_day %>% 
    dplyr::filter(!date_hour %in% min_date_hour)
  
  delete_last_hour <- delete_first_day %>% 
    dplyr::filter(!date_hour %in% max_date_hour)
  
  delete_first_hour$before_hour_usage <- delete_last_hour$hour_usage
  
  
  modeling_data <- delete_first_hour %>% 
    dplyr::select("date_hour", "hour_usage","before_hour_usage", 
                  "before_day_usage", "time_interval", "temperature",
                  "week_type", "date")
  
  modeling_data$week_type <- as.factor(modeling_data$week_type)
  
  
  
  # Training  data 갯수 설정
  
  training_data <- modeling_data %>% 
    dplyr::filter(date_hour >= ymd_hms(training_start_date) & date_hour <= ymd_hms(training_finish_date)) %>%
    dplyr::select(-date, -date_hour) %>% 
    dplyr::filter(!is.na(hour_usage) | !is.na(before_day_usage)|!is.na(before_hour_usage))
  
  # Test  data 갯수 설정
  
  test_data <- modeling_data %>% 
    dplyr::filter(date_hour >= ymd_hms(test_start_date) & date_hour <= ymd_hms(test_finish_date)) %>% 
    dplyr::select(-date, -date_hour) %>% 
    dplyr::filter(!is.na(hour_usage) | !is.na(before_day_usage)|!is.na(before_hour_usage))
  
  # 설정 모델
  
  model_lm <- lm(hour_usage ~ before_hour_usage + before_day_usage + 
                   time_interval + temperature + week_type, data = training_data)
  
  # test data 예측
  
  lm_prediction <- as.data.table(predict(model_lm, test_data, , interval = "predict"))
  
  
  # 결과값
  
  detection_result <- data.table(real_usage = test_data$hour_usage, 
                                 threshold_usage = lm_prediction$upr, 
                                 test_number = 1:length(test_data$hour_usage))
  
  detection_result$anomaly <- detection_result$real_usage > detection_result$threshold_usage
  
  detection_result$date_hour <- modeling_data %>% 
    dplyr::filter(date_hour >= ymd_hms(test_start_date) & date_hour <= ymd_hms(test_finish_date)) %>% 
    dplyr::select(date_hour)
  
  detection_result
  
}
