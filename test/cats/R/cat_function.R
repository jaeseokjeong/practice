get_koreaweather <- function(start_date, end_date, personal_key){
  
  #기본 주소  
  url1<- "http://data.kma.go.kr/apiData/getData?type=json&dataCd=ASOS&dateCd=HR&startHh=00&endHh=23&stnIds=108&schListCnt=999&pageIndex=1"
  
  #뽑아올 날짜
  url2 <- "&startDt="
  
  url3 <- "&endDt="
  
  
  #API Key
  url4 <- "&apiKey="
  mykey <- personal_key
  
  
  
  selected_period <- paste0(url1,
                            url2,
                            start_date,
                            url3,
                            end_date,
                            url4,
                            personal_key)
  
  
  result <- httr::GET(selected_period)
  json <- httr::content(result , as = "text")
  processed_json <- jsonlite::fromJSON(json)
  
  weather_informaiton <- processed_json$info[[4]] %>% 
    dplyr::select(TM,TA,WS,HM) 
  
  weather_informaiton
  
}
