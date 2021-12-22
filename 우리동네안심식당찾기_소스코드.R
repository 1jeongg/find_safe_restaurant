
p0 <- "                   ------------------------------------------------------------------------------------------------- "
p1 <- "\n                                      안녕하세요, 우리동네 안심,모범 식당 찾기 프로그램에 오신 걸 환영합니다."
p2 <- "                                                  다음 선택지 중 둘 중 하나를 골라주세요.\n"
p3 <- "                                              <안심식당> : 코로나19 방역수칙을 준수한 안심식당 찾기"
p3_1 <- "                                    조건: 덜어먹기 가능한 도구 비치, 위생적인 수저관리, 종사자 마스크 착용 유무\n"
p4 <- "                                              <모범식당> : 식품의약품안전처 인증을 받은 모범식당 찾기"
p4_1 <- "                    조건:일반음식점, 영업신고 후 1년 이상 경과된 업소, 지역적 특색이 강하거나, 맛과 위생, 서비스가 우수한 일반음식점"
p5 <- "\n                          모범식당은 안심식당보다 까다로워 그 수가 적고 위치 정보가 제공되지 않지만 신뢰성이 매우 높습니다."
p6 <- "                             안심식당은 코로나, 간단위생 상태만 합격한 곳으로 신뢰성을 떨어지지만 위치정보가 제공됩니다.\n"
p7 <- "**************************************************************************************************************************************"

msg <- paste(p7,p1,p2,p0,p3,p3_1,p4,p4_1,p0,p5,p6,p7, sep='\n')
cat(msg)

choose <- readline("모범식당과 안심식당 중 하나를 골라주세요 : \n")

while(choose != '모범식당' && choose != "안심식당" && choose != "둘다"){
  choose <- readline("모범식당과 안심식당 중 하나를 골라주세요.\n")
}


anshim_func <- function(){
    # 안심 식당 정보 불러오기
    data      <- read.csv("안심식당정보.csv") 
    data_city <- data$시도명
    data_gu   <- data$시군구명
    
    # 도시명 입력받기
    #city <- readline("도시명을 입력해주세요: ")
    
    # 값이 제대로 입력됐는지 확인
    while(length(which(data_city == city)) == 0){
       city <- readline("다음 도시를 찾을 수 없습니다. 도시명을 제대로 입력해주세요: ")
    }
    
    # 구 이름 입력받기 & 값 확인
   # gu <- readline("시군구를 입력해주세요: ")
    while(length(which(data_gu == gu)) == 0){
      gu <- readline("다음 시군구를 찾을 수 없습니다. 시군구명을 제대로 입력해주세요: ")
    }
    
    # 도시에 시군구 명이 있는지 확인
    while(length(which(data_city == city & data_gu == gu)) == 0){
      city <- readline("다음 도시를 찾을 수 없습니다. 도시명을 제대로 입력해주세요: ")
      gu <- readline("다음 시군구를 찾을 수 없습니다. 시군구명을 제대로 입력해주세요: ")
    }
    
    # 우리동네 안심 식당 전체 정보
    cafeteria    <- subset(data, data$시도명 == city & data$시군구명 == gu & data$선정여부 == 'Y')
    name         <- cafeteria$사업장명
    address      <- cafeteria$주소1
    address2     <- cafeteria$주소2
    phone_number <- cafeteria$전화번호
    kind         <- cafeteria$업종상세
    
    #안심식당 정보를 View를 통해 출력
    anshim_rest  <- cafeteria[c(5,8,9,11,12)]
    View(anshim_rest)
    
    # 지도에 위치와 상호명 출력, 패키지가 안 깔려있을 경우 설치
    
    #install.packages("ggmap")
    #install.packages("leaflet")
    library(leaflet)
    library(ggmap)
    register_google(key='AIzaSyDzTXg8A9daznCBrfd7zLovfDZciJb6eww')
    gc <- geocode(enc2utf8(address))
    
    leaflet() %>%
       addTiles() %>%
       addMarkers( lng=gc$lon, lat=gc$lat,
                   popup=paste0("이름: ",name,"<br> 주소: ",address,"(",address2,")<br> 전화번호: ",phone_number,"<br>종류: ",kind))
}

# 모범 음식점
mobum_func <- function(){
    
    #install.packages("ggplot2)
    #install.packages("XML")
    library(ggplot2)
    library(XML)

    #xml파일을 받아옴
    url <- "http://openapi.foodsafetykorea.go.kr/api/9549a859a7a34475ad42/I1590/xml/1/1001"
    xmlFile <- xmlParse(url)
    xmlRoot(xmlFile)
    privateDF <- xmlToDataFrame(getNodeSet(xmlFile, "//row"))
    mobum <- privateDF[c(5,6,8)]
   
    # 도시명 입력받기
    #city <- readline("도시명을 입력해주세요: ")
    #gu <- readline("시군구를 입력해주세요: ")
    mobum_gu <- paste(city, gu, sep=' ')
     
    #모범식당의 위치가 적절한지 확인
    while(length(which(mobum$SIGNGU_NM == mobum_gu)) == 0){
      city <- readline("다음 도시를 찾을 수 없습니다. 도시명을 제대로 입력해주세요: ")
      gu <- readline("다음 시군구를 찾을 수 없습니다. 시군구명을 제대로 입력해주세요: ")
      mobum_gu <- paste(city, gu, sep=' ')
    }
    
    #입력한 위치의 사업장명, 메뉴 출력
    colnames(mobum) <- c("사업장명","위치","메뉴")
    mobum_rest <- mobum[mobum$위치 == mobum_gu, c(1,3)]
    View(mobum_rest)
}

if (choose == "안심식당") anshim_func()
if (choose == "모범식당") mobum_func()
if (choose == "둘다"){
  city <- readline("도시명을 입력해주세요: ")
  gu <- readline("시군구를 입력해주세요: ")
  mobum_func()
  anshim_func()
}
