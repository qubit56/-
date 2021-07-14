# -
감성분석 사전만들기
setwd("c:/rdata")
# 데이터 수집
install.packages("rvest")
install.packages("httr")
install.packages("stringr")
library(rvest)
library(httr)
library(stringr)
url='https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=130966&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page='
url
allReview= c() #방을 만든다. 댓글 모아놓은 방.

for(page in 1:10){
  #link = (page-1)*10+1
  urls = paste(url,page,sep="")
  htxt = read_html(urls) #html페이지를 저장하는 html함수
  comments = html_nodes(htxt,'div.score_result')
  reviews = html_text(comments)
  if(length(reviews)==0){break}
  
  allReview= c(allReview,reviews)
  print(page)
}

length(allReview)
allReview

write(allReview,"영화평점수집.txt")

write.csv(allReview,"영화평점수집.csv")


# 데이터 분석 
install.packages("wordcloud")
library(KoNLP)
library(wordcloud)
library(RColorBrewer)

# 데이터 정제
data1 <- readLines("영화평점수집.txt") 
data1 
data1 <- gsub("[\r\n\t]","", data1)
write(data1,"부산행.txt")
data1 = read.table("부산행.txt", header = TRUE, sep = "\t", stringsAsFactors = F)
class(data1)
data1
data1 = as.character(data1)
write(data1,"부산행_정제.txt")

#데이터 분석
data2 <- sapply(data1,extractNoun,USE.NAMES=F)
data2

data3 <- unlist(data2) # 비순차적으로 정렬합니다.
movie_gsub <- str_replace_all(data3,"[^[:alpha:]]","")  # ?---한글 , 영어 외는 삭제
gsub("\\d+", "", movie_gsub) #숫자제거
gsub("\\.", "", movie_gsub) #점(.) 제거
movie_gsub <- gsub(" ","", movie_gsub)
movie_gsub <- gsub("비공","", movie_gsub)
movie_gsub <- gsub("중간","", movie_gsub)
movie_gsub <- gsub("영화","", movie_gsub)


movie_gsub <- Filter(function(x) {nchar(x) >= 2} ,movie_gsub)
movie_gsub
write(unlist(movie_gsub),"Moviescore2.txt")
data4 <- read.table("Moviescore2.txt")
wordcount <- table(data4) # 테이블 데이터의 개수를 변수에 할당합니다.
head(sort(wordcount, decreasing=T),10)
palete <- brewer.pal(9,"Set3") 

wordcloud(names(wordcount),freq=wordcount,scale=c(5,1),rot.per=0.25,min.freq=1,random.order=F,random.color=T,colors=palete)

