install.packages('rvest')
install.packages('httr')
library(rvest)
library(dplyr)
library(stringr)
library(httr)
clientID <- 'PnRvqOklqoJ3PUo_jth9'
clientPW <- 'TRd9zhQk9z'

urlStr <- 'https://search.naver.com/search.naver?sm=top_hty&fbm=1&ie=utf8&'
name <- read.csv('./bww_R/Data/계곡DB - 통합.csv', header = T, fileEncoding="UTF-8")
query_search_name <- name$name 
query_search_name

count_list<-c()
for(i in query_search_name){
  searchQuery <- paste('query=', i)
  
  searchStr <- iconv(searchQuery, to = 'UTF-8')
  searchStr <- URLencode(searchStr)
  
  otherStr <- '&display=100&start=1&sort=sim'
  reqURL <- paste(urlStr,searchStr,otherStr,sep = '') #paste는 붙이는 함수
  
  download.file(url = paste0(reqURL,1,''), #공백을 없도록
                destfile = '1.html', quiet = T) #파일명을 무엇으로 할것이냐
  rreview <- read_html('1.html')
  
  countreview <- rreview %>%
    html_nodes('.review') %>% 
    html_text()
  
  countreview
  
  countreview1 <- countreview[1]
  countreview1
  countr <- str_extract(countreview1,'[0-9]{1,}')
  count <- as.numeric(countr)
  count
  count_list <- append(count_list, count)
}

count_list

name$g_count <- count_list
View(name)

write.csv(name,'계곡DB_V.02.csv', row.names = F)

# 
# apiResult <- httr::GET(reqURL,
#                        add_headers('X-Naver-Client-Id' = clientID,
#                                    'X-Naver-Client-Secret' = clientPW))
# 
# str(apiResult)
# apiResult
# 
# blog_review_count <- rawToChar(apiResult$content)
# blog_review_count
# 
# Encoding(blog_review_count) <- 'UTF-8'
# 
# head(blog_review_count)
# 
# 
# write.csv(blog_review_count, 'blog_review.csv', row.names = F)
# 
# 
# 







