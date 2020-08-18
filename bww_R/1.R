set_install <- function(){
  install.packages("multilinguer")
  install.packages(c('stringr', 'hash', 'tau', 
                     'Sejong', 'RSQLite', 'devtools'), type = "binary")
  install.packages("remotes")
  remotes::install_github('haven-jeon/KoNLP', upgrade = "never", force = TRUE, 
                          INSTALL_opts=c("--no-multiarch"))
  install.packages(c('RColorBrewer','wordcloud'))
  install.packages("xlsx")
}
set_install()
###############################################################################
set_library <- function(){
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(multilinguer)
  library(rJava)
  library(httr)
  library(readr)
  library(xlsx)
  setwd("C:\\big data class\\2020_environment_data_using_project\\bww_R\\Data")
}
set_library()
###############################################################################
make_zzin <- function(Yangu){Yangu_zzin <- Yangu %>%  #수질데이터 필요한 데이터만 추출, 
  select(ptnm, addr, wmyr, wmod, wmdep, itemamnt, itemtemp, itemph, 
         itemdoc, itembod, itemcod, itemss, itemtcoli, itemtn, 
         itemtp, itemcloa, itemphenol, itemec, itemno3n, itemtoc) %>% 
  filter(!is.na(itemph)&!is.na(itemcod)&!is.na(itembod) #결측치 제거
          &!is.na(itemss)&!is.na(itemtp)&!is.na(itemtoc))
  Yangu_add <-Yangu_zzin$addr 
  Yangu_add <- strsplit(Yangu_add, " ")
  Yangu_add
  Yangu_add[[1]][4]
  for (i in 1:length(Yangu_zzin$addr)){
    Yangu_zzin$add0[i] <- Yangu_add[[i]][2] #상세주소
    Yangu_zzin$add1[i] <- Yangu_add[[i]][3]
    Yangu_zzin$add2[i] <- Yangu_add[[i]][4]
    Yangu_zzin$add3[i] <- Yangu_add[[i]][5]
  }
  return(Yangu_zzin)
}
###############################################################################
wqgf <- function(Yangu_zzin){
  Yangu_zzin$wqg <- ifelse(Yangu_zzin$itemph >= 6.5 &  #수질등급 주기
                           Yangu_zzin$itemph <= 8.5 & #한수원 수질등급 기준
                           Yangu_zzin$itembod <= 1 & 
                           Yangu_zzin$itemcod <= 2 &
                           Yangu_zzin$itemtoc <= 2 &
                           Yangu_zzin$itemss <= 25 &
                           Yangu_zzin$itemdoc >= 7.5 &
                           Yangu_zzin$itemtp <= 0.02, 1,
                     ifelse(Yangu_zzin$itemph >= 6.5 & 
                            Yangu_zzin$itemph <= 8.5 &
                            Yangu_zzin$itembod <= 2 & 
                            Yangu_zzin$itemcod <= 4 &
                            Yangu_zzin$itemtoc <= 3 &
                            Yangu_zzin$itemss <=25 &
                            Yangu_zzin$itemdoc >= 5 &
                            Yangu_zzin$itemtp <= 0.04, 2, 
                      ifelse(Yangu_zzin$itemph >= 6.5 & 
                             Yangu_zzin$itemph <= 8.5 &
                             Yangu_zzin$itembod <= 3 & 
                             Yangu_zzin$itemcod <= 5 &
                             Yangu_zzin$itemtoc <= 4 &
                             Yangu_zzin$itemss <=25 &
                             Yangu_zzin$itemdoc >= 5.0 &
                             Yangu_zzin$itemtp <= 0.1, 3,
                       ifelse(Yangu_zzin$itemph >= 6.5 & 
                              Yangu_zzin$itemph <= 8.5 &
                              Yangu_zzin$itembod <= 5 & 
                              Yangu_zzin$itemcod <= 7 &
                              Yangu_zzin$itemtoc <= 5 &
                              Yangu_zzin$itemss <=25 &
                              Yangu_zzin$itemdoc >= 5.0 &  
                              Yangu_zzin$itemtp <= 0.2, 4,
                        ifelse(Yangu_zzin$itemph >= 6.0 & 
                               Yangu_zzin$itemph <= 8.5 &
                               Yangu_zzin$itembod <= 8 & 
                               Yangu_zzin$itemcod <= 9 &
                               Yangu_zzin$itemtoc <= 6 &
                               Yangu_zzin$itemss <= 100 &
                               Yangu_zzin$itemdoc >= 2.0 &
                               Yangu_zzin$itemtp <= 0.3, 5,     
                          ifelse(Yangu_zzin$itemph >= 6.0 & 
                                 Yangu_zzin$itemph <= 8.5 &
                                 Yangu_zzin$itembod <= 10 & 
                                 Yangu_zzin$itemcod <= 11 &
                                 Yangu_zzin$itemtoc <= 8 &
                                 Yangu_zzin$itemss <= 100 &
                                 Yangu_zzin$itemdoc >= 2.0 &
                                 Yangu_zzin$itemtp <= 0.5, 6, 7))))))
  return(Yangu_zzin$wqg)
}
###############################################################################
Yangu <- read.csv("Yangu2.csv", header = T,
                 stringsAsFactors = F, sep = ",", fill = TRUE)
Goesan <- read.csv("Goesan2.csv", header = T,
                  stringsAsFactors = F, sep = ",", fill = TRUE)
Yeongwol <- read.csv("Yeongwol2.csv", header = T,
                  stringsAsFactors = F, sep = ",", fill = TRUE)
Inje <- read.csv("Inje2.csv", header = T,
                  stringsAsFactors = F, sep = ",", fill = TRUE)

###############################################################################
Yangu_zzin <- make_zzin(Yangu)
Inje_zzin <- make_zzin(Inje)
Goesan_zzin <- make_zzin(Goesan)
Yeongwol_zzin <- make_zzin(Yeongwol)
###############################################################################
Yangu_zzin$wqg <- wqgf(Yangu_zzin)
Inje_zzin$wqg <- wqgf(Inje_zzin)
Goesan_zzin$wqg <- wqgf(Goesan_zzin)
Yeongwol_zzin$wqg <- wqgf(Yeongwol_zzin)
###############################################################################
View(Yangu_zzin)
View(Inje_zzin)
View(Goesan_zzin)
View(Yeongwol_zzin)
###############################################################################
write.xlsx(Yangu_zzin %>% arrange(add1, add2, add3), file = "Yangu_zzin.xlsx")
write.xlsx(Inje_zzin %>% arrange(add1, add2, add3), file = "Inje_zzin.xlsx")
write.xlsx(Goesan_zzin %>% arrange(add1, add2, add3), file = "Goesan_zzin.xlsx")
write.xlsx(Yeongwol_zzin %>% arrange(add1, add2, add3), file = "Yeongwol_zzin.xlsx")
################################################################################
Yangu_zzin <- Yangu %>% 
  select(ptnm, addr, wmyr, wmod, wmdep, itemamnt, itemtemp, itemph, itemdoc, itembod, itemcod, itemcod, itemss, itemtcoli, itemtn, itemtp, itemcloa, itemphenol, itemec, itemno3n) %>% 
  filter(!is.na(itemph)&!is.na(itemcod)&!is.na(itembod)&!is.na(itemss)&!is.na(itemtp))

Goesan_zzin <- Goesan %>% 
  select(ptnm, addr, wmyr, wmod, wmdep, itemamnt, itemtemp, itemph, itemdoc, itembod, itemcod, itemcod, itemss, itemtcoli, itemtn, itemtp, itemcloa, itemphenol, itemec, itemno3n) %>% 
  filter(!is.na(itemph)&!is.na(itemcod)&!is.na(itembod)&!is.na(itemss)&!is.na(itemtp))

Yeongwol_zzin <- Yeongwol %>% 
  select(ptnm, addr, wmyr, wmod, wmdep, itemamnt, itemtemp, itemph, itemdoc, itembod, itemcod, itemcod, itemss, itemtcoli, itemtn, itemtp, itemcloa, itemphenol, itemec, itemno3n) %>% 
  filter(!is.na(itemph)&!is.na(itemcod)&!is.na(itembod)&!is.na(itemss)&!is.na(itemtp))

Inje_zzin <- Inje %>% 
  select(ptnm, addr, wmyr, wmod, wmdep, itemamnt, itemtemp, itemph, itemdoc, itembod, itemcod, itemcod, itemss, itemtcoli, itemtn, itemtp, itemcloa, itemphenol, itemec, itemno3n) %>% 
  filter(!is.na(itemph)&!is.na(itemcod)&!is.na(itembod)&!is.na(itemss)&!is.na(itemtp))

Yangu_add <-Yangu_zzin$addr 
Yangu_add <- strsplit(Yangu_add, " ")
Yangu_add
Yangu_add[[1]][4]
length(Yangu_zzin$addr)
for (i in 1:length(Yangu_zzin$addr)){
  Yangu_zzin$add1[i] <- Yangu_add[[i]][3]
  Yangu_zzin$add2[i] <- Yangu_add[[i]][4]
  Yangu_zzin$add3[i] <- Yangu_add[[i]][5]
}

Yangu_zzin$wqd <- ifelse(6.5 <= Yangu_zzin$itemph |
                        Yangu_zzin$itembod <= 1 | 
                        Yangu_zzin$itemcod <= 2 | 
                        Yangu_zzin$itemss <=25|
                        Yangu_zzin$itemdoc >= 7.5 |
                        Yangu_zzin$itemtp <= 0.02, 1, 2)
View(Yangu_zzin)

Goesan_zzin %>% filter(6.5 <= itemph |
                         itembod <= 1 | itemcod <= 2 |itemss <=25|
                         itemdoc >= 7.5 | itemtp <= 0.02)

View(Goesan_zzin)

Yeongwol_zzin %>% filter(6.5 <= itemph |
                         itembod <= 1 | itemcod <= 2 |itemss <=25|
                         itemdoc >= 7.5 | itemtp <= 0.02)
View(Yeongwol_zzin)

Inje_zzin %>% filter(6.5 <= itemph |
                           itembod <= 1 | itemcod <= 2 |itemss <=25|
                           itemdoc >= 7.5 | itemtp <= 0.02)
View(Inje_zzin)

Inje_zzin <- wqgf(Inje_zzin)
Yangu$itemtoc
View(Yangu)
