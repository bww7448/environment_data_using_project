setwd('C:\\Users\\09\\Desktop\\R\\environment_data_using_project\\bww_R\\Data')
test2 <- read.table(file = "water_qual_202001.csv", fileEncoding = 'UTF-8', header = T,
                    fill = T, sep = ',', quote = "")

install.packages('readr')
library(readr)
test <- read_delim(file = "water_qual_202001.csv", col_names = T, delim = ",")
View(test)
###############################################################################
wq_1912 <- read_delim(file = "water_qual_201912.csv", col_names = T, delim = ",") 
wq_2001 <- read_delim(file = "water_qual_202001.csv", col_names = T, delim = ",")
wq_2002 <- read_delim(file = "water_qual_202002.csv", col_names = T, delim = ",")
wq_2003 <- read_delim(file = "water_qual_202003.csv", col_names = T, delim = ",")
wq_2004 <- read_delim(file = "water_qual_202004.csv", col_names = T, delim = ",")
wq_2005 <- read_delim(file = "water_qual_202005.csv", col_names = T, delim = ",")

View(wq_2005)
###############################################################################
wq_1912_zzin <- make_zzin(wq_1912)
wq_2001_zzin <- make_zzin(wq_2001)
wq_2002_zzin <- make_zzin(wq_2002)
wq_2003_zzin <- make_zzin(wq_2003)
wq_2004_zzin <- make_zzin(wq_2004)
wq_2005_zzin <- make_zzin(wq_2005)

View(wq_1912_zzin)
###############################################################################
Goesan_df <- wq_1912_zzin %>% filter(add0 == "괴산군")
Goesan_df <- rbind(Goesan_df, wq_2001_zzin %>% filter(add0 == "괴산군"))
Goesan_df <- rbind(Goesan_df, wq_2002_zzin %>% filter(add0 == "괴산군"))
Goesan_df <- rbind(Goesan_df, wq_2003_zzin %>% filter(add0 == "괴산군"))
Goesan_df <- rbind(Goesan_df, wq_2004_zzin %>% filter(add0 == "괴산군"))
Goesan_df <- rbind(Goesan_df, wq_2005_zzin %>% filter(add0 == "괴산군"))

Yangu_df <- wq_1912_zzin %>% filter(add0 == "양구군")
Yangu_df <- rbind(Yangu_df, wq_2001_zzin %>% filter(add0 == "양구군"))
Yangu_df <- rbind(Yangu_df, wq_2002_zzin %>% filter(add0 == "양구군"))
Yangu_df <- rbind(Yangu_df, wq_2003_zzin %>% filter(add0 == "양구군"))
Yangu_df <- rbind(Yangu_df, wq_2004_zzin %>% filter(add0 == "양구군"))
Yangu_df <- rbind(Yangu_df, wq_2005_zzin %>% filter(add0 == "양구군"))

Yeongwol_df <- wq_1912_zzin %>% filter(add0 == "영월군")
Yeongwol_df <- rbind(Yeongwol_df, wq_2001_zzin %>% filter(add0 == "영월군"))
Yeongwol_df <- rbind(Yeongwol_df, wq_2002_zzin %>% filter(add0 == "영월군"))
Yeongwol_df <- rbind(Yeongwol_df, wq_2003_zzin %>% filter(add0 == "영월군"))
Yeongwol_df <- rbind(Yeongwol_df, wq_2004_zzin %>% filter(add0 == "영월군"))
Yeongwol_df <- rbind(Yeongwol_df, wq_2005_zzin %>% filter(add0 == "영월군"))

Inje_df <- wq_1912_zzin %>% filter(add0 == "인제군")
Inje_df <- rbind(Inje_df, wq_2001_zzin %>% filter(add0 == "인제군"))
Inje_df <- rbind(Inje_df, wq_2002_zzin %>% filter(add0 == "인제군"))
Inje_df <- rbind(Inje_df, wq_2003_zzin %>% filter(add0 == "인제군"))
Inje_df <- rbind(Inje_df, wq_2004_zzin %>% filter(add0 == "인제군"))
Inje_df <- rbind(Inje_df, wq_2005_zzin %>% filter(add0 == "인제군"))

View(Goesan_df)
View(Yangu_df)
View(Yeongwol_df)
View(Inje_df)
###############################################################################
Goesan_df$wqg <- wqgf(Goesan_df)
Yangu_df$wqg <- wqgf(Yangu_df)
Yeongwol_df$wqg <- wqgf(Yeongwol_df)
Inje_df$wqg <- wqgf(Inje_df)
###############################################################################
Goesan_zzin <- 
  Goesan_df %>% 
  group_by(add0, add1, add2, add3) %>% 
  summarise(mwqg = mean(wqg))
Yangu_zzin <- 
  Yangu_df %>% 
  group_by(add0, add1, add2, add3) %>% 
  summarise(mwqg = mean(wqg))
Yeongwol_zzin <- 
  Yeongwol_df %>% 
  group_by(add0, add1, add2, add3) %>% 
  summarise(mwqg = mean(wqg))
Inje_zzin <- 
  Inje_df %>% 
  group_by(add0, add1, add2, add3) %>% 
  summarise(mwqg = mean(wqg))

###############################################################################
write.csv(Yangu_zzin %>% arrange(add1, add2, add3),
           file = "Yangu_zzin2.csv")
write.csv(Inje_zzin %>% arrange(add1, add2, add3), file = "Inje_zzin2.csv")
write.csv(Goesan_zzin %>% arrange(add1, add2, add3), file = "Goesan_zzin2.csv")
write.csv(Yeongwol_zzin %>% arrange(add1, add2, add3), file = "Yeongwol_zzin2.csv")
################################################################################
str(Yangu_zzin)
