library(dplyr)
library(readxl)
library(xlsx)

?read.xlsx
know_df <- read.xlsx(file = "./인지도/datalab (18).xlsx" , sheetIndex = 1, encoding = "UTF-8") #용추계곡을 계곡의 검색 kno

괴산_list <- data.frame(괴산 = c("화양구곡", "갈론계곡",   
                             "괴산선유동계곡",   "쌍곡구곡"),
                        wqg = c(1, 7,   
                                1, 3))

영월_list <- data.frame(영월 = c("김삿갓계곡",   "어라연계곡",   "이끼계곡",
                             "연하계곡",   "법흥계곡",   "엄둔계곡",
                             "내리계곡"),
                        wqg = c(1, 6, 1,
                                6, 10, 10,
                                1))


양구_list <- data.frame(양구 = c("천미계곡",   "광치계곡",   "두타연계곡",
                             "비수구미",   "남전계곡"),
                        wqg = c(2, 2, 2,
                                2, 2))

인제_list <- data.frame(인제 = c("아침가리계곡",   "내린천",   "백담계곡",
                             "십이선녀탕",   "진동계곡",   "미산계곡",
                             "하추리계곡",   "가야동계곡",
                             "구만동계곡",   "수련동계곡",   "방동계곡",
                             "내심적계곡",   "옥녀탕계곡",
                             "추대계곡",   "대승폭포계곡"),
                        wqg = c(10, 1, 10,
                                10, 10, 30,
                                1, 10,
                                10, 10, 10,
                                10, 10,
                                10, 10))

mean_df<- round(colMeans(know_df),2)

mean_df <- as.data.frame(mean_df)
mean_df


