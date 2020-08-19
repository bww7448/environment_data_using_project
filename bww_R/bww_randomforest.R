library(randomForest)
library(caret)
bww_df <- read_delim(file = "bwwdb.csv", col_names = T, delim = ",")
View(bww_df)
bww_df$sum_rank <- ifelse(bww_df$sum_rank == 0, 0.1, bww_df$sum_rank)
str(bww_df)
bww_df <- rename(bww_df, 'c_count' = 'cafe_count.5km.' , 'tnum' = 'n')
bww_df$wqg <- ifelse(bww_df$wqg > 0, bww_df$wqg, -bww_df$wqg)
bww_df$wqg <- (9 - bww_df$wqg)



hist(bww_df$sum_rank, breaks = seq(0,2000, by = 10))
hist(bww_df$n, breaks = seq(0,62, by = 1))
hist(bww_df$g_count,  breaks = seq(0,500, by = 10))
?hist
library(ggplot2)

bww_df %>% filter(sum_rank <= 300) %>% summarise(n = n())
bww_df %>% filter(n <= 20) %>% summarise(n = n())
bww_df %>% filter(g_count <= 100) %>% summarise(n = n())

table(bww_df$sum_rank)


mean(bww_df$mean_rank)
mean(bww_df$tnum)
mean(bww_df$g_count)
sum(bww_df$mean_rank)
sum(bww_df$tnum)
sum(bww_df$g_count)
median(bww_df$g_count)
max(bww_df$n)
max(bww_df$sum_rank)
max(bww_df$g_count)
max(sqrt(bww_df$g_count))
shapiro.test(bww_df$tnum)
shapiro.test(bww_df$mean_rank)
shapiro.test(bww_df$g_count)

bww_df$g_count2 <- bww_df$g_count%/%100
View(bww_df)
bww_df$fav <- bww_df$tnum * bww_df$mean_rank * bww_df$g_count
summary(bww_df$fav)
bww_df2 <- bww_df
summary(bww_df$n/62 * bww_df$sum_rank/1821 * bww_df$g_count/432)
summary(bww_df$sum_ratio + bww_df$g_count)
bww_df$fav <- (bww_df$sum_ratio + bww_df$g_count)
#bww_df2 <- bww_df
bww_df2 <- bww_df %>% filter(add0 != "가평군")
View(bww_df2)
length(bww_df2)
summary(bww_df2$fav)
bww_df2$fav
bww_zzin$fav_r <- ifelse(bww_df2$fav >= 71, "A", 
                        ifelse(bww_df2$fav >= 64, "B", 
                               ifelse(bww_df2$fav >= 8, "C", "D")))
table(bww_df2$fav_r)
str(bww_zzin)
bww_zzin <- bww_df2 %>% select(-c(1,2,7,8,11,12,13,16,17))
bww_zzin <- bww_zzin %>% select(-c(7,8))
View(bww_zzin)
bww_zzin$wqg <- ifelse(bww_zzin$wqg > 0, bww_zzin$wqg, -bww_zzin$wqg)
bww_zzin$fav_r <- as.factor(bww_zzin$fav_r)
#splitting data
idx <- createDataPartition(bww_zzin$fav_r, p = 0.75, list = F)
bww_train <- bww_zzin[idx,]
bww_test <- bww_zzin[-idx,]
length(bww_train)
#bootstring1
set.seed(5252)
bww_rf <- randomForest(fav_r ~ ., data = bww_train,
                        ntree = 10000, mtry = sqrt(6), importance = T)


# prediction
bww_pr <- predict(bww_rf, newdata = bww_test)
bww_pr

#model accuracy
caret::confusionMatrix(as.factor(bww_pr), as.factor(bww_test$fav_r))

#importance
importance(bww_rf)

#importance(분산값 이용)
varImpPlot(bww_rf, main = "varImPlot of iris")





################################################################################
View(bww_df)
rank_rat <- bww_df %>% group_by(add0) %>% 
  summarise(sum_sum_rank = sum(sum_rank))
rank_rat

bww_df <- read_delim(file = "bww_data.csv", col_names = T, delim = ",")
