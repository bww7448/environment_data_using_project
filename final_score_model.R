y_data <- read.csv('C:\\Bigdataclass\\환경 빅데이터 공모전\\계곡DB_score.csv', header=T,
                   encoding='eu-kr', fileEncoding = "CP949")

library(dplyr)
y_data <- rename(y_data, area = add0)
y_data <- y_data[, c(-7, -8)]
head(y_data)

# 필요없는 열 제거
data <- y_data
hist(y_data$final_score)

y_data$class <- ifelse(y_data$final_score <= 20, 1, 2)

head(y_data)
# 데이터 나누기
library(caret)
set.seed(1)
idx <- createDataPartition(data$final_score, p=0.7, list=F)

data.train <- data[idx, ]
data.test <- data[-idx, ]

# # 회귀분석
# data.lm <- lm(final_score ~ . -area -name, data=data.train)
# data.step <- step(data.lm, direction='both')
# summary(data.step)

# rpart
library(rpart)
library(rpart.plot)
data.train.1 <- data.train[, c(-1, -2)]
data.rpart <- rpart(final_score ~ ., data = data.train.1)

summary(data.rpart)
prp(data.rpart, type=4, digits=3)

str(data.rpart)

data.rpart$cptable

## prediction
data.rpart.pr <- predict(data.rpart, newdata = data.test)

summary(data.rpart.pr)

aa <- data.frame(data.test$final_score, data.rpart.pr)
View(aa)




## clustering
#k-means
library(NbClust)
library(rattle)
library(dplyr)

#compute the number of clusters , 적절한 군집수를 추천하는 패키지
data_clu <- data[, c(-1, -2, -9)]
n_c <- NbClust(data_clu, max.nc = 15, method = 'kmeans')
table(n_c$Best.nc[1,])
barplot(table(n_c$Best.nc[1,]), xlab = 'number of clusters',
        ylab = 'num of criteria', ylim=c(0,20))


# normalization
data.clu.norm <- scale(data_clu)


# standardization
std.preproc <- caret::preProcess(data_clu, method=c('range'))
data.std <- predict(std.preproc, data_clu)
data.std


# kemans() modelling
data.kms <- kmeans(data.clu.norm, 3)
data.kms$size

# plotting
par(mfrow=c(1,1))
plot(data.clu.norm, col = data.kms$cluster)
points(data.kms$centers, col = 1:3, pch = 8, cex = 1.5)

data.kms$cluster

data_compare <- data
data_compare$cluster <- data.kms$cluster
View(data_compare)

###################### Random Forest ################################
library(adabag)
library(caret)
library(e1071)
library(randomForest)

# cafe 뺀거
y_rf <- y_data[, c(-1, -2, -9)]
y_data
head(y_data)
# cafe, wqg 뺀거
y_rf <- y_data[, c(-1, -2, -4, -8, -9)]

y_rf$class <- as.factor(y_rf$class)
y_rf
# boosting1
set.seed(500)

data.rf <- randomForest(class~ ., data = y_rf,
                        ntree = 100, mtry = sqrt(4), importance = T)

data.rf

# prediction
pred_dia_rf <- predict(data.rf, newdata = y_rf)
pred_dia_rf

# model accuracy
table(pred_dia_rf, test_dia$price1)
caret::confusionMatrix(as.factor(pred_dia_rf), test_dia$price1)

# importance
importance(data.rf)

# importance(분산값 이용)
varImpPlot(data.rf, main='varImpoPlot of Iris')


## clustering - no cafe
# columb 빼기
y_nocol <- y_data[, c(-1, -2, -4, -8, -9, -10)]

#k-means
library(NbClust)
library(rattle)
library(dplyr)

#compute the number of clusters , 적절한 군집수를 추천하는 패키지
n_c_nocafe <- NbClust(y_nocafe, max.nc = 15, method = 'kmeans')
table(n_c_nocafe$Best.nc[1,])
barplot(table(n_c$Best.nc[1,]), xlab = 'number of clusters',
        ylab = 'num of criteria', ylim=c(0,20))


# normalization
data.nocol.norm <- scale(y_nocol)


# standardization
std.preproc <- caret::preProcess(y_nocol, method=c('range'))
data.nocol.std <- predict(std.preproc, y_nocol)
data.nocol.std


# kemans() modelling
data.kms <- kmeans(data.nocol.std, 2)
data.kms$cluster

y_rf$cluster <- data.kms$cluster
table(y_rf$class, y_rf$cluster)
table(y_rf$class)

# plotting
par(mfrow=c(1,1))
plot(data.clu.norm, col = data.kms$cluster)
points(data.kms$centers, col = 1:3, pch = 8, cex = 1.5)

data.kms$cluster

data_compare <- data
data_compare$cluster <- data.kms$cluster
View(data_compare)

