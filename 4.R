library('GGally')      
library('lmtest')
library('ISLR')
library('FNN')
my.seed <- 12345
train.percent <- 0.85

?Carseats
data('Carseats')
head(Carseats)
str(Carseats)

set.seed(my.seed)
Carseats$Urban <- as.factor(Carseats$Urban)
inTrain <- sample(seq_along(Carseats$Sales), 
                  nrow(Carseats) * train.percent)
df.train <- Carseats[inTrain, c(1,5,6,10)]
df.test <- Carseats[-inTrain, c(1,5,6,10)]
# описательные статистики по переменным
summary(df.train)

# совместный график разброса переменных
ggp <- ggpairs(df.train)
print(ggp, progress = F)

# цвета по фактору Urban
ggp <- ggpairs(df.train, 
               mapping = ggplot2::aes(color = Urban))
print(ggp, progress = F)



# Модели
model.1 <- lm(Sales ~ Price + Urban + Population,
              data = df.train)
summary(model.1)

#построим матрицу корреляции
Carseats$Urban <- as.numeric(Carseats$Urban)
cor(Carseats[, c(1,5,6,10)])

# исключаем незначимые
model.2 <- lm(Sales ~ Price,
              data = df.train)
summary(model.2)

#Проверка остатков

# тест Бройша-Пагана
bptest(model.2)

# статистика Дарбина-Уотсона
dwtest(model.2)

# графики остатков
par(mar = c(4.5, 4.5, 2, 1))
par(mfrow = c(1, 3))
plot(model.2, 1)
plot(model.2, 4)
plot(model.2, 5)
par(mfrow = c(1, 1))

# фактические значения y на тестовой выборке
y.fact <- Carseats[-inTrain, 1]
y.model.lm <- predict(model.2, df.test)
MSE.lm <- sum((y.model.lm - y.fact)^2) / length(y.model.lm)

# kNN требует на вход только числовые переменные
df.train.num <- as.data.frame(apply(df.train[1:3], 2, as.numeric))
df.test.num <- as.data.frame(apply(df.test[1:3], 2, as.numeric))

for (i in 2:50){
  model.knn <- knn.reg(train = df.train.num[, !(colnames(df.train.num) %in% 'Sales')], 
                       y = df.train.num[, 'Sales'], 
                       test = df.test.num[, -1], k = i)
  y.model.knn <- model.knn$pred
  if (i == 2){
    MSE.knn <- sum((y.model.knn - y.fact)^2) / length(y.model.knn)
  } else {
    MSE.knn <- c(MSE.knn, 
                 sum((y.model.knn - y.fact)^2) / length(y.model.knn))
  }
}

# график
par(mar = c(4.5, 4.5, 1, 1))
plot(2:50, MSE.knn, type = 'b', col = 'darkgreen',
     xlab = 'значение k', ylab = 'MSE на тестовой выборке')
lines(2:50, rep(MSE.lm, 49), lwd = 2, col = grey(0.2), lty = 2)
legend('bottomright', lty = c(1, 2), pch = c(1, NA), 
       col = c('darkgreen', grey(0.2)), 
       legend = c('k ближайших соседа', 'регрессия (все факторы)'), 
       lwd = rep(2, 2))

