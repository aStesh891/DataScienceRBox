require(ggplot2)
require(ISLR)
require(plotly)
require(caret)

# Логистическая регрессия - определить вероятность дефолта (в данных переменная default)

data(Default)

#visualize
gop1 <- ggplot(Default, aes(x=income, y=balance, colour=default))+
  geom_point(size=0.5)

ggplotly(gop1)

#split data
# смотрим сколько процентов имели положительный дефолт - нужно соблюсти такие же пропорции в двух выборках
prop.table(table(Default$default))

index <- sample(1:nrow(Default), 0.8*nrow(Default))
train <- Default[index,]
test <- Default[-index,]

prop.table(table(train$default))
prop.table(table(test$default))

#build logical regression
model <- glm(default ~ student+balance+income, data = train,
             family = binomial)
summary(model)

#сколько процентов объясняем
rSquare <- 1 - model$deviance/model$null.deviance
print(cat("R^2 = ", rSquare))

Default$predicted_prob <- predict(model, newdata = test, type = "response")

#   Пример оценки точности модели 
pcut = 0.5 #порог отсечения (выше - не теряем клиента)
predicted_class <- ifelse(Default$predicted_prob > pcut, "Yes", "No")
tt <- table(pred = predicted_class, obs = Default$default)

#Точность модели
accuracy <- (tt[1,1]+tt[2,2]) / sum(tt)

#Специфичность: доля истинных негативных значений
#                - отнесенным ко всем предсказанным негативным
specificity <- (tt[2,1]+tt[2,2]) / sum(tt)
specificity <- caret::specificity(tt)

#Чувствительность: доля правильно предсказанных положительных результатов
#                  ко всем предсказанным моделью положительным результатам)
sens <- caret::sensitivity(tt)






