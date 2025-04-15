require(ggplot2)
require(readxl)

#1 Пример создания тестовых данных для норм распределения
obj <- rnorm(150, mean=5, sd=3)

#График норм распределения
dat_fr <- data.frame(x=1:150, y=obj)
ggplot(dat_fr, aes(x=x, y=y))+
  geom_point(col="darkblue")+
  geom_smooth(col="darkred")

#Тесты на норм распределение прошли
shapiro.test(obj)
nortest::lillie.test(obj)

#2 Пример создания тестовых данных для пуассоновского распределения
obj1 <- rpois(150, 5) + 25

#График пуас. распределения
dat_fr1 <- data.frame(x=1:150, y=obj1)
ggplot(dat_fr1, aes(x=x, y=y))+
  geom_point(col="darkblue")+
  geom_smooth(col="darkred")

#Тесты на норм распределение не прошли
shapiro.test(obj1)
nortest::lillie.test(obj1)

#Тест Колмогорова-Смирнова (для сравнения двух эмпирических распределений)
#p-value < 2.2e-16 -> H1: выборки из разных распределений 
ks.test(obj, obj1)


#Пример проверки из какого распределения данные сравнивая выборку 
#с эталоном(примером проверяемого распределения)
pressure <- read_xlsx(file.choose())
colnames(pressure) <- "Press"

m <- mean(pressure$Press)
sd <- sd(pressure$Press)
#
etalon <- rnorm(nrow(pressure), mean=m, sd=sd)

ks.test(pressure, etalon)
