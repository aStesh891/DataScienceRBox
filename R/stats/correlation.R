require(ggplot2)
require(readxl)

#1-Student's height/IQ
obj <- read_xlsx(file.choose())

#График
ggplot(obj, aes(height,IQ))+
  geom_point(col="darkblue")

#Проверяем норм распределене
shapiro.test(obj$height)
shapiro.test(obj$IQ)

#Корреляция не значима p-value = 0.7104
cor.test(obj$height, obj$IQ)

###2-Student's Math/Verb success
education <- read_xlsx(file.choose())

#График
ggplot(education, aes(words,Math))+
  geom_point(col="darkblue")

#График 
df.educ <- data.frame(x=c(education[,2]), y=c(education[,3]))

ggplot(df.educ, aes(df.educ$words,df.educ$Math))+
  geom_point(col="darkblue")

#Проверяем норм распределене
shapiro.test(education$words)
shapiro.test(education$Math)

#Корреляция значима p-value = 0.001057
cor.test(education$words, education$Math)

