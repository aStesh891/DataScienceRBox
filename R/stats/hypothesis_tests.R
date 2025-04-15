require(ggplot2)

#5.1. Критерий Стьюдента - Гипотеза о равенстве средних двух генеральных совокупностей

#данные по суточному потреблению энергии (кДж/сутки) для 11 женщин
d.intake <- c(5260, 5470, 5640, 6180, 6390, 6515,
              6805, 7515, 7515, 8230, 8770)
mean(d.intake)

#отличается ли это выборочное среднее значение от установленной нормы в 7725 кДж/сутки?
t.test(d.intake, mu = 7725)

#Сравнение двух независимых выборок
#Рассмотрим пример о суточном расходе энергии (expend) у худощавых женщин (lean) и женщин с избыточным весом (obese)
library(ISwR)
data(energy)
attach(energy)
head(energy)

tapply(expend, stature, mean)

#по умолчанию дисперсии сравниваемых совокупностей не равны выполняется t-тест в модификации Уэлча (меняет var.equal)
t.test(expend ~ stature)
t.test(expend ~ stature, var.equal = TRUE)


#Сравнение двух зависимых выборок
#пример о суточном потреблении энергии, измеренном уже у одних и тех же 11 женщин до и после периода менструаций:
data(intake) # из пакета ISwR
attach(intake)
head(intake, n = 5)

mean(post - pre)
t.test(pre, post, paired = TRUE)

#5.6. Оценка корреляции двух случайных величин
dat <- read.delim("http://figshare.com/media/download/98923/97987")
attach(dat)
cor.test(CAnumber, ZMlength)

#5.7. Критерий хи-квадрат
mice <- matrix(c(13, 44, 25, 29), nrow = 2, byrow = TRUE)
mice # просмотр содержимого матрицы

chisq.test(mice) # тест хи-квадрат

#Критерий хи-квадрат для таблиц сопряженности размером больше 2/2
#исследование трех популяций наземных моллюсков, в ходе которого отмечали цвет раковины

light <- c(12, 40, 45)
dark <- c(87, 34, 75)
very.dark <- c(3, 8, 2)

color.data <- matrix(c(light, dark, very.dark), nrow = 3,
                     dimnames = list(c("Pop1", "Pop2", "Pop3"),
                                     c("Light", "Dark", "Very dark")))
color.data

#отклоняем Н0 об отсутствии разницы между популяциями моллюска по частотам встречаемости разных вариантов окраски раковины.
chisq.test(color.data)

#5.8. Точный тест Фишера. Критерии Мак-Немара и Кохрана-Мантеля-Хензеля

X <- matrix(c(1, 10, 8, 4), ncol = 2)
fisher.test(X)


data <- read.table(header=TRUE, text='
 subject time result
       1  pre      0
       1 post      1
       2  pre      1
       2 post      1
       3  pre      0
       3 post      1
       4  pre      1
       4 post      0
       5  pre      1
       5 post      1
       6  pre      0
       6 post      1
       7  pre      0
       7 post      1
       8  pre      0
       8 post      1
       9  pre      0
       9 post      1
      10  pre      1
      10 post      1
      11  pre      0
      11 post      0
      12  pre      1
      12 post      1
      13  pre      0
      13 post      1
      14  pre      0
      14 post      0
      15  pre      0
      15 post      1
')

# Преобразуем данные в "широкий формат":
install.packages("reshape2")
library(reshape2)

data.wide <- dcast(data, subject ~ time, value.var="result")
data.wide

#Cводим в таблицу сопряженности
ct <- table(data.wide[, c("pre", "post")])
ct

#При больших объемах выборок (50+) более точное приближение достигается при помощи поправки Эдвардса (вкл по умолчанию)
mcnemar.test(ct)
mcnemar.test(ct, correct = FALSE)

#Критерий Кохрана-Мантеля-Хензеля для таблиц сопряженности размером 2*2*K

drug <-
  array(c(11, 10, 25, 27,
          16, 22, 4, 10,
          14, 7, 5, 12,
          2, 1, 14, 16,
          6, 0, 11, 12,
          1, 0, 10, 10,
          1, 1, 4, 8,
          4, 6, 2, 1),
        dim = c(2, 2, 8),
        dimnames = list(
          Group = c("Drug", "Control"),
          Response = c("Success", "Failure"),
          Center = c("1", "2", "3", "4", "5", "6", "7", "8")))
drug

#исход лечения оказался статистически значимо связанным с тем,получали ли испытуемые новый препарат
mantelhaen.test(drug)

#Графически изображаем данные
library(reshape) # для функции melt()
drug.df <- data.frame(melt(drug,
                           id = c("Center", "Group", "Response")))

library(ggplot2)
p <- ggplot(data = drug.df,
            aes(x = Center, y = value, fill = Response)) + 
  ylab("Fraction")

p + geom_bar(stat = "identity", position = "fill") +
  facet_grid(Group~.)



















