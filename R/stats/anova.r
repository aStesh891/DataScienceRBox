# Однофакторный дисперсионный анализ на задаче с томатами

# Создадим таблицу с данными:
tomato <- data.frame(weight =
                             c(1.5, 1.9, 1.3, 1.5, 2.4, 1.5, # water
                               1.5, 1.2, 1.2, 2.1, 2.9, 1.6, # nutrient
                               1.9, 1.6, 0.8, 1.15, 0.9, 1.6), # nutrient+24D
                     trt = rep(c("Water", "Nutrient", "Nutrient+24D"),
                               c(6, 6, 6)))

# Таблица со средними значениями:
Means <- data.frame(weight = as.numeric(tapply(tomato$weight,
                                               tomato$trt, mean)),
                    trt = rep("Means", 3))

# Добавляем таблицу Means к таблице tomato:
tomato <- rbind(tomato, Means)

# Изменяем базовый уровень фактора trt на Water:
tomato$trt <- relevel(as.factor(tomato$trt), ref = "Water")

# 
# Рисуем исходную диаграмму (все точки из группы Means будут при этом
# автомтически залиты черным цветом):
stripchart(weight ~ trt, data = tomato, pch = 19,
           col = c("blue", "red", "black"),
           ylab = "Условия", xlab = "Вес (кг)")

# Добавляем поверх точек из группы Means точки нужного цвета
# (эти новые точки имеют координаты (средняя_1, 4), (средняя_2, 4)
# и (средняя_3, 4); их цвет прописываем "вручную"):
points(x = Means$weight, y = c(4, 4, 4), pch = 19,
       col = c("red", "black", "blue"))

# 
# Измененные данные
tomato2 <-
        data.frame(weight =
                           c(1.25, 1.9, 1.3, 1.5, 2.3, 1.45, # water
                             2.1, 2.2, 2.4, 2.9, 2.8, 3.1, # nutrient
                             0.9, 1.2, 0.8, 1.3, 0.7, 1.4), # nutrient+24D
                   trt = rep(c("Water", "Nutrient", "Nutrient+24D"),
                             c(6, 6, 6)))

# Таблица со средними значениями:
Means2 <- data.frame(weight = as.numeric(tapply(tomato2$weight,
                                                tomato2$trt, mean)),
                     trt = rep("Means", 3))

# Добавляем таблицу Means к таблице tomato:
tomato2 <- rbind(tomato2, Means2)

# Изменяем базовый уровень фактора trt на Water:
tomato2$trt <- relevel(as.factor(tomato2$trt), ref = "Water")

# Рисуем исходную диаграмму (все точки из группы Means будут при этом
# автомтически залиты черным цветом):
stripchart(weight ~ trt, data = tomato2, pch = 19,
           col = c("blue", "red", "black"),
           ylab = "Условия", xlab = "Вес (кг)")

# Добавляем поверх точек из группы Means точки нужного цвета
# (эти новые точки имеют координаты (средняя_1, 4), (средняя_2, 4)
# и (средняя_3, 4); их цвет прописываем "вручную"):
points(x = Means2$weight, y = c(4, 4, 4), pch = 19,
       col = c("red", "black", "blue"))

# 
x = seq(0, 10, 0.1)
plot(x, df(x, 2, 15), type = "l")
abline(v = qf(0.95, 2, 15), lty = 2)

# Выполнение однофакторного дисперсионного анализа:
summary(aov(weight ~ trt, data = tomato))

# Интерпретация результатов:
# F-критерий = 0.885 (Если F ≈ 1, значит группы не отличаются)
#     У нас F < 1 - влияние фактора trt (обработка) на вес томатов не значимо.
# p-value (Pr(>F)) = 0.469 (Если p < 0.05, различия статистически значимы)
#     Здесь p > 0.05, значит, различий нет.


# Двухфакторный дисперсионный анализ
library(HSAUR2)
data(weightgain)
str(weightgain)

library(ggplot2)
ggplot(data = weightgain, aes(x = type, y = weightgain)) + 
        geom_boxplot(aes(fill = source))

require(doBy)
summaryBy(weightgain ~ type + source, data = weightgain,
          FUN = c(mean, sd, length))

plot.design(weightgain) # 

# 
with(weightgain, interaction.plot(x.factor = type,
                                  trace.factor = source,
                                  response = weightgain))

# Двухфакторный дисперсионный анализ:
M1 <- aov(weightgain ~ source + type + source:type, 
          data = weightgain)
summary(M1)



##################################################################################
# Пример на ирисах: анализ размеров лепестков и чашелистиков у разных видов ирисов
##################################################################################
head(iris)

# Однофакторный дисперсионный анализ на виде-длине чашелистиков

# Создадим таблицу с данными:
iris_data <- data.frame(
  length = iris$Sepal.Length,
  species = iris$Species       # Вид ириса
)

# Таблица со средними значениями:
Means_iris <- data.frame(length = as.numeric(tapply(iris_data$length, 
                                                    iris_data$species, mean)),
  species = rep("Means", 3)  # Добавляем строку "Means" как среднее
)

# Добавляем таблицу Means к таблице iris_data:
iris_data <- rbind(iris_data, Means_iris)

# Изменяем базовый уровень фактора species на setosa:
iris_data$species <- relevel(as.factor(iris_data$species), ref = "setosa")  # Базовый уровень - setosa

# Визуализация данных
stripchart(length ~ species, data = iris_data, pch = 19,
           col = c("blue", "red", "black"),
           ylab = "Вид ириса", xlab = "Длина чашелистика (см)")

# Добавляем цвет средние значения
points(x = Means_iris$length, y = c(4, 4, 4), pch = 19,
       col = c("red", "black", "blue"))


# Дисперсионный анализ (ANOVA)
#  Перед ANOVA нужно оставить только реальные группы!!
iris_data_filtered <- subset(iris_data, species != "Means")

anova_model <- aov(length ~ species, data = iris_data_filtered)
summary(anova_model)
# Интерпретация: Если p < 0.05, длина чашелистика различается между видами.
#                Если p > 0.05, вид не влияет на длину чашелистика.


#############################################################################
# Тест Тьюки (TukeyHSD) - какие пары групп (Species) имеют значимые различия
#############################################################################

# Применяем тест Тьюки
tukey_results <- TukeyHSD(anova_model)

# Вывод результатов: если p-value < 0.05 для пары видов - между ними значимые различия
print(tukey_results)

# Визуализация результатов теста Тьюки - график показывает доверительные интервалы:
#   Если интервал не пересекает 0, то разница статистически значима.
#   Если пересекает 0 — значимых различий нет.

# Увеличиваем размер графического окна (иначе ошибка figure margins too large)
par(mar = c(5, 10, 4, 2))  # Настройка отступов (снизу, слева, сверху, справа)

plot(tukey_results, las = 1)







