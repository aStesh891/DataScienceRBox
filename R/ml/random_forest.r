library(randomForest)
library(ggplot2)
library(dplyr)
library(plotly)
require(readxl)

# Загрузка данных "E:/R/data/villa.xls"
villa <- read_xls(file.choose())

# Просмотр структуры данных
str(villa)
summary(villa)

# Удаляем ненужный столбец (идентификатор `N`)
villa <- villa %>% select(-N)

# Разделение данных на тренировочную и тестовую выборки
set.seed(123)
split_ratio <- 0.8  # 80% тренировочных, 20% тестовых данных
train_indices <- sample(1:nrow(villa), size = floor(split_ratio * nrow(villa)))
train_data <- villa[train_indices, ]
test_data <- villa[-train_indices, ]

# Выбор кол-ва деревьев
ntree_values <- c(10, 25, 50, 100, 200, 500)
errors <- sapply(ntree_values, function(ntree) {
  model <- randomForest(Price ~ ., data = train_data, ntree = ntree)
  mean((predict(model, test_data) - test_data$Price)^2)
})
print(errors)
best_ntree <- ntree_values[which.min(errors)]
print(paste("Лучшее количество деревьев:", best_ntree))

# Обучение модели Random Forest
model_rf <- randomForest(
  Price ~ .,
  data = train_data,
  ntree = best_ntree,
  importance = TRUE    # Включение важности переменных
)

print(model_rf)

# Оценка важности переменных (чем больше значение, тем важнее переменная)
importance(model_rf)  #%IncMSE - процент увеличения ошибки; IncNodePurity - вклад переменной в разделение узлов в деревьях
varImpPlot(model_rf)

# Прогнозирование на тестовых данных
test_data$Predicted_Price <- predict(model_rf, test_data)

# Визуализация реальных и предсказанных значений
comparison_plot <- ggplot(test_data, aes(x = Price, y = Predicted_Price)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Диаграмма Шеппарда (Реальные vs Предсказанные значения)",
    x = "Реальные значения (Price)",
    y = "Предсказанные значения (Price)"
  )

ggplotly(comparison_plot)

# Вычисление корреляции между реальными и предсказанными значениями
cor.test(test_data$Price, test_data$Predicted_Price, method = "pearson")

# R^2 измеряет долю дисперсии целевой переменной (Price),которая объясняется моделью
# Модель объясняет 76,48%
(r_squared <- cor(test_data$Price, test_data$Predicted_Price)^2)

# Среднеквадратическая ошибка (измеряет среднюю ошибку предсказания)
# Интерпретация: 53,46. Если сравнить это значение с диапазоном Price 
# (от 5 до 320), это умеренно хорошая ошибка - составляет около 16% от 
# среднего значения Price (среднее ≈ 78,25).
(rmse <- sqrt(mean((test_data$Price - test_data$Predicted_Price)^2)))



