library(tm)
library(lsa)
library(cluster)
library(ggplot2)
library(factoextra)
library(reshape2)
library(text2vec)
library(Matrix)
library(plotly)  # Добавляем для 3D визуализации

# Определяем мешки слов

bag1 = c("quantum", "mechanics", "particle", "wave", "superposition", "entanglement", 
         "spin", "boson", "fermion", "uncertainty", "Heisenberg", "orbital", 
         "spectrum", "absorption", "emission", "tunneling", "field", "resonance", 
         "photon", "mass", "outliers", "distribution", "probability", "histogram")
bag2 = c("wheat", "grain", "harvest", "farmer", "agronomist", "sowing", "drought", 
         "humidity", "fertilizer", "herbicide", "price", "market", "contract", 
         "export", "import", "demand", "supply", "yield", "irrigation", "pesticide")
bag3 = c("data", "algorithm", "model", "machine", "learning", "statistics", 
         "correlation", "regression", "clustering", "classification", "anomaly", 
         "visualization", "prediction", "forecast", "processing", "collection", 
         "outliers", "distribution", "probability", "histogram",
         "gradient descent", "loss function", "metric", "accuracy")
bag4 = c("feature engineering", "dataset", "training", "test", "neural network", 
         "gradient descent", "loss function", "metric", "accuracy", "recall", 
         "f1-score", "auc-roc", "cross-validation", "boosting", "hyperparameter tuning", 
         "pipeline", "balancing", "noise", "overfitting", "embedding", 
         "algorithm", "model", "machine", "learning")

##############################################################
# Создание корпуса текстов
corpus <- c(bag1, bag2, bag3, bag4)
# Токенизация без разбиения фраз
it <- itoken(corpus, preprocessor = tolower, tokenizer = identity)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)

##############################################################
# Разделение на мешки слов
dtm_bag1 <- dtm[1:length(bag1), ]
dtm_bag2 <- dtm[(length(bag1) + 1):(length(bag1) + length(bag2)), ]
dtm_bag3 <- dtm[(length(bag1) + length(bag2) + 1):(length(bag1)+length(bag2)+length(bag3)), ]
dtm_bag4 <- dtm[(length(bag1)+length(bag2)+length(bag3)+1):nrow(dtm),]

# Преобразование в числовые векторы
bag_vectors <- list(
  bag1 = colSums(dtm_bag1),
  bag2 = colSums(dtm_bag2),
  bag3 = colSums(dtm_bag3),
  bag4 = colSums(dtm_bag4)
)

# Вычисление модулей векторов
bag_mods <- lapply(bag_vectors, function(vec) sqrt(sum(vec^2)))

# Функция для вычисления косинусной метрики
cosine_metric <- function(vec1, vec2, mod1, mod2) {
  if (mod1 > 0 && mod2 > 0) {
    return(sum(vec1 * vec2) / (mod1 * mod2))
  } else {
    return(NA)
  }
}

# Вычисление попарных косинусных расстояний
bags <- names(bag_vectors)
cosine_matrix <- matrix(NA, length(bags), length(bags), dimnames = list(bags, bags))
for (i in 1:length(bags)) {
  for (j in i:length(bags)) {
    cosine_matrix[i, j] <- cosine_metric(bag_vectors[[bags[i]]], bag_vectors[[bags[j]]], 
                                         bag_mods[[bags[i]]], bag_mods[[bags[j]]])
    cosine_matrix[j, i] <- cosine_matrix[i, j]
  }
}
# Вывод матрицы на экран
print(cosine_matrix)

# Визуализация гистограммы косинусных расстояний
cosine_distances <- as.vector(cosine_matrix)
ggplot(data.frame(Distance = cosine_distances), aes(x = Distance)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Гистограмма косинусных расстояний между мешками слов", x = "Косинусное расстояние", y = "Частота") +
  theme_minimal()

# --- 3D Визуализация ---
plot_ly(z = ~cosine_matrix, type = "surface") %>%
  layout(title = "3D представление косинусных расстояний")


##############################################################
# --- Создание таблицы пар расстояний ---
bag_pairs <- combn(bags, 2, FUN = function(pair) paste(pair[1], "-", pair[2]))
cosine_distances <- combn(bags, 2, FUN = function(pair) {
  cosine_metric(bag_vectors[[pair[1]]], bag_vectors[[pair[2]]],
                    bag_mods[[pair[1]]], bag_mods[[pair[2]]])
})

# Формируем датафрейм для визуализации
cosine_data <- data.frame(
  Pair = bag_pairs,
  Distance = cosine_distances
)

# --- Визуализация гистограммы с парами ---
ggplot(cosine_data, aes(x = Pair, y = Distance, fill = Pair)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  labs(title = "Гистограмма косинусных расстояний между мешками слов",
       x = "Пары мешков",
       y = "Косинусное расстояние") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# --- Улучшение 3D визуализации ---
library(reshape2)

# Преобразуем матрицу в формат long
cosine_matrix_long <- melt(cosine_matrix)
colnames(cosine_matrix_long) <- c("Bag1", "Bag2", "Distance")

# Создание 3D-графика
plot_ly(
  data = cosine_matrix_long,
  x = ~Bag1, y = ~Bag2, z = ~Distance,
  type = "scatter3d", mode = "markers",
  marker = list(size = 5, color = ~Distance, colorscale = "Viridis")
) %>%
  layout(
    title = "3D представление косинусных расстояний между мешками слов",
    scene = list(
      xaxis = list(title = "Bag 1"),
      yaxis = list(title = "Bag 2"),
      zaxis = list(title = "Косинусное расстояние")
    )
  )
######################################################################
# --- Вычисление LSA-метрики ---
library(text2vec)

# Определение корпуса текстов
corpus <- c(bag1, bag2, bag3, bag4)

# Токенизация
it <- itoken(corpus, preprocessor = tolower, tokenizer = identity)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)

# Преобразование DTM в TF-IDF
tfidf_model <- TfIdf$new()
tfidf <- tfidf_model$fit_transform(dtm)

# Проверка матрицы TF-IDF
print(tfidf)


# Применение LSA (сингулярное разложение) для получения скрытых семантических значений
lsa_model <- lsa(tfidf, dimcalc_share(0.9))  # Сохраняем 90% информации
lsa_space <- lsa_model$tk %*% diag(lsa_model$sk)

# Выделение мешков в пространстве LSA
lsa_bag1 <- lsa_space[1:length(bag1), ]
lsa_bag2 <- lsa_space[(length(bag1) + 1):(length(bag1) + length(bag2)), ]
lsa_bag3 <- lsa_space[(length(bag1) + length(bag2) + 1):(length(bag1)+length(bag2)+length(bag3)), ]
lsa_bag4 <- lsa_space[(length(bag1)+length(bag2)+length(bag3)+1):nrow(lsa_space), ]

# Преобразование в числовые векторы
lsa_vectors <- list(
  bag1 = colSums(lsa_bag1),
  bag2 = colSums(lsa_bag2),
  bag3 = colSums(lsa_bag3),
  bag4 = colSums(lsa_bag4)
)

# Вычисление модулей векторов в пространстве LSA
lsa_mods <- lapply(lsa_vectors, function(vec) sqrt(sum(vec^2)))

# Функция для вычисления LSA-метрики
lsa_metric <- function(vec1, vec2, mod1, mod2) {
  if (mod1 > 0 && mod2 > 0) {
    return(sum(vec1 * vec2) / (mod1 * mod2))
  } else {
    return(NA)
  }
}

# Вычисление попарных LSA расстояний
lsa_matrix <- matrix(NA, length(bags), length(bags), dimnames = list(bags, bags))
for (i in 1:length(bags)) {
  for (j in i:length(bags)) {
    lsa_matrix[i, j] <- lsa_metric(lsa_vectors[[bags[i]]], lsa_vectors[[bags[j]]], 
                                   lsa_mods[[bags[i]]], lsa_mods[[bags[j]]])
    lsa_matrix[j, i] <- lsa_matrix[i, j]
  }
}

# Вывод матрицы на экран
print(lsa_matrix)

# --- Визуализация LSA метрики ---
lsa_distances <- as.vector(lsa_matrix)
lsa_data <- data.frame(
  Pair = bag_pairs,
  Distance = combn(bags, 2, FUN = function(pair) {
    lsa_metric(lsa_vectors[[pair[1]]], lsa_vectors[[pair[2]]],
               lsa_mods[[pair[1]]], lsa_mods[[pair[2]]])
  })
)

# --- Гистограмма ---
ggplot(lsa_data, aes(x = Pair, y = Distance, fill = Pair)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  labs(title = "Гистограмма LSA-расстояний между мешками слов",
       x = "Пары мешков",
       y = "LSA расстояние") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# --- 3D Визуализация LSA ---
lsa_matrix_long <- melt(lsa_matrix)
colnames(lsa_matrix_long) <- c("Bag1", "Bag2", "Distance")

plot_ly(
  data = lsa_matrix_long,
  x = ~Bag1, y = ~Bag2, z = ~Distance,
  type = "scatter3d", mode = "markers",
  marker = list(size = 5, color = ~Distance, colorscale = "Viridis")
) %>%
  layout(
    title = "3D представление LSA-расстояний между мешками слов",
    scene = list(
      xaxis = list(title = "Bag 1"),
      yaxis = list(title = "Bag 2"),
      zaxis = list(title = "LSA расстояние")
    )
  )



################################ GloVe #####################################

library(data.table)

# Загружаем предобученную модель GloVe (50-мерная)
glove_file <- "glove.6B.50d.txt"   
glove_raw <- fread(glove_file, header = FALSE, quote = "", sep = " ", data.table = FALSE)

# Преобразуем в матрицу
words <- glove_raw[, 1]  
vectors <- as.matrix(glove_raw[, -1])  
rownames(vectors) <- words

# Создаём список мешков слов с названиями
bags <- list("Physics" = bag1, "Agriculture" = bag2, "Data Analysis 1" = bag3, "Data Analysis 2" = bag4)

# Функция для получения среднего эмбеддинга мешка слов
get_mean_vector <- function(words, model) {
  words <- words[words %in% rownames(model)]  # Фильтруем слова, которых нет в модели
  if (length(words) == 0) return(rep(0, ncol(model)))  # Если слов нет, возвращаем 0-вектор
  vectors <- model[words, , drop = FALSE]  # Получаем векторы
  return(colMeans(vectors))  # Средний вектор
}

# Вычисляем средние векторы для каждого мешка
bag_vectors <- do.call(rbind, lapply(bags, get_mean_vector, model = vectors))
rownames(bag_vectors) <- names(bags)  # Добавляем имена строк

# Функция для косинусного сходства
cosine_similarity <- function(A, B) {
  sum(A * B) / (sqrt(sum(A^2)) * sqrt(sum(B^2)))
}

# Вычисляем попарное косинусное сходство между мешками
sim_matrix <- outer(1:nrow(bag_vectors), 1:nrow(bag_vectors), 
                    Vectorize(function(i, j) cosine_similarity(bag_vectors[i, ], bag_vectors[j, ])))

# Преобразуем в формат для визуализации
sim_df <- melt(sim_matrix)
colnames(sim_df) <- c("Bag1", "Bag2", "Similarity")

# Преобразуем индексы в имена мешков
sim_df$Bag1 <- factor(rownames(bag_vectors)[sim_df$Bag1], levels = names(bags))
sim_df$Bag2 <- factor(rownames(bag_vectors)[sim_df$Bag2], levels = names(bags))

# Статичная визуализация с числами
ggplot(sim_df, aes(x = Bag1, y = Bag2, fill = Similarity)) +
  geom_tile() +
  geom_text(aes(label = round(Similarity, 2)), color = "black", size = 5) +  # Добавляем числа
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Semantic Similarity Between Word Bags", x = "Bag", y = "Bag") +
  theme_minimal()

# Интерактивная тепловая карта через plotly
plotly_heatmap <- plot_ly(
  x = names(bags), 
  y = names(bags), 
  z = sim_matrix, 
  type = "heatmap", 
  colorscale = "Blues", 
  text = round(sim_matrix, 2),  # Числовые значения в подсказке
  hoverinfo = "text"
)

plotly_heatmap






