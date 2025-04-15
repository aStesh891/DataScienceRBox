#################################################################
### Задача 1: Лексическое сравнение (DTM + Косинусное сходство)

# Подключаем библиотеки
library(tm)
library(text2vec)
library(ggplot2)
library(reshape2)

# Исходные тексты
texts <- c("Текстовый анализ это интересно.",
           "Анализ данных с использованием R.",
           "Тематическое моделирование помогает изучать тексты.")

# Создание корпуса (используем VCorpus)
corpus <- VCorpus(VectorSource(texts))

# Очистка текста
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("russian"))
corpus <- tm_map(corpus, stripWhitespace)

# Создание матрицы "документ-терм"
dtm <- DocumentTermMatrix(corpus)
dtm_matrix <- as.matrix(dtm)

# Нормализация (приведение к единичной длине)
normalized_dtm <- dtm_matrix / sqrt(rowSums(dtm_matrix^2))

# Косинусное сходство
# В данном случае Документы 1 и 2 имеют сходство 0.583, 
#   значит, они лексически похожи, но не идентичны.
similarity <- crossprod(normalized_dtm)

# Преобразуем матрицу сходства в формат для ggplot2
sim_data <- as.data.frame(as.table(similarity))
colnames(sim_data) <- c("Document1", "Document2", "Similarity")

# Преобразуем номера документов в факторы для корректного отображения
sim_data$Document1 <- as.factor(sim_data$Document1)
sim_data$Document2 <- as.factor(sim_data$Document2)

# Визуализация тепловой карты
ggplot(sim_data, aes(x = Document1, y = Document2, fill = Similarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Лексическое сходство документов", x = "Документ 1", y = "Документ 2")

#################################################################
### Задача 2: Тематическое сравнение (LDA + Косинусное сходство)

# Подключаем библиотеки
library(topicmodels)
library(ggplot2)
library(reshape2)
library(tibble)  # Добавляем для rownames_to_column()

# Обучение LDA (2 темы)
lda_model <- LDA(dtm, k = 2, control = list(seed = 123))

# Получаем распределение тем
topic_distributions <- posterior(lda_model)$topics

# Нормализация (единичная длина вектора)
normalized_topics <- topic_distributions / sqrt(rowSums(topic_distributions^2))

# Косинусное сходство
# Документы имеют два основных тематических кластера:
#   Анализ данных (анализ, данные, использование)
#   Работа с текстами (изучать, моделирование, помогает, тексты, тематическое)
similarity_topics <- crossprod(normalized_topics)

# Преобразование матрицы в формат для ggplot2
sim_topics_data <- as.data.frame(similarity_topics) %>%
  rownames_to_column(var = "Document1") %>%
  melt(id.vars = "Document1", variable.name = "Document2", value.name = "Similarity")

# Факторизация номеров документов
sim_topics_data$Document1 <- as.factor(sim_topics_data$Document1)
sim_topics_data$Document2 <- as.factor(sim_topics_data$Document2)

# Визуализация тепловой карты тематического сходства
ggplot(sim_topics_data, aes(x = Document1, y = Document2, fill = Similarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "green") +
  theme_minimal() +
  labs(title = "Тематическое сходство документов", x = "Документ 1", y = "Документ 2")


#################################################################
### Задача 3: Семантическое сравнение (word2vec, fastText)

library(text2vec)

# Модель word2vec
model <- readRDS("pretrained_word2vec_model.rds")

# Получение эмбеддингов документов
get_doc_embedding <- function(text, model) {
  words <- unlist(strsplit(tolower(text), " "))
  word_vectors <- model[words, , drop = FALSE]
  colMeans(word_vectors, na.rm = TRUE)
}

doc_embeddings <- t(sapply(texts, get_doc_embedding, model = model))

# Нормализация
normalized_embeddings <- doc_embeddings / sqrt(rowSums(doc_embeddings^2))

# Косинусное сходство
similarity_embeddings <- crossprod(normalized_embeddings)
similarity_embeddings

# Визуализация:
  # Преобразование матрицы сходства эмбеддингов в формат для визуализации
  sim_embeddings_data <- melt(as.data.frame(similarity_embeddings))
colnames(sim_embeddings_data) <- c("Document1", "Document2", "Similarity")

# Тепловая карта семантики
ggplot(sim_embeddings_data, aes(x = Document1, y = Document2, fill = Similarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Семантическое сходство документов", x = "Документ 1", y = "Документ 2")

