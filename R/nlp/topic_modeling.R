# Установка и подключение необходимых пакетов
install.packages(c("text2vec", "tm", "ggplot2", "reshape2"))
library(text2vec)
library(tm)
library(ggplot2)
library(reshape2)


# Определяем мешки слов
bags <- list(
  "Квантовая физика" = c(
    "квант", "механика", "частица", "волна", "суперпозиция", "запутанность",
    "спин", "бозон", "фермион", "неопределенность", "гейзенберг", "орбиталь",
    "спектр", "поглощение", "излучение", "туннелирование", "поле", "резонанс",
    "фотон", "масса", "энергия", "измерение", "атом", "молекула", "протон",
    "нейтрон", "электрон", "двойной", "эксперимент", "коллапс", "волновая",
    "функция", "наблюдатель", "интерференция", "дифракция", "вероятность",
    "оператор", "матрица", "энтропия", "бозе-эйнштейн", "конденсат",
    "бозон", "хиггса", "космология", "темная", "материя", "аннигиляция",
    "калибровка", "симметрия", "спонтанная"
  ),
  "Рынок пшеницы" = c(
    "пшеница", "зерно", "урожай", "фермер", "агроном", "посев", "засуха",
    "влажность", "удобрение", "гербицид", "цена", "биржа", "контракт",
    "экспорт", "импорт", "спрос", "предложение", "трейдер", "производство",
    "переработка", "элеватор", "мельница", "глютен", "белок", "качество",
    "сорта", "климат", "логистика", "доставка", "субсидии", "квота",
    "налог", "стоимость", "урожайность", "механизация", "техника",
    "автоматизация", "биотехнологии", "устойчивость", "торговля",
    "биржевая", "котировка", "доллар", "евро", "продовольствие",
    "аналитика"
  ),
  "Анализ данных 1" = c(
    "данные", "алгоритм", "модель", "машинное", "обучение", "статистика",
    "корреляция", "регрессия", "кластеризация", "классификация",
    "аномалия", "визуализация", "предсказание", "прогноз", "обработка",
    "сбор", "очистка", "выбросы", "распределение", "вероятность",
    "гистограмма", "медиана", "среднее", "мода", "стандартное",
    "отклонение", "байес", "оптимизация", "overfitting", "underfitting",
    "решающее", "дерево", "нейронная", "сеть", "градиентный", "бустинг",
    "pca", "big data", "sql", "хранилище", "аналитика", "дашборд",
    "кластер", "api"
  ),
  "Анализ данных 2" = c(
    "feature engineering", "набор данных", "тренировочный", "тестовый",
    "нейросеть", "градиентный спуск", "функция потерь", "метрика", 
    "точность", "recall", "f1-score", "auc-roc", "кросс-валидация",
    "бустинг", "ранжирование", "настройка гиперпараметров", "pipeline",
    "балансировка", "шум", "переобучение", "генерация признаков",
    "фильтрация", "обогащение данных", "интерпретируемость",
    "shapley values", "tensorflow", "pytorch", "rnn", "cnn",
    "трансформер", "оптимизатор", "adam", "sgd", "momentum",
    "обучение без учителя", "разреженные данные", "эмбеддинги",
    "n-gram", "косинусное сходство", "word2vec", "fasttext",
    "bert", "gpt", "h2o", "spark", "kafka"
  )
)


corpus <- VCorpus(VectorSource(bags))

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


# Функция вычисления метрики Говера
hoover_metric <- function(v1, v2) {
  return(sum(abs(v1 - v2)) / sum(v1 + v2))
}

hoover_1_4 <- hoover_metric(bags$`Квантовая физика`, bags$`Анализ данных 2`)


# Данные для графика
hoover_values <- data.frame(
  Comparison = c("Квантовая физика vs Анализ данных", 
                 "ынок зерна vs Анализ данных", 
                 "Анализ данных (1)  vs Анализ данных (2)"),
  HooverMetric = c(hoover_1_4, hoover_2_4, hoover_3_4)
)

# Построение столбчатого графика
ggplot(hoover_values, aes(x = Comparison, y = HooverMetric, fill = Comparison)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  scale_fill_manual(values = c("blue", "red", "green")) +  # Цвета столбцов
  theme_minimal() +
  labs(title = "Метрика Говера между тематическими группами",
       x = "Сравниваемые группы",
       y = "Метрика Говера") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  geom_text(aes(label = round(HooverMetric, 3)), vjust = -0.5, size = 5)  # Подписи значений



corpus <- VCorpus(VectorSource(bags))

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
similarity

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











# Преобразуем в DataFrame
bags_df <- stack(bags)
colnames(bags_df) <- c("Word", "Topic")


### Векторизация слов с GloVe

# Загрузка предобученной модели GloVe
glove_model <- text2vec::read.wordvectors("glove.6B.50d.txt", type = "txt")

# Функция для получения вектора слова
get_word_vector <- function(word, model) {
  if (word %in% rownames(model)) {
    return(model[word, ])
  } else {
    return(rep(0, ncol(model)))  # Если слова нет в модели, вернуть нулевой вектор
  }
}

# Преобразуем слова в векторы
word_vectors <- t(sapply(bags_df$Word, get_word_vector, model = glove_model))

# Добавляем информацию о теме
word_vectors_df <- as.data.frame(word_vectors)
word_vectors_df$Topic <- bags_df$Topic


### Анализ сходства тем

# Нормализация векторов
normalize_vector <- function(vec) {
  return(vec / sqrt(sum(vec^2)))
}
normalized_vectors <- t(apply(word_vectors, 1, normalize_vector))

# Вычисление косинусного сходства
similarity_matrix <- crossprod(normalized_vectors)

# Создание DataFrame для визуализации
similarity_df <- as.data.frame(similarity_matrix)
rownames(similarity_df) <- bags_df$Word
colnames(similarity_df) <- bags_df$Word

### Визуализация сходства тем
# Преобразование в длинный формат
sim_long <- melt(similarity_df)
colnames(sim_long) <- c("Word1", "Word2", "Similarity")

# Визуализация тепловой карты
ggplot(sim_long, aes(x = Word1, y = Word2, fill = Similarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Косинусное сходство слов в темах", x = "Слово 1", y = "Слово 2")






# Установка и подключение необходимых библиотек
install.packages(c("text2vec", "slam"))
library(text2vec)
library(slam)

# 🔹 Функция для расчета расстояния Хеллингера
hellinger_distance <- function(p, q) {
  sqrt(0.5 * sum((sqrt(p) - sqrt(q))^2))
}

# 🔹 Функция для подготовки мешков слов (bag-of-words)
prepare_bow <- function(texts, vocab = NULL) {
  it <- itoken(texts, progressbar = FALSE)
  vectorizer <- vocab_vectorizer(vocab)
  dtm <- create_dtm(it, vectorizer)
  dtm <- normalize(dtm, margin = 1)  # Нормализация вероятностей
  return(as.matrix(dtm))
}

# 🔹 Исходные мешки слов
bag1 <- c("квантовая физика волны частицы энергия суперпозиция")
bag2 <- c("рынок пшеницы спрос предложение экспорт импорт цены")
bag3 <- c("анализ данных машинное обучение регрессия кластеризация алгоритмы")
bag4 <- c("анализ данных статистика корреляция прогнозирование выборки")

# Создаем словарь из всех мешков
all_texts <- c(bag1, bag2, bag3, bag4)
vocab <- create_vocabulary(itoken(all_texts, progressbar = FALSE))
vocab <- prune_vocabulary(vocab, term_count_min = 1)

# Генерируем DTM (матрицу мешков слов)
dtm_bags <- prepare_bow(c(bag1, bag2, bag3, bag4), vocab)

# 🔹 Вычисляем расстояние Хеллингера между мешками
hellinger_matrix <- matrix(0, nrow = 4, ncol = 4)

for (i in 1:4) {
  for (j in 1:4) {
    hellinger_matrix[i, j] <- hellinger_distance(dtm_bags[i, ], dtm_bags[j, ])
  }
}

# 🔹 Визуализация матрицы расстояний
heatmap(hellinger_matrix, main = "Hellinger Distance Between Bags", col = heat.colors(10))




library(text2vec)
library(tm)
library(ggplot2)
library(reshape2)
library(data.table)

# Определяем мешки слов
bags <- list(
  "Квантовая физика" = c("квант", "механика", "частица", "волна"),
  "Рынок пшеницы" = c("пшеница", "зерно", "урожай", "фермер"),
  "Анализ данных 1" = c("данные", "алгоритм", "модель", "машинное"),
  "Анализ данных 2" = c("feature engineering", "набор данных", "обучение")
)

# Создаем корпус текстов
corpus <- Corpus(VectorSource(unlist(bags)))

# Очистка текста
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("russian"))
corpus <- tm_map(corpus, stripWhitespace)

# Создание матрицы "документ-терм"
dtm <- DocumentTermMatrix(corpus)
dtm_matrix <- as.matrix(dtm)

# Нормализация
normalized_dtm <- dtm_matrix / sqrt(rowSums(dtm_matrix^2))

# Преобразуем в DataFrame
bags_df <- stack(bags)
colnames(bags_df) <- c("Word", "Topic")

# === Загрузка GloVe ===
glove_path <- "glove.6B.50d.txt"  # Проверь путь к файлу
glove_model <- fread(glove_path, header = FALSE, quote = "", stringsAsFactors = FALSE)

# Разделяем первое слово и вектор
rownames(glove_model) <- glove_model$V1
glove_model$V1 <- NULL
glove_matrix <- as.matrix(glove_model)

# Функция для получения вектора слова
get_word_vector <- function(word, model) {
  if (word %in% rownames(model)) {
    return(model[word, ])
  } else {
    return(rep(0, ncol(model)))
  }
}

# Преобразуем слова в векторы
word_vectors <- t(sapply(bags_df$Word, get_word_vector, model = glove_matrix))

print(word_vectors)






