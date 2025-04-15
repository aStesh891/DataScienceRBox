# Загрузка библиотек
library(dplyr)   #для обработки данных и визуализации
require(ggplot2)
require(vegan)
library(readr)

library(caret)   #для обучения модели и использования SVM
library(class)
library(e1071)

library(kernlab) #для кластеризации и дополнительной работы с SVM

library(pROC)    #для построения ROC-кривых

# Выявление фальшивых банкнот
#   Процесс построения и настройки метода опорных векторов (SVM) 
#   для классификации данных о банкнотах

# Загрузка данных из интернета
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt"
columns <- c("Variance", "Skewness", "Curtosis", "Entropy", "Class")
banknotes <- read_csv(url, col_names = columns)
head(banknotes)

set.seed(123)
banknotes$Class <- factor(banknotes$Class)
trainIndex <- createDataPartition(banknotes$Class, p = 0.75, list = FALSE)
trainData <- banknotes[trainIndex, ]
testData <- banknotes[-trainIndex, ]

# Визуализация данных с использованием PCA
mod.pca <- rda(banknotes[,-5],scale=TRUE)
scores <- as.data.frame(scores(mod.pca,displey="sites",
                               scaling=3)$sites)
scores$Class <- banknotes$Class

# Подписи осей с долями объясненной дисперсии
a1 <- as.integer(100*mod.pca$CA$eig[1]/sum(mod.pca$CA$eig))
a2 <- as.integer(100*mod.pca$CA$eig[2]/sum(mod.pca$CA$eig))
axX <- paste("PC1 (", a1,"%)")
axY <- paste("PC2 (", a2,"%)") 

# Draw up a table for the "sceleton" of points
l <- lapply(unique(scores$Class),function(c){
  f <- subset(scores,scores$Class == c)
  f [chull(f),]
})
hull <- do.call(rbind,l)

# Визуализация ординации с PCA
ggplot()+
  geom_polygon(hull,mapping=aes(x=PC1,y=PC2,fill=Class),
               alpha=0.4,lty=0)+
  geom_point(scores,mapping=aes(x=PC1,y=PC2,shape=Class,colour=Class),
             size=2)+
  xlab(axX)+ylab(axY)+
  scale_colour_manual(values = c("darkorange","darkgreen"))+
  coord_equal()+theme_classic()+
  ggtitle("    Ordinatory Diagram")

# Определим параметры для SVM

# Цель:
#  Подобрать лучшие параметры ядра (kernel) и штрафа (cost) для модели SVM, 
#  чтобы максимизировать метрику AUC (Area Under ROC Curve).

kernels <- c("linear", "polynomial", "radial")
best_auc <- 0
best_model <- NULL

# Обучим модель SVM с тремя ядрами и подберем лучшие параметры ядра и штрафа
for (kernel in kernels) {
  for (cost in c(0.1, 1, 10, 100)) {
    # Обучение SVM (probability = TRUE: Вычисляются вероятности классов)
    svm_model <- svm(Class ~ ., data = trainData, 
                     kernel = kernel, 
                     cost = cost, probability = TRUE)
    
    # Предсказания на тестовой выборке
    predictions <- predict(svm_model, testData, decision.values = TRUE)
    
    aucs <- sapply(levels(banknotes$Class), function(class) {
      # Построение ROC-кривой для каждого класса
      roc_obj <- roc(response = testData$Class == class, 
                     predictor = as.numeric(predictions == class))
      # Расчет площади под ROC-кривой
      auc(roc_obj)
    })
    
    mean_auc <- mean(aucs, na.rm = TRUE)
    cat("Ядро:", kernel, "Штраф C:", cost, "Средний AUC:", 
        mean_auc, "\n")
    
    # Сохраняем лучшую модель по среднему AUC
    if (mean_auc > best_auc) {
      best_auc <- mean_auc
      best_model <- svm_model
    }
  }
}
#
print(best_model)

# Функция для построения ROC-кривой для каждого класса
plot_multiclass_roc <- function(model, testData) {
  par(mfrow = c(1, 1))
  colors <- c("darkred", "darkblue", "darkgreen")
  
  for (i in 1:length(levels(banknotes$Class))) {
    class <- levels(banknotes$Class)[i]
    roc_obj <- roc(response = testData$Class == class, 
                   predictor = as.numeric(predict(model, testData) == class))
    
    plot(roc_obj, col = colors[i], lwd = 2, 
         main = "ROC-кривые для каждого класса", add = i > 1)
    legend("bottomright", legend = levels(banknotes$Class), col = colors, 
           lwd = 2)
  }
}
#
# Предсказания на тестовой выборке с лучшей моделью
pred <- predict(best_model, testData)

# Матрица ошибок
conf_mat <- confusionMatrix(pred, testData$Class)
print(conf_mat)

# Общая точность
cat("Общая точность:", conf_mat$overall["Accuracy"], "\n")

# Вызов функции для построения ROC-кривых на тестовой выборке 
#     с лучшей моделью
plot_multiclass_roc(best_model, testData)



# Выполняется кластеризация с использованием спектральной кластеризации из kernlab
banknotes_matrix <- banknotes
banknotes_matrix$Class <- as.numeric(banknotes_matrix$Class)
# Содержит метки кластеров для каждого наблюдения, определенные алгоритмом
#   specc(): Спектральная кластеризация - группирует данные на основе 
#            спектральных свойств графа расстояний (centers - кол-во кластеров).
sc <- specc(banknotes_matrix[,-5], centers = 2)
length(sc)

s = data.frame(x = scores$PC1, y = scores$PC2, class = as.factor(sc))
x_range <- seq(min(s$x), max(s$x), length.out = 100)
y_range <- seq(min(s$y), max(s$y), length.out = 100)
grid <- expand.grid(PC1 = x_range, PC2 = y_range)

# Обучение SVM на данных PCA(в пространстве главных компонентов (PC1 и PC2))
svm_pca_model <- svm(Class ~ PC1 + PC2, data = scores, 
              kernel = "radial", cost = 1, probability = TRUE)
grid$Class <- predict(svm_pca_model, newdata = grid)
grid$ClassNum <- as.numeric(grid$Class)

# Визуализации решающей границы в пространстве PCA
#   Показывает как SVM разделяет классы в двумерном пространстве PCA
ggplot() +
  geom_polygon(data = hull, aes(x = PC1, y = PC2, fill = Class),
               alpha = 0.4, lty = 0) +
  geom_point(data = scores,
             aes(x = PC1, y = PC2, shape = Class, colour = Class),
             size = 2) +
  geom_contour(data = grid, aes(x = PC1, y = PC2, z = ClassNum),
               breaks = c(1.5, 2.5), colour = "black",lwd = 1) + # Разделяющая линия
  xlab(axX) + ylab(axY) +
  scale_colour_manual(values = c("purple", "darkgreen")) +
  coord_equal() + theme_classic() +
  ggtitle("Ordinatory Diagram with SVM Decision Boundary")

