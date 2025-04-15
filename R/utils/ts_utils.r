
# Загружаем библиотеки
if(!require(corrplot))
{
  install.packages("corrplot")
  library(corrplot)
}
if(!require(RColorBrewer))
{
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

# Функция для графика корреляционной матрицы
my_mtest <- function(obj, method = c("pearson", "kendall", "spearman"),
                     alternative = c("two.sided", "less", "greater")) {
  # Выбор первого элемента из предложенных значений, если не указано конкретное значение
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  
  # Копируем матрицу корреляций
  copy.cor <- cor(obj, method = method)
  
  # Выполняем тестирование корреляции
  copy.cor.mtest <- cor.mtest(obj, method = method, 
                              alternative = alternative)
  
  # Создаем график корреляционной матрицы
  corrplot(copy.cor,
           method = 'number',
           type = 'lower',
           p.mat = copy.cor.mtest$p,
           col = brewer.pal(n = 5, name = 'Dark2'))
}

# Функция преобразует все фактор. переменные в датафрейме в числ. значения, начиная с 0
# Не изменяет другие типы переменных.
factor.to.numeric <- function(x) {
  # Эта часть проходит по каждому столбцу датафрейма/списка x применяя функцию
  as.data.frame(lapply(x, function(c) {
    if (is.factor(c)) {
      # Преобразует факторные уровни в числа, начиная с 0 (по умолчанию преобразование фактора начинается с 1, поэтому мы вычитаем 1).
      return(as.numeric(c) - 1)
    }
    return(c)
  }))
}

# Функция строит два графика:
  # Эмпирическая кривая распределения(КФР):как распределены данные x+кривая распределения.
  # Плотность распределения(ФПР):сглаженное распределение данныx x, 
  # сравнивая его с предсказанной плотностью pd.
graph_distr <- function(x,pc,pd,main_name=""){
  op <- par(mfrow=c(1,1),pty="s")
  par(mfrow=c(1,2))
  mn <- paste("Эмпирическая КФР и ",main_name)
  plot(x,pc,type="l",col="red",lwd=2,main=mn)
  plot(ecdf(x),add=T)
  
  mn <- paste("Эмпирическая ФПР и ",main_name)
  plot(density(x),lwd=2,col="blue",main=mn)
  lines(x,pd,col="red",lwd=2)
  par(op)
}



library(MASS)
library(plotly)
#########################################################################
# Функция для вычисления расстояний Махаланобиса и построения графика
mahalanobis_plot <- function(mts_data) {
  # Вычисление расстояния Махаланобиса
  mahal_dist <- mahalanobis(mts_data[, colnames(mts_data)], 
                            colMeans(mts_data[, colnames(mts_data)]), 
                            cov(mts_data[, colnames(mts_data)]))
  
  # Порог для определения аномалий (например, 97.5-й процентиль)
  threshold <- quantile(mahal_dist, 0.975)
  
  # Поиск аномальных значений
  anomalous_points <- which(mahal_dist > threshold)
  
  # Создание графика с plotly
  p <- plot_ly(x = 1:length(mahal_dist), y = mahal_dist, type = "scatter", mode = "lines+markers",
               name = "Расстояние Махаланобиса", marker = list(color = 'blue')) %>%
    add_markers(x = anomalous_points, y = mahal_dist[anomalous_points], 
                marker = list(color = 'red', size = 10), 
                name = "Аномалии") %>%
    layout(title = "Расстояние Махаланобиса",
           xaxis = list(title = "Наблюдение"),
           yaxis = list(title = "Расстояние"),
           legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2)) %>%
    add_lines(y = rep(threshold, length(mahal_dist)), line = list(color = "darkred", dash = "dash"), 
              name = "Порог аномалии")
  
  # Вывод графика и списка аномальных значений
  list(plot = p, anomalous_values = anomalous_points)
}


#
min_max_norm <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

jo_func <- function(jo, test, p = 0.05) {
  if (p == 0.05) {
    nc <- 2
  } else if (p == 0.1) {
    nc <- 1
  } else if (p == 0.01) {
    nc <- 3
  } else {
    stop("Неправильное значение p")
  }
  
  nn <- nrow(jo)
  r <- 0
  for (i in seq(nn, 1, -1)) {
    if (jo[i, nc] < test[i]) {
      r <- r + 1
    }
  }
  return(r)
}