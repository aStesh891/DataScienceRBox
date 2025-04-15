#
require(corrplot)
my_mtest <- function(obj, method = c("pearson", "kendall", "spearman"),
                     alternative = c("two.sided", "less", "greater")) {
  # Выбор первого элемента из предложенных значений, если не указано конкретное значение
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  
  # Копируем матрицу корреляций
  copy.cor <- cor(obj, method = method)
  
  # Выполняем тестирование корреляции
  copy.cor.mtest <- cor.mtest(obj, method = method, alternative = alternative)
  
  # Создаем график корреляционной матрицы
  corrplot(copy.cor,
           method = 'number',
           type = 'lower',
           p.mat = copy.cor.mtest$p,
           col = RColorBrewer::brewer.pal(n = 5, name = 'Set2'))
}

#
factor.to.numeric <- function(x) {
  as.data.frame(lapply(x, function(c) {
    if (is.factor(c)) {
      return(as.numeric(c) - 1)
    }
    return(c)
  }))
}
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

my_normalize <- function(x, n, m) 
{ 
  for(i in 1:n)
  {
    imu <- mean(x[,i])
    imin <- min(x[,i])
    imax <- max(x[,i])
    
    for(j in 1:m)
    {
      
    }
  }
}