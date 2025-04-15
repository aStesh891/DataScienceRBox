#
require(ggplot2)
# Для экологических анализов, включая PCA (анализ главных компонентов)
require(vegan)

head(iris)

# Зависимость между разными переменными, разделенными по виду Species
ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+
  facet_grid(facets=~Species)+
  geom_point(col="darkgreen")+
  geom_smooth(col="red")

ggplot(iris,aes(x=Petal.Length,y=Petal.Width))+
  facet_grid(facets=~Species)+
  geom_point(col="darkgreen")+
  geom_smooth(col="red")

##############################################################################
# Ординация на основе PCA( анализ главных компонентов)
# (Visualisation in space of principal components(library(vegan)))

# Исключается столбец Species(целевой)-анализ проводится на числ переменных.
#   scale = TRUE: Масштабирует переменные (приводит их к одинаковому масштабу).
mod.pca <- rda(iris[,-5],scale=TRUE)

# Извлекает координаты объектов (наблюдений) в пространстве главных компонентов.
scores <- as.data.frame(scores(mod.pca, displey="sites", scaling=3)$sites)

# Добавляет информацию о виде Species в данные.
scores$Species <- iris$Species


# Подпись осей - Доли объясненной дисперсии (важность компонент)
# в основе лежат понятие собственных значений - отнесенная к сумме всех частот
a1 <- as.integer(100*mod.pca$CA$eig[1]/sum(mod.pca$CA$eig))
a2 <- as.integer(100*mod.pca$CA$eig[2]/sum(mod.pca$CA$eig))

axX <- paste("PC1 (", a1,"%)")
axY <- paste("PC2 (", a2,"%)") 

# Контур области по самым выдающимся точкам
l <- lapply(unique(scores$Species),function(c){
  f <- subset(scores,scores$Species == c)
  f [chull(f),] # chull(f) находит точки, формирующие контур
})
# Объединяет все контуры в один датафрейм.
hull <- do.call(rbind,l)

# Диаграмма ординации
ggplot()+
  geom_polygon(hull,mapping=aes(x=PC1,y=PC2,fill=Species),  
               alpha=0.4,lty=0)+ # Добавляет контуры (полигоны) для каждого вида.
  geom_point(scores,mapping=aes(x=PC1,y=PC2,shape=Species,colour=Species),
             size=2)+            # Добавляет точки для каждого наблюдения.
  xlab(axX)+ylab(axY)+
  scale_colour_manual(values = c("purple","darkgreen","blue"))+
  coord_equal()+theme_classic()+
  ggtitle("    Ordinatory Diagram")

