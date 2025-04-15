require(ggplot2)
require(readxl)
require(gridExtra)

#prices box plot
prices <- read_xlsx(file.choose())

#default - bad
boxplot(prices)

#price analysis
prices_def <- data.frame(x=1:length(prices$Price), y=prices$Price)

ggplot(prices_def, aes(x, y))+
  geom_boxplot()+
  theme_gray()+
  xlab("Amount")+
  ylab("Price")

#numbers check
summary(prices$Price)

################################################################################
#salaries vector box plot

fir <- c(50000, rep(10000, 5), rep(1000, 45), rep(100, 50), rep(10, 100))

#default - same
boxplot(fir)

#salaries analysis
fir_def <- data.frame(x=1:length(fir), y=fir)

ggplot(fir_def, aes(x, y))+
  geom_boxplot()+
  theme_gray()+
  xlab("Amount")+
  ylab("Salary")

#numbers check
summary(fir)

################################################################################
#tomato's box plot
tomatos <- read.delim(file.choose(), header=TRUE, sep=" ", dec=".")

ggplot(tomatos, aes(x=tomatos$trt, y=tomatos$weight, fill=tomatos$trt)) + 
  geom_boxplot(alpha=0.3)+
  theme(legend.position="none")+
  xlab("Treatment")+
  ylab("Weight")

################################################################################
#dreissena box plot
dress <- read.delim(file.choose(), header = TRUE, sep = " ", dec = ".")

infection_plot <- ggplot(dress, aes(x=dress$Lake, y=dress$Infection, fill=dress$Lake)) + 
  geom_boxplot(alpha=0.9)+
  theme(legend.position="none")+
  xlab("Lake")+
  ylab("Infection")

length_plot <- ggplot(dress, aes(x=dress$Lake, y=dress$Length, fill=dress$Lake)) + 
  geom_boxplot(alpha=0.6)+
  theme(legend.position="none")+
  xlab("Lake")+
  ylab("Length")

day_plot <- ggplot(dress, aes(x=dress$Lake, y=dress$Day, fill=dress$Lake)) + 
  geom_boxplot(alpha=0.2)+
  theme(legend.position="none")+
  xlab("Lake")+
  ylab("Day")

############ group v1
layout <- matrix(c(1, 2, 3), ncol = 3, byrow = TRUE)

grid.arrange(infection_plot, length_plot, day_plot,
             layout_matrix = layout) 

############ group v2
grid.arrange(infection_plot, length_plot, day_plot,
             top = "Dreissena Lake Box Plot", bottom = "03/07/2024",
             widths = c(1, 1, 1), heights = c(1))

################################################################################
#proba box plot
proba <- read.csv(file.choose(), sep=" ")

#default - good
boxplot(proba)

#numbers check
summary(proba)



