require(readxl)
require(ggplot2)
#
dress <- read.delim(file.choose(), header = TRUE, sep = " ", dec = ".")
ggplot(data=dress, aes(x=Length, y=Infection, colour=Lake))+
  theme_dark()+
  geom_point()

#
plot(dress$Length, dress$Infection)
###################################

avia <- read.csv(file.choose(), sep=" ")
ggplot(avia, aes(x=1:nrow(avia), y=x))+
  theme_dark()+
  geom_line(col="gold")+
  geom_smooth(lty=2, lwd=0.75)
############################################

press <- read_xlsx(file.choose())
press$Давление <- as.double(press$Давление)
colnames(press) <- c("Press")

ggplot(press, aes(x=1:nrow(press), y=Press))+
  theme_dark()+
  geom_point(col="gold")+
  geom_hline(yintercept = mean(press$Press), col="green")+
  xlab("Number")+
  ggtitle("  Sistolic pressure")
############################################

prices <- read_xlsx(file.choose())

ggplot(prices, aes(x=Data, y=Price))+
  theme_dark()+
  geom_point(col="skyblue", size=0.3)+
  geom_hline(yintercept = mean(prices$Price), col="darkred")+
  xlab("Years")+
  ggtitle("  Price for x")+
  geom_smooth(col="gold", lwd=0.5, lty=2)
############################################

tomatos <- read.delim(file.choose(), header=TRUE, sep=" ", dec=".")

ggplot(data=tomatos)+
  geom_point(mapping=aes(x=1:nrow(tomatos), y=weight, color=trt), position = "jitter")+
  theme_gray()+
  xlab("Amount")+
  ylab("Weight")+
  geom_hline(yintercept = mean(tomatos$weight), col="darkred")+
  ggtitle("  Weight for treatment")
############################################
