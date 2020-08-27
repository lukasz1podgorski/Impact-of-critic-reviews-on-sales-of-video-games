dane <- read.csv(file="C:/Users/lukis/Desktop/Video_Games_Sales_as_at_22_Dec_2016.csv", header=TRUE, sep=",")
dane$User_Score <- as.numeric(as.character(dane$User_Score))
dane$Critic_Score <- dane$Critic_Score / 10
dane$Global_Sales <- dane$Global_Sales * 1000000
library(dplyr)
dane_omit <- na.omit(dane) 
dane_omit
summary(dane_omit)
length(dane_krytycy$Critic_Score)
u=1.96

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

#KRYTYCY
dane_krytycy=filter(dane_omit, Critic_Score > 5.0)
length(dane_krytycy$Global_Sales)
summary(dane_krytycy)
mean(dane_krytycy$Global_Sales)
sd(dane_krytycy$Global_Sales)
var(dane_krytycy$Global_Sales)
median(dane_krytycy$Global_Sales)
result <- getmode(dane_krytycy$Global_Sales)
print(result)
#Przedzia³ ufnoœci
ci_kr=(u*sd(dane_krytycy$Global_Sales)/length(dane_krytycy$Global_Sales))




#U¯YTKOWNICY
dane_user=filter(dane_omit, User_Score > 5)
length(dane_user$Global_Sales)
summary(dane_user)
mean(dane_user$Global_Sales)
sd(dane_user$Global_Sales)
var(dane_user$Global_Sales)
median(dane_user$Global_Sales)
result <- getmode(dane_user$Global_Sales)
print(result)
#Przedzia³ ufnoœci
ci_u=(u*sd(dane_user$Global_Sales)/length(dane_user$Global_Sales))

x <- data.frame("Grupa" = c("Krytycy","U¿ytkownicy"), "Sprzeda¿_Globalna" = c(mean(dane_krytycy$Global_Sales),mean(dane_user$Global_Sales)), "sd" = c(sd(dane_krytycy$Global_Sales),sd(dane_user$Global_Sales)), "ci"=c(ci_kr,ci_u))
library(ggplot2)
ggplot(x, aes(x=Grupa, y=Sprzeda¿_Globalna, group=1)) +
    geom_line() +
    geom_errorbar(width=.1, aes(ymin=Sprzeda¿_Globalna-ci, ymax=Sprzeda¿_Globalna+ci)) +
    geom_point(shape=21, size=3, fill="white")


#TESTOWANIE (ze wzglêdu na nieznany rozk³ad, zdecydowano siê wykonaæ
# obustronny niesparowany nieparametryczny test Wilcoxona dla dwóch œrednich)
#H0: m_kr_sales=m_user_sales
#H1: m_kr_sales=!=m_user_sales

wilcox.test(dane_krytycy$Global_Sales, dane_user$Global_Sales , alternative = "two.sided")
#p-value = 0.175 -> nie ma podstaw do odrzucenia h0, na poz ufnosci 95% mo¿na stwierdziæ, ¿e nie ma ró¿nicy w globalnej sprzeda¿y gier wideo pomiêdzy pozytywnymi ocenami u¿ytkowników, a krytyków.
  