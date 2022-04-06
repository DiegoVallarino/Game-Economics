Game Economics models are seeking to understand how players behave. There is a huge amount of data relating to how the different millions of players in the world play.

# Upload libraries
library(car)
library(MASS)
library(psych)
library(agricolae)
library(mixlm)
library(readxl)
library(cluster)
library(factoextra)
library(FactoMineR)
library (performance)
library(vcvComp)
library(lmtest)
library(readxl)

# Upload the Data
Fortnite <- read_excel("Fortnite.xlsx")
df<-Fortnite
df1<-df
df1$`Mental State`<-as.factor(df1$`Mental State`)

# Some descriptive statistics
str(df)

# Model development
modelPlace<-lm(formula = Placed ~ Date + Revives + Accuracy + `Head Shots` + 
    `Distance Traveled`, data = df1)
summary(modelPlace)
performance_accuracy(modelPlace)
performance::binned_residuals(modelPlace, term='Accuracy')
bptest(modelPlace)
shapiro.test(modelPlace$residuals)

# Elimination Model
modelEli<-lm(formula = Eliminations ~ Date + `Time of Day` + `Mental State` + 
    Assists + Hits + `Head Shots` + `Materials Used` + `Damage to Players` + 
    Placed, data = df)
summary(modelEli)

performance_accuracy(modelEli)
performance::binned_residuals(modelEli)
bptest(modelEli)
shapiro.test(modelEli$residuals)

# How about "Mental State"
boxplot(df1$Eliminations~df1$`Mental State`, main = "Estado mental y eliminacion del juego", xlab = "Estado Mental", ylab = "Eliminacion del juego")

boxplot(df1$Placed~df1$`Mental State`, main = "Estado mental y puesto en el juego", xlab = "Estado Mental", ylab = "Puesto en el juego")

plot(df1$Placed, df1$`Mental State`, main = "Estado mental y lugar donde terminó en el juego", ylab = "Estado Mental", xlab = "Lugar donde terminó el juego")

plot(df1$`Time of Day`, df1$`Mental State`, main = "Estado mental en las horas que jugaba", ylab = "Estado Mental", xlab = "Momento del dia")