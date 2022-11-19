####### Importation Data #################### 

library(tidyverse)
library(readxl)
library(tseries)
library(lmtest)
library(dplyr)
library(ggplot2)
library(aTSA)
library(dataseries)


url("https://www.brh.ht/wp-content/uploads/agregatsmon.xls")
setwd(file.path("/Applications/Devoir Marthe Manipulation"))
local<-file.path("Brh_Data.xls")
download.file("https://www.brh.ht/wp-content/uploads/agregatsmon.xls",local)
Brh_Data<-read_excel(local, skip = 2)

tail(Brh_Data, n=10)
head(Brh_Data,n=6)


# changer le nom da la 1ere colonne en date
Brh_Data<-rename(Brh_Data, Date = ...1)

#Eliminer les colonnes et les lignes inutiles
Brh_Data<-Brh_Data[1:518, ]
Brh_Data<-Brh_Data[-c(1), ]
Brh_Data<-Brh_Data[ ,-c( 5) ]
Brh_Data<-Brh_Data[,-c(8)]
Brh_Data<-Brh_Data[,-c(9)]
Brh_Data<-Brh_Data[,-c(12)]
Brh_Data<-Brh_Data[,-c(16)]
Brh_Data<-Brh_Data[,-c(18)]
Brh_Data<-Brh_Data[,-c(19)]
Brh_Data<-Brh_Data[,-c(23)]
Brh_Data<-Brh_Data[,-c(27)]
Brh_Data<-Brh_Data[,-c(38)]
Brh_Data<-Brh_Data[,-c(46)]
Brh_Data<-Brh_Data[,-c(33)]

# conversion date en format numerique
Brh_Data$Date <- as.numeric(Brh_Data$Date)


# Conversion la colonne date en format correct, en prenant 30 decembre comme point de depart
Brh_Data$Date <- as.Date(Brh_Data$Date, 
                                 origin = "1899-12-30")

# Formattage des 17 dates manquantes
# Hint: taper les valeurs en observant le fichier excel 

Brh_Data$Date[309] <- "2004-07-01"
Brh_Data$Date[310] <- "2004-08-01"
Brh_Data$Date[311] <- "2004-09-01"
Brh_Data$Date[312] <- "2004-10-01"
Brh_Data$Date[313] <- "2004-11-01"
Brh_Data$Date[314] <- "2004-12-01"
Brh_Data$Date[315] <- "2005-01-01"
Brh_Data$Date[316] <- "2005-02-01"
Brh_Data$Date[317] <- "2005-03-01"
Brh_Data$Date[318] <- "2005-04-01"
Brh_Data$Date[319] <- "2005-05-01"
Brh_Data$Date[320] <- "2005-06-01"

Brh_Data$Date[360] <- "2008-10-01"
Brh_Data$Date[361] <- "2008-11-01"
Brh_Data$Date[362] <- "2008-12-01"
Brh_Data$Date[363] <- "2009-01-01"
Brh_Data$Date[364] <- "2009-02-01"



# 
#Creer un objet comportant tous les colonnes de donnees du fichier de octobre 1990 a date
Brh_Data <- Brh_Data %>% 
  filter(Date >= "1990-10-01")

#CHoix des variables
Brh_variable<-Brh_Data%>%
  select("Date","Multiplicateur(M3/B)","Multiplicateur(M2/B)","Multiplicateur(M1/B)")

# Nettoyage du nouveau dataframe
Brh_variable<-Brh_variable[1:225, ]

str(Brh_variable)

# convertir en donnee numeri
Brh_variable$`Multiplicateur(M3/B)`<-as.numeric(Brh_variable$`Multiplicateur(M3/B)`)
Brh_variable$`Multiplicateur(M2/B)`<-as.numeric(Brh_variable$`Multiplicateur(M2/B)`)
Brh_variable$`Multiplicateur(M1/B)`<-as.numeric(Brh_variable$`Multiplicateur(M1/B)`)
      
# changer le nom des colonnes
names(Brh_variable)<-c("Date","MM3", "MM2", "MM1")

# Teste de stationarite pour les variables
ggplot(Brh_variable, aes(x = Date, y = MM3))+
  geom_line()+
  labs(title = " Multiplicateur M3/B",
       subtitle = "Periode: Octobre 1990 - Octobre 2021")

adf.test(Brh_variable$MM3)


ggplot(Brh_variable, aes(x = Date, y = MM2))+
  geom_line()+
  labs(title = "Multiplicateur(M2/B)",
       y = "MM2",
       subtitle = "Periode: Octobre 1990 - Octobre 2021")

adf.test(Brh_variable$MM2)


ggplot(Brh_variable, aes(x = Date, y =MM1))+
  geom_line()+
  labs(title = " Multiplicateur(M1/B)",
       y = "MM1",
       subtitle = "Periode: Octobre 1990 - Octobre 2021")

adf.test(Brh_variable$MM1)


A <- diff(Brh_variable$MM3)
plot(a, ylab = "MM3")
adf.test(A, k=2)


B <- diff(Brh_variable$MM2)
plot(b, ylab = "MM2")
adf.test(B, k=2)

C <- diff(Brh_variable$MM1)
plot(c, ylab = "MM1")
adf.test(C, k= 2 )

# Test de la causalite de Granger
grangertest(MM3 ~ MM2, data = Brh_variable, order = 1)
grangertest(MM3 ~ MM1, data = Brh_variable, order = 2)


# Regression lineaire sur la teste de la causalite de granger       
library(readr)
reg_brh_variable<-lm(MM3~MM1+MM2, data = Brh_variable)
summary(reg_brh_variable)


