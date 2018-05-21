##########
### 01 ### wczytanie danych i bibliotek
##########

library(DataExplorer)
library(ggplot2)
library(Hmisc)
library(MASS)
library(lattice)

library(caret)

db <- read.csv("./DATA/helocdata.csv")


##########
### 02 ### przeglad danych i czyszcznie
##########

attach(db)

#######
####### sprawdzamy special values

for (i in colnames(db)[-1]){
  zmiennaiter <- get(i)
  
  #percent_7 <- sum(zmiennaiter %in% c(-7, -8)) / nrow(db)
  percent_7 <- sum(zmiennaiter==-7) / nrow(db)
  percent_8 <- sum(zmiennaiter==-8) / nrow(db)
  percent_9 <- sum(zmiennaiter==-9) / nrow(db)
  percent_good <- 1-percent_7-percent_8-percent_9

  if (i==colnames(db)[2]){
    specialvalues <- data.frame(zm = i, p7 = percent_7, p8 = percent_8, p9 = percent_9)
  } else {
    specialvalues = rbind(specialvalues, data.frame(zm = i, p7 = percent_7, p8 = percent_8, p9 = percent_9))
  }
  
}

specialvalues$p7 <- round(specialvalues$p7, 2)
specialvalues$p8 <- round(specialvalues$p8, 2)
specialvalues$p9 <- round(specialvalues$p9, 2)

colnames(specialvalues) <- c("Zmienna", "% wartości -7", "% wartości -8", "% wartości -9")

rownames(specialvalues) <- specialvalues$Zmienna

specialvalues <- specialvalues[,2:4]

Hmisc::latex(object = specialvalues[1:12,], rowlabel = "Zmienna", file=paste0(getwd(),"/WYKRESY/T01_p1.tex"))
Hmisc::latex(object = specialvalues[13:nrow(specialvalues),], rowlabel = "Zmienna", file=paste0(getwd(),"/WYKRESY/T01_p2.tex"))

# MSinceMostRecentDelq - wrzucamy na 0

# MSinceMostRecentInqexcl7days
# wrzucamy na 0, ale tworzymy nowa zmienna indykatorowa, czy byl wczensiej Inquiry 7+
# jednak nie tworzymy, bo mamy juz number of inq last 6 mths czyli informacja jest tam zakodowana


#######
####### które wiersze mają -9?

czyma9 <- function(x){
  if(-9 %in% x){1} else {0}
}

ilema9 <- function(x){
  sum(x==-9)
}

Sprawdz9 <- data.frame(Czy = apply(db, 1, czyma9), Ile = apply(db, 1, ilema9))
Sprawdz9 <- Sprawdz9[Sprawdz9$Czy==1,]

# usun te, ktore maja wiecej niz 10 wierszy z brakami

usun <- as.numeric(rownames(Sprawdz9[Sprawdz9$Ile>10,]))

db2 <- db[-usun,]



######
###### sprawdzamy dla db2

detach(db)
attach(db2)


for (i in colnames(db2)[-1]){
  zmiennaiter <- get(i)
  
  #percent_7 <- sum(zmiennaiter %in% c(-7, -8)) / nrow(db)
  percent_7 <- sum(zmiennaiter==-7) / nrow(db2)
  percent_8 <- sum(zmiennaiter==-8) / nrow(db2)
  percent_9 <- sum(zmiennaiter==-9) / nrow(db2)
  percent_good <- 1-percent_7-percent_8-percent_9
  
  if (i==colnames(db)[2]){
    specialvalues2 <- data.frame(zm = i, p7 = percent_7, p8 = percent_8, p9 = percent_9)
  } else {
    specialvalues2 = rbind(specialvalues2, data.frame(zm = i, p7 = percent_7, p8 = percent_8, p9 = percent_9))
  }
  
}

colnames(specialvalues2) <- c("Zmienna", "% wartości -7", "% wartości -8", "% wartości -9")

detach(db2)

#######
####### czyscimy -7 i -8

db2$MSinceMostRecentDelq <- ifelse(db2$MSinceMostRecentDelq %in% c(-7, -8), 0, db2$MSinceMostRecentDelq)

db2$MSinceMostRecentInqexcl7days <- ifelse(db2$MSinceMostRecentInqexcl7days %in% c(-7, -8), 0,
                                           db2$MSinceMostRecentInqexcl7days)



db2$NetFractionRevolvingBurden <- ifelse(db2$NetFractionRevolvingBurden==-8, 0, db2$NetFractionRevolvingBurden)
db2$NetFractionInstallBurden <- ifelse(db2$NetFractionInstallBurden==-8, 0, db2$NetFractionInstallBurden)
db2$NumRevolvingTradesWBalance <- ifelse(db2$NumRevolvingTradesWBalance==-8, 0, db2$NumRevolvingTradesWBalance)
db2$NumInstallTradesWBalance <- ifelse(db2$NumInstallTradesWBalance==-8, 0, db2$NumInstallTradesWBalance)
db2$NumBank2NatlTradesWHighUtilization <- ifelse(db2$NumBank2NatlTradesWHighUtilization==-8, 0, db2$NumBank2NatlTradesWHighUtilization)
db2$PercentTradesWBalance <- ifelse(db2$PercentTradesWBalance==-8, 0, db2$PercentTradesWBalance)




#######
####### sprawdz specjalne wartosci po raz ostatni

detach(db)
attach(db2)


for (i in colnames(db2)[-1]){
  zmiennaiter <- get(i)
  
  #percent_7 <- sum(zmiennaiter %in% c(-7, -8)) / nrow(db)
  percent_7 <- sum(zmiennaiter==-7) / nrow(db2)
  percent_8 <- sum(zmiennaiter==-8) / nrow(db2)
  percent_9 <- sum(zmiennaiter==-9) / nrow(db2)
  percent_good <- 1-percent_7-percent_8-percent_9
  
  if (i==colnames(db)[2]){
    specialvalues2 <- data.frame(zm = i, p7 = percent_7, p8 = percent_8, p9 = percent_9)
  } else {
    specialvalues2 = rbind(specialvalues2, data.frame(zm = i, p7 = percent_7, p8 = percent_8, p9 = percent_9))
  }
  
}

colnames(specialvalues2) <- c("Zmienna", "% wartości -7", "% wartości -8", "% wartości -9")

detach(db2)


#######
####### skonsoliduj poziomy faktorow + stworz faktory

db2$MSinceOldestTradeOpen <- ifelse(db2$MSinceOldestTradeOpen==-8,0,db2$MSinceOldestTradeOpen)
db2$ExternalRiskEstimate <- ifelse(db2$ExternalRiskEstimate==-9, 0, db2$ExternalRiskEstimate)
           
db2$MaxDelq2PublicRecLast12M <- ifelse(db2$MaxDelq2PublicRecLast12M %in% c(5,6), 5,
                                       db2$MaxDelq2PublicRecLast12M)

db2$MaxDelqEver <- as.factor(db2$MaxDelqEver)
db2$MaxDelq2PublicRecLast12M <- as.factor(db2$MaxDelq2PublicRecLast12M)

db2$RiskPerformance <- ifelse(db2$RiskPerformance=="Good", 1, 0)


db <- db2 # dla jasnosci zapisu
#saveRDS(object = db, file = paste0(getwd(),"/DATA/helocdataclean"))



##########
### 03 ### ploty
##########

db <- readRDS(paste0(getwd(),"/DATA/helocdataclean"))

dbplot <- db
dbplot['RiskPlot'] <- ifelse(dbplot$RiskPerformance==1,"1","0")

ggplot(dbplot, aes(x=ExternalRiskEstimate, y=MSinceOldestTradeOpen, colour = RiskPlot)) +
  geom_point(alpha = 0.5)

caret::featurePlot(x = dbplot[,2:13], y = factor(dbplot[,c("RiskPerformance")]), plot = "pairs")


##########
### 04 ### czesc Darka
##########

#statystyki liczbowe
describe(db)
summary(db)

#wykresy rozkładu zmiennych
plot_bar(db) #factory
plot_histogram(db) #numeryczne

#zmienne vs objaśniana
plot_boxplot(db, by = "RiskPerformance")  
parallelplot(~db[,-1] | RiskPerformance, data = db)
# tu musza wszystkie byc numeryczne
parcoord(db[,-c(1, 11, 12)], var.label = db$RiskPerformance, col=db$RiskPerformance)

#korelacja
plot_correlation(db)

#szybki raport podsumowujący
create_report(db)
