##########
### 01 ### wczytanie danych i bibliotek
##########

# library(DataExplorer)
library(ggplot2)
library(Hmisc)
library(MASS)
library(lattice)
library(sqldf)
library(dplyr)

library(caret)

db <- read.csv("./DATA/helocdata.csv")


##########
### 02 ### przeglad danych co do special values + czyszczenie z `-9`
########## (co oznacza brak jakichkolwiek danych z credit bureau)

attach(db)

#######
####### sprawdzamy special values: [-7, -8, -9]

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


## eksport do latexowych tabelek
# Hmisc::latex(object = specialvalues[1:12,], rowlabel = "Zmienna", file=paste0(getwd(),"/WYKRESY/T01_p1.tex"))
# Hmisc::latex(object = specialvalues[13:nrow(specialvalues),], rowlabel = "Zmienna", file=paste0(getwd(),"/WYKRESY/T01_p2.tex"))



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

# reszta wierszy to takie, ktore maja brak w `ExternalRiskEstimate` jedynie - zbudujemy
# na to osobny model



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


### uzupelniamy `-8` (Not valid - czyli too old) dla zmiennej `MSinceOldestTradeOpen`
#   biorac przecietna liczbe `MSinceOldestTradeOpen` dla danej wartosci analizowanej zmien.

colnames(db2)[colnames(db2)=="MSinceOldestTradeOpen"] <- "MSinceOldestTradeOpenNull"

db2 = sqldf("
SELECT t1.*,
  CASE WHEN t1.MSinceOldestTradeOpenNull = -8 THEN
  (SELECT ROUND(AVG(pom.MSinceOldestTradeOpenNull),0)
  FROM db2 AS pom
  WHERE pom.MSinceOldestTradeOpenNull != -8
  AND t1.AverageMInFile = pom.AverageMInFIle
  ) ELSE t1.MSinceOldestTradeOpenNull END AS MSinceOldestTradeOpen
FROM db2 AS t1
")


# mamy jedno NA bo nie bylo innych rekordow o tym samym AvgMInFile
# db2[db2$MSinceOldestTradeOpenNull==-8,c("MSinceOldestTradeOpenNull","MSinceOldestTradeOpen")]

# wysamplujmy dla tego braku wartosc z rozkladu
db2[is.na(db2$MSinceOldestTradeOpen)==TRUE,c("MSinceOldestTradeOpen")] <- 
  sample(db2$MSinceOldestTradeOpen[is.na(db2$MSinceOldestTradeOpen)==FALSE], 1)

# pozbywamy sie kolumny
db2 <- dplyr::select(db2, -MSinceOldestTradeOpenNull)




##########
### 03 ### FeatureEngineering
##########


### NumTrades60Ever2DerogPubRec

# > cor(db2$NumTrades60Ever2DerogPubRec, db2$NumTrades90Ever2DerogPubRec)
# [1] 0.8904734

db2 <- dplyr::select(db2, -NumTrades90Ever2DerogPubRec)



### MSinceMostRecentTradeOpen

db2$MSinceMostRecentTradeOpen <- db2$MSinceMostRecentTradeOpen + 1



### db2$PercentTradesNeverDelq

as.numeric(cut(db2$PercentTradesNeverDelq, breaks = c(0, 65, 90, 99, 100),
      labels = c(1, 2, 3, 4),
      include.lowest = TRUE)) -> db2$PercentTradesNeverDelq


### db2$MSinceMostRecentDelq

# -7 oznaczan `Condition not met`

# > unique(db2$MaxDelqEver[db2$MSinceMostRecentDelq==-7])
# [1] 8 7

# `7` - unknown deliquency, `8` - current and never delinquent

# 7    8 
# 132 4532

db2$MSinceMostRecentDelq <- db2$MSinceMostRecentDelq + 1

# -7 czyli deliquency
db2$MSinceMostRecentDelq <- ifelse(db2$MSinceMostRecentDelq %in% c(-6, -7),
                                   0, db2$MSinceMostRecentDelq)

# -8 czyli too old

as.numeric(cut(db2$MSinceMostRecentDelq, c(0, 0.01, 3, 6, 12, 24, Inf),
                                  labels = c(6, 1, 2, 3, 4, 5),
                include.lowest = TRUE)) -> db2$MSinceMostRecentDelq



### db2$NumInqLast6Mexcl7days -> db2$InqInLast7days

db2$InqInLast7days = db2$NumInqLast6M - db2$NumInqLast6Mexcl7days

db2$InqInLast7days <- ifelse(db2$InqInLast7days!=0, 1, 0)

db2 <- dplyr::select(db2, -NumInqLast6Mexcl7days)


### db2$MSinceMostRecentInqexcl7days

db2$MSinceMostRecentInqexcl7days <- db2$MSinceMostRecentInqexcl7days + 1

db2$MSinceMostRecentInqexcl7days <- ifelse(db2$MSinceMostRecentInqexcl7days %in% c(-6, -7),
                                           0, db2$MSinceMostRecentInqexcl7days)

as.numeric(cut(db2$MSinceMostRecentInqexcl7days, c(0, 0.01, 3, 6, 12, Inf),
                                    labels = c(5, 1, 2, 3, 4),
               include.lowest = TRUE )) -> db2$MSinceMostRecentInqexcl7days





##########
### 04 ### FeatureEngineering - Model for ExternalRiskEstimator
##########


db2 %>% dplyr::select(-RiskPerformance) %>%
  dplyr::filter(ExternalRiskEstimate!=-9) -> dbhelp

# model

modexternal <- lm(ExternalRiskEstimate ~ ., data = dbhelp)

# dane do przeskorowania - flaga

db2$FlagExternal <- ifelse(db2$ExternalRiskEstimate==-9, 1, 0)

db2$Id <- 1:nrow(db2)

# dane do przeskorowania

db2 %>% dplyr::select(-RiskPerformance) %>%
  dplyr::filter(ExternalRiskEstimate==-9) %>%
  dplyr::select(-ExternalRiskEstimate) -> dbscore

dbscore['ExternalRiskEstimate'] <- round(predict.lm(modexternal, dbscore), 0)

# left join

colnames(db2)[colnames(db2)=="ExternalRiskEstimate"] <- "ExternalRiskEstimateNull"

sqldf("
SELECT t1.*,
  CASE WHEN t1.ExternalRiskEstimateNull = -9 THEN t2.ExternalRiskEstimate
  ELSE t1.ExternalRiskEstimateNull END AS ExternalRiskEstimate
FROM db2 AS t1
LEFT JOIN dbscore AS t2
ON t1.Id = t2.Id
") -> db2

# wyczysc kolumne `ExternalRiskEstimateNull`

db2 <- dplyr::select(db2, -ExternalRiskEstimateNull)

db2 <- dplyr::select(db2, -FlagExternal, -Id)



##########
### 05 ### korelacje miedzy zmiennymi numerycznymi
##########

numeryczne <- c("MSinceMostRecentTradeOpen", "AverageMInFile", "NumSatisfactoryTrades",
                "NumTrades60Ever2DerogPubRec", "MSinceMostRecentDelq", "NumTotalTrades",
                "MSinceMostRecentInqexcl7days", "NetFractionRevolvingBurden",
                "NetFractionInstallBurden", "NumRevolvingTradesWBalance",
                "NumInstallTradesWBalance", "MSinceOldestTradeOpen", "ExternalRiskEstimate")


kowar <- cor(db2[,numeryczne])
colnames(kowar) <- 1:ncol(kowar)
rownames(kowar) <- 1:nrow(kowar)

# numeryczne[c(3,6)]

### silna korelacja!
# plot(db2$NumSatisfactoryTrades, db2$NumTotalTrades)

### sa podobnie skorelowane z innymi!
# kowar[c(3,6),]

### zostawiamy tylko zmienna ..

db2 <- dplyr::select(db2, -NumTotalTrades)

#######
####### sprawdz specjalne wartosci po raz ostatni


zmienne8 = apply(db2, 2, function(x) sum(x==-8))
zmienne8 = zmienne8[zmienne8!=0]

# > zmienne8
# NumRevolvingTradesWBalance           NumInstallTradesWBalance 
# 156                                861 
# NumBank2NatlTradesWHighUtilization 
# 583 

db2$NumRevolvingTradesWBalance <- ifelse(db2$NumRevolvingTradesWBalance==-8,
                                         0, db2$NumRevolvingTradesWBalance)

db2$NumInstallTradesWBalance <- ifelse(db2$NumInstallTradesWBalance==-8,
                                       0, db2$NumInstallTradesWBalance)

db2$NumBank2NatlTradesWHighUtilization <- ifelse(
                    db2$NumBank2NatlTradesWHighUtilization==-8,
                    0, db2$NumBank2NatlTradesWHighUtilization)

##########
### 06 ### zmienna odpowiedzi - 1 to 'bad' bo w tej konwencji na stronie jest
##########



db2$RiskPerformance <- ifelse(db2$RiskPerformance=="Bad", 1, 0)


db <- db2 # dla jasnosci zapisu

saveRDS(object = db, file = paste0(getwd(),"/DATA/helocdataclean"))

write.csv(x = db, file = paste0(getwd(),"/DATA/helocdataclean.csv"),
          row.names = FALSE)





#######################################

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
