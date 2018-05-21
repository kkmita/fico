
dane<-read.table("C:/Users/LENOVO/Desktop/FICO/heloc_dataset_v1.csv",sep=",",h=T,na.strings=c("",NA))

#zmienna celu: RiskPerformance

kolumny_nic<-numeric(24)
for(i in 1:24){
  kolumny_nic[i]<-sum(is.na(dane[,i]))
}
#nie ma pustek

dane_med<-dane
colnames(dane_med)[1]<-"class"
dane_med$class<-ifelse(dane_med$class=="Good",1,0)
#podzial na testowy i treningowy
set.seed(123)
testowy<-sample(1:10459)
dane_test<-dane_med[testowy[1:3137],]
dane_proba<-dane_med[-testowy[1:3137],]

sum(dane_proba$class==1)

#wybor zmiennych

#W celu wybrania najbardziej istotnych zmiennych
#posłuzylam sie dwiema metodami. Pierwsza z nich dotyczyła modelu logistycznego, w którym zmienna objasniana
#uzalezniona jest tylko od stałej. Nastepnie wyznaczylam dewiacje modelu i uszeregowalam modele ze wzgledu na jej
#wartosc (malejaco). Druga metoda to drzewa klasyfikacyjne. W wyniku uzycia metod otrzymałam ocene istotnosci
#dla kazdej zmiennej.

#modele z "_caly" sa budowane na wszystkich zmiennych
#wiem, że raczej nie powinnam ucinac bo jest ich malo ale warto zobaczyc c sie stanie i zawsze to wiecej metod :D

#1) drzewa
library(rpart)
wypelnione=1
waznosc<-numeric(10000)
waznosc_names<-numeric(10000)

for(i in c(5,10,20,50,75,100,150,200,300,500)){
  tree<-rpart(class~.,data=dane_proba,minsplit=i)
  c<-100/sum(tree$variable.importance)
  dlugosc<-length(tree$variable.importance)
  cosik<-tree$variable.importance*c
  waznosc[wypelnione:(wypelnione+dlugosc-1)]<-tree$variable.importance*c
  waznosc_names[wypelnione:(wypelnione+dlugosc-1)]<-names(tree$variable.importance*c)
  wypelnione<-wypelnione+dlugosc
}
waznosc[waznosc>0]
waznosc_names[waznosc>0]
table(waznosc_names[waznosc>0])
a<-data.frame(waznosc[waznosc>0],waznosc_names[waznosc>0])
names(a)<-c("kol1","kol2")
library(plyr)
a<-ddply(a,"kol2",numcolwise(sum))
a<-a[order(a$kol1,decreasing=T),]

# 13 zmiennych

#kol2        kol1
#2                ExternalRiskEstimate 377.3774149
#3            MaxDelq2PublicRecLast12M 130.2192498
#12             PercentTradesNeverDelq 117.6639390
#4                         MaxDelqEver 116.7904583
#9          NetFractionRevolvingBurden  97.7026824
#5                MSinceMostRecentDelq  67.8955562
#13              PercentTradesWBalance  22.4633145
#1                      AverageMInFile  15.2674392
#7           MSinceMostRecentTradeOpen  15.2674392
#8               MSinceOldestTradeOpen  15.2674392
#11              NumSatisfactoryTrades  15.2674392
#10 NumBank2NatlTradesWHighUtilization   8.7068220
#6        MSinceMostRecentInqexcl7days   0.1108061

# glm

nazwy<-names(dane_proba)[-1]
modele<-sapply(nazwy, function(x){
  a<-glm(substitute(class~i,list(i=as.name(x))),family="binomial",data=dane_proba)
  a$null.deviance-a$deviance
})
wybrane_zmienne_glm<-names(sort(modele,decreasing =T))



#ostateczny wybor zmiennych
zm<-intersect(wybrane_zmienne_glm,a$kol2)
length(zm)
zm_ost<-numeric(14)
for(w in 1:13){
  zm_ost[w]<-which(names(dane_proba)==zm[w])
}
zm_ost[14]<-1
dane_ostateczne<-dane_med[,zm_ost]
dane_ost_pr<-dane_proba[,zm_ost]
dane_ost_test<-dane_test[,zm_ost]

##############################################3##############

#logistyczny  
library(MASS)
model_glm<-glm(class~.,data=dane_ost_pr,family="binomial")
pred<-predict(model_glm,newdata=dane_ost_test,type="response")
odp<-ifelse(pred>0.5,1,0)
tab1<-table(odp,dane_ost_test$class)
sum(diag(tab1))/sum(tab1)
#acc skuteczność 0.6866433
auc(dane_ost_test$class,pred)
#0.7496

model_glm_caly<-glm(class~.,data=dane_proba,family="binomial")
pred_caly<-predict(model_glm_caly,newdata=dane_test,type="response")
odp_caly<-ifelse(pred_caly>0.5,1,0)
tab2<-table(odp_caly,dane_test$class)
sum(diag(tab2))/sum(tab2)
#0.7153331 acc
library(pROC)
auc(dane_test$class,pred_caly)
# auc Area under the curve: 0.7809



#drzewa decyzyjne

#Do wyznaczenia optymalnych wartosci minsplit (minimalna liczba obserwacji, która musi byc w wezle zeby dokonac
# podziału) oraz cp posłuzyłam sie dwiema petlami. Wartosci wybrałam dla 10 wartosci z ciagu odpowiednio
#od 0.0001 do 0.3 oraz od 5 do 2500.



library(rpart)
ilosc0gdy1<-matrix(0,ncol=10,nrow=10)
ilosc0gdy0<-matrix(0,ncol=10,nrow=10)
ilosc1gdy1<-matrix(0,ncol=10,nrow=10)
ilosc1gdy0<-matrix(0,ncol=10,nrow=10)
procent_popr<-matrix(0,ncol=10,nrow=10)
n1 = length(which(dane_ost_test$class==1))
n2 = length(which(dane_ost_test$class==0)) 
n=n1+n2

index2=1
for (j in c(0.0001,0.0005,0.001,0.005,0.01,0.05,0.075,0.1,0.2,0.3)){
  index=1
  for(i in c(5,20,50,100,200,300,500,1000,2000,2500)){
    tree<-rpart(class~.,data=dane_ost_pr,minsplit=i,cp=j)
    P=ifelse(predict(tree,newdata=dane_ost_test)>0.5,1,0)
    ilosc0gdy1[index2,index]<-length((1:n)[P==0 & dane_ost_test$class==1])
    ilosc0gdy0[index2,index]<-length((1:n)[P==0 & dane_ost_test$class==0])
    ilosc1gdy1[index2,index]<-length((1:n)[P==1 & dane_ost_test$class==1])
    ilosc1gdy0[index2,index]<-length((1:n)[P==1 & dane_ost_test$class==0])
    procent_popr[index2,index]<-(ilosc1gdy1[index2,index]+ilosc0gdy0[index2,index])/(ilosc1gdy1[index2,index]+ilosc0gdy0[index2,index]+ilosc1gdy0[index2,index]+ilosc0gdy1[index2,index])
    index=index+1
  }
  index2=index2+1
}
procent_popr*100
ilosc1gdy1/n1*100
ilosc1gdy1+ilosc1gdy0

#Wybrane parametry w pierwszym przypadku to: minsplit-50 oraz cp-0,005.
#acc 71.24641
tree<-rpart(class~.,data=dane_ost_pr,minsplit=50,cp=0.005)
pred<-predict(tree,newdata=dane_ost_test)
auc(dane_ost_test$class,pred)
#Area under the curve: 0.7578

#drzewa_caly

ilosc0gdy1<-matrix(0,ncol=10,nrow=10)
ilosc0gdy0<-matrix(0,ncol=10,nrow=10)
ilosc1gdy1<-matrix(0,ncol=10,nrow=10)
ilosc1gdy0<-matrix(0,ncol=10,nrow=10)
procent_popr<-matrix(0,ncol=10,nrow=10)
n1 = length(which(dane_test$class==1))
n2 = length(which(dane_test$class==0)) 
n=n1+n2

index2=1
for (j in c(0.0001,0.0005,0.001,0.005,0.01,0.05,0.075,0.1,0.2,0.3)){
  index=1
  for(i in c(5,20,50,100,200,300,500,1000,2000,2500)){
    tree<-rpart(class~.,data=dane_proba,minsplit=i,cp=j)
    P=ifelse(predict(tree,newdata=dane_test)>0.5,1,0)
    ilosc0gdy1[index2,index]<-length((1:n)[P==0 & dane_test$class==1])
    ilosc0gdy0[index2,index]<-length((1:n)[P==0 & dane_test$class==0])
    ilosc1gdy1[index2,index]<-length((1:n)[P==1 & dane_test$class==1])
    ilosc1gdy0[index2,index]<-length((1:n)[P==1 & dane_test$class==0])
    procent_popr[index2,index]<-(ilosc1gdy1[index2,index]+ilosc0gdy0[index2,index])/(ilosc1gdy1[index2,index]+ilosc0gdy0[index2,index]+ilosc1gdy0[index2,index]+ilosc0gdy1[index2,index])
    index=index+1
  }
  index2=index2+1
}
procent_popr*100
ilosc1gdy1/n1*100
ilosc1gdy1+ilosc1gdy0 

#Wybrane parametry w pierwszym przypadku to: minsplit-50 oraz cp-0,005.
#acc 71.11890
tree<-rpart(class~.,data=dane_proba,minsplit=50,cp=0.005)
pred<-predict(tree,newdata=dane_test)
auc(dane_test$class,pred)
#bardzo mała różnica, ale zawsze coś.. byłaby bardziej widoczna gdyby było więcej zmiennych
#Area under the curve: 0.7589

#2)lda
model_lda<-lda(class~.,data=dane_ost_pr)
z<-predict(model_lda,newdata=dane_ost_test)
tabela_m_lda<-table(z$class,dane_ost_test$class)
sum(diag(tabela_m_lda))/sum(tabela_m_lda)
#acc 0.6885559
#auc 0.749
auc(dane_ost_test$class,unlist(z[3]))


#caly
model_lda2<-lda(class~.,data=dane_proba)
z2<-predict(model_lda2,newdata=dane_test)
tabela2<-table(z2$class,dane_test$class)
tabela2
sum(diag(tabela2))/sum(tabela2)
#acc 0.7131017
auc(dane_test$class,unlist(z2[3]))
#auc 0.7796

#######KNN bez bootsrap 
library(MASS)
library(class)
Ilosc0gdy1<-numeric(20)
Ilosc0gdy0<-numeric(20)
Ilosc1gdy1<-numeric(20)
Ilosc1gdy0<-numeric(20)
Procent_popr<-numeric(20)
N1 = length(which(dane_ost_test$class==1))
N2 = length(which(dane_ost_test$class==0)) 
N=N1+N2

index=1
for(i in c(1,3,5,7,9,11,13,15,17,21,25,29,35,45,55,75,101,121,151,201)){
  model_knn= knn(dane_ost_pr[,1:13],dane_ost_test[,1:13],cl=dane_ost_pr[,14],k=i,prob=FALSE)
  Ilosc0gdy1[index]<-length((1:N)[model_knn==0 & dane_ost_test$class==1])
  Ilosc0gdy0[index]<-length((1:N)[model_knn==0 & dane_ost_test$class==0])
  Ilosc1gdy1[index]<-length((1:N)[model_knn==1 & dane_ost_test$class==1])
  Ilosc1gdy0[index]<-length((1:N)[model_knn==1 & dane_ost_test$class==0])
  Procent_popr[index]<-(Ilosc1gdy1[index]+Ilosc0gdy0[index])/(Ilosc1gdy1[index]+Ilosc0gdy0[index]+Ilosc1gdy0[index]+Ilosc0gdy1[index])
  
  index=index+1
}

Procent_popr*100
Ilosc1gdy1/N1*100
Ilosc1gdy1+Ilosc1gdy0  

model_knn<- knn(dane_ost_pr[,1:13],dane_ost_test[,1:13],cl=dane_ost_pr[,14],k=45,prob=T)
z<-predict(model_knn,newdata=dane_ost_test)
auc(dane_ost_test$class,unlist(z[3]))
#70.13070 dla param. 45 (k-(liczba obiektów najblizszych w stosunku do obiektu
#klasyfikowanego, liczba “sasiadów”)
#auc 0.749


#caly

Ilosc0gdy1d<-numeric(20)
Ilosc0gdy0d<-numeric(20)
Ilosc1gdy1d<-numeric(20)
Ilosc1gdy0d<-numeric(20)
Procent_poprd<-numeric(20)
N1 = length(which(dane_test$class==1))
N2 = length(which(dane_test$class==0)) 
N=N1+N2

index=1
for(i in c(1,3,5,7,9,11,13,15,17,21,25,29,35,45,55,75,101,121,151,201)){
  model_knn= knn(dane_proba[,2:24],dane_test[,2:24],cl=dane_proba[,1],k=i,prob=FALSE)
  Ilosc0gdy1d[index]<-length((1:N)[model_knn==0 & dane_test$class==1])
  Ilosc0gdy0d[index]<-length((1:N)[model_knn==0 & dane_test$class==0])
  Ilosc1gdy1d[index]<-length((1:N)[model_knn==1 & dane_test$class==1])
  Ilosc1gdy0d[index]<-length((1:N)[model_knn==1 & dane_test$class==0])
  Procent_poprd[index]<-(Ilosc1gdy1d[index]+Ilosc0gdy0d[index])/(Ilosc1gdy1d[index]+Ilosc0gdy0d[index]+Ilosc1gdy0d[index]+Ilosc0gdy1d[index])
  index=index+1
}

Procent_poprd*100
Ilosc1gdy1d/N1*100
Ilosc1gdy1d+Ilosc1gdy0d
#k=21 acc 69.33376
#! tutaj lepiej jednak na wybranych zmiennych
model_knn<-knn(dane_proba[,2:24],dane_test[,2:24],cl=dane_proba[,1],k=21,prob=T)
z<-predict(model_knn,newdata=dane_test)
auc(dane_test$class,unlist(z[3]))
#  0.751

#svm
#linear

install.packages("e1071")
library(e1071)
#
model_svm<- svm(dane_ost_pr[,-14],as.factor(dane_ost_pr[,14]),kernel="linear")
yhat1 <- predict(model_svm,newdata=dane_ost_test[,-14])
tabela_svm_lin<-table(yhat1,newdata=dane_ost_test[,14])
sum(diag(tabela_svm_lin))/sum(tabela_svm_lin)
#0.6917437 acc
#auc 0.681


#caly
model_svm_c = svm(dane_proba[,-1],as.factor(dane_proba[,1]),kernel="linear")
yhat2 = predict(model_svm_c,newdata=dane_test[,-1])
tabela_svm_lin_c<-table(yhat2,newdata=dane_test[,1])
sum(diag(tabela_svm_lin_c))/sum(tabela_svm_lin_c)
#acc 0.7150143
#auc 0.69



#svm
#polynomial

#
model_svm = svm(dane_ost_pr[,-14],as.factor(dane_ost_pr[,14]),kernel="polynomial")
yhat1 = predict(model_svm,newdata=dane_ost_test[,-14])
tabela_svm_lin<-table(yhat1,newdata=dane_ost_test[,14])
sum(diag(tabela_svm_lin))/sum(tabela_svm_lin)
#0.6920625
#auc 0.679

#caly
model_svm_c = svm(dane_proba[,-1],as.factor(dane_proba[,1]),kernel="polynomial")
yhat2 = predict(model_svm_c,newdata=dane_test[,-1])
tabela_svm_lin_c<-table(yhat2,newdata=dane_test[,1])
sum(diag(tabela_svm_lin_c))/sum(tabela_svm_lin_c)
#acc 0.7022633
#auc 0.677

#radial
model_svm = svm(dane_ost_pr[,-14],as.factor(dane_ost_pr[,14]),kernel="radial")
yhat1 = predict(model_svm,newdata=dane_ost_test[,-14])
tabela_svm_lin<-table(yhat1,newdata=dane_ost_test[,14])
sum(diag(tabela_svm_lin))/sum(tabela_svm_lin)
#0.7233025
#auc 0.683

#caly
model_svm_c = svm(dane_proba[,-1],as.factor(dane_proba[,1]),kernel="radial")
yhat2 = predict(model_svm_c,newdata=dane_test[,-1])
tabela_svm_lin_c<-table(yhat2,newdata=dane_test[,1])
sum(diag(tabela_svm_lin_c))/sum(tabela_svm_lin_c)
#acc 0.7248964
#auc 0.692
#jadro radialne najlepsze

#svm po roznych gamma i kosztach
silosc0gdy1ddd<-matrix(0,ncol=5,nrow=5)
silosc0gdy0ddd<-matrix(0,ncol=5,nrow=5)
silosc1gdy1ddd<-matrix(0,ncol=5,nrow=5)
silosc1gdy0ddd<-matrix(0,ncol=5,nrow=5)
silosc<-numeric()
sn1 = length(which(dane_ost_test$class==1))
sn2 = length(which(dane_ost_test$class==0)) 
sn=sn1+sn2

index2=1
for (j in c(0.01,0.1,1,4,10)){
  index=1
  for(i in c(0.0001,0.01,0.1,1,3)){
    model= svm(dane_ost_pr[,-14],as.factor(dane_ost_pr[,14]),gamma=i,cost=j)
    P= predict(model,newdata=dane_ost_test[,-14])
    silosc0gdy1ddd[index2,index]<-length((1:sn)[P==0 & dane_ost_test$class==1])
    silosc0gdy0ddd[index2,index]<-length((1:sn)[P==0 & dane_ost_test$class==0])
    silosc1gdy1ddd[index2,index]<-length((1:sn)[P==1 & dane_ost_test$class==1])
    silosc1gdy0ddd[index2,index]<-length((1:sn)[P==1 & dane_ost_test$class==0])
    silosc[index]<-(silosc1gdy1ddd[index]+silosc0gdy0ddd[index])/(silosc1gdy1ddd[index]+silosc0gdy0ddd[index]+silosc1gdy0ddd[index]+silosc0gdy1ddd[index])
    index=index+1
  }
  index2=index2+1
}

silosc
silosc1gdy1ddd/sn1*100
silosc1gdy1ddd+silosc1gdy0ddd 

#caly
silosc0gdy1ddd<-matrix(0,ncol=5,nrow=5)
silosc0gdy0ddd<-matrix(0,ncol=5,nrow=5)
silosc1gdy1ddd<-matrix(0,ncol=5,nrow=5)
silosc1gdy0ddd<-matrix(0,ncol=5,nrow=5)
sn1 = length(which(dane_ost_test$class==1))
sn2 = length(which(dane_ost_test$class==0)) 
sn=sn1+sn2

index2=1
for (j in c(0.01,0.1,1,4,10)){
  index=1
  for(i in c(0.0001,0.01,0.1,1,3)){
    model= svm(dane_proba[,-1],as.factor(dane_proba[,1]),gamma=i,cost=j)
    P= predict(model,newdata=dane_test[,-1])
    silosc0gdy1ddd[index2,index]<-length((1:sn)[P==0 & dane_test$class==1])
    silosc0gdy0ddd[index2,index]<-length((1:sn)[P==0 & dane_test$class==0])
    silosc1gdy1ddd[index2,index]<-length((1:sn)[P==1 & dane_test$class==1])
    silosc1gdy0ddd[index2,index]<-length((1:sn)[P==1 & dane_test$class==0])
    index=index+1
  }
  index2=index2+1
}

silosc
silosc1gdy1ddd/sn1*100
silosc1gdy1ddd+silosc1gdy0ddd 



# bagging drzew

library(rpart)
library(foreign)


bagging_drzew <- function(formula, data, B=10, ...){
  drzewa=vector("list",B)
  n<-nrow(data)
  for (i in 1:B){
    w<-sample(x = 1:n,size=n,replace=T)
    dane=data[w,]
    drzewa[[i]]=rpart(formula=formula,data=data,...)
  }
  return(drzewa)
}


modele <- bagging_drzew(as.formula("class~."),dane_ost_pr)
B <- length(modele)
predykcje <- vector("list",B)
predykcje <- lapply(modele, function(x){
  ifelse(predict(x, newdata=dane_ost_test)>0.5,1,0)  })
wyniki<-matrix(nrow=11,ncol=3137)
for(i in 1:10){
  wyniki[i,] <-as.vector(predykcje[[i]])
}
for(i in 1:3137){
  wyniki[11,i] <-sum(wyniki[1:10,i])
}

wyniki[11,]
pred_bag<-ifelse(wyniki[11,]>=5,1,0)
tabela_bagging_drzew<-table(pred_bag,dane_test$class)
#pred_bag    0    1
#0 1227  578
#1  366  966

sum(diag(tabela_bagging_drzew))/sum(tabela_bagging_drzew)
#0.6990755
#auc 0.779


#cale
modele <- bagging_drzew(as.formula("class~."),dane_proba)
B <- length(modele)
predykcje <- vector("list",B)
predykcje <- lapply(modele, function(x){
  ifelse(predict(x, newdata=dane_test)>0.5,1,0)  })
wyniki<-matrix(nrow=11,ncol=3137)
for(i in 2:11){
  wyniki[i,] <-as.vector(predykcje[[i-1]])
}
for(i in 1:3137){
  wyniki[1,i] <-sum(wyniki[2:11,i])
}

wyniki[1,]
pred_bag<-ifelse(wyniki[1,]>=5,1,0)
tabela_bagging_drzew<-table(pred_bag,dane_test$class)
sum(diag(tabela_bagging_drzew))/sum(tabela_bagging_drzew)
#0.6995732
#0.781

# lasy losowe
install.packages("randomForest")
library(randomForest)
forest<-randomForest(class~.,data=dane_proba)
pred_forest<-predict(forest,newdata=dane_test,type="class",ntree=100)
odp6<-as.numeric(pred_forest>0.5)
tabela_lasy_losowe<-table(odp6,dane_test$class)
#odp6    0    1
#0 1267  531
#1  326 1013

sum(diag(tabela_lasy_losowe))/sum(tabela_lasy_losowe)
#acc 0.7268091 ładnie :) 
#auc 0.782

###############
forest<-randomForest(class~.,data=dane_ost_pr)
pred_forest<-predict(forest,newdata=dane_ost_test,type="class",ntree=100)
odp6<-as.numeric(pred_forest>0.5)
tabela_lasy_losowe<-table(odp6,dane_test$class)
#odp6    0    1
#0 1253  539
#1  340 1005

sum(diag(tabela_lasy_losowe))/sum(tabela_lasy_losowe)
#acc 0.719796
#auc 0.784