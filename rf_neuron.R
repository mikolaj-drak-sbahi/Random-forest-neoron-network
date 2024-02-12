#Funkcja ACC - accuracy
acc <- function(y.true, y.pred) { sum(y.pred==y.true)/length(y.true) }

#Funkcja roc.function
library(ROCR)
roc.function<-function(y_pred,testY){
  pred <- prediction(as.numeric(y_pred), as.numeric(testY))
  perf.auc <- performance(pred, measure = "auc")
  auc<-round(unlist(perf.auc@y.values),2)
  perf <- performance(pred,"tpr","fpr")
  plot(perf,main=paste("Krzywa ROC i parametr AUC=",auc),colorize=TRUE, lwd = 3)
  abline(a = 0, b = 1, lwd = 2, lty = 2) #przekatna
}



#LAS LOSOWY---------------------
car<-read.csv("Car_Insurance_Claim.csv", stringsAsFactors = T)
car <- car[-1]
car<-na.omit(car)
str(car)
summary(car)
#zmienna celu w tym przypadku musze zmienic na factor
car$OUTCOME <- factor(car$OUTCOME, levels = c(0, 1))
levels(car$OUTCOME)<-c("nie dostał","dostał")

library(caTools)
set.seed(123)
split = sample.split(car$OUTCOME, SplitRatio = 0.7)
training_set = subset(car, split == TRUE)
test_set = subset(car, split == FALSE)

#rozklad zmiennej celu
prop.table(table(car$OUTCOME))
#nie dostał     dostał 
#0.6887962     0.3112038 

prop.table(table(training_set$OUTCOME))
#nie dostał     dostał 
#0.6888149     0.3111851 

prop.table(table(test_set$OUTCOME))
#nie dostał     dostał 
#0.6887526     0.3112474 

#możemy zauważyć, że w przypadku całego zbioru jak i zbiorów
#treningowego i testowego odszkodowania nie otrzymało
#prawie 69% osób

library(randomForest)
set.seed(123)
#model lasu
car.rf.1 <- randomForest(OUTCOME~., data=training_set, ntree=500)
car.pred.1<- predict(car.rf.1, newdata = test_set, type="class")

#macierz pomylek
table(car.pred.1, test_set$OUTCOME)
#car.pred.1     nie dostał   dostał
#nie dostał         1524      230
#dostał              160      531
#160 osób, które nie dostały odszkodowania, zostało błędnie sklasyfikowanych
#jako osoby, które dostały
#230 osób, które dostały odszkodowania, zostało błędnie sklasyfikowanych
#jako osoby, które nie dostały
#łącznie 390 na 2445 osób błędnie sklasyfikowanych

acc.rf=acc(car.pred.1, test_set$OUTCOME)
acc.rf
#acc = 0.8404908 więc jest wysoka dokładność
roc.function(car.pred.1, test_set$OUTCOME)
#auc = 0.8 także wysokie

#wykres błedu
plot(car.rf.1)
#Im blizej siebie znajdują się linie na wykresie, tym lepszy model.
#więc ten model nie jest idealny
#Wykres narysowany jest na podstawie tablicy błędów. 


car.rf.1.legend <- colnames(car.rf.1$err.rate)
legend(x=300,y=0.5,legend = car.rf.1.legend,lty=c(1,2,3),
       col=c(1,2,3))

min.err <- min(car.rf.1$err.rate[,"OOB"])
min.err.idx <- which(car.rf.1$err.rate[,"OOB"]== min.err)
car.rf.1$err.rate[min.err.idx,]
#Wypisuje informacje dla indeksów, które odpowiadają minimalnej wartości błędu OOB
#OOB        nie dostał  dostał 
#0.1553296  0.1007890  0.2760563
#widzimy, że większy błąd jest w przypadku osób, które dostały odszkodowanie

#ważność zmiennych
car.rf.1$importance
varImpPlot(car.rf.1,main="Wykres istotności zmiennych\n Las losowy") 
#z wykresu ważności zmiennych widzimy, że najważniejszymi zmiennymi
#wpływającymi na otrzymanie odszkodowania są
#driving_experience i credit_score
#za to najmniej ważne są race i vehicle_type

#wykres rozkładu minimalnej głębokości:
library(randomForestExplainer)
car.rf.min <- min_depth_distribution(car.rf.1)
head(car.rf.min, n=40)
plot_min_depth_distribution(car.rf.min,k=nrow(test_set))
#w przypadku rozkładu minimalnej głębokości podobnie jak w ważności zmiennych
#pierwsze miejsce zajmuje driving_experince, a ostatnie vehicle_type

#Grupowanie
car.rf.3 <- randomForest(x=car, y=NULL,ntree=1000, proximity=TRUE,
                           oob.prox = FALSE)
car.rf.3

library(cluster)
rf.hclust <- hclust(as.dist(car.rf.3$proximity),method="ward.D2")
plot(rf.hclust)
rf.clust <- cutree(rf.hclust,k=5)
table(rf.clust,car$OUTCOME)
#najwięcej danych znajduje się w klastrze 1, a najmniej w 4



#ADABAG----------------------
library(adabag)

car.boo <- boosting(OUTCOME~., training_set,mfinal=50)
car.boo$importance
#w przypadku adabag także driving_experience jest najważniejszą zmienną
car.boo$trees[[2]]

car.boo.pred=predict(car.boo, newdata=test_set, type="class")
car.boo.pred$confusion
car.boo.pred$class

table(car.boo.pred$class, test_set$OUTCOME)
#                nie dostał  dostał
#dostał              157      545
#nie dostał         1527      216
#adabag poprawił wynik lasu ponieważ tutaj tylko 373 osoby
#zostały błędnie sklasyfikowane czyli o 17 mniej

acc(car.boo.pred$class, test_set$OUTCOME)
#acc = 0.8474438 czyli trochę wyższe niż w przypadku lasu
roc.function(car.boo.pred$prob[,2], test_set$OUTCOME)
#auc = 0.92, tu widzimy znaczącą poprawę
#ponieważ w lesie auc wynosiło 0.8


#Metoda gradientowa GBM-------------------------
library(gbm)

#Zbiór wykorzystywany do modelowania musi zawierać wartości numeryczne.
library(caret)
car.num <- car
car.num$OUTCOME <- ifelse(car.num$OUTCOME == "dostał", 1, 0)
car.dv <- dummyVars("~ .",car[-18], fullRank = F)
car.d = as.data.frame(predict(car.dv, newdata=car[-18]))
car.d=cbind(car.d,car.num[18])
str(car.d)
summary(car.d)

library(caTools)
set.seed(12345)
split.g = sample.split(car.d$OUTCOME, SplitRatio = 0.7)
car.d.Train <- subset(car.d, split.g == TRUE)
car.d.Test <- subset(car.d, split.g == FALSE)

str(car.d.Train)

car.d.gbm <- gbm(OUTCOME~.,distribution="bernoulli",data= car.d.Train,
                n.trees=500,shrinkage = 0.05)
car.d.gbm

#Optymalna liczba drzew
liczba.drzew=gbm.perf(car.d.gbm)

#Testujemy zbudowany model
car.d.gbm.pred=predict(car.d.gbm, car.d.Test, n.trees = liczba.drzew)
car.d.gbm.pred


library(pROC)
gbm.roc = roc(car.d.Test$OUTCOME, car.d.gbm.pred)
x=plot(gbm.roc)
x
#auc = 0.9057 czyli wyższy niż w przypadku lasu ale niższy niż w Adabag
coords(gbm.roc, "best")
#treshold -0.5266911
car.d.gbm.pred.class = ifelse(car.d.gbm.pred > -0.5266911, 1, 0)
#dzielimy w zależności od treshold

table(car.d.gbm.pred.class, car.d.Test$OUTCOME)
#car.d.gbm.pred.class    0    1
#0                      1435  134
#1                       249  627
#384 osoby źle sklasyfikowane czyli mniej niż w lesie ale więcej niż w Adabag
acc(car.d.gbm.pred.class, car.d.Test$OUTCOME)
#acc = 0.8433538 także wyższe niż w lesie i niższe niż w Adabag


#Podsumowanie:
#Zbudowane przeze mnie modele uczenia zespołowego mają na celu ocenienie
#które zmienne znajdujące się w danych uczących mają największy wpływ na to
#czy ktoś otrzyma odszkodowanie, czy nie.
#Na podstawie tych modeli będzie można w przyszłości przewidzieć czy dana osoba
#otrzyma takie odszkodowanie.
#Największą dokładnością wykazał się model Adabag.
#Miał najmniej pomyłek oraz najwyższe acc i auc.
#Druga pod względem dokładności okazała się być metoda gradientowa GBM.
#Najsłabszy okazał się być las losowy.
#Najważniejszymi zmiennymi mającymi wpływ na otrzymanie odszkodowania
#są zmienne driving_experience i credit_score.
#Podsumowując wszystkie 3 modele są dokładne, ponieważ w każdym z nich
#acc wynosiło około 0.84, a auc od 0.8 do nawet 0.92.



#SIECI----------------------
#Funkcja normalizacji
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
#Funkcja błędu MSE
MSE<- function(y.true, y.pred) { sum((y.true - y.pred)^2)/length(y.true)}

library(caret)
ins<-read.csv("Insurance.csv", stringsAsFactors = T)
ins.dv <- dummyVars("~ .",ins[-7], fullRank = F)
ins.d = as.data.frame(predict(ins.dv, newdata=ins[-7]))
summary(ins.d)
ins.d=cbind(ins.d, ins[7])

ins.d<- as.data.frame(lapply(ins.d, normalize))
str(ins.d)

library(caTools)
set.seed(123)
split.s = sample.split(ins.d$charges, SplitRatio = 0.7)
training_set.s = subset(ins.d, split.s == TRUE)
test_set.s = subset(ins.d, split.s == FALSE)

#można porównać rozkład danych znormalizowanych i treningowych
summary(ins.d$charges)
summary(training_set.s$charges)
#rozkłady są podobne

#Regresja
library(neuralnet)
ins.nn1<-neuralnet(charges~age+sex.female+sex.male+bmi+children+smoker.no+
                   smoker.yes+region.northeast+region.northwest+
                   region.southeast+region.southwest, data=training_set.s)
ins.nn1
plot(ins.nn1)

#ocena modelu
ins_pred<-compute(ins.nn1, test_set.s)
pred_results<-ins_pred$net.result

postResample(pred_results, test_set.s$charges)
#   RMSE     Rsquared     MAE 
#0.08229280 0.76503434 0.06149229 
#R^2 czyli miara dopasowania modelu do danych uczących dosyć wysoka
MSE(pred_results, test_set.s$charges)
#MSE = 0.006772105, błąd średniokwadratowy niski więc model jest dobry
cor(pred_results, test_set.s$charges)
#cor 0.8746624

#wysoka korelacja pomiędzy wartościami otrzymanymi z modelu sieciowego a tymi, 
#które rzeczywiście występują w zbiorze danych
#dosyć wysokie R^2, duża korelacja i niskie błędy więc model jest dobrej jakości


#poprawa wydajności modelu
set.seed(123)
ins.nn2<-neuralnet(charges~age+sex.female+sex.male+bmi+children+smoker.no+
                   smoker.yes+region.northeast+region.northwest+
                   region.southeast+region.southwest, 
                   data=training_set.s, hidden = 4)
plot(ins.nn2)
ins_pred2<-compute(ins.nn2, test_set.s)
pred_results2<-ins_pred2$net.result

postResample(pred_results2, test_set.s$charges)
#   RMSE     Rsquared     MAE 
#0.06606620 0.85016325 0.03885286 
MSE(pred_results2, test_set.s$charges)
#MSE 0.004364743
cor(pred_results2, test_set.s$charges)
#cor 0.922043
#po wprowadzeniu 4 neuronów w warstwie ukrytej model poprawił się
#błędy się zmniejszyły, a R^2 i korelacja zwiększyły i są bliskie 1

#Dodanie kolejnych warstw
set.seed(123)
ins.nn43<-neuralnet(charges~age+sex.female+sex.male+bmi+children+smoker.no+
                       smoker.yes+region.northeast+region.northwest+
                       region.southeast+region.southwest, 
                       data=training_set.s, hidden = c(4,3,3))
plot(ins.nn43)

#każdy neuron jednej warstwy jest połączony z neuronem kolejnej warstwy
ins_pred43<-compute(ins.nn43, test_set.s)
pred_results43<-ins_pred43$net.result

postResample(pred_results43, test_set.s$charges)
#    RMSE    Rsquared     MAE 
#0.06499004 0.85592011 0.03887343 
MSE(pred_results43, test_set.s$charges)
#MSE 0.004223706
cor(pred_results43, test_set.s$charges)
#cor 0.9251595

#po dodaniu kolejnych warstw ukrytych wyniki delikatnie się polepszyły
#jednak nie jest to mocno widoczna poprawa w porównaniu z jedną warstwą
#ukrytą z 4 neuronami


#Klasyfikacja
ins2=read.csv("Insurance.csv",stringsAsFactors =T)

#dzielę zmienną celu charges tak żeby była podzielona w zależności od
#wysokości ubezpieczenia na high, median i low
summary(ins2)
q3=quantile(ins2$charges,0.75)
q1=quantile(ins2$charges,0.25)
m=max(ins2$charges)

temp=cut(ins2$charges,c(0,q1-0.01,q3,m))
levels(temp)=c("Low","Median","High")
table(temp)

ins2$charges=temp

#normalizacja
ins2.dv <- dummyVars("~ .",ins2[-7], fullRank = F)
ins2.d = as.data.frame(predict(ins2.dv, newdata=ins2[-7]))
summary(ins2.d)

ins2.d<- as.data.frame(lapply(ins2.d, normalize))
str(ins2.d)
ins2.d$charges=ins2$charges

set.seed(123)
split.k = sample.split(ins2.d$charges, SplitRatio = 0.7)
training_set.k = subset(ins2.d, split.k == TRUE)
test_set.k = subset(ins2.d, split.k == FALSE)

#rozkład danych
prop.table(table(ins2.d$charges))
prop.table(table(training_set.k$charges))
#w jednym i w drugim podobnie

#Ponieważ mamy 3 różne etykiety, więc dodajemy trzy zmienne do zbioru
training_set.k$Low = training_set.k$charges == "Low"
training_set.k$Median = training_set.k$charges == "Median"
training_set.k$High = training_set.k$charges == "High"
training_set.k=training_set.k[-12]

#Model
set.seed(123)
ins2.nn1= neuralnet(Low+Median+High~age+sex.female+sex.male+bmi+children+
                    smoker.no+smoker.yes+region.northeast+region.northwest+
                    region.southeast+region.southwest,
                    data = training_set.k,
                    hidden=4)
plot(ins2.nn1)
#warstwa wejściowa taka jak wcześniej, ale w wyjściowej są 3 neurony


#ocena
net1.predict = compute(ins2.nn1, test_set.k[-12])
str(net1.predict)
net1.predict$net.result

#Wybieramy w każdym wierszu kolumnę z największą wartością.
best.column=apply(net1.predict$net.result, 1, which.max)
net.prediction = c("Low","Median","High")[best.column]

#Porządkujemy macierz pomyłek
ordered.net.prediction <- factor(net.prediction, levels=c("Low","Median","High"), ordered=TRUE)
table(test_set.k$charges, ordered.net.prediction)
#       Low  Median High
#Low    101      0    0
#Median   4    196    0
#High     8     15   78

#model jest dobry, ponieważ jest niewiele pomyłek
#w przypadku grupy najwyższych ubezpieczeń nie ma pomyłek


#Podsumowanie:
#Zbudowane modele sieci mają na celu przewidzieć wysokość ubezpieczenia
#na podstawie danych wejściowych.
#Najlepszy okazał się model z 3 warstwami ukrytymi (1 warstwa z 4 neuronami 
#i 2 warstwy z 3 neuronami). 
#Miał on najwyższy wynik miary R^2 i największą korelację.
#Był on tylko delikatnie lepszy od modelu z jedną warstwą ukrytą z 4 neuronami.
#Oba modele miały R^2 na poziomie około 0.85 oraz korelację w okolicach 0.92.
#Miały też bardzo mały błąd średniokwadratowy.
#Trochę słabszy okazał się model z jednym neuronem w warstwie ukrytej
#ale też jest dosyć dobry
#Następnie stworzyłem model klasyfikacyjny
#gdzie podzieliłem zmienną celu na 3 grupy.
#Model okazał się być dobry ponieważ macierz pomyłek
#wykazała tylko 27 błędnych dopasowań z 402 obserwacji.

