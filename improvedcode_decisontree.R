load("C:/Users/Mishaal/Desktop/Caravan Insurance.rda")
str(data)
data<- as.data.frame(data)
summary(data)
for (i in names(data[1:42])){data[,i] <- as.factor(data[,i])}
str(data)
data$Purchase<-as.factor(data$Purchase)
data$Purchase
train<-data[1:4500,]
test<-data[4501:5822,]
table(train$Purchase)
prop.table(table(train$Purchase))
n_total<-nrow(train)
new_frac<-0.5
#nw_n_total<-4500/0.5
library(ROSE)
bothsampling_result <- ovun.sample(Purchase ~ .,
                                   data = train,
                                   method = "both",
                                   N=n_total,
                                   p=new_frac,
                                   seed = 2018)
bothsampled_purchase <- bothsampling_result$data
table(bothsampled_purchase$Purchase)
prop.table(table(bothsampled_purchase$Purchase))
library(rpart)
library(caret) 
cust.rp<-rpart(Purchase~. , data=bothsampled_purchase)
cust.rp
cust.rp$cptable
print(cust.rp$cptable)
cust4.var.imp <-varImp(cust.rp, useModel=rpart)
cust4.var.imp
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(cust.rp)
png("Plot_final.png", width = 8, height = 8, units = 'in', res = 2000)
rpart.plot(cust.rp)
dev.off()
#prediction_train<- predict(cust.rp, bothsampled_purchase, type="class")
#prediction_test<- predict(cust.rp, test, type="class")
#combined<-c(prediction_train, prediction_test)
#combined
prediction_train2<- predict(cust.rp, bothsampled_purchase, type="prob")[, 2]
prediction_test2<- predict(cust.rp, test, type="prob")[, 2]
all <- c(prediction_train2, prediction_test2)
install.packages("pROC")
library("pROC")
roc_obj <- roc(bothsampled_purchase$Purchase,prediction_train2)
roc_obj
plot(roc_obj)
coords(roc_obj, "best", "threshold")
roc_obj <- roc(test$Purchase,prediction_test2)
roc_obj
plot(roc_obj)
coords(roc_obj, "best", "threshold")
prediction_test3<- predict(cust.rp, test, type="prob")[, 2]
final_predict_test<-factor(ifelse(prediction_test3>0.533281,"Yes","No"))
accuracy.meas(test$Purchase, prediction_test2, threshold = 0.533281)
CM2<-confusionMatrix(data=final_predict_test,reference = test$Purchase)
CM2
final_predict_train<-factor(ifelse(prediction_train2>0.533281,"Yes","No"))
CM4<-confusionMatrix(data=final_predict_train,reference = train$Purchase)
CM4
CM3<-confusionMatrix(data=final_predict_train,reference = bothsampled_purchase$Purchase)
CM3
auc(roc(response=test$Purchase,predictor=prediction_test3))
write.csv(data,file="original.csv")
write.csv(final_predict_test,file="final_predict_test.csv")
write.csv(final_predict_train,file="final_predict_train1.csv")
#write.csv(all,file="pred_insurance.csv")
#accuracy.measperf <- performance(prediction_test3,"tpr","fpr")(bothsampled_purchase$Purchase,prediction_train2  , threshold =0.5332821)
roc.curve(test$Purchase, prediction_test2)
#install.packages("DMwR")
PRcurve(prediction_test2,test$Purchase)
library(ROCR)
pred <- prediction(prediction_test2,test$Purchase)
perf <- performance(pred,"tpr","fpr")
perf1 <- performance(pred, "prec", "rec")
perf2 <- performance(pred, "sens", "spec")
plot(perf)
plot(perf2)
plot(perf1)

