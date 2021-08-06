#Installation of required packages
install.packages("kernlab")
library(kernlab)

#Data Initialisation
data <- read.csv(file.choose())
View(data)

#Data Modification
attach(data)
str(data)
data$size_category <- as.factor(data$size_category)

#Normalization of Values
normalize <- function(x){
  return((x-min(x))/((max(x)-min(x))))
}
final <- as.data.frame(lapply(data[3:30], normalize))
View(final)
final <- cbind(final,size_category)


#Sampling of Data
ind <- sample(2,nrow(final),replace = T,prob=c(0.8,0.2))
train <- final[ind==1,]
test <- final[ind==2,]

#Model Formation
model1 <- ksvm(size_category~.,data=train,kernel="rbfdot")

#Prediction of Data
pred1 <- predict(model1,test)
pred1
table(pred1,test$size_category)
 agree <- pred1==test$size_category
table(agree)
prop.table(table(agree)) #81% Accuracy
