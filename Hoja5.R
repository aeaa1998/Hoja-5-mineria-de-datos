library("ggpubr")
library("ggplot2")
library(dplyr)
library(rpart)
library(caret)
library(tree)
library(rsample) 
library(rpart.plot)
library(randomForest)
library(ipred)
library(corrplot)
library(e1071)
library(klaR)


#Functions
conf_matrix <- function(df.true, df.pred, title = "", true.lab ="True Class", pred.lab ="Predicted Class",
                        high.col = 'red', low.col = 'white') {
  #convert input vector to factors, and ensure they have the same levels
  df.true <- as.factor(df.true)
  df.pred <- factor(df.pred, levels = levels(df.true))
  
  #generate confusion matrix, and confusion matrix as a pecentage of each true class (to be used for color) 
  df.cm <- table(True = df.true, Pred = df.pred)
  df.cm.col <- df.cm / rowSums(df.cm)
  
  #convert confusion matrices to tables, and binding them together
  df.table <- reshape2::melt(df.cm)
  df.table.col <- reshape2::melt(df.cm.col)
  df.table <- left_join(df.table, df.table.col, by =c("True", "Pred"))
  
  #calculate accuracy and class accuracy
  acc.vector <- c(diag(df.cm)) / c(rowSums(df.cm))
  class.acc <- data.frame(Pred = "Class Acc.", True = names(acc.vector), value = acc.vector)
  acc <- sum(diag(df.cm)) / sum(df.cm)
  
  #plot
  ggplot() +
    geom_tile(aes(x=Pred, y=True, fill=value.y),
              data=df.table, size=0.2, color=grey(0.5)) +
    geom_tile(aes(x=Pred, y=True),
              data=df.table[df.table$True==df.table$Pred, ], size=1, color="black", fill = 'transparent') +
    scale_x_discrete(position = "top",  limits = c(levels(df.table$Pred), "Class Acc.")) +
    scale_y_discrete(limits = rev(unique(levels(df.table$Pred)))) +
    labs(x=pred.lab, y=true.lab, fill=NULL,
         title= paste0(title, "\nAccuracy ", round(100*acc, 1), "%")) +
    geom_text(aes(x=Pred, y=True, label=value.x),
              data=df.table, size=4, colour="black") +
    geom_text(data = class.acc, aes(Pred, True, label = paste0(round(100*value), "%"))) +
    scale_fill_gradient(low=low.col, high=high.col, labels = scales::percent,
                        limits = c(0,1), breaks = c(0,0.5,1)) +
    guides(size=F) +
    theme_bw() +
    theme(panel.border = element_blank(), legend.position = "bottom",
          axis.text = element_text(color='black'), axis.ticks = element_blank(),
          panel.grid = element_blank(), axis.text.x.top = element_text(angle = 30, vjust = 0, hjust = 0)) +
    coord_fixed()
  
} 



trainSetGiven = read.csv("./data/train.csv", header = TRUE)


#Join the two sets of data
dataSet <- bind_rows(trainSetGiven)
dataSet$Id=NULL

set.seed(1234)

dataSetCompleteNumeric =dataSet[, !sapply(dataSet, is.character)]
dataSetCompleteNumeric = dataSetCompleteNumeric[complete.cases(dataSetCompleteNumeric), ]
dataSet = dataSet[complete.cases(dataSet$SalePrice),]

#caro > 150,000
#barato < 100,000
#medio 100,000 - 150,000
caros = dataSetCompleteNumeric[dataSetCompleteNumeric$SalePrice>150000,] 
caros$SaleVar = 'caro'
barato = dataSetCompleteNumeric[dataSetCompleteNumeric$SalePrice<100000,] 
barato$SaleVar = 'barato'
medio = dataSetCompleteNumeric[dataSetCompleteNumeric$SalePrice>100000 & dataSetCompleteNumeric$SalePrice<150000,] 
medio$SaleVar = 'medio'

#Proporciones caros
split <- initial_split(caros, prop = .65)
trainCaro <- training(split)
testCaro <- training(split)


#Proporciones medios
split <- initial_split(medio, prop = .65)
trainMedio <- training(split)
testMedio <- training(split)

#Proporciones baratos
split <- initial_split(barato, prop = .65)
trainBarato <- training(split)
testBarato <- training(split)



# Proporcion 35, 65
train <- bind_rows(trainCaro, trainMedio, trainBarato)
test  <- bind_rows(testCaro, testMedio, testBarato)
test$SaleVar = as.factor(test$SaleVar)
train$SaleVar = as.factor(test$SaleVar)
trainCopy <- train

#Realizamos nuestra prediccion con todo el conjunto
naiveBayesModelA<-naiveBayes(as.factor(trainCopy$SaleVar)~., data=trainCopy)
predBayesBigData<-predict(naiveBayesModelA, newdata = test)

confussionMatrixBigData<-caret::confusionMatrix(predBayesBigData,test$SaleVar)
confussionMatrixBigData
conf_matrix(test$SaleVar, predBayesBigData, title="confussionMatrixBigData")
print("Podemos ver que nuestro accuary es decente pero no bueno con un 73.48% eso es porque este metodo funciona con dataset menores en la prediccion")

#Realizamos nuestra prediccion con un subset
naiveBayesModelS<-naiveBayes(as.factor(trainCopy$SaleVar)~., data=trainCopy)
predBayesSmallData<-predict(naiveBayesModelA, newdata = test[,1:20])

confussionMatrixSmallData<-caret::confusionMatrix(predBayesSmallData,test$SaleVar)
confussionMatrixSmallData
conf_matrix(test$SaleVar, predBayesSmallData, title="confussionMatrixBigData")
print("Como podemos observar al jalar un subset nuestro accuary mejoro a 81% siendo una mejor prediccion")




