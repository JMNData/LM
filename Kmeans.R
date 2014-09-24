#Add Libraries
library("e1071")
library("Metrics")
library("rpart")
library("randomForest")
library("party")
library("MASS")
library("ridge")

#Set Globals
setwd("C:\\Users\\Administrator\\Documents\\GitHub\\LM")

#User Functions
##FIgure out the number of clusters to build
userfunction.KmeansClusterApproximate = function(dataset){
  x = round(sqrt(nrow(dataset)/2), digits=0)
  if x > 
    return(round(sqrt(nrow(dataset)/2), digits=0))
}
##Build a cluster model, predict each row, append to dataset
userfunction.Kmeans = function(InputData, numberofclusters){ 
  InputData = na.omit(InputData)
  fit = kmeans(InputData, numberofclusters)
  return(fit$cluster)
  #return cluster for append to dataset
}

userfunction.KmeansClusterPlot = function(TestData, Name){
  wss <- (nrow(TestData)-1)*sum(apply(TestData,2,var))
  for (i in 2:25) wss[i] <- sum(kmeans(TestData,centers=i)$withinss)
  plot(1:25, wss, type="b", xlab=Name,ylab="Within groups sum of squares")
  
}

WeightedGini <- function(solution, weights, submission){
  df = data.frame(solution = solution, weights = weights, submission = submission)
  df <- df[order(df$submission, decreasing = TRUE),]
  df$random = cumsum((df$weights/sum(df$weights)))
  totalPositive <- sum(df$solution * df$weights)
  df$cumPosFound <- cumsum(df$solution * df$weights)
  df$Lorentz <- df$cumPosFound / totalPositive
  n <- nrow(df)
  gini <- sum(df$Lorentz[-1]*df$random[-n]) - sum(df$Lorentz[-n]*df$random[-1])
  return(gini)
}

NormalizedWeightedGini <- function(solution, weights, submission) {
  WeightedGini(solution, weights, submission) / WeightedGini(solution, weights, solution)
}

#####MODEL TRAINING DATA######
#Read Files
Train = read.csv("data\\train.csv", header = TRUE)

#Adjust NA's to 0
Train[is.na(Train)] = 0.0

##Find cluster sizes based off sample of 10k
#userfunction.KmeansClusterPlot(Train[21:29], "Crime Clusters") #3
#userfunction.KmeansClusterPlot(Train[30:66], "Geo Clusters") #3
#userfunction.KmeansClusterPlot(Train[67:302], "Weather Clusters")
cclusters = 3
gclusters = 3
wclusters = 15

##Build Clusters
Train.new = data.frame(Train[1:19], CrimeCluster = userfunction.Kmeans(Train[21:29], cclusters))
Train.new = data.frame(Train.new, GeoCluster = userfunction.Kmeans(Train[30:66], gclusters))
#Train.new = data.frame(Train.new, WeatherCluster = userfunction.Kmeans(Train[67:302], wclusters))
keep = c("target", "var11","var12","var13","var14","var15","var16","var17")
Train.new = subset(Train.new[keep])
trainx = sapply(Train.new, scale)

#Build MODEL
#model = lm.ridge(target~., data=Train.new)
#model = linearRidge(target~., data=Train.new)
#model = glm(target~.,data=Train.new)
#model = glm(target~.,data=Train.new)
# 
# as.matrix(cor(Train.new))
 corrgram(trainx)


#model = svm(target~.,data=Train)
Predicted.Train = cbind(Train, predict = predict(model, Train[2:22], interval="predict"))
# Train.sub = subset(Predicted.Train,id>100000)
NormalizedWeightedGini(Predicted.Train$target, Predicted.Train$var11,Predicted.Train$predict)
# ftable(predict(model), Train$target)

#####TEST  DATA######
#Read Files
Test = read.csv("data\\test.csv", header = TRUE)

#Adjust NA's to 0
Test[is.na(Test)] = 0

#Create clusters for analysis of crime, weather, and geo
Test.new = data.frame(Test[1:18], CrimeCluster = userfunction.Kmeans(Test[20:28], cclusters))
Test.new = data.frame(Test.new, GeoCluster = userfunction.Kmeans(Test[29:65], gclusters))
#Test.new = data.frame(Test.new, WeatherCluster = userfunction.Kmeans(Test[66:301], wclusters))

keep = c("var11","var12","var13","var14","var15","var16","var17")
Test.new = subset(Test.new[keep])
??
Predicted = cbind(Test, predict = predict(model, Test.new, interval="predict"))

#output to files
out = c("id", "predict")
write.csv(Predicted[out], "data\\results.csv", row.names = FALSE)
