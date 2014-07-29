#Add Libraries
library("e1071")
library("Metrics")

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

#####MODEL TRAINING DATA######
#Read Files
Train = read.csv("data\\train.csv", header = TRUE, nrows=100000)

#Adjust NA's to 0
Train[is.na(Train)] = 0

##Find cluster sizes based off sample of 10k
#userfunction.KmeansClusterPlot(Train[21:29], "Crime Clusters") #3
#userfunction.KmeansClusterPlot(Train[30:66], "Geo Clusters") #3
#userfunction.KmeansClusterPlot(Train[67:302], "Weather Clusters")
cclusters = 3
gclusters = 4
wclusters = 15



##Build Clusteres

Train.new = data.frame(Train[1:19], CrimeCluster = userfunction.Kmeans(Train[21:29], cclusters))
Train.new = data.frame(Train.new, GeoCluster = userfunction.Kmeans(Train[30:66], gclusters))
Train.new = data.frame(Train.new, WeatherCluster = userfunction.Kmeans(Train[67:302], wclusters))
Train = Train.new
remove(Train.new)

#Build SVM
model = svm(target~., data=Train[2:22])

#Predict Test
Predicted.Train = cbind(Train, predict = predict(model, Train[2:22], interval="predict"))


str(Train)
str(Test)



#####TEST  DATA######
#Read Files
Test = read.csv("data\\test.csv", header = TRUE)

#Adjust NA's to 0
Test[is.na(Test)] = 0

#Create clusters for analysis of crime, weather, and geo
Test.new = data.frame(Test[1:18], CrimeCluster = userfunction.Kmeans(Test[20:28], cclusters))
Test.new = data.frame(Test.new, GeoCluster = userfunction.Kmeans(Test[29:65], gclusters))
Test.new = data.frame(Test.new, WeatherCluster = userfunction.Kmeans(Test[66:301], wclusters))
Test = Test.new
remove(Test.new)

Predicted = cbind(Test, predict = predict(model, Test[2:21], interval="predict"))

#output to files
out = c("id", "predict")
write.csv(Predicted[out], "data\\results.csv", row.names = FALSE)



