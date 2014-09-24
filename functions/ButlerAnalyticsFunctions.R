#Add Libraries
library("e1071")
library("Metrics")
library("rpart")
library("randomForest")
library("party")
library("MASS")
library("ridge")
library("corrgram")

#User Functions
##FIgure out the number of clusters to build
userfunction.KmeansClusterApproximate = function(dataset){
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