
#####MODEL TRAINING DATA######
#Read Files
Train = read.csv("data\\train.csv", header = TRUE)

#Adjust NA's to 0
Train[is.na(Train)] = 0.0

#sample at 30%
Train.sample = Train[sample(nrow(Train), nrow(Train)*.3),]

#Training set for numeric
keepnumerics = c("target", "var15","var17")
Train.sample.numeric = subset(Train.sample[keepnumerics])

#Training set for categorical
keepcategorical = c("target", "var1","var2","var3","var5","var6","var8","var9")
Train.sample.categorical = subset(Train.sample[keepcategorical])
summary(Train.sample.categorical)


#Build MODEL
model = glm(target~.,data=Train.sample.numeric)
model2 = randomForest(target~.,data=Train.sample.categorical)


#ensemble builder
cat("Linear Model")
Predicted.Train = cbind(Train.sample, predictglm = predict(model, Train.sample.numeric[-c(1)], interval="predict"))
NormalizedWeightedGini(Predicted.Train$target, Predicted.Train$var11,Predicted.Train$predictglm)
sqrt((sum((Predicted.Train$target-Predicted.Train$predictglm)^2))/nrow(Predicted.Train))

cat("Random Forest")
Predicted.Train = cbind(Predicted.Train, predictrf = predict(model2, Train.sample.categorical[-c(1)], interval="predict"))
NormalizedWeightedGini(Predicted.Train$target, Predicted.Train$var11,Predicted.Train$predictrf)
sqrt((sum((Predicted.Train$target-Predicted.Train$predictsvm)^2))/nrow(Predicted.Train))

cat("Ensemble")
Predicted.Train = cbind(Predicted.Train, predict1=(Predicted.Train$predictrf+Predicted.Train$predictglm)/2)
Predicted.Train = cbind(Predicted.Train, predict2=(Predicted.Train$predictrf*2+Predicted.Train$predictglm)/3)
Predicted.Train = cbind(Predicted.Train, predict3=(Predicted.Train$predictrf+Predicted.Train$predictglm*2)/3)
Predicted.Train = cbind(Predicted.Train, predict4=(Predicted.Train$predictrf*4+Predicted.Train$predictglm)/5)
Predicted.Train = cbind(Predicted.Train, predict5=(Predicted.Train$predictrf+Predicted.Train$predictglm*4)/5)

#Error Measurement
cat("Equal Weight")
cat(sqrt((sum((Predicted.Train$target-Predicted.Train$predict1)^2))/nrow(Predicted.Train)))

cat("66% RF")
cat(sqrt((sum((Predicted.Train$target-Predicted.Train$predict2)^2))/nrow(Predicted.Train)))

cat("66% GLM")
cat(sqrt((sum((Predicted.Train$target-Predicted.Train$predict3)^2))/nrow(Predicted.Train)))

cat("80% RF")
cat(sqrt((sum((Predicted.Train$target-Predicted.Train$predict4)^2))/nrow(Predicted.Train)))

cat("80% GLM")
cat(sqrt((sum((Predicted.Train$target-Predicted.Train$predict5)^2))/nrow(Predicted.Train)))
