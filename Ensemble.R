#Set Globals
setwd("C:\\Users\\Administrator\\Documents\\GitHub\\LM")

#Run Sources
source("functions\\ButlerAnalyticsFunctions.R")
source("functions\\Train.R")

remove(Train)
remove(Train.sample)
remove(Train.sample.categorical)
remove(Train.sample.numeric)



#####TEST  DATA######
#Read Files
Test = read.csv("data\\test.csv", header = TRUE)

#Adjust NA's to 0
Test[is.na(Test)] = 0

#Create Subsets
keepnumerics = c("var15","var17")
Test.numeric = subset(Test[keepnumerics])
keepcategorical = c("var1","var2","var3","var5","var6","var8","var9")
Test.categorical = subset(Test[keepcategorical])

#Predict
A = predictn = predict(model, Test.numeric, interval="predict")
B =  predictc = predict(model2, Test.categorical, interval="predict")
Predicted = cbind(Test, predict = (A  + B*4)/5)



#output to files
out = c("id", "predict")
write.csv(Predicted[out], "data\\results.csv", row.names = FALSE)
