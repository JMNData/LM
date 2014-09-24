#Set Globals
setwd("C:\\Users\\Administrator\\Documents\\GitHub\\LM")

#Run Sources
source("functions\\ButlerAnalyticsFunctions.R")
source("functions\\Train.R")



keep = c("target", "crimeVar1","crimeVar2","crimeVar3","crimeVar4","crimeVar5","crimeVar6","crimeVar7","crimeVar8","crimeVar9")
Train.c = subset(Train.sample[keep])
Train.c = cbind(Train.c$target, crimeComp = crimeVar1*crimeVar2*crimeVar3*crimeVar4*crimeVar5*crimeVar6*crimeVar7*crimeVar8*crimeVar9)

corrgram(Train.c, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)
a = Train[]





#####TEST  DATA######
#Read Files
Test = read.csv("data\\test.csv", header = TRUE)

#Adjust NA's to 0
Test[is.na(Test)] = 0

#Create clusters for analysis of crime, weather, and geo
Test.new = data.frame(Test[1:18], CrimeCluster = userfunction.Kmeans(Test[20:28], cclusters))
Test.new = data.frame(Test.new, GeoCluster = userfunction.Kmeans(Test[29:65], gclusters))
#Test.new = data.frame(Test.new, WeatherCluster = userfunction.Kmeans(Test[66:301], wclusters))

keep = c("var15","var17")
Test.new = subset(Test.new[keep])

Predicted = cbind(Test, predict = predict(model, Test.new, interval="predict"))

#output to files
out = c("id", "predict")
write.csv(Predicted[out], "data\\results.csv", row.names = FALSE)
