data = read.csv(file.choose())
data[data == "?"] = NA
data$heart = ifelse(data$heart > 0, 1, 0)

##Merubah variabel yang seharusnya kategorik
cols = c("sex","cp","fbs","ecg","exang","slope","ca","thal","heart")
data[,cols] = lapply(data[,cols],as.factor)

data[!complete.cases(data),]

##Imputasi NA dengan Modus
mode = function(x) {
  ux = unique(x)
  tab = tabulate(match(x, ux))
  ux[tab == max(tab)]
}
NA.mode = function(x) replace(x, is.na(x), mode(x))
data = replace(data, TRUE, lapply(data, NA.mode))
data[!complete.cases(data),]

##Cek Outlier
#Boxplot
library(dplyr)
out.ind = c()
for (i in names(data[sapply(data,is.numeric)])){
  out.ind = c(out.ind, which(data[,i] %in% c(boxplot.stats(data[,i])$out)))
}
out.ind = unique(out.ind)

#Mahalanobis
data.num = Filter(is.numeric, data)
xbar = colMeans(data.num)
S = cov(data.num)
md = mahalanobis(data.num, xbar, S)
chisq = qchisq(p = 1-0.05, df = ncol(data.num))
out.md = which(md > chisq)

#Buang outlier
out.final = out.ind[out.ind %in% out.md]
out.final
data = data[-out.final,]

attach(data)

##Deskriptif
summary(data)

plot(sex, heart, xlab = "Sex", ylab="Heart",col=c(3,2))
plot(cp, heart, xlab = "CP", ylab="Heart",col=c(3,2))
plot(fbs, heart, xlab = "FBS", ylab="Heart",col=c(3,2))
plot(ecg, heart, xlab = "Restecg", ylab="Heart",col=c(3,2))
plot(exang, heart, xlab = "Exang", ylab="Heart",col=c(3,2))
plot(slope, heart, xlab = "Slope", ylab="Heart",col=c(3,2))
plot(ca, heart, xlab = "CA", ylab="Heart",col=c(3,2))
plot(thal, heart, xlab = "Thal", ylab="Heart",col=c(3,2))

boxplot(data.num)

heart.yes = data[which(heart == 1),]
summary(heart.yes)


##Reglog
fullmodel = glm(heart~., family = binomial, data = data)
summary(fullmodel)

model0 = glm(heart~1, family = binomial, data = data)

##Bandingin saturated sama model tanpa x
anova(model0, fullmodel, test = "Chisq")

##Backward stepwise
backwards = step(fullmodel)
summary(backwards)
##Bentuk model backward manual
backmodel = glm(heart~sex+cp+bps+chol+exang+oldpeak+slope+ca+thal, family = binomial,data = data)

##Keluarkan chol
remodel = glm(heart~sex+cp+bps+exang+oldpeak+slope+ca+thal, family = binomial, data = data)
summary(remodel)


anova(backmodel, fullmodel, test = "Chisq")
anova(remodel, fullmodel, test = "Chisq")
anova(remodel, backmodel, test = "Chisq")

accuracy.log = function(model, y){
  pred = round(predict(model, type = "response"))
  Kes = table(y, pred)
  hitratio = sum(diag(Kes))/sum(Kes)
  hasil = list("Kesimpulan" = Kes, "Hit Ratio" = hitratio)
  print(hasil)
}
accuracy.log(fullmodel, heart)
accuracy.log(backmodel, heart)
accuracy.log(remodel, heart)

odds.log = function(model){
  hasil = cbind(Estimates=coef(model),Odds=exp(coef(model)))
  print(hasil)
}
odds.log(model)


library(rpart)
library(rpart.plot)
library(MASS)

tree1 = rpart(heart ~ ., data = data)
tree1
tree1$variable.importance

rpart.plot(tree1, extra=2)

accuracy.tree = function(tree,y){
  pred = predict(tree, data, type="class")
  Kes = table(y, pred)
  hitratio = sum(diag(Kes))/sum(Kes)
  hasil = list("Kesimpulan" = Kes,"Hit Ratio" = hitratio)
  print(hasil)
}
accuracy.tree(tree1, heart)

##Feature Selection
feature = names(sort(tree1$variable.importance[1:5], decreasing=TRUE))
data2 = data[,c(feature)]

tree2 = rpart(heart ~ ., data = data2)
tree2
tree2$variable.importance

rpart.plot(tree2, extra=2)
