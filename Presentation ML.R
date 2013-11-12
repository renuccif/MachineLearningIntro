


##################################
##################################
####### Introduction to ML #######
##################################
##################################


# see "http://www.slideshare.net/renuccif/data-analytics-session-1-2013-27929321" on SlideShare.


# "Knowledge of the business 
# -> goals 
# -> metrics 
# -> Statistics 
# -> data visualization 
# -> data-mining 
# -> artificial intelligence 
# -> Machine Learning 
# -> business value"


########################
### Basic Statistics ###
########################


# definition : a statistic = a function of data

iris
head(iris)
dim(iris)
iris[1,2]
iris[1,]
iris[,5]
iris[,"Species"]
iris$Species

summary(iris)
mean(iris[,1])
mean(iris[,5])
sd(iris[1:100,1])
cor(iris[,"Sepal.Length"],iris[,"Petal.Length"])


mean(iris$Sepal.Length)
mean(iris[,"Sepal.Length"])
mean(iris[iris$Species=="virginica","Sepal.Length"])
mean(iris[iris$Species=="versicolor","Sepal.Length"])
mean(iris[iris$Species=="setosa","Sepal.Length"])

density(iris$Sepal.Length)
cor(iris[,1:4])


###########################
### Basic Visualization ###
###########################


plot(density(iris$Sepal.Length))
plot(density(iris$Sepal.Width))
plot(density(iris[iris$Species=="versicolor",]$Sepal.Length))
lines(density(iris[iris$Species=="setosa",]$Sepal.Length),col=2)
lines(density(iris[iris$Species=="virginica",]$Sepal.Length),col=3)

pairs(iris)
pairs(iris,col=iris$Species)
pairs(iris[,1:4],col=iris$Species)

install.packages("rgl")
library(rgl)
plot3d(iris)
plot3d(iris,col=iris$Species)


############################
### Basic Data Analytics ###
############################


install.packages("rpart")
library(rpart)
treeModel = rpart(Species ~ . , data = iris)
summary(treeModel)

library(rpart.plot)
prp(treeModel)


###  Classification  ###


rand = sample(nrow(iris),nrow(iris))
iris = iris[rand,]

learn_set = iris[1:120,] # 80% of the dataset
test_set = iris[121:150,] # 20% of the dataset
dim(learn_set)
dim(test_set)

head(learn_set)
head(test_set)

treeModel = rpart(Species ~ . , data = learn_set)
prp(treeModel)

help(predict)
?predict

predictedSpecies = predict(treeModel,test_set,type = "class")
head(predictedSpecies)
length(predictedSpecies)

predictedSpecies.Probabilities = predict(treeModel,test_set,type = "prob")
head(predictedSpecies.Probabilities)
dim(predictedSpecies.Probabilities)

test_set$Species
as.vector(predictedSpecies)


install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)

confusionMatrix(predictedSpecies,test_set$Species)$table
confusionMatrix(predictedSpecies,test_set$Species)


### many other classification algorithms, each adressing a given type of problem
### enough features ? too many ?

###  Regression  ###


library(caret)
data(cars)
head(cars)
index = sample(nrow(cars),0.8*nrow(cars))
train = cars[index,]
test = cars[-index,]

dim(train);dim(test)

cor(cars)
pairs(cars)
cor(cars)["Price",]
levelplot(as.matrix(cor(cars)[1,2:ncol(cars)]))


# /!\ correlations -> two times the same information ?
# /!\ correlations is not causality !


# Price = constant + something*Mileage + something else * Cylinder  + ...

linearModel = lm(Price  ~ ., train)
linearModel
summary(linearModel)


Price.predictions = predict(linearModel,test)
cbind(test$Price,Price.predictions)

errorRate = sqrt(sum((Price.predictions-test$Price)^2)/sum((test$Price)^2))
errorRate


# tells us what are the relevant features

improvedLinearModel = step(linearModel,direction="both")
Price.improvedPredictions = predict(improvedLinearModel,test)
cbind(test$Price,Price.improvedPredictions)

errorRate = sqrt(sum((Price.improvedPredictions-test$Price)^2)/sum((test$Price)^2))
errorRate

length(improvedLinearModel$coefficients)
length(linearModel$coefficients)
attributes(linearModel)


### Time Series Analysis ###


sales = scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
sales
sales = ts(sales, frequency=12, start=c(1987,1))

sales

plot.ts(sales)

# cor ? pairs ? Doesn't make sense here !

library("TTR")

logsales = log(sales)
plot(logsales)

before = logsales[1:72]
before = ts(before, frequency=12, start=c(1987,1))

after = logsales[73:84]
after = ts(after, frequency=12, start=c(1993,1))

model = HoltWinters(before)
plot(model)

library(forecast)
logsales.prediction = forecast.HoltWinters(model, h=12)

plot(logsales.prediction)
lines.ts(after,col=3)

components = decompose(sales)
plot(components)

components = decompose(logsales)
plot(components)

trend = components$trend
linear = lm(trend ~ seq(1:length(trend)))
summary(linear)

plot(trend)
predictions = ts(predict(linear,trend), frequency=12, start=c(1987,1))
lines(predictions,col=2)


###  Clustering  ###

# method 1 : I know the number of clusters

iris[,1:4]
head(iris[,1:4])
head(iris[,-5])
clustersModel = kmeans(iris[,1:4], 3)
plot3d(iris, col=clustersModel$cluster)
plot3d(iris, col=iris[,5])

table(clustersModel$cluster, iris$Species)


# method 2 : I don't know the number of clusters, or I want to choose it

distance = dist(iris[,-5], method="euclidean")

cluster = hclust(distance, method="median")
cluster = hclust(distance, method="average")
cluster = hclust(distance, method="complete")

plot(cluster, hang=-1, label=iris$Species)


plot(cluster, hang=-1, label=as.integer(iris$Species)^15)



