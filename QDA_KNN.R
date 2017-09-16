require(ISLR)
require(MASS)

##fit small model using linear discriminant analysis
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
lda.fit
plot(lda.fit)
##get data for year 2005 so we can try to make predictions
Smarket.2005=subset(Smarket, Year==2005)
lda.pred = predict( lda.fit, Smarket.2005 )
##plot class predicitons
predFrame = data.frame( lda.pred )
#predFrame['class']
table( lda.pred$class , Smarket.2005$Direction)
mean( lda.pred$class==Smarket.2005$Direction )
##.55, somewhat better than average, about the same as we were able to do with logistic regression 

##now try qda
qda.fit = qda(Direction~Lag1+Lag2+Lag3, data=Smarket, subset=Year<2005)
qda.fit
qda.pred = predict( qda.fit, Smarket.2005 )
qda.pred.c = qda.pred$class
table( qda.pred.c, Smarket.2005$Direction )
mean( qda.pred.c==Smarket.2005$Direction )
## 0.579 , best so far and didn't overfit to training data 
## perhaps the higher flexibility of the QDA allowed a better approximation

## k-nearest neighbors 
##attach Smarket and separate out our training and testing data
library(class)
attach(Smarket)
train = Year < 2005
train.X = cbind(Lag1,Lag2)[train,]
test.X = cbind(Lag1,Lag2)[!train,]
train.dir = Direction[train]
test.dir = Direction[!train]
set.seed(1)
##predictions
knn.pred = knn( train.X, test.X, train.dir, k=1 )
table( knn.pred, Smarket.2005$Direction )
mean( knn.pred==Smarket.2005$Direction )
##0.5, not good
knn.pred = knn( train.X, test.X, train.dir, k=2 )
mean( knn.pred==Smarket.2005$Direction )
##2 improves slightly
knn.pred = knn( train.X, test.X, train.dir, k=3 )
mean( knn.pred==Smarket.2005$Direction )
##3 is around 0.53
knn.pred = knn( train.X, test.X, train.dir, k=4 )
mean( knn.pred==Smarket.2005$Direction )
##4 is back down to ~ .51, probably hit a peak at k=3