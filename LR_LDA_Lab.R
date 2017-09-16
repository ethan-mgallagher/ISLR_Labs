#load ISLR dataset
require(ISLR)
require(MASS)
#S&P 500 observations, each LagX variable is Percentage return for X days previous
# Volume is volume of shares traded, today is percentage of return for today etc.
names(Smarket)
summary(Smarket)

#plot
pairs( Smarket, col=Smarket$Direction)
##Logistic Regression
##Try to build a model to predict market direction based on lag and volume traded using Logistic Regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary( glm.fit )
##Not good results, no coefficents with significance
glm.probs=predict(glm.fit, type="response")
##what we're trying to predict is qualitative, so we need to threshold like this
glm.pred=ifelse(glm.probs>0.50,"Up","Down")
glm.pred[1:5]
##make a table of predicted versus true direction
attach(Smarket)
table(glm.pred,Direction)
##observe poor large number of mistakes of classifier, especially false "Up" predictions
mean(glm.pred==Direction)
##mean equals 0.5216, so right about half the time, not really better than guessing on training data
##obviously predicting stock market is not so easy

##with training and testing data
train = Year < 2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
summary( glm.fit )
##Stil not good really
##now try to predict on test data
glm.probs=predict(glm.fit, newdata=Smarket[!train,], type="response")
##what we're trying to predict is qualitative, so we need to threshold like this
glm.pred=ifelse(glm.probs>0.50,"Up","Down")
##test data
Direction.2005 = Smarket$Direction[!train]
table(glm.pred, Direction.2005)
##observe poor large number of mistakes of classifier, especially false "Up" predictions
mean(glm.pred==Direction.2005)
##that didn't help, worse than guessing on test data though not by much. Overfitting perhaps

##try smaller model
glm.fit=glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
summary( glm.fit )
##Stil not good really
##now try to predict on test data
glm.probs=predict(glm.fit, newdata=Smarket[!train,], type="response")
##what we're trying to predict is qualitative, so we need to threshold like this
glm.pred=ifelse(glm.probs>0.50,"Up","Down")
##make table on test data
table(glm.pred, Direction.2005)
##observe poor large number of mistakes of classifier, especially false "Up" predictions
mean(glm.pred==Direction.2005)
##now we do a little better than just guessing with a mean of .55
summary(glm.pred)

##now try some LDA with a smaller model
lda.fit = lda( Direction~Lag1+Lag2, data=Smarket )
lda.fit
lda.pred = predict( lda.fit)$class
lda.pred
table(lda.pred, Direction)
##looks about the same as the logistic regressions
mean(lda.pred==Direction)
##.528, so slightly better than chance. This is on training data however