#project for stat 521
rm(list=ls(all=TRUE))

library(foreign) #to read.dta
library(car)

cont <- read.dta("cont.dta")
attach(cont, pos=2)
x <- (log(iy/100) - log(workingagepop/100 + 0.05))
y <- (log(school/100) - log(workingagepop/100 + 0.05))
cont = cbind(cont,x,y)

contn <- subset(cont,(n==1))
conti <- subset(cont,(i==1))
conto <- subset(cont,(o==1))

## Non-oil, unrestricted regression
reg1 <- lm(log(gdpadult1985) ~ log(iy/100) + log(workingagepop/100 + 0.05) , data=contn)
print(summary(reg1))

## Intermediate, unrestricted regression
reg2 <- lm(log(gdpadult1985) ~ log(iy/100) + log(workingagepop/100 + 0.05) , data=conti)
print(summary(reg2))

## OECD, unrestricted regression
reg3 <- lm(log(gdpadult1985) ~ log(iy/100) + log(workingagepop/100 + 0.05) , data=conto)
print(summary(reg3))

################################################################################################
#Now we run the restricted regressions:

## Non-oil, restricted regression
reg1b <- lm( log(gdpadult1985) ~ x , data=contn)
print(summary(reg1b))

## Intermediate, restricted regression
reg2b <- lm(log(gdpadult1985) ~ x , data=conti)
print(summary(reg2b))

## OECD, restricted regression
reg3b <- lm(log(gdpadult1985) ~ x , data=conto)
print(summary(reg3b))

################################################################################################
#Linear hypothesis test
hypothesis.matrix <- matrix(c(0, 1, 1) , nrow=1 , ncol =3)

print(linearHypothesis( reg1, hypothesis.matrix, rhs=0))
print(linearHypothesis( reg2, hypothesis.matrix, rhs=0))
print(linearHypothesis( reg3, hypothesis.matrix, rhs=0))

################################################################
################################################################
################################################################
###### Solow Model with Human Capital


## Non-oil, unrestricted regression
reg1c <- lm(log(gdpadult1985) ~ log(iy/100) + log(workingagepop/100 + 0.05) + log(school/100) , data=contn)
print(summary(reg1c))

## Intermediate, unrestricted regression
reg2c <- lm(log(gdpadult1985) ~ log(iy/100) + log(workingagepop/100 + 0.05) + log(school/100), data=conti)
print(summary(reg2c))

## OECD, unrestricted regression
reg3c <- lm(log(gdpadult1985) ~ log(iy/100) + log(workingagepop/100 + 0.05) + log(school/100) , data=conto)
print(summary(reg3c))

################################################################################################
#Now we run the restricted regressions:

## Non-oil, restricted regression
reg1d <- lm( log(gdpadult1985) ~ x + y , data=contn)
print(summary(reg1d))

## Intermediate, restricted regression
reg2d <- lm(log(gdpadult1985) ~ x + y , data=conti)
print(summary(reg2d))

## OECD, restricted regression
reg3d <- lm(log(gdpadult1985) ~ x + y , data=conto)
print(summary(reg3d))

################################################################################################
#Linear hypothesis test
hypothesis.matrix2 <- matrix(c(0, 1, 1,1) , nrow=1 , ncol =4)

print(linearHypothesis( reg1c, hypothesis.matrix2, rhs=0))
print(linearHypothesis( reg2c, hypothesis.matrix2, rhs=0))
print(linearHypothesis( reg3c, hypothesis.matrix2, rhs=0))

################################################################
################################################################
################################################################
###### Test for unconditional convergence

## Non-oil
reg4 <- lm( log(gdpadult1985)-log(gdpadult1960) ~ log(gdpadult1960) , data=contn) 
print(summary(reg4))

## Intermediate
reg5 <- lm( log(gdpadult1985)-log(gdpadult1960) ~ log(gdpadult1960) , data=conti)
print(summary(reg5))

## OECD
reg6 <- lm( log(gdpadult1985)-log(gdpadult1960) ~ log(gdpadult1960) , data=conto)
print(summary(reg6))

################################################################
################################################################
################################################################
###### Test for conditional convergence

## Non-oil
reg4b <- lm( log(gdpadult1985)-log(gdpadult1960) ~ log(gdpadult1960) +log(iy/100) + log(workingagepop/100 + 0.05) , data=contn)
print(summary(reg4b))

## Intermediate
reg5b <- lm( log(gdpadult1985)-log(gdpadult1960) ~ log(gdpadult1960)+ log(iy/100) + log(workingagepop/100 + 0.05) , data=conti)
print(summary(reg5b))

## OECD
reg6b <- lm( log(gdpadult1985)-log(gdpadult1960) ~ log(gdpadult1960) + log(iy/100) + log(workingagepop/100 + 0.05) , data=conto)
print(summary(reg6b))

################################################################
################################################################
################################################################
###### Test for conditional convergence with human capital

## Non-oil
reg4c <- lm( log(gdpadult1985)-log(gdpadult1960) ~ log(gdpadult1960) +log(iy/100) + log(workingagepop/100 + 0.05) + log(school/100), data=contn)
print(summary(reg4c))

## Intermediate
reg5c <- lm( log(gdpadult1985)-log(gdpadult1960) ~ log(gdpadult1960)+ log(iy/100) + log(workingagepop/100 + 0.05)+ log(school/100) , data=conti)
print(summary(reg5c))

## OECD
reg6c <- lm( log(gdpadult1985)-log(gdpadult1960) ~ log(gdpadult1960) + log(iy/100) + log(workingagepop/100 + 0.05)+ log(school/100) , data=conto)
print(summary(reg6c))

################################################################
################################################################
################################################################
###### Test for conditional convergence with human capital, restricted regression

## Non-oil
reg4d <- lm( log(gdpadult1985)-log(gdpadult1960) ~ log(gdpadult1960) + x + y, data=contn)
print(summary(reg4d))

## Intermediate
reg5d <- lm( log(gdpadult1985)-log(gdpadult1960) ~ log(gdpadult1960)+ x +y , data=conti)
print(summary(reg5d))

## OECD
reg6d <- lm( log(gdpadult1985)-log(gdpadult1960) ~ log(gdpadult1960) + x +y , data=conto)
print(summary(reg6d))

################################################################################################
#Linear hypothesis test
hypothesis.matrix3 <- matrix(c(0,0, 1, 1,1) , nrow=1 , ncol =5)

print(linearHypothesis( reg4c, hypothesis.matrix3, rhs=0))
print(linearHypothesis( reg5c, hypothesis.matrix3, rhs=0))
print(linearHypothesis( reg6c, hypothesis.matrix3, rhs=0))
