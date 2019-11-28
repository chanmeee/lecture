setwd("C:/Users/Chanmi Yoo/Desktop/Regression Analysis/2019-2/Data Sets/Chapter  6 Data Sets")

#7.1
## (1) 1
## (2) 1
## (3) 2
## (4) 3 

#7.4 
a <- read.table("CH06PR09.txt")
names(a) <- c("Y","X1","X2","X3")
lm1 <- lm(Y~X1,data=a)
lm13 <- lm(Y~X1+X3, data=a)
lm132 <- lm(Y~X1+X3+X2,data=a)
anova(lm1); anova(lm13); anova(lm132)

# (a)
## SSR(X1) = 136366, SSR(X3|X1) = 2033565, SSR(X2|X1,X3) = 6674
## SSE(X1,X2,X3) = 985530
## df = 1,1,1,48

# (b)
## SSR(X2|X1,X3) = 6674
## SSE(X1,X2,X3) = 985530

## [alternatives] H0: B2 = 0 vs. H1: B2 is not 0.
## [test statistic] F* = (6674 / 1) / (985530 / 48) = 0.3250556
## [critical value] F(.95; 1, 17) = 4.451322
## [decision rule] If F* <= 4.451322, conclude H0. If F* > 4.451322, conclude H1.
## [conclusion] Since F* <= 4.451322, conclude H0. Thus, B2 = 0.
## [P-value] 0.57123

# (c)
lm12 <- lm(Y~X1+X2, data=a)
lm2 <- lm(Y~X2, data=a)
lm21 <- lm(Y~X2+X1, data=a)
anova(lm12); anova(lm2); anova(lm21)

## YES, they are same.
## SSR(X1) + SSR(X2|X1) = 136366 + 5726 = 142092 
## SSR(X2) + SSR(X1|X2) = 11395 + 130697 = 142092
## YES

#7.5
b <- read.table("CH06PR15.txt")
names(b) <- c("Y","X1","X2","X3")
lm2 <- lm(Y~X2, data=b)
lm21 <- lm(Y~X2+X1, data=b)
lm213 <- lm(Y~X2+X1+X3, data=b)
anova(lm2); anova(lm21); anova(lm213)

# (a)
## SSR(X2) = 4860.3, SSR(X1|X2) = 3896.0, SSR(X2|X1,X3) = 364.2
## SSE(X1,X2,X3) = 4248.8
## df = 1,1,1,42

# (b)
## SSR(X3|X1,X2) = 364.2
## SSE(X1,X2,X3) = 4248.8

## [alternatives] H0: B3 = 0 vs. H1: B3 is not 0.
## [test statistic] F* = (364.2 / 1) / (4248.8 / 42) = 3.600169
## [critical value] F(.975; 1, 42) = 5.403859
## [decision rule] If F* <= 5.403859, conclude H0. If F* > 5.403859, conclude H1.
## [conclusion] Since F* <= 5.403859, conclude H0. Thus, B3 = 0.
## [P-value] 0.06468

#7.6 
lm1 <- lm(Y~X1, data=b)
lm123<- lm(Y~X1+X2+X3, data=b)
anova(lm1); anova(lm123)

## SSR(X2,X3|X1) = 480.9 + 364.2 = 845.1 
## SSE(X1,X2,X3) = 4248.8

## [alternatives] H0: B2 = B3 = 0 vs. H1: Not both B2 and B3 is not 0.
## [test statistic] F* = (845.1 / 2) / (4248.8 / 42) = 4.176968
## [critical value] F(.975; 2, 42) = 4.03271
## [decision rule] If F* <= 4.03271, conclude H0. If F* > 4.03271, conclude H1.
## [conclusion] Since F* > 4.03271, conclude H1. Thus, Not both B2 and B3 is not 0.
## [P-value] 0.022

#7.13
lm1 <- lm(Y~X1,data=a)
anova(lm1)
lm2 <- lm(Y~X2, data=a)
anova(lm2)

lm12 <- lm(Y~X1+X2, data=a)
lm21 <- lm(Y~X2+X1, data=a)
anova(lm12)
anova(lm21)

lm132 <- lm(Y~X1+X3+X2, data=a)
anova(lm132)

anova(lm(Y~X1+X3,data=a))

## R-squared 1 = .0431, R-squared 1,2 = .0072
## R-squared 1|2 = .0415, R-squared 2|3 = .0019
## R-squared 2|1,3 = .0067
## R-squared = .6883


#7.18
meanx1 <- mean(b$X1); varx1 <- var(b$X1)
meanx2 <- mean(b$X2); varx2 <- var(b$X2)
meanx3 <- mean(b$X3); varx3 <- var(b$X3)
meany <- mean(b$Y); vary <- var(b$Y)
xs1 <- (b$X1-meanx1)/sqrt(varx1)
xs2 <- (b$X2-meanx2)/sqrt(varx2)
xs3 <- (b$X3-meanx3)/sqrt(varx3)
ys <- (b$Y-meany)/sqrt(vary)
bb <- lm(ys~xs1+xs2+xs3); summary(bb)

anova(lm(ys~xs1+xs2))
anova(lm(ys~xs2+xs3))
anova(lm(ys~xs1+xs3))

#(a)
## Y.hat = -.59067 * X1 -.11062 * X2 -.23393 * X3

#(b)
## R-squared 1,2 = .32262, R-squared 1,3 = .32456, R-squared 2,3 = .44957

#(c)
## s.y = 17.2365, s.1 = 8.91809, s.2 = 4.31356, s.3 = .29934
## b1 = 17.2365 / 8.91809 * (-.59067) = -1.141622
## b2 = 17.2365 / 4.31356 * (-1.141622) = -4.561793
## b3 = 17.2365 / .29934 * (-.23393) = -13.47008
## b0 = 61.5652 + 1.141622 * 38.3913 + .44203 * 50.4348 + 13.4700 * 2.28696 = 158.4927


#7.26

