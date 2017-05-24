
library(vegan)

can<- read.csv("cannonical.correlations.csv",header=T,sep=",",dec=".")
attach(cannon)

X=as.matrix(cannon[1,4:20])
X=as.matrix(cannon[1,23:27])
Y=as.matrix(cannon[1,30:65])

CCorA(Y, X, stand.Y= TRUE, stand.X= TRUE, permutations = 0)

biplot(x, plot.type="biplots", xlabs, plot.axes = 1:2, int=0.5, 
       col.Y="red", col.X="blue", cex=c(0.7,0.9))