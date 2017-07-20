#
rG1<- sample_n(wG1, 61, replace= F)   # Random sample from wG1 with N=70 trees (without replacement).
#Perform qap tests of graph correlation. I use the adjacency matrices as input.
prosop<- base::array(dim=c(2,61,12))
prosop[1,,]<- as.matrix(rG1)
prosop[2,,]<- as.matrix(wUG1)
#Perform qap tests of graph correlation
q<-qaptest(prosop, gcor, g1=1, g2=2)

#Examine the results
#summary(q)
q$testval 

#plot(q)

# hdist
q.prosop<-qaptest(prosop,hdist,g1=1,g2=2)
#summary(q.prosop)
#plot(q.prosop)
q.prosop$testval
