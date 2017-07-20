# Fully randomized assignment of the GRAZED dataset, no. resamps: 999.
# I trimm the grazed dataset to delete trees so that I get N=61.
# Trees with 0 records are excluded.
# Datasets from the G and UG sites have now 61x12
#
rndmz<- 999                     # Number of randomizations required
rand_qobs<- NULL                # QAP q value for observed matrix in each resampling.
rand_q_mean_resamp<- NULL       # mean QAP q value for 1000 random marices of each resampling.
rand_hdist_obs<- NULL           # Hamming h value for observed matrix in each resampling.
rand_hdist_mean_resamp<- NULL   # mean Hamming h value for 1000 random marices of each resampling.
#-----------------------------
for (i in 1:rndmz) {
  rG1<- sample_n(wG1, 61, replace= F)   # Random sample from wG1 with N=70 trees (without replacement).
  #Perform qap tests of graph correlation. I use the adjacency matrices as input.
  prosop<- base::array(dim=c(2,61,12))
  prosop[1,,]<- as.matrix(rG1)
  prosop[2,,]<- as.matrix(wUG1)
  
  #Perform qap tests of graph correlation
  q<-qaptest(prosop, gcor, g1=1, g2=2)
  # Store
  rand_qobs<- append(rand_qobs,q$testval)
  rand_q_mean_resamp<- append(rand_q_mean_resamp, mean(q$dist))
  
  # hdist
  q.prosop<-qaptest(prosop,hdist,g1=1,g2=2)
  # Store
  rand_hdist_obs<- append(rand_hdist_obs, q.prosop$testval)
  rand_hdist_mean_resamp<- append(rand_hdist_mean_resamp, mean(q.prosop$dist))
}
plot(density(rand_hdist_mean_resamp), xlim= c(min(rand_hdist_obs), max(rand_hdist_mean_resamp)))
abline(v= mean(rand_hdist_obs), col = "blue", lty = 3)
