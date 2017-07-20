#
rndmz<-10                     # Number of randomizations required
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

#-----------------------------
