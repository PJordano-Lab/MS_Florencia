# --------------------------------------------------------------------
# [Title]: Modularity analysis.
# [Date]: 11Jun2013     [Loc]: Sevilla
# Pedro Jordano.
# --------------------------------------------------------------------
## First version 11Jun2013 Revised DATE
# --------------------------------------------------------------------
# Interaction matrices to compute the modularity.
mymat<-                         # [Assign here]
#
# Batch to generate the null models for modularity M significance test. ----- 
# (for each matrix, @30 min for 100 null replicates)
require(bipartite)
TIME <- Sys.time()
# Modularity for observed matrix.
# Give here the mean observed modularity values.
mod_obs <- 0.32505              # [Assign here]
#
Msig <- function (mat, mlike)  {
    require(bipartite)
    # mat is the input matrix for which M is tested
    # mlike is the observed mean M value
    nulls <- nullmodel(mat, N=100, method=3)
    modules.nulls <- sapply(nulls, computeModules)
    like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
    z <- (mlike - mean(like.nulls))/sd(like.nulls)
    p <- 2*pnorm(-abs(z))
    cat("\n\n","P value for modularity M= ", mlike, "\n", 
        "No. resamplings= ", N, "\n",
        "P=  ",p)
} 
#
Msig(mymat, mod_obs)
#
Sys.time() - TIME 
#

