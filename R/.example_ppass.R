

library(eRm)


## ========== RM - eRm 
my_data <- eRm::sim.rasch(200, 12)
my_rm <- RM(my_data)

res_pp <- PPass(my_rm)

## ========== PCM - eRm

set.seed(2751)

THRES  <- matrix(c(-2,-1.23,1.11,3.48,1
                   ,2,-1,-0.2,0.5,1.3,-0.8,1.5),nrow=2)
# slopes
sl     <- rep(1,6)
THRESx <- rbind(0,THRES)
THETA  <- rnorm(200)
simdat_gpcm <- sim_gpcm(thres = THRESx,alpha = sl,theta = THETA)

my_pcm <- PCM(simdat_gpcm)


len1 <- apply(my_pcm$X,2, function(x) length(unique(x))-1)
itemss <- unlist(lapply(1:length(len1), function(x) rep(x, each=len1[x])))
wohin_zeile <- unlist(lapply(len1, function(x) 1:x))
names(wohin_zeile) <- NULL

thres <- matrix(NA,ncol=ncol(my_pcm$X), nrow=max(len1))
das <- cbind(my_pcm$betapar,wohin_zeile,itemss)

for(i in 1:nrow(das))
{
thres[das[,"wohin_zeile"][i],das[,"itemss"][i]] <- my_pcm$betapar[i]
}


