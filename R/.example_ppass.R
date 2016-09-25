
library(eRm)


## ========== RM - eRm 
my_data <- eRm::sim.rasch(200, 12)
my_rm <- eRm::RM(my_data)

res_pp1 <- PPass(my_rm)



## ========== PCM - eRm

set.seed(2751)

THRES  <- matrix(c(-2,-1.23,1.11,3.48,1
                   ,2,-1,-0.2,0.5,1.3,-0.8,1.5),nrow=2)
THRES <- rbind(THRES,c(-0.2,NA,NA,NA,NA,NA))

sl          <- rep(1,6)
THRESx      <- rbind(0,THRES)
THETA       <- rnorm(200)
simdat_gpcm <- sim_gpcm(thres = THRESx,alpha = sl,theta = THETA)

my_pcm <- eRm::PCM(simdat_gpcm)

res_pp2 <- PPass(my_pcm)

