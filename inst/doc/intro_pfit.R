## ----settings------------------------------------------------------------
knitr::opts_chunk$set(message = FALSE, results='hide')

## ----start---------------------------------------------------------------
library(PP)

set.seed(1337)

# simulate some intercepts
diffpar <- seq(-3,3,length=15)
# simulate some slope parameters
sl     <- round(runif(15,0.5,1.5),2)
la     <- round(runif(15,0,0.25),2)
ua     <- round(runif(15,0.8,1),2)

# simulate response matrix
awm <- matrix(sample(0:1,100*15,replace=TRUE),ncol=15)

## ----1pl-----------------------------------------------------------------
# MLE
res1plmle <- PP_4pl(respm = awm,thres = diffpar,type = "mle")
# WLE
res1plwle <- PP_4pl(respm = awm,thres = diffpar,type = "wle")
# MAP estimation
res1plmap <- PP_4pl(respm = awm,thres = diffpar,type = "map")

## ----234-pl--------------------------------------------------------------

# ------------------------------------------------------------------------
## 2PL model ##### 
# ------------------------------------------------------------------------
# MLE
res2plmle <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,type = "mle")
# WLE
res2plwle <- PP_4pl(respm = awm,thres = diffpar, slopes = sl,type = "wle")

# ------------------------------------------------------------------------
## 3PL model ##### 
# ------------------------------------------------------------------------
# MLE
res3plmle <- PP_4pl(respm = awm,thres = diffpar,
                    slopes = sl,lowerA = la,type = "mle")
# WLE
res3plwle <- PP_4pl(respm = awm,thres = diffpar,
                    slopes = sl,lowerA = la,type = "wle")
# ------------------------------------------------------------------------
## 4PL model ##### 
# ------------------------------------------------------------------------
# MLE
res4plmle <- PP_4pl(respm = awm,thres = diffpar,
                    slopes = sl,lowerA = la,upperA=ua,type = "mle")
# WLE
res4plwle <- PP_4pl(respm = awm,thres = diffpar,
                    slopes = sl,lowerA = la,upperA=ua,type = "wle")

## ----pfit----------------------------------------------------------------

# ------------------------------------------------------------------------
## 1PL model ##### 
# ------------------------------------------------------------------------
## LZ*-Index ##### 
pfit1pl_lz <- Pfit(respm=awm,pp=res1plwle,fitindices="lzstar")
## LZ*-Index combined with Infit-Outfit ##### 
pfit1pl_li <- Pfit(respm=awm,pp=res1plwle,fitindices=c("lzstar","infit","outfit"))
# ------------------------------------------------------------------------
## 2PL model ##### 
# ------------------------------------------------------------------------
## LZ*-Index ##### 
pfit2pl_lz <- Pfit(respm=awm,pp=res2plwle,fitindices="lzstar")
## LZ*-Index combined with Infit-Outfit ##### 
pfit2pl_li <- Pfit(respm=awm,pp=res2plwle,fitindices=c("lzstar","infit","outfit"))
# ------------------------------------------------------------------------
## 3PL model ##### 
# ------------------------------------------------------------------------
## LZ*-Index ##### 
pfit3pl_lz <- Pfit(respm=awm,pp=res3plwle,fitindices="lzstar")
## LZ*-Index combined with Infit-Outfit ##### 
pfit3pl_li <- Pfit(respm=awm,pp=res3plwle,fitindices=c("lzstar","infit","outfit"))
# ------------------------------------------------------------------------
## 4PL model ##### 
# ------------------------------------------------------------------------
## LZ*-Index ##### 
pfit4pl_lz <- Pfit(respm=awm,pp=res4plwle,fitindices="lzstar")
## LZ*-Index combined with Infit-Outfit ##### 
pfit4pl_li <- Pfit(respm=awm,pp=res4plwle,fitindices=c("lzstar","infit","outfit"))

## ----wle-----------------------------------------------------------------
# ------------------------------------------------------------------------
## 1PL model ##### 
# ------------------------------------------------------------------------
## LZ*-Index ##### 
## mle ####
pfit1pl_mle_l <- Pfit(respm=awm,pp=res1plmle,fitindices="lzstar")
## wle ####
pfit1pl_wle_l <- Pfit(respm=awm,pp=res1plwle,fitindices="lzstar")
## map ####
pfit1pl_map_l <- Pfit(respm=awm,pp=res1plmap,fitindices="lzstar")

## ----plot, echo=FALSE, message=FALSE, warning=FALSE----------------------
# eine Grafik erzeugen

res.pp <- Pfit(respm=awm,pp=res1plmle,fitindices=c("lzstar"),SE=TRUE)
x<-seq(-4,4,length=200)
s = 1
mu = 0
y <- (1/(s*sqrt(2*pi))) * exp(-((x-mu)^2)/(2*s^2))
plot(x,y, type="l", lwd=2, col = "blue", xlim = c(-4,4),xlab="", ylab="")
title(main="Density plot of lz* Person-Fit", xlab="density", ylab="score")
lines(density(res.pp$lzstar[,"lzstar"], bw = 0.5), lwd = 2, lty = 2)
rug(res.pp$lzstar[,"lzstar"],col="red")

# zweite Grafik erzeugen
x <- 1:nrow(res.pp$lzstar)
avg <- res.pp$lzstar[,"lzstar"]
sdev <- res.pp$lzstar[,"lzs_se"]

plot(avg, x,
     xlim=range(c(avg-sdev, avg+sdev)),
     pch=19, ylab="Person", xlab="Person-Fit +/- SD",
     main="Plot of Person-Fit with SE"
)
arrows(avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
abline(v=0,col = "red", lwd = 3)

