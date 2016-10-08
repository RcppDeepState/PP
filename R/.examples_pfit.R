################# Pfit #############################################################


### data creation ##########

set.seed(1337)


# intercepts
diffpar <- seq(-3,3,length=15)
# slope parameters
sl     <- round(runif(15,0.5,1.5),2)
la     <- round(runif(15,0,0.25),2)
ua     <- round(runif(15,0.8,1),2)

# response matrix
awm <- matrix(sample(0:1,100*15,replace=TRUE),ncol=15)

# ------------------------------------------------------------------------
## 1PL model ##### 
# ------------------------------------------------------------------------
# MLE
res1plmle <- PP_4pl(respm = awm,thres = diffpar,type = "mle")
# WLE
res1plwle <- PP_4pl(respm = awm,thres = diffpar,type = "wle")
# MAP estimation
res1plmap <- PP_4pl(respm = awm,thres = diffpar,type = "map")
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

##########################################################################################

# ------------------------------------------------------------------------
## 1PL model ##### 
# ------------------------------------------------------------------------
## LZ*-Index ##### 
Pfit(respm=awm,pp=res1plwle,fitindices="lzstar")
## LZ*-Index combined with Infit-Outfit ##### 
Pfit(respm=awm,pp=res1plwle,fitindices=c("lzstar","infitoutfit"))
# ------------------------------------------------------------------------
## 2PL model ##### 
# ------------------------------------------------------------------------
## LZ*-Index ##### 
Pfit(respm=awm,pp=res2plwle,fitindices="lzstar")
## LZ*-Index combined with Infit-Outfit ##### 
Pfit(respm=awm,pp=res2plwle,fitindices=c("lzstar","infitoutfit"))
# ------------------------------------------------------------------------
## 3PL model ##### 
# ------------------------------------------------------------------------
## LZ*-Index ##### 
Pfit(respm=awm,pp=res3plwle,fitindices="lzstar")
## LZ*-Index combined with Infit-Outfit ##### 
Pfit(respm=awm,pp=res3plwle,fitindices=c("lzstar","infitoutfit"))
# ------------------------------------------------------------------------
## 4PL model ##### 
# ------------------------------------------------------------------------
## LZ*-Index ##### 
Pfit(respm=awm,pp=res4plwle,fitindices="lzstar")
## LZ*-Index combined with Infit-Outfit ##### 
Pfit(respm=awm,pp=res4plwle,fitindices=c("lzstar","infitoutfit"))

##########################################################################################

# ------------------------------------------------------------------------
## 1PL model ##### 
# ------------------------------------------------------------------------
## LZ*-Index ##### 
## mle ####
Pfit(respm=awm,pp=res1plmle,fitindices="lzstar")
## wle ####
Pfit(respm=awm,pp=res1plwle,fitindices="lzstar")
## map ####
Pfit(respm=awm,pp=res1plmap,fitindices="lzstar")
