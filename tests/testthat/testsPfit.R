
# context("1PL model Pfit")
# 
# # ------------------------- testing 1>>>
# 
# ################# Pfit #############################################################
# 
# 
# ### data creation ##########
# 
# set.seed(1337)
# library(PerFit)
# 
# # intercepts
# diffpar <- seq(-3,3,length=15)
# # slope parameters
# sl     <- round(runif(15,0.5,1.5),2)
# la     <- round(runif(15,0,0.25),2)
# ua     <- round(runif(15,0.8,1),2)
# 
# # response matrix
# awm <- matrix(sample(0:1,100*15,replace=TRUE),ncol=15)
# 
# # ------------------------------------------------------------------------
# ## 1PL model ##### 
# # ------------------------------------------------------------------------
# # MLE
# res1plmle <- PP_4pl(respm = awm,thres = diffpar,type = "mle")
# # WLE
# res1plwle <- PP_4pl(respm = awm,thres = diffpar,type = "wle")
# # MAP estimation
# res1plmap <- PP_4pl(respm = awm,thres = diffpar,type = "map")
# 
# # ------------------------------------------------------------------------
# ## 1PL model ##### 
# # ------------------------------------------------------------------------
# ## LZ*-Index ##### 
# ## mle ####
# Pfit(respm=awm,pp=res1plmle,fitindices="lzstar")
# ## wle ####
# Pfit(respm=awm,pp=res1plwle,fitindices="lzstar")
# ## map ####
# Pfit(respm=awm,pp=res1plmap,fitindices="lzstar")

# calculate person fit with the PerFit package to check if they are equal
#t
#test_that("Output = the same - with or without removing duplicates",{

#for(te in 1:length(estmod))
#{
#expect_that(res234pl_dup1[[te]],equals(res234pl_dup1[[te]]))  
#}

#})




# ------------------------- testing 2>>>

#t
#test_that("errors - warnings misspelling and length #1",{
#  expect_that(PP_4pl(respm = awm,thres = diffpar, slopes = sl,type = "aaa"), throws_error())
#  expect_that(PP_4pl(respm = awm,thres = diffpar[-1], slopes = sl), throws_error())
#  expect_that(PP_4pl(respm = awm[,-1],thres = diffpar, slopes = sl), throws_error()) 
#  expect_that(PP_4pl(respm = awm,thres = diffpar[-1], slopes = sl[-1]), throws_error()) 
#  expect_that(PP_4pl(respm = awm,thres = diffpar, slopes = sl,lowerA = la[-1]), throws_error()) 
#  expect_that(PP_4pl(respm = awm,thres = diffpar[-1], slopes = sl[-1],upperA = ua[-1]), throws_error()) 
#  expect_that(PP_4pl(respm = awm,thres = diffpar[-1], slopes = sl[-1],upperA = ua[-1],lowerA = la[-1]), throws_error())
#  expect_that(PP_4pl(respm = awm,thres = diffpar[-1], slopes = sl[-1],upperA = ua[-1],lowerA = la[-1]), throws_error())
#  expect_that(PP_4pl(respm = awm2,thres = diffpar, slopes = sl), throws_error())
#  expect_that(PP_4pl(respm = awm2,thres = diffparM2, slopes = sl), throws_error())
#})


















