<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Getting started with PP}
-->
# Getting started with PP

First of all a few questions, whether the `PP` package is the one you are looking for.


1. You have a data set with item responses from several persons
2. You have item parameter estimates (e.g. difficulty parameters) from these items from (perhaps from a former application of these items)
3. and these items follow the 1/2/3/4-PL model or the generalized partial credit model
4. You are interested in estimating each persons **ability** with traditional approaches (mle, wle, map, eap) or
5. drawing **plausible values** for each person e.g. to apply secondary analysis or
6. getting **robust** ability estimates with Jackknife Methods or with weighted likelihood approaches


If you agree to question 1-3 and at least to one of question 3-6, then this package could be the right one for your tasks.


In future versions:

1. The range of model will be enlarged - especially estimators for the nominal response model and the nested logit model are planned to be integrated
2. **Personfit** statistics will be added for getting insights in response processes and to identify suspicious response patterns


Now a simple example.


## 2-PL Model


In this example, we want to gain ability estimates for each person in our sample. Each person answered questions in a test which has been scaled by the 2-PL model. Item parameters (difficulty and slope) are known and are supposed as fixed. What are the next steps?

1. Get the item responses and the item parameters inside the R workspace
2. Check your data and reshape them, so they can be processed by the PP functions
3. Decide which type of estimation you prefer (`?PP_4pl`)
4. Run the estimation


### Get

Getting the data inside the R workspace is quite easy in this case, because we merely load the data set which already comes with the PP package. It contains 60 response sets (60 persons answered to a test of 12 items) - and we have additional information, which we do not take into account for now. We first inspect the data, and in a second step informations about item difficulty and the slope parameters are extracted from the dataset attributes.


```r
library(PP)

data(fourpl_df)

dim(fourpl_df)
```

```
## [1] 60 14
```

```r
head(fourpl_df)
```

```
##        id sex Item1 Item2 Item3 Item4 Item5 Item6 Item7 Item8 Item9 Item10
## 1 LVL0694   w     1     1     1     1     0     0     1     0     0      0
## 2 BBU1225   w     1     1     1     1     1     1     1     1     1      0
## 3 MJN2028   w     1     1     1     1     1     1     1     1     0      0
## 4 TSU0771   m     1     1     1     0     1     1     1     1     1      0
## 5 XDS0698   w     1     1    NA     1     1     1     1     0     0      0
## 6 BOS1292   w     0     0     0     0     0     0     0     0     0      0
##   Item11 Item12
## 1      0      0
## 2      0      0
## 3      0      0
## 4      0      1
## 5      0      0
## 6      0      0
```

```r
diff_par <- attr(fourpl_df,"diffpar")
slope_par <- attr(fourpl_df,"slopes")
```



### Check and Reshape


Now we explore the item response data regarding full or zero score, missing values etc. . Considering this information, we are able to decide which estimation method we will apply.



```r
# extract items and transform the data.frame to matrix
itmat <- as.matrix(fourpl_df[,-(1:2)])


# are there any full scores?
fullsc <- apply(itmat,1,function(x) (sum(x,na.rm=TRUE)+sum(is.na(x))) == length(x))
any(fullsc)
```

```
## [1] FALSE
```

```r
# are there 0 scores?
nullsc <- apply(itmat,1,function(x) sum(x,na.rm=TRUE) == 0)
any(nullsc)
```

```
## [1] TRUE
```

```r
# are there missing values? how many and where?
nasc <- apply(itmat,1,function(x) sum(is.na(x)))
any(nasc > 0)
```

```
## [1] TRUE
```

```r
#which(nasc)

# is our data dichotomous as expected?
apply(itmat,2,function(x) table(x))
```

```
##   Item1 Item2 Item3 Item4 Item5 Item6 Item7 Item8 Item9 Item10 Item11
## 0     9    17    13    16    21    23    24    28    44     53     53
## 1    51    43    46    44    38    37    35    32    16      7      7
##   Item12
## 0     53
## 1      7
```

```r
# are there any duplicates?
rdup <- duplicated(itmat)
sum(rdup)
```

```
## [1] 8
```



### Decide and Run


We use the `PP_4pl()` function for our estimation. So perhaps you are thinking:"Why are we using a function to fit the 4-PL model, when we acutally have a dataset which stems from a 2-PL model scaled test?!" This is because with the `PP_4pl()` function you can fit the:

1. **1-PL model** (Rasch model) by submitting: the data matrix, item difficulties and **nothing else**, since the 1-PL model is merely a 4-PL model with: any slope = 1, any lower asymptote = 0 and any upper asymptote = 1!
2. **2-PL model** by submitting: the data matrix, item difficulties and slope parameters. Lower and upper asymptotes are automatically set to 0 und 1 respectively.
3. **3-PL model** by submitting anything except the upper asymptote parameters
4. **4-PL model** $\rightarrow$ submit all parameters ...

In this case, difficulty parameters and slopes are available, so we will submit them, and a 2-PL model is fitted **automatically**.

We decide to apply a common maximum likelihood estimation (`type = "mle"`), and do not remove duplicated response patterns (see argument: `ctrl=list()`) from estimation because there are only 8 duplicated patterns. If the data set would have been much larger, duplicated patterns are more likely and therefore choosing to remove these patterns would speed up the estimation process significantly. (Choosing this option or not, does not change the numerical results!)



```r
library(PP)

res1plmle <- PP_4pl(respm = itmat, thres = diff_par, slopes = slope_par, type = "mle")
```

```
## Estimating:  2pl model ... 
## type = mle 
## Estimation finished!
```

```r
summary(res1plmle)
```

```
## PP Version:  0.6.0.2 
## 
##  Call: PP_4pl(respm = itmat, thres = diff_par, slopes = slope_par, type = "mle") 
## - job started @ Tue Feb  7 21:47:52 2017 
## 
## Estimation type: mle 
## 
## Number of iterations: 5 
## -------------------------------------
##       estimate     SE
##  [1,]  -0.8555 0.7270
##  [2,]   1.9097 0.9416
##  [3,]   1.1454 0.9211
##  [4,]   1.7883 0.9410
##  [5,]   0.4274 0.8792
##  [6,]     -Inf     NA
##  [7,]   2.8310 0.9618
##  [8,]   1.7414 0.9406
##  [9,]  -2.4328 0.8520
## [10,]  -0.2126 0.7750
## [11,]   1.1454 0.9211
## [12,]   4.0258 1.1822
## [13,]  -1.6514 0.7451
## [14,]  -3.6859 1.2125
## [15,]   1.0795 0.9168
## --------> output truncated <--------
```

Some facts:

1. The function returns the **point estimators** and **standard errors** for each person. Whatever the (valid) input was, the main result is always a matrix with two columns and a number of rows which equals the number of persons (`res1plmle$resPP$resPP`).
2. The function deals with **missing values**. These are treated as if the person has never seen this item.
3. The order of the output estimates = order of the input. So the first estimate belongs to the first response pattern of the input data matrix.



### Edit

In the last step, we add the estimates to the data.frame we extracted the item responses from in the first place.



```r
dafest <- data.frame(fourpl_df,res1plmle$resPP$resPP)

head(dafest,10)
```

```
##         id sex Item1 Item2 Item3 Item4 Item5 Item6 Item7 Item8 Item9
## 1  LVL0694   w     1     1     1     1     0     0     1     0     0
## 2  BBU1225   w     1     1     1     1     1     1     1     1     1
## 3  MJN2028   w     1     1     1     1     1     1     1     1     0
## 4  TSU0771   m     1     1     1     0     1     1     1     1     1
## 5  XDS0698   w     1     1    NA     1     1     1     1     0     0
## 6  BOS1292   w     0     0     0     0     0     0     0     0     0
## 7  KFF1422   w     1     1     1     1     1     1     1     1     1
## 8  DCQ0198   w     1     1     1     1     0     1     1     1     1
## 9  FTT1492   w     0     0     1     1     0     0     0     0     0
## 10 GCP0645   m     1     1     1     1     1     0     0     1     0
##    Item10 Item11 Item12   estimate        SE
## 1       0      0      0 -0.8554973 0.7270360
## 2       0      0      0  1.9096810 0.9415755
## 3       0      0      0  1.1453588 0.9210920
## 4       0      0      1  1.7882890 0.9409769
## 5       0      0      0  0.4274013 0.8792348
## 6       0      0      0       -Inf        NA
## 7       0      0      1  2.8310343 0.9618330
## 8       0      1      0  1.7413789 0.9405966
## 9       0      0      0 -2.4327540 0.8520338
## 10      0      0      0 -0.2125844 0.7749617
```


One shortcoming of the plain maximum likelihood estimate is the fact, that the **extreme scores** do not lead to valid parameter estimates (`-Inf` and `Inf` are hardly useful for practitioners). One possibility to overcome this issue, is to change the estimation method - for instance `type = wle` performs weighted likelihood estimation, which is on the one hand less biased than the mle estimate, and on the other hand provides reasonable estimates for the extreme scores.


### Rerun



```r
library(PP)

res1plwle <- PP_4pl(respm = itmat,thres = diff_par, slopes = slope_par, type = "wle")
```

```
## Estimating:  2pl model ... 
## type = wle 
## Estimation finished!
```

```r
summary(res1plwle)
```

```
## PP Version:  0.6.0.2 
## 
##  Call: PP_4pl(respm = itmat, thres = diff_par, slopes = slope_par, type = "wle") 
## - job started @ Tue Feb  7 21:47:52 2017 
## 
## Estimation type: wle 
## 
## Number of iterations: 5 
## -------------------------------------
##       estimate     SE
##  [1,]  -0.8788 0.7263
##  [2,]   1.9065 0.9416
##  [3,]   1.0816 0.9169
##  [4,]   1.7814 0.9409
##  [5,]   0.3270 0.8679
##  [6,]  -4.6556 1.7254
##  [7,]   2.7746 0.9583
##  [8,]   1.7322 0.9405
##  [9,]  -2.2872 0.8257
## [10,]  -0.2907 0.7668
## [11,]   1.0816 0.9169
## [12,]   3.7316 1.0975
## [13,]  -1.5967 0.7408
## [14,]  -3.2952 1.0712
## [15,]   1.0089 0.9116
## --------> output truncated <--------
```


So, this was what we were finally looking for.


## Parameters and Person Fit 

For estimating person parameters and examining person fit in one step, use `PPass()` (for assessment). Using this function has several advantages over the using the other methods consecutively.

1. you ony need 1 Function
2. the data input format now is a `data.frame`
3. the data output format is a new `data.frame` with person fit statistics added



```r
PPass(fourpl_df, items = 3:14, mod="2PL", thres = diff_par, slopes = slope_par, type = "wle")
```

```
## Estimating:  2pl model ... 
## type = wle 
## Estimation finished!
```

```
## $personparameter
##       estimate     SE
##  [1,]  -0.8788 0.7263
##  [2,]   1.9065 0.9416
##  [3,]   1.0816 0.9169
##  [4,]   1.7814 0.9409
##  [5,]   0.3270 0.8679
##  [6,]  -4.6556 1.7254
##  [7,]   2.7746 0.9583
##  [8,]   1.7322 0.9405
##  [9,]  -2.2872 0.8257
## [10,]  -0.2907 0.7668
## [11,]   1.0816 0.9169
## [12,]   3.7316 1.0975
## [13,]  -1.5967 0.7408
## [14,]  -3.2952 1.0712
## [15,]   1.0089 0.9116
## [16,]  -0.2593 0.7700
## [17,]   2.1651 0.9423
## [18,]  -1.3301 0.7265
## [19,]   0.2756 0.8331
## [20,]   0.2899 0.8349
## [21,]  -0.9461 0.7244
## [22,]   0.1429 0.8166
## [23,]  -1.2063 0.7235
## [24,]   0.3353 0.8405
## [25,]   0.2930 0.9038
## [26,]  -0.2722 0.7687
## [27,]  -4.6556 1.7254
## [28,]  -0.4552 0.7515
## [29,]   0.3928 0.8476
## [30,]  -1.8723 0.7666
## [31,]   0.3099 0.8374
## [32,]  -0.2593 0.7700
## [33,]  -0.6851 0.7350
## [34,]  -1.0173 0.7232
## [35,]  -3.2958 1.1100
## [36,]  -1.8723 0.7666
## [37,]  -0.7915 0.7296
## [38,]   1.0816 0.9169
## [39,]   3.0581 0.9811
## [40,]  -0.2109 0.7751
## [41,]  -0.5691 0.7426
## [42,]  -0.4420 0.7527
## [43,]   1.0816 0.9169
## [44,]  -0.7896 0.7297
## [45,]  -0.2366 0.7724
## [46,]   2.0440 0.9419
## [47,]  -0.2252 0.7736
## [48,]  -0.9366 0.7246
## [49,]   1.9074 0.9416
## [50,]  -2.4421 0.8538
## [51,]   1.1923 0.9239
## [52,]   1.0816 0.9169
## [53,]  -1.5486 0.7375
## [54,]  -0.2907 0.7668
## [55,]  -1.3320 0.7266
## [56,]   1.9065 0.9416
## [57,]   2.8185 0.9610
## [58,]   0.0584 0.8062
## [59,]  -0.4882 0.7488
## [60,]  -1.1793 0.7231
## 
## $personfit
##              lz    lz_unst    lzstar    infit      in_t in_chisq in_df
##  [1,]  0.851526  -4.291534  0.851270 0.731543 -0.921808    5.971    11
##  [2,]  1.081243  -1.945076  1.103312 0.360774 -1.494176    2.325    11
##  [3,]  1.188937  -1.890521  1.211518 0.278782 -1.640463    2.183    11
##  [4,] -1.515153  -6.862706 -1.547649 1.630995  1.173282   37.883    11
##  [5,]  1.231817  -2.397093  1.079636 0.386560 -1.475371    3.021    10
##  [6,]  0.621728  -0.442421 -0.048534 0.109050 -0.730600    0.468    11
##  [7,]  0.465012  -2.802622  0.480230 0.815220 -0.345239    4.041    11
##  [8,] -1.477296  -6.833982 -1.509608 1.579800  1.095845   43.091    11
##  [9,] -0.172781  -4.871710 -0.399537 1.140202  0.465367    8.633    11
## [10,]  0.661254  -4.153698  0.660467 0.768969 -0.525600    6.121    11
## [11,]  1.188937  -1.890521  1.211518 0.278782 -1.640463    2.183    11
## [12,]  0.303526  -2.250430  0.176257 0.863038 -0.058998    3.633    11
## [13,]  0.191157  -5.072471  0.176453 0.980446  0.032505    7.801    11
## [14,]  0.295261  -2.605669  0.034840 0.879269  0.025710    4.629    11
## [15,]  0.212824  -3.929202  0.199562 0.952339  0.103204    7.609    11
## [16,]  0.192908  -4.955480  0.169889 0.952662  0.007085    8.826    11
## [17,] -0.345696  -4.448141 -0.356015 1.268801  0.674740    9.358    11
## [18,]  1.202077  -3.801234  1.225635 0.606804 -1.574289    4.979    11
## [19,]  0.239373  -4.415604  0.209801 0.844847 -0.173035   10.495    11
## [20,]  0.856672  -3.181027  0.864859 0.594228 -0.810557    4.660    11
## [21,]  0.536997  -4.783857  0.536582 0.848601 -0.465516    7.117    11
## [22,]  0.481380  -4.062110  0.466904 0.779507 -0.358000    7.502    11
## [23,] -2.462325  -9.072459 -2.489157 1.323032  1.163062   95.626    11
## [24,]  0.177729  -4.487500  0.144794 1.005236  0.176420    7.803    11
## [25,]  1.268181  -2.362597  1.078523 0.377364 -1.530761    2.969    10
## [26,] -3.487701 -11.485566 -3.680647 2.246407  2.522526   41.268    11
## [27,]  0.621728  -0.442421 -0.048534 0.109050 -0.730600    0.468    11
## [28,] -0.271268  -5.878557 -0.301942 1.116131  0.434019   11.764    11
## [29,]  1.145608  -2.499376  1.170758 0.401421 -1.384886    3.157    11
## [30,]  0.384345  -4.520382  0.383658 0.870050 -0.296349    7.369    11
## [31,] -0.868859  -6.586556 -0.965652 1.519058  1.073162   15.410    11
## [32,]  0.192908  -4.955480  0.169889 0.952662  0.007085    8.826    11
## [33,]  0.611786  -4.558630  0.610538 0.794900 -0.593953    7.041    11
## [34,] -0.820324  -6.784220 -0.820232 1.215031  0.812533   16.039    11
## [35,] -0.528042  -4.080942 -1.837798 1.244378  0.564666   16.865    10
## [36,]  0.384345  -4.520382  0.383658 0.870050 -0.296349    7.369    11
## [37,]  0.063670  -5.460497  0.059074 1.023644  0.178303    8.820    11
## [38,]  1.188937  -1.890521  1.211518 0.278782 -1.640463    2.183    11
## [39,] -1.809901  -6.058126 -2.154895 1.986248  2.005306   17.717    11
## [40,]  0.947680  -3.559881  0.960811 0.610643 -1.004104    5.007    11
## [41,] -2.971783 -10.336027 -3.055835 1.837762  2.129252   59.535    11
## [42,] -0.479765  -6.225780 -0.518472 1.215427  0.684205   12.470    11
## [43,] -2.909690 -10.277591 -3.027971 2.351794  1.930253   63.415    11
## [44,]  0.218296  -5.221862  0.214648 0.965869 -0.013927    8.241    11
## [45,]  0.469980  -4.442628  0.459625 0.798141 -0.418258    8.059    11
## [46,]  0.194479  -3.540406  0.198455 0.974262  0.114593    6.129    11
## [47,]  1.146885  -3.218890  1.169670 0.514802 -1.367366    4.210    11
## [48,] -3.280219 -10.462579 -3.282772 1.746514  2.239253   76.445    11
## [49,] -0.798333  -5.422195 -0.815830 1.389765  0.843864   18.460    11
## [50,] -0.224480  -4.751146 -0.529750 1.180264  0.535368    8.675    11
## [51,] -0.005185  -4.272985 -0.019556 1.110637  0.377350    8.415    11
## [52,]  1.188937  -1.890521  1.211518 0.278782 -1.640463    2.183    11
## [53,] -1.649999  -7.745528 -1.798717 1.400349  1.319315   23.776    11
## [54,]  0.661254  -4.153698  0.660467 0.768969 -0.525600    6.121    11
## [55,] -0.082080  -5.624461 -0.092168 1.034625  0.216006    9.864    11
## [56,]  1.081243  -1.945076  1.103312 0.360774 -1.494176    2.325    11
## [57,] -0.219202  -3.838965 -0.268940 1.243743  0.690457    6.550    11
## [58,]  0.505119  -4.097731  0.492696 0.778957 -0.381749    7.300    11
## [59,]  0.550022  -4.519610  0.546407 0.829063 -0.405671    6.796    11
## [60,] -5.479144 -13.410529 -5.521982 2.003677  2.946832  133.904    11
##       in_pv    outfit      ou_t ou_chisq ou_df ou_pv
##  [1,] 0.875  0.497569 -0.220570    5.971    11 0.875
##  [2,] 0.997  0.193783 -0.496540    2.325    11 0.997
##  [3,] 0.998  0.181890 -1.138823    2.183    11 0.998
##  [4,] 0.000  3.156939  1.533919   37.883    11 0.000
##  [5,] 0.981  0.274656 -0.991232    3.021    10 0.981
##  [6,] 1.000  0.038962  2.343647    0.468    11 1.000
##  [7,] 0.969  0.336790  0.277075    4.041    11 0.969
##  [8,] 0.000  3.590922  1.709484   43.091    11 0.000
##  [9,] 0.656  0.719456  0.607559    8.633    11 0.656
## [10,] 0.865  0.510092 -0.440162    6.121    11 0.865
## [11,] 0.998  0.181890 -1.138823    2.183    11 0.998
## [12,] 0.979  0.302717  0.904963    3.633    11 0.979
## [13,] 0.731  0.650106  0.260116    7.801    11 0.731
## [14,] 0.948  0.385740  1.056783    4.629    11 0.948
## [15,] 0.748  0.634114 -0.189042    7.609    11 0.748
## [16,] 0.638  0.735488 -0.075549    8.826    11 0.638
## [17,] 0.589  0.779831  0.355939    9.358    11 0.589
## [18,] 0.932  0.414907 -0.113510    4.979    11 0.932
## [19,] 0.487  0.874544  0.077808   10.495    11 0.487
## [20,] 0.947  0.388295 -0.823788    4.660    11 0.947
## [21,] 0.789  0.593120 -0.060787    7.117    11 0.789
## [22,] 0.757  0.625199 -0.318471    7.502    11 0.757
## [23,] 0.000  7.968867  2.782823   95.626    11 0.000
## [24,] 0.731  0.650225 -0.279481    7.803    11 0.731
## [25,] 0.982  0.269873 -1.005527    2.969    10 0.982
## [26,] 0.000  3.439013  2.112402   41.268    11 0.000
## [27,] 1.000  0.038962  2.343647    0.468    11 1.000
## [28,] 0.382  0.980313  0.274402   11.764    11 0.382
## [29,] 0.988  0.263113 -1.171310    3.157    11 0.988
## [30,] 0.768  0.614100  0.349185    7.369    11 0.768
## [31,] 0.164  1.284204  0.597231   15.410    11 0.164
## [32,] 0.638  0.735488 -0.075549    8.826    11 0.638
## [33,] 0.796  0.586729 -0.170368    7.041    11 0.796
## [34,] 0.140  1.336624  0.648407   16.039    11 0.140
## [35,] 0.077  1.533211  1.497482   16.865    10 0.077
## [36,] 0.768  0.614100  0.349185    7.369    11 0.768
## [37,] 0.638  0.735032  0.060114    8.820    11 0.638
## [38,] 0.998  0.181890 -1.138823    2.183    11 0.998
## [39,] 0.088  1.476381  0.991627   17.717    11 0.088
## [40,] 0.931  0.417222 -0.655597    5.007    11 0.931
## [41,] 0.000  4.961277  2.580233   59.535    11 0.000
## [42,] 0.329  1.039175  0.338816   12.470    11 0.329
## [43,] 0.000  5.284566  2.765427   63.415    11 0.000
## [44,] 0.692  0.686744  0.000692    8.241    11 0.692
## [45,] 0.708  0.671622 -0.178110    8.059    11 0.708
## [46,] 0.865  0.510788  0.061818    6.129    11 0.865
## [47,] 0.963  0.350815 -0.803783    4.210    11 0.963
## [48,] 0.000  6.370417  2.683910   76.445    11 0.000
## [49,] 0.071  1.538374  0.792375   18.460    11 0.071
## [50,] 0.652  0.722908  0.684046    8.675    11 0.652
## [51,] 0.676  0.701219 -0.038559    8.415    11 0.676
## [52,] 0.998  0.181890 -1.138823    2.183    11 0.998
## [53,] 0.014  1.981350  1.011915   23.776    11 0.014
## [54,] 0.865  0.510092 -0.440162    6.121    11 0.865
## [55,] 0.543  0.822040  0.310807    9.864    11 0.543
## [56,] 0.997  0.193783 -0.496540    2.325    11 0.997
## [57,] 0.834  0.545862  0.471653    6.550    11 0.834
## [58,] 0.774  0.608293 -0.340089    7.300    11 0.774
## [59,] 0.815  0.566342 -0.273701    6.796    11 0.815
## [60,] 0.000 11.158686  3.379092  133.904    11 0.000
```







