#' Personfit statistics
#' 
#' Compute several person fit statistic for the 1-PL, 2-PL, 3PL and 4-PL. 
#'
#' Please note that currently only the LZ-Index, LZ*-Index and INFIT-OUTIFT as well as the polytomouse version of INFIT-OUTFIT are implemented. Other person fit staistics will be added soon.
#'
#' 
#'@param respm	      numeric response matrix
#'@param pp 		      object of the class fourpl with estimated personparameter
#'@param fitindices		character vector of desired person fit statistics. c("lz","lzstar","infitoutfit")
#'
#' @return list of person parameter
#'
#' @rdname pfit
#' @seealso \link{PPall}, \link{PP_4pl}, \link{PPass}
#'
#'
#'@author Jan Steinfeld
#'@references 
#'\itemize{
#' \item Armstrong, R. D., Stoumbos, Z. G., Kung, M. T. & Shi, M. (2007). On the performance of the lz person-fit statistic.  \emph{Practical Assessment, Research & Evaluation}, \bold{12(16)}. Chicago	
#' \item De La Torre, J., & Deng, W. (2008). Improving Person-Fit Assessment by Correcting the Ability Estimate and Its Reference Distribution. Journal of Educational Measurement, \bold{45(2)}, 159-177.
#' \item Drasgow, F., Levine, M. V. & Williams, E. A. (1985) Appropriateness measurement with polychotomous item response models and standardized indices. \emph{British Journal of Mathematical and Statistical Psychology}, \bold{38(1)}, 67--86.
#' \item Karabatsos, G. (2003) Comparing the Aberrant Response Detection Performance of Thirty-Six Person-Fit Statistics. \emph{Applied Measurement In Education}, \bold{16(4)}, 277--298.
#' \item Magis, D., Raiche, G. & Beland, S. (2012) A didactic presentation of Snijders's l[sub]z[/sub] index of person fit with emphasis on response model selection and ability estimation. \emph{Journal of Educational and Behavioral Statistics}, \bold{37(1)}, 57--81.
#' \item Meijer, R. R. & Sijtsma, K. (2001) Methodology review: Evaluating person fit. \emph{Applied Psychological Measurement}, \bold{25(2)}, 107--135.
#' \item Molenaar, I. W. & Hoijtink, H. (1990) The many null distributions of person fit indices. \emph{Psychometrika}, \bold{55(1)}, 75--106. 
#' \item Mousavi, A. & Cui, Y. Evaluate the performance of and of person fit: A simulation study.
#' \item Reise, S. P. (1990). A comparison of item-and person-fit methods of assessing model-data fit in IRT.  \emph{Applied Psychological Measurement}, \bold{14(2)}, 127-137.
#' \item Snijders, T. B. (2001) Asymptotic null distribution of person fit statistics with estimated person parameter. \emph{Psychometrika}, \bold{66(3)}, 331--342. 
#' \item Wright, B. D. & Masters, G. N. (1990). Computation of OUTFIT and INFIT Statistics.  \emph{Rasch Measurement Transactions}, 3, 84-85.
#'}
#' 
#' @example ./R/.examples_pfit.R
#' @keywords Person fit, LZ-Index, Infit-Outfit
#' @export
Pfit <- function(respm,pp,fitindices) UseMethod("Pfit",object=pp)
# ------------------------------------------------------------------------------------------------------------------------------------------------------------


#'@method Pfit fourpl
#'@export
  Pfit.fourpl <- function(respm, pp, fitindices=c("lz","lzstar","infitoutfit")){

    if(any(pp$type%in%c("eap","robust"))) stop("Only 'mle','wle' and 'map' ability estimates are supported \n")

    pfitfunctions <- list("lz" = lz,
                          "lzstar" = lzstar,
                          "infitoutfit" = InfitOutfit
    )
    
    fitindices <- match.arg(fitindices, several.ok = TRUE)  

    pfitfunctions_red <- pfitfunctions[names(pfitfunctions)%in%fitindices]
    
    args <- list(list("data"=respm, 
         "thetas"=pp$resPP$resPP[,"estimate"], 
         "betas"=pp$ipar$thres[2,], 
         "lowerAs"=pp$ipar$lowerA, 
         "slopes"=pp$ipar$slopes, 
         "higherAs"=pp$ipar$upperA,
         "method"=pp$type, 
         "mu"=pp$ipar$mu, 
         "sigma"=sqrt(pp$ipar$sigma2)
    ))
    
    out <- mapply(function(x,y) do.call("y",x), x=args, y=pfitfunctions_red,SIMPLIFY = FALSE)
    names(out) <- names(pfitfunctions_red)
    class(out) <- append(class(out),"PPfit")
    return(out)
  }
  
  
  # ------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #'@rdname pfit
  #' 
  #' @method Pfit gpcm
  #' @export
  Pfit.gpcm <- function(respm, pp, fitindices=c("infitoutfit")){
    if(any(pp$type%in%c("map","eap","robust"))) stop("Only 'mle' and 'wle' ability estimates are supported \n")
    
    if(!all(fitindices%in%c("infitoutfit"))){ warning("Only 'infitoutfit' are currently supported. The calculation is executed with infitoutfit \n"); fitindices <- "infitoutfit"}
    if(any(pp$ipar$slopes>1)) warning("Currently only the PCM-Modell is supported \n")
    
    pfitfunctions <- list("infitoutfit" = InfitOutfitpoly)

    fitindices <- match.arg(fitindices, several.ok = TRUE)  

    pfitfunctions_red <- pfitfunctions[names(pfitfunctions)%in%fitindices]
    
    args <- list(list("data"=respm, 
                      "thetas"=pp$resPP$resPP[,"estimate"], 
                      "thresholds"=pp$ipar$thres, 
                      "slopes"=NULL
    ))
    
    out <- mapply(function(x,y) do.call("y",x), x=args, y=pfitfunctions_red,SIMPLIFY = FALSE)
    names(out) <- names(pfitfunctions_red)
    class(out) <- append(class(out),"PPfit")
    return(out)
  }
  # ------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #' @rdname pfit
  #' 
  #' @method Pfit gpcm4pl
  #' @export
  Pfit.gpcm4pl <- function(respm, pp, fitindices){
      cat("the mixed method for person fits is not yet implemented \n")
  }
  # ------------------------------------------------------------------------------------------------------------------------------------------------------------
