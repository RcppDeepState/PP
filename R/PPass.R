#### PPass function - brings it all together

#' Person Assessment function
#' 
#' Estimate Person Paramters and calculate Person Fit in one step to gain resonse pattern assessment. Submit a data.frame which contains item responses, or an fitted model (Rasch Model and Partial Credit Model are supported) of the \code{eRm} package.
#' 
#' @param \ldots Submit arguments to the underlying functions: \code{PP_4pl}, \code{PP_gpcm} and \code{PPall} (see documentation files) for person parameter estimation.
#' 
#' @rdname PPass
#' 
#' @example R/.example_ppass.R
#' 
#' @export
#'

PPass <- function(...) UseMethod("PPass")

# ---------------------------------------------------------------------



#' @param respdf A data.frame which contains the items, and perhaps other informations. Each row is a person related resonse patter. Each column denotes a variable.
#' 
#' @param items A numeric (integer) vector which indicates the positions of the items in the data.frame (\code{respdf}). If \code{items} = 'all', \bold{all columns} are treated as items.
#' 
#' @param mod Choose your data generating model. This argument switches between the three person parameter estimating functions \code{PP_4pl}, \code{PP_gpcm} and \code{PPall}.
#' 
#' @param fitindices A character vector which denotes the fit indices to compute.
#' 
#' 
#' @details PPass fuses Person Parameter estimation and Person Fit computation into a single function.
#' 
#' @return The original data.frame and
#' 
#' \itemize{
#' 
#' \item The Person Parameter estimates incl. Standard Errors (2 columns)
#' 
#' \item Person Fit Indices you chose (1 or more)
#' 
#' }
#' 
#' @author Manuel Reif, Jan Steinfeld
#' 
#' @method PPass default
#' 
#' @rdname PPass
#' 
#' @export
#' 
#' @seealso \link{PP_4pl}, \link{PP_gpcm}, \link{PPall}, \link{Pfit}
#' 

PPass.default <- function(respdf, items="all", mod=c("1PL","2PL","3PL","4PL","PCM","GPCM","MIXED"), fitindices= c("lz","lzstar","infitoutfit"), ...)
{

  
########### ESTIMATE PERSON PARAMETERS ###############################  
  
## checks concering the input  
  
stopifnot(is.data.frame(respdf)) # muss ein df sein als input
stopifnot((is.numeric(items) & all(items) >= 1) | all(items == "all")) # indices oder alles
  
## create matrix, keep the rest
if(items == "all") # all variables are items
  {
    respm <- as.matrix(respdf)
  } else {
    
  rest  <- respdf[ , -items, drop=FALSE]
  respm <- as.matrix(respdf[ , items, drop=FALSE])
  }
# if not a matrix, extract the items and convert to matrix

# check if first element is character
if(is.character(respm[1,1])) stop("At least one response is of type character!\n")


if(mod %in% c("1PL","2PL","3PL","4PL"))
  {
  pp_est <- PP_4pl(respm, ...)
  } else if(mod %in% c("PCM", "GPCM"))
    {
    pp_est <- PP_gpcm(respm, ...)
     
    } else if("MIXED"){ # mixed
            pp_est <- PPall(respm, ...)
            }
  
  
########### CALCULATE PERSON FIT ###############################

fit_calc <- Pfit(respm=respm,pp=pp_est,fitindices=fitindices)
# rename the colnames and combine to data.frame  
for(l in names(fit_calc)){
    colnames(fit_calc[[l]]) <- paste0(l,"_",colnames(fit_calc[[l]]))
  }
fit_calc <- do.call(cbind,fit_calc)
########### PUT IT ALL TOGETHER ############################### 
  out <- list("personparameter"=pp_est,"personfit"=fit_calc)
return(out)
  
}





# ================= for eRm input =================================


#' @param RMobj A fitted Rasch Model (\code{RM()}) object which stems from the \code{eRm} package.
#' 
#' @rdname PPass
#' 
#' @export
#' 
#' @method PPass Rm
#' 
#' 
PPass.Rm <- function(RMobj, fitindices= c("lz","lz_star","infitoutfit"), ...)
{

  # geht leider nicht anders weil sowohl PCM als auch RM die Klassen Rm als auch eRm haben.
  
  if(RMobj$model == "RM")
    {
    
    pp_est <- PP_4pl(respm=RMobj$X, thres=RMobj$betapar * (-1), ...)
    

    
    } else if(RMobj$model == "PCM")
      {
       
      # get threshold parameters:
      
      # create threshold matrix:
      tps <- eRm::thresholds(RMobj)$threshpar
      
      len1        <- apply(RMobj$X, 2, function(x) length(unique(x))-1)
      itemss      <- unlist(lapply(1:length(len1), function(x) rep(x, each=len1[x])))
      wohin_zeile <- unlist(lapply(len1, function(x) 1:x))
      
      names(wohin_zeile) <- NULL
      
      thres <- matrix(NA,ncol=ncol(RMobj$X), nrow=max(len1))
      
      for(i in 1:length(tps))
      {
        thres[wohin_zeile[i], itemss[i]] <- tps[i]
      }
      
      thres <- rbind(0,thres)
      
      ########### ESTIMATE PERSON PARAMETERS ###############################  
      
      pp_est <- PP_gpcm(respm=RMobj$X, thres=thres, ...)
       
      
      } else {
        
        stop("I don't know this model!")
        
      }

  ########### CALCULATE PERSON FIT ###############################  

  fit_calc <- Pfit(respm=RMobj$X,pp=pp_est,fitindices=fitindices)
  # rename the colnames and combine to data.frame  
  for(l in names(fit_calc)){
    colnames(fit_calc[[l]]) <- paste0(l,"_",colnames(fit_calc[[l]]))
  }
  fit_calc <- do.call(cbind,fit_calc)
  ########### PUT IT ALL TOGETHER ############################### 
  out <- list("personparameter"=pp_est,"personfit"=fit_calc)
  return(out)
}



