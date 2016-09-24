#### PPass function - brings it all together

#' Person Assessment function
#' 
#' Estimate Person Paramters and estimate Person Fit in one step to gain resonse pattern assessment.
#' 
#' @param \ldots Submit arguments to the underlying functions: \code{PP_4pl}, \code{PP_gpcm} and \code{PPall} (see documentation files).
#' 
#' @rdname PPass
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
#' @author Manuel Reif, Jan Steinfeld
#' 
#' @method PPass default
#' 
#' @rdname PPass
#' 
#' @export
#' 
#' @seealso \link{PP_4pl}, \link{PP_gpcm}, \link{PPall}
#' 

PPass.default <- function(respdf, items="all", mod=c("1PL","2PL","3PL","4PL","PCM","GPCM","MIXED"), fitindices= c("lz","lzstar"), ...)
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
  
  
########### ESTIMATE PERSON FIT ###############################

  
  
  
########### PUT IT ALL TOGETHER ############################### 
  
  
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
PPass.Rm <- function(RMobj, fitindices= c("lz","lz_star"), ...)
{
  
########### ESTIMATE PERSON PARAMETERS ###############################  
  
pp_est <- PP_4pl(respm=RMobj$X, thres=RMobj$betapar * (-1), ...)

  
########### ESTIMATE PERSON FIT ###############################
  
  
  
  
########### PUT IT ALL TOGETHER ###############################
  
pp_est  
}





