#### PPass function - brings it all together


PPass <- function(respdf, items="all", mod=c("1PL","2PL","3PL","4PL","PCM","GPCM","MIXED"), ...)
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

  
  
  
  
  
  
}
