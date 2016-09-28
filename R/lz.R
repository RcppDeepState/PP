# LZ-Statistik
lz <- function( data, 
                thetas, 
                betas, 
                lowerAs=NULL, 
                slopes=NULL,
                higherAs=NULL,...){ 
  if(is.null(slopes)) slope <- rep(1,length(betas))
  if(is.null(lowerAs))lowerAs <- rep(0,length(betas))
  if(is.null(higherAs))higherAs <- rep(1,length(betas))
     ai <- slopes
     ci <- lowerAs
     di <- higherAs
    
  # calculate the propability of each person to solve an item ("-x" within the probabilit.)
  submatrix <- (matrix( thetas, ncol = nrow( data ), nrow = ncol( data ) ,byrow = TRUE) - betas) * ai

   Pi     <- t( ci + (di-ci)/(1+exp(-submatrix)) )
   Pi_1   <- 1-Pi

  l0      <- rowSums( (data * log(Pi)) + ((1-data) * log(Pi_1)) ,na.rm=TRUE)
  mean_l0 <- rowSums( (Pi * log(Pi)) + ((Pi_1) * log(Pi_1)) ,na.rm=TRUE)
  var_l0  <- rowSums( (Pi * (Pi_1)) * (log(Pi/(Pi_1))^2) ,na.rm=TRUE)
  lz      <- (l0 - mean_l0) / sqrt(var_l0) 
  lz <- round(lz,3)
  l0 <- round(l0,3)
  out <- cbind("lz"=lz,"unstandardized"=l0)
  return(out)
  
}
