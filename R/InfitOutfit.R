InfitOutfit <- function( data, 
                         thetas, 
                         betas, 
                         lowerAs=NULL, 
                         slopes=NULL, 
                         higherAs=NULL,... ){

  if(is.null(slopes)) slope <- rep(1,length(betas))
  if(is.null(lowerAs))lowerAs <- rep(0,length(betas))
  if(is.null(higherAs))higherAs <- rep(1,length(betas))

  if(!all(apply(data,2,function(x) { all(na.omit(x) %in% 0:1) }))) stop("Please check the input, only 0/1/NA are allowed \n")
  X <- data
  L <- ncol(X)
  N <- as.numeric(apply(X,1,function(x) sum(!is.na(x))))
  
  ai <- slopes
  ci <- lowerAs
  di <- higherAs
  # calculate the propability of each pearson to not solve an item
  submatrix <- (matrix( thetas, ncol = nrow( data ), nrow = ncol( data ) ,byrow = TRUE) - betas) * ai
  
  
  # --------------------------------------------------------------------------------
  # probabilit. of solving an Item Expected Response (in der Literatur Pi nik)
   Pni     <- t( ci + (di-ci)/(1+exp(-submatrix)) )
  # --------------------------------------------------------------------------------

  # first: finde the k categories of each item
       k_temp <- apply(X,2,max,na.rm=TRUE)
       k  <- as.vector( sapply( k_temp, function(x) seq(0,x) ) )
  # second: calculate for each the Prop

  # ------------------------------------------------  
  Qni <- Pni * ( 1-Pni )
  
  # Variance of xni
  Wni <- ( ( (0-Pni)^2 ) * (1-Pni) ) + ( ( (1-Pni)^2 ) * Pni)
  
  # Kurtosis of xni
  Cni <- ( ( (0-Pni)^4 ) * (1-Pni) ) + ( ( (1-Pni)^4 ) * Pni)
  
  # Score Residual: (Pni ist in der Literatru Eni)
  Yni <- X - Pni
  
  # Standardized Residual (Qni ist in der Literatur Wni)
  Zni <- Yni / sqrt( Qni )
  
  # Fit Mean Square
  
  # OUTFIT  (unweighted)
  Un <- rowSums( ((Zni)^2), na.rm=TRUE ) / N
  
  # INFIT  (weighted)
  Vn <- rowSums( (Yni)^2, na.rm=TRUE ) / rowSums( Qni, na.rm=TRUE )
  
  # standardized INFIT
  # Variance term
  qni2 <- rowSums(Cni - Wni^2,na.rm=TRUE ) / (rowSums(Wni,na.rm=TRUE))^2
  # infit.z
  ti <- ( (Vn^(1/3)) - 1)*(3/sqrt(qni2))+(sqrt(qni2)/3)
  
  # Variance term
  varInfit <- rowSums(Cni / Wni^2,na.rm=TRUE ) / (N^2) - (1/N)
  # outfit.z
  tu <- ( (Un^(1/3)) - 1)*(3/sqrt(varInfit))+(sqrt(varInfit)/3)

# additional output
  chisq   <- rowSums( ((Zni)^2), na.rm=TRUE )
  Zni2 <- rowSums( ((Zni)^2), na.rm=TRUE )
  pvalue  <- 1 - pchisq(Zni2, N-1 )
  df      <- N - 1 
  
  out <- cbind(
    "Chisq"       = round(chisq,3),
    "df"          = df,
    "pvalue"      = round(pvalue,3),
    "outfit"      = round(Un,6),
    "outfit.t"    = round(tu,6),
    "infit"       = round(Vn,6),
    "infit.t"     = round(ti,6)
  )
  return(out)

}