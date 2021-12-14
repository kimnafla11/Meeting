t_square <- function(trdat, tedat, alpha) {
  
  obs = nrow(trdat);
  dim = ncol(trdat);
  
  mu = colMeans(trdat);
  
  # Control Limit from F-Dist Based on In-control data
  
  
  CL = qf(1-alpha,dim,obs - dim)*(dim*(obs+1)*(obs-1)/(obs*(obs-dim)));
  
  library(MASS)
  library(pracma)
  sinv = ginv(cov(trdat));  #(ginv, inv, solve는 모드 유사한 역할)
  
  
  mu_mat = repmat(mu, nrow(tedat),1);
  dte = tedat-mu_mat;
  
  
  
  Tsq_mat = matrix(numeric(0), nrow(tedat),1) 
  
  for( i in 1:nrow(tedat)) {
    Tsq_mat[i,1] = as.double(dte[i,]) %*% sinv %*% t(t(as.double(dte[i,])));
  }
  
  ret <- list(
    Tsq_mat =Tsq_mat,
    CL = CL
  )
  return (ret)                                    
}