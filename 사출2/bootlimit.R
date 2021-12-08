# 단측 검정
bootlimit = function(stat, alpha=0.05, m=100){
  
  # Bootstrap sampling 기법을 이용한 control limit 산출 
  
  # @param stat : 통계량 (일반적으로 정상 구간 내 통계량)
  # @param alpha : 유의수준
  # @param m : 반복횟수
  
  set.seed(1000)
  stat = as.vector(stat)
  CL_mat = matrix(0, m, 1)
  
  for(i in 1:m){
    sample_temp = sample(stat, size=length(stat), replace=T, prob=NULL)
    CL = quantile(sample_temp, 1-alpha)
    CL_mat[i,] = CL
  }
  cl = mean(CL_mat)
  
  return(cl)
}
