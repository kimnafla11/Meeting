plot(0, type = 'n', ylim=c(0,0.2), xlim = c(0,100), ann = FALSE)

title(main = 'Poisson distribution', xlab = 'x', ylab = 'p(x)')

lambda = c(5, 10, 30, 50, 70)

for(i in 1:5){
  x = 1:100
  y = dpois(x, lambda[i], log = FALSE) # 포아송분포 함수
  
  points(x,y, type = 'b', col = rainbow(5)[i])
  
  abline(v = lambda[i], col = rainbow(5)[i], lty = 3)
  text(lambda[i], max(y)+0.01, paste0('lambda=', lambda[i]), col = rainbow(5)[i]) # 그래프마다 텍스트 표시
  # text(글씨 위치 x값, 글씨위치 y값, paste0(), col = )
}

# 포아송분포의 평균과 분산은 람다이기때문에 람다가 커지면 커질수록 그래프가 펑퍼짐해짐
