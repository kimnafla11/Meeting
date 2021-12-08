df = read.csv('C:/Users/User/Desktop/breakdown.csv')
head(df)
str(df)
df1 = df[,-c(1:6,15)] #작업지시번호부터 금형(하)까지 열제외
head(df1)
df1$작업지시번호 = as.factor(df1$작업지시번호)#작지번 3개 2018050026-14 2018070012-10 2018070025-7
as.factor(df1$도번)#도번2개 299158074 1031721300

library(corrplot)
par(mfrow=c(1,3))
df14 = df1[df$작업지시번호=='2018050026-14',] #작업지시번호가 저거인 행만
cor(a)
corrplot(cor(a),'circle')

df10 = df1[df$작업지시번호=='2018070012-10',]
head(b)
cor(b)
corrplot(cor(b),'circle')

df7 = df1[df$작업지시번호=='2018070025-7',]
cor(c)
corrplot(cor(c),'circle')

#스케일링--------------------------------------

df1 = scale(df1) #표준정규분포(standard) 스케일링 (x-mu)/std
head(df1)

df14 = scale(df14)
df10 = scale(df10)
df7 = scale(df7)
### T-square Decomposition -------------------------------------------------------------------------------
# 원래 T-square할때 평균이랑 공분산은 정상범주여야함 --> train data
# 전체
cm14 = colMeans(df14) #거리 구할때 중간점(평균) 얘는 벡터임, 정상범주여야함.
cov14 = cov(df14) #p*p행렬 공분산행렬, 정상범주여야함.

t2_mat14 = matrix(0,nrow(df14),1)
for(i in 1:nrow(df14)){
  t2_mat14[i,] = (as.numeric((df14[i,]-cm14))) %*% solve(cov14) %*% as.numeric((df14[i,]-cm14))
}
# t2_mat (전체)

decom_mat14 = matrix(0,nrow(df14),ncol(df14))
world14 = matrix(0,nrow(df14),1)
for(j in 1:ncol(df14)){
  cm14 = colMeans(df14[,-j])
  cov14 = cov(df14[,-j])
  for(i in 1:nrow(df14)){
    world14[i,] = (as.numeric((df14[i,-j]-cm14))) %*% solve(cov14) %*% as.numeric((df14[i,-j]-cm14)) #변수 하나씩 뺀거
  }
  decom_mat14[,j] = sqrt((t2_mat14-world14)^2)
}
decom_mat14 = as.data.frame(decom_mat14) #데이터프레임으로 형변환
colnames(decom_mat14) = colnames(df14) #변수명
head(decom_mat14)


plot(t2_mat14,ylim=c(0,200), col='red')

# 원인 변수 해석--------------------
# 이상 원인이 있는 변수 intpre변수에 넣기기
cl=60
outlier = decom_mat14[which(t2_mat14>cl),]
abline(h=cl, col='blue')
intpre = c() #이상원인이 큰 변수 인덱스 추출
table(intpre) #빈도수
for (i in 1:nrow(outlier)){
  intpre = append(intpre, which(outlier[i,]==max(outlier[i,])))
}
par(mfrow=c(1,1))
plot(t2_mat14, ylim=c(0,200),col='red')
abline(h=cl, col='blue', lwd=2)
abline(v=which(t2_mat14>cl), col=adjustcolor(intpre+1, alpha=0.3),lwd=2) # 원인변수 v라인
legend('topleft', colnames(df1), col=adjustcolor(2:9, alpha=0.7), lwd=1, cex=.7) #범례
# 관측치 원인변수 -------------------


t2_mat14[which(t2_mat14>150 & t2_mat14<180)]
# 2957
t2_mat14[2957,]
t(decom_mat14[2957,])

intdecom = as.integer(decom_mat14[2957,])
names(intdecom) = colnames(decom_mat14)
barplot(sort(intdecom)[4:8], horiz=T,col=adjustcolor(c(6,9,3,5,2), alpha=0.5), border='white')


# 원인변수 빈도수 -----------------
freq = c(table(intpre))
names(freq) = colnames(df14)[sort(unique(intpre))]
table(intpre)

barplot(sort(freq, decreasing = F), col=adjustcolor(c(4,7,5,8,3,2), alpha=0.5), border='white', horiz=T)

#####bootstrap---------
# stat : 통계량
# alpha : 유의수준
# m : 반복횟수
cl = bootlimit(stat = t2_mat[1:10000], alpha=0.005, m=1000)
abline(h=cl,col='blue',lwd=2)
#boot_limit의 평균값을 plot에 가로로(h) 찍음

