df = read.csv('C:/Users/User/Desktop/breakdown.csv')
head(df)


Date_time = df$일시

df1 = df[,-c(1,2,4,6)] #수치형변수만 남겨놓기
head(df1)

par(mfrow=c(1,1))
for(i in 1:ncol(df1)){
  plot(df1[,i])
  abline(v=which(df$일시=='2018 33:22'), col=adjustcolor('blue', alpha = 0.5))
  abline(v=which(df$일시=='2018 48:59'), col=adjustcolor('red', alpha = 0.5))
}

###
df1$작업지시번호 = as.factor(df1$작업지시번호)

df1$작업지시번호

work_n1 = which(df1$작업지시번호== '20 6-14')
work_n2 = which(df1$작업지시번호== '201 -10')
work_n3 = which(df1$작업지시번호== '20 8 025-7')

##
plot(df1$PEA 압력)
abline(v=which(df$일시=='201 3:22'), col=adjustcolor('hotpink'))
abline(v=which(df$일시=='201 59:48'), col=adjustcolor('hotpink'))

points(x=work_n1, y=df1[work_n1,]$PEA 압력,col='red')
points(x=work_n2, y=df1[work_n2,]$PEA 압력,col='blue')
points(x=work_n3, y=df1[work_n3,]$PEA 압력,col='dark green')



plot(df1$V. 치)
points(x=work_n1, y=df1[work_n1,]$V. 치,col='red')
points(x=work_n2, y=df1[work_n2,]$V. 치,col='blue')
points(x=work_n3, y=df1[work_n3,]$V. 치,col='green')
abline(v=which(df$일시=='2018-07-21 08:33:22'), col=adjustcolor('hotpink'))

work_n1 = which(df1$작업지시번호== '2 0 -14')
work_n2 = which(df1$작업지시번호== '20 10')
work_n3 = which(df1$작업지시번호== '2018 5-7')
#37920
head(df1)
View(df1)
par(mfrow=c(3,3))
for(i in 3:ncol(df1)){
  plot(df1[,i])
  points(x=work_n1, y=df1[work_n1,i],col='red')
  points(x=work_n2, y=df1[work_n2,i],col='blue')
  points(x=work_n3, y=df1[work_n3,i],col='dark green')
  abline(v=which(df$일시=='201 1:04'), col=adjustcolor('cyan'))
  abline(v=which(df$일시=='2018- 22'), col=adjustcolor('red'))
}