# 작성일 220308
# 데이터 저장용

# 데이터 불러오기 ----
df = read.csv('C:/수정낙동강수질데이터.csv')
head(df)

# 데이터 전처리 ----
str(df)
df$창녕함안보.방류량 = as.numeric(df$창녕함안보.방류량)
df = na.omit(df)
names(df) = c('시간','구포대교수위','구포대교유량','월촌리수위','월촌리유량','상동수온','상동ph','상동ec','상동do','창녕함안보방류량','하굿둑수온','하굿둑염분도')
names(df)
head(df)


# 포아송 변수 합친 데이터 ----
poi = poisson_transfer(df$하굿둑염분도, df$하굿둑염분도, 0.45, 95)
names(poi) = c('포아송')
df = cbind(df, poi)
head(df)

# write.csv
write.csv(df,'하굿둑염분도0308.csv')
