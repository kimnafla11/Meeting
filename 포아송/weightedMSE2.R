# 작성일 220308
# Fitted Values Weighted MSE구하기

# 데이터 불러오기 ----
df = read.csv('C:/수정낙동강수질데이터.csv')
head(df)

# 데이터 전처리 ----
str(df)
df$창녕함안보.방류량 = as.numeric(df$창녕함안보.방류량)
df = na.omit(df)
df = df[,-1] # 필요 없는 변수 빼기
names(df) = c('구포대교수위','구포대교유량','월촌리수위','월촌리유량','상동수온','상동ph','상동ec','상동do','창녕함안보방류량','하굿둑수온','하굿둑염분도')
names(df)

# 포아송 값 변환 ----
# 3. 랜포모델로 regression/ weighted MSE구하기
# 하다보니까 느꼈는데 데이터를 0~1 사이 값으로 scaling한 후 MSE를 비교해야한다(둘이 단위 다르니까.)

library(randomForest)

ori = df[,-11] # regression 할 때 하굿둑 염분도 변수 제거
head(ori)
poi = poisson_transfer(df$하굿둑염분도, df$하굿둑염분도, 0.45, 95)
names(poi) = c('포아송')
poi = cbind(ori, poi)
head(poi)

# randomforest Model
p_rf = randomForest(poi$포아송~., data = poi)
o_rf = randomForest(df$하굿둑염분도~., data = df)

# fitting값 시각화

# 변환 전
plot(df$하굿둑염분도, type = 'o')
points(o_rf$predicted, col = 'red', type = 'o')

# 변환 후
plot(poi$포아송, type = 'o', col = 'blue')
points(p_rf$predicted, col = 'red', type = 'o')

# mse
o_mse = mean((o_rf$predicted - df$하굿둑염분도)^2) # 0.004621957
p_mse = mean((p_rf$predicted - poi$포아송)^2) # 0.004330225

# r_square
SST = sum((df$하굿둑염분도 - mean(df$하굿둑염분도))^2)
SSE = sum((o_rf$predicted - mean(df$하굿둑염분도))^2)
o_r_sqrt = SSE/SST # 0.4249079

SST = sum((poi$포아송 - mean(poi$포아송))^2)
SSE = sum((p_rf$predicted - mean(poi$포아송))^2)
p_r_sqrt = SSE/SST # 0.9119829


# weighted MSE
w0 = zero_to_one((1-df$하굿둑염분도)/(1-sum(df$하굿둑염분도)))
o_wmse = mean((w0*(o_rf$predicted - df$하굿둑염분도)^2))# 0.002373978
w1 = zero_to_one(df$하굿둑염분도/sum(df$하굿둑염분도))
o_wmse = mean((w1*(o_rf$predicted - df$하굿둑염분도)^2))# 0.002373978


w2 = zero_to_one((1-poi$포아송)/(1-sum(poi$포아송)))
p_wmse = mean((w2*(p_rf$predicted - poi$포아송))^2)# 0.002038168
w3 = zero_to_one(poi$포아송/sum(poi$포아송))
p_wmse = mean((w3*(p_rf$predicted - poi$포아송))^2)# 0.002038168



o_mse 
p_mse