poi = read.csv('D:/낙동강/optipoi.csv')

str(poi)
str(df_zzin)
poi = poi[,-1]
poi = na.omit(poi)
library(corrplot)
poi_cor = cor(df_zzin)
corrplot(poi_cor, method = 'circle')


str(ori)
ori$창녕함안보방류량 = as.numeric(ori$창녕함안보방류량)
ori = ori[,-1]
ori = na.omit(ori)
ori_cor = cor(ori)
corrplot(ori_cor,method = 'circle')


his_poi = table(poi$포아송하굿둑염분도)
barplot(his_poi, col = 'lightblue',xlab='하굿둑염분도 alpha=0.1, windowsize=49', ylab='빈도수')

his = table(zero_to_one(salt$하굿둑_염분도.psu.))
barplot(his, col = 'lightblue',xlab='하굿둑염분도(Psu)', ylab='빈도수')
