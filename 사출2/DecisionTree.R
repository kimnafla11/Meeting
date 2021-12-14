df = read.csv('C:/Users/User/Documents/wn18_11tem.csv')
df = df[,-1]
df$oco = as.integer(df$oco)
head(df)
str(df)

library(rpart)
library(rpart.plot)
fit = rpart(df$oco~., data = df, method = 'class',control=list(maxdepth = 3))
rpart.plot(fit)
summary(fit)
