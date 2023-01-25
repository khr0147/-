getwd()
df<-read.csv("movie.CSV", encoding = "cp949", fileEncoding = "euc-kr")
head(df)
str(df)
summary(df)

#결측치 확인
df$천만여부 <- as.factor(df$천만여부)
df$매출액 <- as.numeric(df$매출액)
str(df)
is.na(df)
complete.cases(df)
colSums(is.na(df))

#이상치
boxplot(df$매출액)
hist(df$매출액)
iqr <- IQR(df$매출액)
df_iqr <- df[(df$매출액<median(df$매출액) + iqr*2)
             &(df$매출액>median(df$매출액)-iqr*2),] #df[행,열]
hist(df_iqr$매출액)
df$매출액 <- log1p(df$매출액)
hist(df$매출액)

#데이터 분해, 결합
head(df)
under_2010 <-df[df$연도 <= 2010,]
over_2010 <- df[df$연도 > 2010,]
df2010 <- rbind(under_2010, over_2010)
str(df1)

#index
colnames(df)
df[,c("매출액","천만여부")] 
df[30:70,c("매출액","천만여부")]

#merge
df1<-df[,c("연도","천만여부")]
df2<-df[,c("연도","매출액")]
head(df1)
head(df2)
df_merge <- merge(df1, df2, all = T)
#help("merge")
head(df_merge)

#기술통계
library(dplyr)
summary(df)
colnames(df)
df1<- df[,c("넷플릭스","매출액")]
str(df1)
df1$넷플릭스 <- as.factor(df1$넷플릭스)
print(df1 %>%  group_by(넷플릭스) %>% summarise(mean(매출액)))
print(df1 %>%  group_by(넷플릭스) %>% summarise(var(매출액)))
print(df1 %>%  group_by(넷플릭스) %>% summarise(sd(매출액)))

#정규성 검정
shapiro.test(df$매출액)
hist(df$매출액)
ks.test(df$매출액,pnorm)

#독립성검정
df_chisq <- table(df$매출액, df$시리즈여부)
chisq.test(df_chisq)

#등분산검정
library(lawstat)
levene.test(df$누적관객수, df$넷플릭스)
bartlett.test(df$누적관객수, df$넷플릭스)

#anova
colnames(df)
anova <- aov(제작비 ~ 스크린수 , data = df)
summary(anova)
summary(df)
print(df %>% group_by(스크린수) %>% summarise(mean(제작비)))

