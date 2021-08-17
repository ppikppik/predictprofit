#install.packages('tseries')
library(tseries)

hyundai_car<- read.csv('hyundai_car.csv')
gs_boo <- read.csv('gs_boo.csv')


#시계열 데이터로 만들기
df_ts <- ts(hyundai_car[,12], start=1985, frequency=1)

df_ts
diff(df_ts)

##원 시계열
#1.비정상적 평균 확인 : 추세
par(oma=c(0,0,0,0))
par(mfrow = c(1,1))
plot(df_ts, main="<Figure 1> Margin of Hyundai MOBIS(1985.12~2019.12)", ylab="margin(1milion Won)", xlab="Year")

par(oma=c(0,0,5,0))
par(mfrow = c(1,2))
acf(df_ts , main="ACF", ylab="")
pacf(df_ts, main="PACF", ylab="")
mtext("<Figure 2> ACF, PACF of MOBIS's Margin",outer=TRUE,cex=2)

adf.test(df_ts,k=0)

par(oma=c(0,0,0,0))
par(mfrow = c(1,1))
plot(diff(df_ts), main="<Figure 3> Margin of Hyundai MOBIS(1985.12~2019.12)", ylab="margin(1milion Won)", xlab="Year")

par(oma=c(0,0,5,0))
par(mfrow = c(1,2))
acf(diff(df_ts) , main="ACF", ylab="")
pacf(diff(df_ts), main="PACF", ylab="")
mtext("<Figure 2> ACF, PACF of MOBIS's Margin",outer=TRUE,cex=2)

adf.test(diff(df_ts),k=0)


##GS건설
df_ts <- ts(gs_boo[,12], start=1981, frequency=1)
par(oma=c(0,0,0,0))
par(mfrow = c(1,1))
plot(df_ts, main="<Figure 1> Margin of GS(1981.12~2019.12)", ylab="margin(1milion Won)", xlab="Year")

par(oma=c(0,0,5,0))
par(mfrow = c(1,2))
acf(df_ts , main="ACF", ylab="")
pacf(df_ts, main="PACF", ylab="")
mtext("<Figure 2> ACF, PACF of GS's Margin",outer=TRUE,cex=2)

adf.test(df_ts,k=0)

par(oma=c(0,0,0,0))
par(mfrow = c(1,1))
plot(diff(df_ts), main="<Figure 3> Margin of Hyundai MOBIS(1985.12~2019.12)", ylab="margin(1milion Won)", xlab="Year")

par(oma=c(0,0,5,0))
par(mfrow = c(1,2))
acf(diff(df_ts) , main="ACF", ylab="")
pacf(diff(df_ts), main="PACF", ylab="")
mtext("<Figure 2> ACF, PACF of GS's Margin",outer=TRUE,cex=2)



adf.test(diff(df_ts))

