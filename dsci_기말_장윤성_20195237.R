library(tidyverse)

#samsung 주가 2020~
samsung <- read_csv("samsung2020.csv", locale=locale("ko", encoding="EUC-KR"), col_names=TRUE)
samsung
table(is.na(samsung))
samsung %>% ggplot(aes(Date,Close)) + geom_line()

#ncsoft 주가 2020~
nc <- read_csv("nc2020.csv", locale=locale("ko", encoding="EUC-KR"), col_names=TRUE)
nc
table(is.na(nc))
nc %>% ggplot(aes(Date,Close)) + geom_line()

#kb은행 주가 2020~
kb <- read_csv("kb2020.csv", locale=locale("ko", encoding="EUC-KR"), col_names=TRUE)
kb
table(is.na(kb))
kb %>% ggplot(aes(Date,Close)) + geom_line()

#뉴스심리지수 2020~
news <- read_csv("news_psychology.csv", locale=locale("ko", encoding="UTF-8"), col_names=TRUE)
news
table(is.na(news))

#뉴스심리지수가 행으로 되어있어 열로 변환
newsrow <- news[5:ncol(news)]

table(is.na(newsrow))

names(newsrow)
newsrow[1,]


# 뉴스심리지수 데이터가 다름 Date와 news col로 바꿈
newsrow <- newsrow %>% gather(names(newsrow), key='Date', value='newscol')

# 그냥 ggplot을 하면 그림이 그려지지 않는다. 하여 date 형식으로 바꾸어줌.
newsrow$Date <- as.Date(newsrow$Date)

# group = 1 을 해주면 그림이 그려진다.
newsrow %>% ggplot(aes(Date,newscol, group = 1)) + geom_line()

# ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ
# 삼성전자, 뉴스심리지수 데이터 병합
mergesamsung <- merge(samsung, newsrow, by="Date")
print(mergesamsung)
table(is.na(mergesamsung))

# 삼성전자의 종가와 newscol 데이터의 차이가 커 log를 적용후 알맞은 값을 더해 데이터를 겹쳐봄.
mergesamsung %>% ggplot() + geom_line(aes(Date, log(Close)), color="red") + geom_line(aes(Date, log(newscol)+6.5), color="blue")

# 모델 학습 위해 필요한 데이터만 뽑아 dataframe 생성
str(mergesamsung)
samsung_data <- mergesamsung %>% select(Date, Open, Close, newscol)
str(samsung_data)

# 데이터의 값이 차이가 너무 커 그냥 모델에 넣으면 newscol데이터의 의미가 잘 나타나지 않음 정규화 실시.
# 정규화를 위한 열 선택
cols <- c("Open", "Close", "newscol")
# 평균 계산 및 저장
mean_vals <- sapply(samsung_data[cols], mean)
# 표준편차 계산 및 저장
sd_vals <- sapply(samsung_data[cols], sd)
# 열 별로 정규화 수행
samsung_data[cols] <- apply(samsung_data[cols], 2, function(x) (x - mean(x)) / sd(x))
# 평균이 0에 수렴하고 분산이 1이 되게 정규화.
mean(samsung_data$Open)
sd(samsung_data$Open)
mean(samsung_data$Close)
sd(samsung_data$Close)
mean(samsung_data$newscol)
sd(samsung_data$newscol)

# 모델 학습
samsung_model <- lm(Close ~ Open + newscol , data=samsung_data)

# 결과
summary(samsung_model)

# 2차원 회귀선 그리기
# 정규화 돌려놓기
normalized_values <- predict(samsung_model, samsung_data[2:4])
normalized_values <- normalized_values * sd_vals["Close"] + mean_vals["Close"]

samsung2d <- samsung_data %>% mutate(pred = normalized_values)
# 원래의 값으로 복원
samsung2d$Open <- samsung2d$Open * sd_vals[1] + mean_vals[1]
samsung2d$Close <- samsung2d$Close * sd_vals[2] + mean_vals[2]
samsung2d$newscol <- samsung2d$newscol * sd_vals[3] + mean_vals[3]

# 데이터확인
tail(samsung2d)

plot(samsung2d$Close[700:length(samsung2d$Close)], col = "blue", type = "l", main = "삼성전자")
lines(samsung2d$pred[700:length(samsung2d$Close)], col = "red", type="l")

# 새로운 데이터 예측
predicted_values <- predict(samsung_model, data.frame(Open=(c(71300)-mean_vals["Open"])/sd_vals["Open"], newscol=(c(96.35)-mean_vals["newscol"])/sd_vals["newscol"]))

predicted_values * sd_vals["Close"] + mean_vals["Close"]  # 71151.06원 -> 실제값 : 72300원

# ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ
# ncsoft, 뉴스심리지수 데이터 병합
mergenc <- merge(nc, newsrow, by="Date")
print(mergenc)
table(is.na(mergenc))

# ncsoft의 종가와 newscol 데이터의 차이가 커 log를 적용후 알맞은 값을 더해 데이터를 겹쳐봄.
mergenc %>% ggplot() + geom_line(aes(Date, log(Close)), color="red") + geom_line(aes(Date, log(newscol)+8.5), color="blue")

# 모델 학습 위해 필요한 데이터만 뽑아 dataframe 생성
str(mergenc)
nc_data <- mergenc %>% select(Date, Open, Close, newscol)
str(nc_data)

# 데이터의 값이 차이가 너무 커 그냥 모델에 넣으면 newscol데이터의 의미가 잘 나타나지 않음 정규화 실시.
# 정규화를 위한 열 선택
cols <- c("Open", "Close", "newscol")
# 평균 계산 및 저장
mean_vals <- sapply(nc_data[cols], mean)
# 표준편차 계산 및 저장
sd_vals <- sapply(nc_data[cols], sd)
# 열 별로 정규화 수행
nc_data[cols] <- apply(nc_data[cols], 2, function(x) (x - mean(x)) / sd(x))
# 평균이 0에 수렴하고 분산이 1이 되게 정규화.
mean(nc_data$Open)
sd(nc_data$Open)
mean(nc_data$Close)
sd(nc_data$Close)
mean(nc_data$newscol)
sd(nc_data$newscol)

# 모델 학습
nc_model <- lm(Close ~ Open + newscol , data=nc_data)

# 결과
summary(nc_model)

# 2차원 회귀선 그리기
# 정규화 돌려놓기
normalized_values <- predict(nc_model, nc_data[2:4])
normalized_values <- normalized_values * sd_vals["Close"] + mean_vals["Close"]

nc2d <- nc_data %>% mutate(pred = normalized_values)
# 원래의 값으로 복원
nc2d$Open <- nc2d$Open * sd_vals[1] + mean_vals[1]
nc2d$Close <- nc2d$Close * sd_vals[2] + mean_vals[2]
nc2d$newscol <- nc2d$newscol * sd_vals[3] + mean_vals[3]

# 데이터확인
tail(nc2d)

plot(nc2d$Close[700:length(nc2d$Close)], col = "blue", type = "l", main = "Nc Soft")
lines(nc2d$pred[700:length(nc2d$Close)], col = "red", type="l")

# 새로운 데이터 예측
predicted_values <- predict(samsung_model, data.frame(Open=(c(323500)-mean_vals["Open"])/sd_vals["Open"], newscol=(c(96.35)-mean_vals["newscol"])/sd_vals["newscol"]))

predicted_values * sd_vals["Close"] + mean_vals["Close"]  # 325892원 -> 실제값 : 323500원

# ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ
# kb 국민은행, 뉴스심리지수 데이터 병합
mergekb <- merge(kb, newsrow, by="Date")
print(mergekb)
table(is.na(mergekb))

# ncsoft의 종가와 newscol 데이터의 차이가 커 log를 적용후 알맞은 값을 더해 데이터를 겹쳐봄.
mergekb %>% ggplot() + geom_line(aes(Date, log(Close)), color="red") + geom_line(aes(Date, log(newscol)+6), color="blue")

# 모델 학습 위해 필요한 데이터만 뽑아 dataframe 생성
str(mergekb)
kb_data <- mergekb %>% select(Date, Open, Close, newscol)
str(kb_data)

# 데이터의 값이 차이가 너무 커 그냥 모델에 넣으면 newscol데이터의 의미가 잘 나타나지 않음 정규화 실시.
# 정규화를 위한 열 선택
cols <- c("Open", "Close", "newscol")
# 평균 계산 및 저장
mean_vals <- sapply(kb_data[cols], mean)
# 표준편차 계산 및 저장
sd_vals <- sapply(kb_data[cols], sd)
# 열 별로 정규화 수행
kb_data[cols] <- apply(kb_data[cols], 2, function(x) (x - mean(x)) / sd(x))
# 평균이 0에 수렴하고 분산이 1이 되게 정규화.
mean(kb_data$Open)
sd(kb_data$Open)
mean(kb_data$Close)
sd(kb_data$Close)
mean(kb_data$newscol)
sd(kb_data$newscol)

# 모델 학습
kb_model <- lm(Close ~ Open + newscol , data=kb_data)

# 결과
summary(kb_model)

# 2차원 회귀선 그리기
# 정규화 돌려놓기
normalized_values <- predict(kb_model, kb_data[2:4])
normalized_values <- normalized_values * sd_vals["Close"] + mean_vals["Close"]

kb2d <- kb_data %>% mutate(pred = normalized_values)
# 원래의 값으로 복원
kb2d$Open <- kb2d$Open * sd_vals[1] + mean_vals[1]
kb2d$Close <- kb2d$Close * sd_vals[2] + mean_vals[2]
kb2d$newscol <- kb2d$newscol * sd_vals[3] + mean_vals[3]

# 데이터확인
tail(kb2d)

plot(kb2d$Close[700:length(kb2d$Close)], col = "blue", type = "l", main = "KB")
lines(kb2d$pred[700:length(kb2d$Close)], col = "red", type="l")

# 새로운 데이터 예측
predicted_values <- predict(kb_model, data.frame(Open=(c(48250)-mean_vals["Open"])/sd_vals["Open"], newscol=(c(96.35)-mean_vals["newscol"])/sd_vals["newscol"]))

predicted_values * sd_vals["Close"] + mean_vals["Close"]  # 48265.55원 -> 실제값 : 48350원


# 그냥 종합적인 그래프 시각화
ggplot() +
  geom_line(data = samsung, aes(Date, log(Close)), color = "blue") +
  geom_line(data = kb, aes(Date, log(Close)+0.3), color = "green") +
  geom_line(data = nc, aes(Date, log(Close)-2.3), color = "red") +
  geom_line(data = mergesamsung, aes(Date, log(newscol)+6.3), color = "black") +
  labs(title = "Close Prices", x = "Date", y = "Close") +
  theme_minimal()
