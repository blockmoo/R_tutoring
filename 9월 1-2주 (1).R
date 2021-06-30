# lag & lead : 시계열 데이터를 분석할 때 많이 사용.
# 특정 칼럼의 행을 위로 올리거나(lead) 아니면 내리는(lag) 함수
# lead() 나 lag() 함수는 시계열 데이터를 분석할 때 많이 사용하는 편
# 특정 그룹id와 날짜/시간 기준으로 정렬(sorting)을 해놓은 다음에,
# lead() 나 lag() 함수를 가지고 행을 하나씩 내리고, 
# 직전 날짜/시간 대비 이후의 값의 변화, 차이(difference)를 구하는 식
df <- data.frame(year = 2000:2005, value = (0:5)^2)
(scrambled <- df[sample(nrow(df)), ])


# lead() : 벡터 값을 n = 1L (양의 정수값) 의 값 만큼 
# 앞에서 제외하고, 제일 뒤의 n = 1L 값만큼의 값은 NA 로 채워놓은 값을 반환
x <- 1:10
lead(x, n=1)
lead(x, 2)

# lag() : n = 1L(양의 정수값) 만큼 제일 앞자리부터 뒤로 옮기고, 
# n = 1L 개수 만큼의 자리에 NA 값을 채워넣은 값을 반환
x <- 1:10
lag(x,n=1)
lag(x,2)

scrambled %>%
  mutate(prev = lag(value), after = lead(value)) %>%
  arrange(year)


scrambled %>%
  mutate(prev = lag(value, order_by = year), after = lead(value, order_by = year)) %>%
  arrange(year)



lag(scrambled$value, order_by = scrambled$year)
lag(scrambled$value)
lead(scrambled$value, order_by = scrambled$year)
lead(scrambled$value)


install.packages("tidyr")
library(tidyr)

head(table4a)
## 가로형/세로형 변환 (wide-form/long-form)


#gather : 가로형 변환(wide-form), 한 개체에 대한 반복측정
table4a
gather(table4a,'1999','2000', key="yy", value="num")
gather(table4a,'1999','2000', key="year", value="cases")

gather(table4a,"year","count",2:3)
gather(table4a, "year","count","1999":"2000")
gather(table4a, "year","count",-1)
head(table2)

#실습1
library(dplyr)
mtcars$name = rownames(mtcars)
rownames(mtcars) = NULL
mtcars %>%
  select(name, mpg, cyl, disp) -> mtcars01
head(mtcars01)

mtcars01 %>% 
  gather("key","value", mpg, cyl, disp) -> mtcars02

#실습2
setwd("C:/Users/hyerin/Downloads")
getwd()
test <- read.csv("test.csv", header = T)
test %>%
  gather("key","value",3:6)

#spread
spread(table2, type, count)
table2
#실습3 spread
table3
table3 %>%
  spread(year, rate)

#separate
table3 %>%
  separate(rate, into = c("cases","pop"))

table3 %>%
  separate(rate, c("case","pop"))

#실습
stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10,0,1),
  Y = rnorm(10,0,2),
  Z = rnorm(10,0,4)
)
stocks

#1 
stocks %>%
  gather(stock, price, 2:4) ->stocksm

#2 stock의 x,y,z를 col으로 뿌려주고, 해당하는 price들을 각 field에 넣어줌.
stocksm %>%
  spread(stock, price) 

#3 stocksm의 time 열을 년도/월/일로 나눠주는 함수
stocksm %>%
  separate(time, c("year","month","day"), sep="-")





## separate의 옵션들 fill, extra
#1
(df <- data.frame(x=1:2, y=c("a,b","d,e,f")))
df %>%
  separate(y, c("y1","y2","y3"))

df %>%
  separate(y, c("y1","y2","y3"), sep=",", fill="warn")

df %>%
  separate(y, c("y1","y2","y3"), sep=",", fill="right")


df %>%
  separate(y, c("y1","y2","y3"), sep=",", fill="left")

#2
(df <- data.frame(x=1:2, y=c("a,b,cd,e","d,e,f")))
df %>%
  separate(y, c("y1","y2","y3"))


df %>%
  separate(y, c("y1","y2","y3"), sep=",", extra="warn")

df %>%
  separate(y, c("y1","y2","y3"), sep=",", extra="drop")

df %>%
  separate(y, c("y1","y2","y3"), sep=",", extra="merge")

#separate_rows
table3

table3 %>%
  separate_rows(rate)

#unite
table5 %>%
  unite(century, year, col="year", sep="") %>%
  separate(rate, c("y1","y2"))

#missing values dataset
(x <- tibble(ColA=LETTERS[1:6], ColB=c(1,NA,NA,4,5,NA)))

#drop_na : drop rows containing NA
x %>%
  drop_na(ColB)

#fill : 가장 최근값으로 결측치 처리, NA바로 앞에 나왔던 값으로 채우기
x %>%
  fill(ColB)

x %>%
  fill(ColB, .direction="up")

#replace_na
x %>%
  replace_na(list(ColB =-9))

x %>%
  replace_na(list(ColB=0))

#평균으로 결측치 대체
x %>%
  replace_na(list(ColB=mean(x$ColB, na.rm = T)))

#중앙값으로 결측치 대체
x %>%
  replace_na(list(ColB=median(x$ColB, na.rm=T)))

mtcars %>%
  complete(cyl,gear,carb)


set.seed(1234)
data.frame(X=sample(LETTERS[1:2],5,replace = T), Y=sample(LETTERS[5:10], 5, replace = T), Z=1)

head(mtcars)

df <- data.frame(
  X=c("A", rep("B",4)),
  Y = c("I","H","H","I","E"),
  Z=rnorm(5,1)
)
df %>%
  group_by(X,Y) %>%
  tally() %>%
  spread(Y,n)

df %>%
  complete(X,Y) %>%
  group_by(X,Y) %>%
  tally() %>%
  spread(Y,n)

df %>%
  expand(X,Y)

mtcars %>%
  expand(cyl, gear, carb)

distinct(mtcars, cyl)
distinct(mtcars, gear)
distinct(mtcars, carb)


#########################
## ggplot2 ##############----------------------------------------
#########################
library(ggplot2)
ggplot(mtcars, aes(mpg, hp, col=factor(cyl), shape=factor(cyl))) + geom_point()

iris %>%
  ggplot(aes(Sepal.Length, Sepal.Width, col=Species)) + geom_point()

mtcars %>% 
  ggplot(aes(mpg, hp, col=factor(cyl), shape=factor(cyl))) +
  geom_point(shape=21, col="blue")


#아래의 2개의 결과는 같음.
mtcars %>% 
  ggplot(aes(mpg, hp)) + geom_point(aes(col=cyl))


mtcars %>% 
  ggplot(aes(mpg, hp, col=cyl)) +
  geom_point()
#-------------------
ggplot(mtcars, aes(mpg, hp)) +
  geom_point(col=ifelse(mtcars$mpg>20, "red","blue"))

# 실습 1
#mtcars 데이터에서 x축은 wt, y축은 qsec으로 하는 그래프를 그리고 
#qsec값이 18보다 작으면 green, 18보다 같거나 크면 red인 그래프를 그리시오/
ggplot(mtcars, aes(wt, qsec)) +
  geom_point(col=ifelse(mtcars$qsec <18, "green","red"))

#count 막대그래프
ggplot(mtcars, aes(cyl)) + geom_bar()
ggplot(mtcars, aes(factor(cyl))) + geom_bar()

# 실습 : mtcars의 gear에 대한 막대그래프를 그리시오.
ggplot(mtcars, aes(gear)) + geom_bar()

ggplot(mtcars, aes(factor(cyl), fill=factor(cyl))) +
  geom_bar()

ggplot(mtcars, aes(cyl, fill=factor(cyl))) +
         geom_bar()



























































































