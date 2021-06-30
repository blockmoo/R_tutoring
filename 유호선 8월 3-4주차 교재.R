install.packages("dplyr")
install.packages("readxl")

library(dplyr)
library(tibble)
library(readxl)



head(iris)

rename(iris, Length=Sepal.Length) %>% head()
a <- rownames_to_column(iris, var="rowname") %>% head()
a <- remove_rownames(a)
column_to_rownames(a, var="rowname")


a <- select(iris, 1) %>% head(10)
b <- select(iris, 2) %>% head(5)
bind_cols(a,b) %>% head(10)

iris_rownames <- rownames_to_column(iris, var = "rowname")
a <- slice(iris_rownames, 1:5)
b <- slice(iris_rownames, 11:15)
bind_rows(a,b)

mtcars_name <- rownames_to_column(mtcars, var="car_name")
a <- select(mtcars_name, car_name, mpg) %>% slice(1:4)
b <- select(mtcars_name, car_name, mpg) %>% slice(3:6)

intersect(a,b) # 교집합
setdiff(a,b)   # 차집합
union(a,b)     # 합집합
mtcars_name <- rownames_to_column(mtcars, var="car_name")
left_join(a,b,by=("car_name"))
right_join(a,b, by=("car_name"))
inner_join(a,b, by=("car_name"))
full_join(a,b, by=("car_name"))




# 실습
# 스타벅스 메뉴와 주문메뉴를 join을 이용해서 연결한다.
menu <- read.csv("C:/Users/hyerin/Downloads/Starbucks_menu.csv",header = T)
head(menu)

customer <- read.csv("C:/Users/hyerin/Downloads/customer.csv",header = T)
head(customer)

left_join(customer, menu, by=c("주문메뉴" = "스타벅스메뉴")) %>% slice(2:5)
right_join(customer, menu, by=c("주문메뉴" = "스타벅스메뉴"))
full_join(customer, menu, by=c("주문메뉴" = "스타벅스메뉴"))
semi_join( menu,customer, by=c("스타벅스메뉴" = "주문메뉴"))
inner_join(customer, menu, by=c("주문메뉴" = "스타벅스메뉴"))
head(menu)
anti_join(customer, menu, by=c("주문메뉴" = "스타벅스메뉴"))
# diff of semi & inner
# inner : 교집합. 모든 join 한 열들을 보여줌
# semi : 교집합되는 데이터들만 보여줌.(2개의 데이터셋 중 앞에 있는 데이터셋 기준)
# anti : 교집합이 안되는 데이터들만 보여줌.


#---------------------------------------------------------------
install.packages("foreign")
install.packages("ggplot2")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

setwd("C:/Users/hyerin/Downloads/유호선 8월 3-4주차 교재")
getwd()

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)
# 복사본 만들기
welfare <- raw_welfare
head(welfare)
tail(welfare)
View(welfare)

welfare <- rename(welfare,
                  sex = h10_g3,      
                  birth = h10_g4,    
                  marriage = h10_g10,
                  religion = h10_g11, 
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)

### 09-2 --------------------------------------------
class(welfare$sex)
table(welfare$sex)
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)
table(is.na(welfare$sex))
welfare$sex <- ifelse(welfare$sex ==1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)



class(welfare$income)
summary(welfare$income)
qplot(welfare$income) + xlim(0,1000)

summary(welfare$income)
#결측치 제거가 필요함.

#0이거나 9999이면 결측치로 분류한 후 제거
welfare$income <- ifelse(welfare$income %in% c(0,9999), NA, welfare$income)
table(is.na(welfare$income)) #NA값 12044개

sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income = mean(income))


ggplot(sex_income, aes(x = sex, y = mean_income)) + geom_col()

### 09-3 --------------------------------------------
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)
table(is.na(welfare$birth))
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)

welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))
  
### 09-4 --------------------------------------------
welfare <- welfare%>%
  mutate(ageg = ifelse(age < 30, "young",
                       ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)
#실습 : 10,20,30,40,50대,60대 이상으로 범주를 만드시오.


ageg_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income = mean(income))

ggplot(ageg_income, aes(x=ageg, y=mean_income)) + geom_col() +
  scale_x_discrete(limits = c("young","middle","old"))


### 09-5 --------------------------------------------
welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg, sex) %>%
  summarise(mean_income = mean(income))


sex_age <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age, sex) %>%
  summarise(mean_income = mean(income))

ggplot(sex_age, aes(x=age, y=mean_income, col=sex)) + geom_line()


### 09-6 --------------------------------------------
class(welfare$code_job)
table(welfare$code_job)

library(readxl)
list_job <- read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)
dim(list_job)

welfare <- left_join(welfare, list_job, by="code_job")


welfare %>%
  filter(!is.na(income)) %>%
  select(code_job, job) %>%
  head(10)


job_income <- welfare %>%
  filter(!is.na(income) & !is.na(job)) %>%
  group_by(job) %>%
  summarise(mean_income = mean(income))


head(job_income)

top10 <- job_income %>%
  arrange(desc(mean_income)) %>%
  head(10)

top10

ggplot(top10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip()


### 09-7 --------------------------------------------
# 남성 직업 빈도 상위 10개 추출
job_male <- welfare %>%
  filter(!is.na(job)& sex == "male") %>%
  group_by(job) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(10)

ggplot(data = job_male, aes(x = reorder(job, n), y=n)) +
  geom_col() +
  coord_flip()

#여성 직업 빈도 상위 10개 추출
job_female <- welfare %>%
  filter(!is.na(job) & sex == "female") %>%
  group_by(job) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(10)

ggplot(data = job_female, aes(x = reorder(job, n), y=n)) +
  geom_col() +
  coord_flip()


### 09-8 --------------------------------------------
class(welfare$religion)
table(welfare$religion)

#결측치 처리
welfare$religion <- ifelse(welfare$religion == 9, NA, welfare$religion)
table(is.na(welfare$religion))

#종교 유무 이름 부여
welfare$religion <- ifelse(welfare$religion == 1, "yes", "no")
table(welfare$religion)
qplot(welfare$religion)

#혼인 상태 변수 검토 
class(welfare$marriage)
table(welfare$marriage)

#이혼여부 변수 만들기
welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                                ifelse(welfare$marriage == 3, "divorce", NA))
head(welfare$group_marriage,30)
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))

# 이후 분석에서 marriage, divorce만 사용하고 나머지 NA는 제거한 후 분석
religion_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(religion, group_marriage) %>%
  summarise_at(n=n())

religion_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(religion, group_marriage) %>%
  summarise(n=n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100,1))

religion_marriage

religion_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  count(religion, group_marriage) %>%
  group_by(religion) %>%
  mutate(pct = round(n/sum(n)*100,1))

# 이혼 추출
divorce <- religion_marriage %>%
  filter(group_marriage == "divorce") %>%
  select(religion, pct)

#연령대별 이혼율 표
ageg_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(ageg, group_marriage) %>%
  summarise(n=n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100,1))

ageg_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  count(ageg, group_marriage) %>%
  group_by(ageg) %>%
  mutate(pct = round(n/sum(n)*100,1))


ageg_divorce <- ageg_marriage %>%
  filter(group_marriage == "divorce") %>%
  select(ageg, pct)

ggplot(ageg_divorce, aes(x=ageg, y=pct)) + geom_col()

ageg_religion_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(ageg, religion, group_marriage) %>%
  summarise(n=n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100,1))


ageg_religion_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  count(ageg, religion, group_marriage) %>%
  group_by(ageg, religion) %>%
  mutate(pct = round(n/sum(n)*100,1))

df_divorce <- ageg_religion_marriage %>%
  filter(group_marriage == "divorce") %>%
  select(ageg, religion, pct)


### 09-9 --------------------------------------------
class(welfare$code_region)
table(welfare$code_region)
list_region <- data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전, 충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도"))
list_region

#지역명 변수 추가
welfare <- left_join(welfare, list_region, by="code_region")
welfare %>%
  select(code_region, region) %>%
  head()

#지역별 연령대 비율표 만들기
region_ageg <- welfare %>%
  group_by(region, ageg) %>%
  summarise(n=n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100,2))

welfare %>%
  count(region, ageg) %>%
  group_by(region) %>%
  mutate(pct = round(n/sum(n)*100,2))

ggplot(region_ageg, aes(x=region, y=pct, fill=ageg)) +
  geom_col() +
  coord_flip()

#노년층 비율 내림차순 정렬
list_order_old <- region_ageg %>%
  filter(ageg=="old") %>%
  arrange(pct)

#지역명 순서 변수 만들기
order <- list_order_old$region
order

ggplot(region_ageg, aes(x=region, y=pct, fill=ageg)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order)

# 연령대 순으로 막대 색깔 나열하기
class(region_ageg$ageg)
levels(region_ageg$ageg)

region_ageg$ageg <- factor(region_ageg$ageg,
       level = c("old", "middle", "young"))
class(region_ageg$ageg)
levels(region_ageg$ageg)


welfare %>%
  group_by(region, religion) %>%
  summarise(n=n())



#### 지역별 종교인의 비율 ------------
region_religion <- welfare %>%
  count(region, religion) %>%
  group_by(region) %>%
  mutate(pct = round(n/sum(n)*100,1)) %>%
  filter(religion == "yes") %>%
  select(region,pct)


#### 지역별 여자 비율 -----------
welfare %>%
  count(region, sex) %>%
  group_by(region) %>%
  mutate(pct = round(n/sum(n)*100,1)) %>%
  filter(sex == "female") %>%
  select(region, pct)


#### 지역별 이혼 비율
welfare %>%
  filter(!is.na(group_marriage)) %>%
  count(region, group_marriage) %>%
  group_by(region) %>%
  mutate(pct = round(n/sum(n)*100,1)) %>%
  filter(group_marriage == "divorce") %>%
  select(region, pct)


#### 가구원수를 코딩하라.--------------------------
#가구원 : 1 --> single
#가구원 : 2 --> couple
#가구원 : 3 --> one kid
#가구원 : 4이상 --> large 를
# welfare의 새로운 열 household에 값으로 추가하여라.


welfare$household <- ifelse(welfare$h1001_1 == 1, "single",
       ifelse(welfare$h1001_1 == 2, "couple",
              ifelse(welfare$h1001_1 == 3, "one kid","large")))
head(welfare$household)


## 지역별 가구원수의 비율
table(welfare$household)

welfare %>%
  count(region, household) %>%
  group_by(region) %>%
  mutate(pct = round(n/sum(n)*100,1))


# single의 지역별 비율
welfare %>%
  count(region, household) %>%
  group_by(region) %>%
  mutate(pct = round(n/sum(n)*100,1)) %>%
  filter(household == "single") %>%
  arrange(pct) %>%
  select(region, pct)


# couple의 지역별 비율
welfare %>%
  count(region, household) %>%
  group_by(region) %>%
  mutate(pct = round(n/sum(n)*100,1)) %>%
  filter(household == "couple") %>%
  arrange(pct) %>%
  select(region, pct)


# one kid의 지역별 비율
welfare %>%
  count(region, household) %>%
  group_by(region) %>%
  mutate(pct = round(n/sum(n)*100,1)) %>%
  filter(household == "one kid") %>%
  arrange(pct) %>%
  select(region, pct)


# large의 지역별 비율
welfare %>%
  count(region, household) %>%
  group_by(region) %>%
  mutate(pct = round(n/sum(n)*100,1)) %>%
  filter(household == "large") %>%
  arrange(pct) %>%
  select(region, pct) 


#==================================================
## 교재 ===========================================
iris %>%
  mutate(class = case_when(
    Sepal.Length > 5 & Sepal.Width > 3 ~ "class1",
    Sepal.Length <= 5 & Sepal.Width <= 3 ~ "class2",
    Petal.Length > 1.5 ~ "class3",
    Petal.Width > 0.3 ~ "class4",
    TRUE ~ "classX"
  )) -> iris_class

head(welfare$ageg)


## 실습(case_when) : welfare의 age를 00대, 10~50대, 60대 이상으로 
# 나눠서 ageg2의 변수에 값을 할당한 뒤 해당 데이터를 wel이라는 데이터셋에 저장함.

welfare %>%
  mutate(ageg2 = case_when(
    age < 10 ~ "00대",
    10 <= age & age < 20 ~ "10대",
    20 <= age & age < 30 ~ "20대",
    30 <= age & age < 40 ~ "30대",
    40 <= age & age < 50 ~ "40대",
    50 <= age & age < 60 ~ "50대",
    TRUE ~ "60대 이상"
  )) -> wel

## 실습(case_when) : 이혼 여부 변수 만들기 
head(welfare$marriage)

welfare %>%
  mutate(group_marriage = case_when(
    marriage == 1 ~ "marriage",
    marriage == 3 ~ "divorce",
    TRUE ~ NA_character_
  )) -> wel2

head(wel2$group_marriage)


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
?table4a


### 2019 복지 패널 데이터
setwd("C:/Users/hyerin/Downloads/(2019년 14차 한국복지패널조사) 데이터(beta1)_spss")
getwd()
wel19 <- read.spss("koweps_h14_2019_beta1.sav", to.data.frame = T)
View(wel19)


head(wel19$h1406_2)
distinct(wel19, h1406_1)
table(wel19$h1207_9 - wel19$h1407_4 + wel19$h1407_5)
class(wel19$h1207_9)
welfare$re

wel19$expend <- wel19$h1407_9 - wel19$h1407_4 + wel19$h1407_5
summary(wel19$expend)










































































































