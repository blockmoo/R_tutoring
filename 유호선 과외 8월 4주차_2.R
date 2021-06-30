
library(dplyr)

# 1. iris 데이터를 species별로 sepal.length, petal.length의 평균을 구하시오. (dplyr 패키지 이용)

iris %>%
  group_by(Species) %>%
  summarise_at(vars(Sepal.Length, Petal.Length),mean)

# 2. mtcars 데이터를 gear 갯수별로 그룹화하고 wt, hp 열의 평균과 최댓값을 각각 구하시오.
mtcars %>%
  group_by(gear) %>%
  summarise_at(vars(wt, hp),  funs(mean=mean(., na.rm=T), max=max(., na.rm=T)))





iris %>%
  group_by(Species) %>%
  summarise(n=n())


# 같은 구문 
tally(mtcars)


mtcars %>%
  summarise(n = n())

#
mtcars %>%
  group_by(cyl) %>%
  tally()

mtcars %>%
  count(cyl, sort=T)  # 갯수를 내림차순으로 작성. 


# 3. mtcars data를 cyl, gear별로 그룹화하여 각각의 갯수를 구하는 표를 나타내시오.
mtcars %>%
  group_by(cyl, gear) %>%
  summarise(n = n())

iris %>%
  filter(Sepal.Length > 7) %>%
  head()

iris %>%
  distinct(Species)

# sampling : 전체에서 비율이 0.5가 되도록 뽑음.
iris %>%
  sample_frac(0.5, replace = T) %>%
  head()

# sampling : 갯수 지정
iris %>%
  sample_n(6, replace = T)

# select row
iris %>%
  slice(15:16)

# arrange는 default값이 오름차순임
iris %>%
  arrange(Sepal.Length) %>% head

iris %>%
  arrange(desc(Sepal.Length)) %>% head


# 4. mtcars를 gear별로 그룹화하고 hp, wt의 mean값들을 구해서 hp_mean의 값을 기준으로 오름차순 정렬하시오.
mtcars %>%
  group_by(gear) %>%
  summarise_at(vars(hp,wt), funs(mean = mean)) %>%
  arrange(hp_mean) 

# 5. iris 데이터를 Petal.Length를 기준으로 오름차순으로 정렬한 뒤 상위 5개만 출력하시오.
iris %>%
  arrange(Petal.Length) %>%
  top_n(5, Petal.Length)

# 조건에 해당하는 열 선택 
iris %>%
  select(Sepal.Length, Species) %>% head

# starts_with : 조건의 단어로 시작하는 문자 찾는 것.
iris %>%
  select(starts_with("Sepal")) %>% head

iris %>%
  select(ends_with("Width")) %>% head

iris %>%
  select(contains("Length")) %>% head


# 6. iris 데이터에서 "Length"를 포함하는 열의 데이터에서 set.seed(123)을 두고 
# 0.3의 비율로 반복 sampling 한 결과에서 각 열의 mean, max 값을 반올리마여 소수둘째자리까지 나타내시오.

set.seed(123)
iris %>%
  select(contains("Length")) %>%
  sample_frac(0.3, replace = T) %>%
  summarise_all(funs(mean, max)) %>%
  round(2)


# contains() : Contains a literal string.
# matches () : Matches a regular expression.

iris %>%
  select(Petal.Length:Species) %>% head

iris %>%
  select(contains("pal")) %>% head

df1 <- data.frame(colnm = 1:5, col1 = 24, col2 = 46)
df1 %>% 
  select(matches("col\\d+"))

exam <- read.table("C:\\Users\\hyerin\\Downloads\\csv_exam.txt", sep=",", header = T)
head(exam)
dim(exam)

# %in% : 여러 조건 중에 하나에 해당하는 것을 고를 때
exam %>%
  filter(class %in% c(1,3,5))


# 7. exam 파일에서 class = 1또는 2인 학생들의 평균 수학점수를 구하시오.
exam %>%
  filter(class %in% c(1,2)) %>%
  summarise_at(vars(math),funs(math_mean=mean))

# one_of는 변수 그룹을 선별할 때 사용하는 옵션이지만 변수 그룹에 
# 있을 수도 있지만 없을 수도 있다는 뜻임.
iris %>%
  select(one_of(c("Petal.Length", "Petal.Width"))) %>% head

mtcars %>%
  mutate(gpm = 1/mpg)

mtcars %>%
  transmute(gpm = 1/mpg)

head(faithful)

faithful %>%
  mutate_all(funs(log(.), log2(.))) %>% head


faithful %>%
  mutate_all(funs(A=log(.), B=log2(.))) %>% head

faithful %>%
  mutate_if(is.numeric, funs(log = log)) %>% head()

head(faithful)



















