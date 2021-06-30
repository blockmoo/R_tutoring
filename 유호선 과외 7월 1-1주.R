getwd()
setwd("C:/Users")

x = 1
y = 2
z = x + y
z

source(a.R)

x = 'SEO'
mode(x)
x = 0.5
mode(x)
x = 2+3i
mode(x)
x = F
mode(x)

x = c(1,2,3)
y = c(4,5,6)
z = rbind(x,y)
attributes(z)
z

# Vector------------------------------------------
x = c(10,18,12,15,9,7)
x
length(x)

x = c(10,18,12,15,'9',7)
x

y = c('seo', '92 95','A')
is.vector(y)


# Matrix--------------------------------------------
z = matrix(x, nrow=2, ncol = 3)
z
is.matrix(z)


# Array---------------------------------------------
a = 1:12
z = array(a, dim=c(2,3,2))
is.array(z)


# List-----------------------------------------------
b = list(x=x, y=y, z=z)
is.list(b)
b$x

c = list(x,y,z)
c
c[[1]]


# Data frame ----------------------------------------
x = 1:3
y = c('a','b','c')
z = data.frame(x,y)
z
is.data.frame(z)
rownames(z)
colnames(z)


# Class ----------------------------------------------
x = rnorm(100)
head(x)
tail(x)

y = hist(x)
y

plot(y)
plot(x)

png(filename="C:/Users/hyerin/Downloads/myplot.png",width=300,height=600)
plot(y)
dev.off()



##########################################
## VECTOR ################################---------------
##########################################
x = 7
x
is.vector(x)
x = c('a','b','c')
x
x = c(T,T,F,F)
x

# 빈 벡터 만들기 
x = vector('character',5)
x
length(x)


x = c(2,-3,5,4,7,2,-1,2,9,0)
x[c(2,4,6,8)]
x[2:5]
x[c(1:3,5)]

idx = c(2,4,6,8)
x[idx]
2:5
a = 2:5
y = x[2:5];y


x[-1]
x[-(2:5)]
x[c(-2,-4)]

x = 1:5
y = c(x[1:3], 3.5, x[4:5])
y

y = y[-c(1,3,4)]
y


x = matrix(2,2,2)
length(x)
x + 1:4
x + 11:14
x = 10:1
x[5] = 0
x
x[5:10] = 0
x
x[5:10] = c(2,3,4,5,6,7)
x

x = c(10,15,40); y = c(5,2,3)
x + y
x - y
x * y
x / y
x %% y
x %/% y
x + 2
x - 2
x * 2
x / 2



x = c(9,2.2, 3.7)
x^2

round(x^2, 2)
ceiling(x)
floor(x)
seq(1,10,by=2)
seq(1,10,length=5)
seq(10,1, by=-2)
x = c(10,5,7); seq(x)
x
seq(x)
seq(c(10,5,7,6))
x = NULL; seq(x)


rep(3,5)
rep(c(2,4,6),3)
rep(c(5,2,3), each=3)
rep(c(5,2,3), each=3,2) # each가 먼저 실행되고 이후에 전체 반복횟수가 나온다.
rep(c(5,2,3), 2, each=3)
rep(c(1,2,3), c(3,2,1))


x = c(10,20,NA, 30,40)
length(x)
mean(x)
mean(x, na.rm=T)
mode(x)
x = c(T, F, NA, T, F, F); mode(x)
x = c('a', NA, 'c'); mode(x)
x = c(10,20,NULL, 30,40)
mean(x)
length(x)
is.null(x)
z = NULL
is.null(z)
for (i in 1:10) x = c(x,i)
x = NULL
for(i in 1:10){
  x = c(x,i)
  print(x)
}
x = 1:7; x == 4
x != 4
x > 4
x = c('seo', 'lee', 'park', ' choi')
x == 'park'
x != 'park'
x < 'seo'
x < 'lee'
x = c(1,5,10); y = c(3,5,7)
x == y
x >= y
x = 1:7
all(x > 4)
all(x > 0)

x = 1:7
any(x > 4)
any(x > 10)
x = 1:3
y = c(1,2,3)
all(x == y)
identical(x,y)
class(x)
class(y)

typeof(x)
typeof(y)

### Filter ---------------------------------
x = c(-2,7,5,0,-10)
y = x[x > 3]
y
x[c(F,F,T,T,T)]
which(x<3)
x[x > 3] = 10
x

#subset-----------------------------
x = c(-2,7,NA,5,0,NA,-10)
x[x > 3]
subset(x, x>3)
head(iris)
 
#which-----------------------------
x = c(-2,7,5,0,-10)
which(x > 3)

x = c('seo','lee','park','choi')
which(x == 'park')


#ifelse-----------------------------
x = 1:10
y = ifelse(x %% 3 == 0, 0, 1)
y
x = c('A', 'A', 'B', 'C', 'C','A')
ifelse(x == 'A', 1, 0)


#sort & order-------------------------------
x = c(3,1,7,5)
sort(x)
sort(x, decreasing = T)
order(x)
x[order(x)]

x = c('kim','park','lee','choi')
sort(x)
order(x)


x = 1:3
names(x) = c('a','b','c')
x
x[c('a',"c")]
names(x) = NULL
x


####################
###Matrix & Array  1-2 ###-----------------------------------------
####################
x = matrix(nrow=2, ncol=3)
x = 1:12
matrix(x,3,4)
matrix(x,3,4,byrow=T)
A = matrix(1:6, 2,3)
length(A)
dim(A)
nrow(A)
ncol(A)

A = matrix(1:4, 2,2); B = matrix(2:5, 2,2)
A * B
A %*% B
solve(A)
t(A)
eA = eigen(A)
eA$val
eA$vec

A = matrix(1:6, 2,3)
A[1,3]
A[2,]
A[,2]
A[1:2,2]
A[2,1:2]
A[,c(1,3)]
A[,-1]
A = matrix(,2,3)
A
A[1,] = rep(0,3);A
A[1,] = c(2,3)
A[,c(1,3)] = 2
A

#Extract---------------------------------
A = matrix(1:20, 4,5);A
A[,A[2,]>7]
A[2,] >7
A[A[,3] >= 10 & A[,3] <= 11,1:2]
A[,3] >=10 & A[,3] <=11
x = c(5,4,7,12)
A[x %% 2 == 1,]
l = which(x %% 2 ==1)
A[l,]


#apply------------------------------------
A = matrix(c(1:3, 9,7,8,7,1:8),3,5)
A
apply(A,2,sum)
apply(A,1,mean, trim=0.2)
apply(A,1,mean,trim=0.2, na.rm=T)


#rbind--------------------------------------
x = c(1,5,2); y = matrix(1:6, 2,3)
rbind(x,y)
x = matrix(1:6, 3,2)
y = matrix(9:1, 3,3)
cbind(x,y)


A = matrix(1:6, 2,3)
x = A[1,] ; x
is.vector(x)


x = as.matrix(x);x
x = as.vector(x);x
A = array(0, dim=c(2,2,2))
as.vector(A)
as.matrix(A)
A = matrix(1:6, 2,3); A
rownames(A) = c('K','J')
colnames(A) = 3:1
A


####################
### List ###-----------------------------------------
####################
x = list(name = 'Kim', salary=6000, union=T);x
x$name
x = list('Kim', 6000, T); x
x[[1]]
a = 1:3; b = c('a','b','c')
x = list(a,b);x
x = list(N=a, K=b)
x
x = vector(mode='list',2)
x
















