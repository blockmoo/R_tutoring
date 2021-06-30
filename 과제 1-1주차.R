#Q1
getwd()
setwd("C:\Users\hyerin\Documents")

#Q2
x = c(5,6,12,3,8,10,6)
y = c(4,9,15,6,4,2,3)
z = x + y
w = (x + y) /2


#Q3
x = c(11,2,23,41,15,33,7,4,3,11,50)
x[c(4,11)]

x[4:8]
x[c(4,5,6,7,8)]

x[-c(1,5,8,9)]


#Q4
y = 100:157
mean(y[20:25])

y = y[-c(2,10,15,17)]

png(filename="C:/Users/hyerin/Downloads/myplot.png",width=300,height=600)
plot(y)
dev.off()


#Q5
x = 20:37
x[2:8] = 7:1

x = x + 3

matrix(x, 3,6,byrow = T)

#Q6
x = c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7)
round(x^2,1)

#Q7
z = seq(2,80, length = 20)
round(z %% 3,1)

#Q8
rep(c(6,0,3), each = 2,3)
rep(c(1,3,5), c(2,3,6))
111:120
x = seq(10,50, by=10)
x[3] = NA

#Q9
x = 1:100
mean(x[x%%6 == 0])

head(iris)
iris[iris$Species == "setosa",]
which(iris$Species == "setosa")
subset(iris, iris$Species == 'setosa')




















