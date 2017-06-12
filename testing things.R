A <- data.frame(x=c(1,2,3,4),y=c(5,6,7,8))
B <- data.frame(x=c(1,2,3,7,10,22),z=c(9,10,11,12,32,12))
merge(A,B,all=T)

head(A)

library(reshape2)
melted <- melt(A)
plot(melted$value, col=melted$variable)

 