# A <- data.frame(x=c(1,2,3,4),y=c(5,6,7,8))
# B <- data.frame(x=c(1,2,3,7,10,22),z=c(9,10,11,12,32,12))
# merge(A,B,all=T)
# 
# head(A)
# 
# library(reshape2)
# melted <- melt(A)
# plot(melted$value, col=melted$variable)
# 
#  
source("common2.R")
library(ggplot2)
testdata_txt <- read.table("CCLE_copynumber_byGene_2013-12-03.txt",sep ="\t",header=T)
# print(colnames(testdata_txt))
# print(head(testdata_txt))
x_sel <- "A1BG"
y_sel <- "CDH2" 
x <- testdata_txt[testdata_txt[,2]==x_sel,-c(1:5)][1,]
y <- testdata_txt[testdata_txt[,2]==y_sel,-c(1:5)][1,]
print(x)
print(y)
plotdata = data.frame(x = x, y = y)
qplot(x,y, plotdata)