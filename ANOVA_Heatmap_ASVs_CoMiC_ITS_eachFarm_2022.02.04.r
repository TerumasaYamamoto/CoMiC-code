library("tidyverse")
library("reshape2")

setwd("C:/Users/TeruY/Desktop/dada2")
data <- read.csv("anova_lm_for_allASVs_eachfarm.csv",header=T, row.names=1)

Fval <- cbind(data[,1],data[,3],data[,5],data[,7],data[,9],data[,11],data[,13],data[,15],data[,17],data[,19],data[,21],data[,23],data[,25],data[,27],data[,29])
Pval <- cbind(data[,2],data[,4],data[,6],data[,8],data[,10],data[,12],data[,14],data[,16],data[,18],data[,20],data[,22],data[,24],data[,26],data[,28],data[,30])
Fval <- Fval[1:20,]
Pval <- Pval[1:20,]
colnames(Fval) <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O")
colnames(Pval) <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O")
rownames(Fval) <- rownames(data) [1:20]
rownames(Pval) <- rownames(data) [1:20]

melt.F <- melt(Fval)
melt.P <- melt(Pval) 

melt <- cbind(melt.F,melt.P[,3])
colnames(melt) <- c("ASVs", "Farm","Fval","Pval")

melt$ASVs <- factor(melt$ASVs, levels = c("ASV7","ASV4","ASV12","ASV22","ASV11","ASV13","ASV16","ASV10","ASV41","ASV34","ASV23","ASV15","ASV8","ASV49","ASV173","ASV45","ASV28","ASV21","ASV14","ASV25"))
melt$Farm <- factor(melt$Farm, levels=c("O","N","M","L","K","J","I","H","G","F","E","D","C","B","A"))

ggplot (melt, aes(x=ASVs, y=Farm, fill =Fval))+
geom_tile()+
scale_fill_gradient2(low = "white", mid = "#f8766d", high = "#753c37", midpoint=50)+
geom_text(aes(label = Pval))+
theme(axis.text.x = element_text(angle = 10, hjust=0.5,vjust=1))

ggsave(file = "ASVs.anova.Heatmap.png", height=5,width=10)
