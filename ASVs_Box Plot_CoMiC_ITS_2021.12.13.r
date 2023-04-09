library(vegan)
library(tidyverse)
library(reshape2)

setwd("C:/Users/TeruY/Desktop/dada2")
DESIGN <- read.csv("experimental_design_ITS.csv",header=T)
ASV.table <- read.table(file="rarefied_ASV_table.txt",header=T)
DESIGN <- na.omit(DESIGN)

ASV <- ASV.table [,1:(ncol(ASV.table)-7)] # I have changed 6 into 7 because the table contained the Species column.
percent <- ASV / mean(colSums(ASV)) *100
percent.t <- t(percent)
data <- cbind(percent.t, DESIGN)
data$Type <- factor (data$Type, levels=c("Wall","Grass","Dung","Manure","Soil"))
data$Area <- factor (data$Area, levels=c("Center","North","East"))

for (i in 1:500){
ggplot(data,aes(y = data[,i], x = Farm, fill = Type)) +
geom_boxplot(aes(y = data[,i], x = Farm, fill = Type))+   #Change
# scale_fill_manual(values = c("#C77CFF","#7CAE00","#00BFC4","#F8766D"))+  # if you want to change the colors
theme_classic()+
theme(text=element_text(size=14,color="black"),axis.text=element_text(size=12,color="black"))+
labs (y="Relative abundance(%)",x="")+ # if you want to change the axis titles
facet_wrap(~Type)+
ggtitle(colnames(data)[i])

ggsave(file = paste("ASVs/",colnames(data)[i],"_facetType.png",sep=""), width = 12, height = 5)}
