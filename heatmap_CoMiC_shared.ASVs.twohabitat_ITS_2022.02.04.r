library(ggplot2)
library(reshape2)

# Import files
setwd("C:/Users/TeruY/Desktop/dada2")
DESIGN <- read.csv("experimental_design_ITS.csv",header=T)
data <- read.csv(file="shared.ASVs.twohabitats.csv",header=T)
DESIGN <- na.omit(DESIGN)
data[,1] <- c("Everywhere", "Wall&Dung","Grass&Dung","Dung&Manure","Manure&Soil", "Dung&Soil", "Soil&Grass")
melt <- melt(data)
colnames(melt)[1:2] <- c("Shared.ASVs","Farm") 

melt$Shared.ASVs <- factor(melt$Shared.ASVs,levels = c("Everywhere", "Wall&Dung","Grass&Dung","Dung&Manure","Manure&Soil","Dung&Soil", "Soil&Grass"))
melt$Farm <- factor(melt$Farm, levels=c("O","N","M","L","K","J","I","H","G","F","E","D","C","B","A"))

ggplot (melt, aes(x=Shared.ASVs, y=Farm, fill =value))+
geom_tile()+
scale_fill_gradient2(low = "white", mid = "#f8766d", high = "#753c37", midpoint=30)+
geom_text(aes(label = round(value, 1)))+
theme(axis.text.x = element_text(angle = 10, hjust=0.5,vjust=1))

ggsave(file = "Heatmap.png", height=5)
