library(ggplot2)
library(vegan)
library(ggrepel)

# Import files
setwd("C:/Users/TeruY/Desktop/dada2")
DESIGN <- read.csv(file = "experimental_design_ITS.csv",header=T)
ASV.table <- read.table(file="rarefied_ASV_table.txt",header=T)
DESIGN <- na.omit(DESIGN) 

# % table
ASV <- ASV.table [,1:(ncol(ASV.table)-7)] 
taxonomy <- ASV.table [,(ncol(ASV.table)-6):ncol(ASV.table)]  
percent <- ASV / mean(colSums(ASV)) *100
# Remove "k__","p__", "c__"  before phylum name
taxonomy <- data.frame(lapply(taxonomy, function(x){gsub(pattern="k__", replacement = "", x)}),stringsAsFactors = FALSE)
taxonomy <- data.frame(lapply(taxonomy, function(x){gsub(pattern="p__", replacement = "", x)}),stringsAsFactors = FALSE)
taxonomy <- data.frame(lapply(taxonomy, function(x){gsub(pattern="c__", replacement = "", x)}),stringsAsFactors = FALSE)

# aggregate
percent.t <- t(percent)
DESIGN$Type <- factor(DESIGN$Type, levels=c("Wall","Grass","Dung","Manure","Soil"))
aggregated <- aggregate(percent.t, by=list(DESIGN$Type,DESIGN$Farm),FUN = sum,na.rm=F)
name <- NULL
for (k in 1:nrow(aggregated)){　 
name <- cbind(name, paste(aggregated[k,1], aggregated[k,2], sep="_"))}
rownames (aggregated) <- name
aggregated <- aggregated[,3:ncol(aggregated)] 
aggregated.t <- t(aggregated)

write.csv(aggregated.t, "aggregated.ASVs.csv")

# 
aggregated.t.filter <- aggregated.t[rowSums(aggregated.t)>1,] # Remove singlton
Result <- NULL
for (i in 1:ncol(aggregated.t.filter)){  
if (i%%5 == 0){
result <- NULL
Everywhere <- cbind(aggregated.t.filter[,i-4],aggregated.t.filter[,i-3], aggregated.t.filter[,i-2], aggregated.t.filter[,i-1],aggregated.t.filter[,i])
WallDung <- cbind(aggregated.t.filter[,i-4],aggregated.t.filter[,i-2])
GrassDung <- cbind(aggregated.t.filter[,i-3],aggregated.t.filter[,i-2])
DungManure <- cbind(aggregated.t.filter[,i-2],aggregated.t.filter[,i-1])
ManureSoil <- cbind(aggregated.t.filter[,i-1],aggregated.t.filter[,i])
DungSoil <- cbind(aggregated.t.filter[,i-2],aggregated.t.filter[,i])
SoilGrass <- cbind(aggregated.t.filter[,i],aggregated.t.filter[,i-3])

countif <- NULL
for (k in 1:nrow(Everywhere)){
countif <- rbind(countif,sum(Everywhere[k,] > 0, na.rm=TRUE))}
Everywhere.filter <- cbind(Everywhere,countif)
Everywhere.filter <- Everywhere.filter[Everywhere.filter[,6]>4,]
shared.Everywhere <- nrow(Everywhere.filter)
if (length(Everywhere.filter)==2){shared.Everywhere<-1}
if (length(shared.Everywhere)==0){shared.Everywhere<-1}

WallDung.filter <- WallDung[WallDung[,1]>0,]
WallDung.filter <- WallDung.filter[WallDung.filter[,2]>0,]
shared.WallDung <- nrow(WallDung.filter)
if (length(WallDung.filter)==2){shared.WallDung<-1}

GrassDung.filter <- GrassDung[GrassDung[,1]>0,]
GrassDung.filter <- GrassDung.filter[GrassDung.filter[,2]>0,]
shared.GrassDung <- nrow(GrassDung.filter)
if (length(GrassDung.filter)==2){shared.GrassDung<-1}

DungManure.filter <- DungManure[DungManure[,1]>0,]
DungManure.filter <- DungManure.filter[DungManure.filter[,2]>0,]
shared.DungManure <- nrow(DungManure.filter)
if (length(DungManure.filter)==2){shared.DungManure<-1}

ManureSoil.filter <- ManureSoil[ManureSoil[,1]>0,]
ManureSoil.filter <- ManureSoil.filter[ManureSoil.filter[,2]>0,]
shared.ManureSoil <- nrow(ManureSoil.filter)
if (length(ManureSoil.filter)==2){shared.ManureSoil<-1}

DungSoil.filter <- DungSoil[DungSoil[,1]>0,]
DungSoil.filter <- DungSoil.filter[DungSoil.filter[,2]>0,]
shared.DungSoil <- nrow(DungSoil.filter)
if (length(DungSoil.filter)==2){shared.DungSoil<-1}

SoilGrass.filter <- SoilGrass[SoilGrass[,1]>0,]
SoilGrass.filter <- SoilGrass.filter[SoilGrass.filter[,2]>0,]
shared.SoilGrass <- nrow(SoilGrass.filter)
if (length(SoilGrass.filter)==2){shared.SoilGrass<-1}

result <- rbind(shared.Everywhere, shared.WallDung, shared.GrassDung, shared.DungManure, shared.ManureSoil, shared.DungSoil, shared.SoilGrass)
Result <- cbind(Result, result)}}

colnames(Result) <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O")
write.csv(Result, "shared.ASVs.twohabitats.csv")

