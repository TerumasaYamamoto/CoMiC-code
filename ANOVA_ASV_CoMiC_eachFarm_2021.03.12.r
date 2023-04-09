library(lme4)
library(lmerTest)

setwd("~/R/Analysis/6_CoMiC/16S.2021.03.12")
ASV.table <- read.table(file="rarefied_ASV_table.txt",header=T)
DESIGN <- read.csv("experimental_design.csv",header=T)
EachFarm <- read.csv("each_farm.csv",header=T)
DESIGN <- na.omit(cbind(EachFarm,DESIGN))

ASV <- ASV.table [,1:(ncol(ASV.table)-6)] # I've changed 7--> 6
taxonomy <- ASV.table [,(ncol(ASV.table)-5):ncol(ASV.table)]  # I've changed 6--> 5
percent <- ASV / mean(colSums(ASV)) *100
# Remove "k__","p__", "c__"  before phylum name
taxonomy <- data.frame(lapply(taxonomy, function(x){gsub(pattern="k__", replacement = "", x)}),stringsAsFactors = FALSE)
taxonomy <- data.frame(lapply(taxonomy, function(x){gsub(pattern="p__", replacement = "", x)}),stringsAsFactors = FALSE)
taxonomy <- data.frame(lapply(taxonomy, function(x){gsub(pattern="c__", replacement = "", x)}),stringsAsFactors = FALSE)

percent$rowMeans <- rowMeans(percent)
percent <- percent[order(percent$rowMeans, decreasing=T),]
TABLE <- percent
TABLE <- t(TABLE)
TABLE <- TABLE[-nrow(TABLE),]
write.csv(TABLE, "top50ASVs.csv")

Results.all <- NULL
for (k in 1:15){
Results <- NULL
for (i in 1:ncol(TABLE)){
anova <- anova(lm(TABLE[,i]~DESIGN[,k]+DESIGN$Type))
Fval <- anova[1,4]
Pval <- anova[1,5]

Bind <- c(Fval, Pval)
Results <- rbind(Results, Bind)}
NAME <- colnames(DESIGN)[k]
colnames(Results) <- c(paste(NAME,".F",sep=""),paste(NAME,".P",sep=""))
Results.all <- cbind(Results.all,Results)}

rownames(Results.all) <- colnames(TABLE)

Results.asterisk <- Results.all
for (k in 1:ncol(Results.all)){
if(k%%2 == 0){
for (i in 1:nrow(Results.all)){
if (Results.all[i,k] < 0.001) {Results.asterisk[i,k] <- "***"
} else if (Results.all[i,k] < 0.01) {Results.asterisk[i,k] <- "**"
} else if (Results.all[i,k] < 0.05) {Results.asterisk[i,k] <- "*"
} else {Results.asterisk[i,k]<- "n.s"}}
}}

write.csv(Results.asterisk, "anova_lm_for_allASVs_eachfarm.csv") 