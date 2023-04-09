library(igraph)  
library(Hmisc)  
library(Matrix)  

# Import files
setwd("C:/Users/TeruY/Desktop/dada2")
DESIGN <- read.csv("experimental_design_ITS.csv",header=T)
DESIGN <- na.omit(DESIGN)

ASV.table <- read.table(file="rarefied_ASV_table.txt",header=T)
ASV <- ASV.table [,1:(ncol(ASV.table)-7)]
taxonomy <- ASV.table [,(ncol(ASV.table)-6):ncol(ASV.table)]
taxonomy.full <- taxonomy
percent <- ASV / mean(colSums(ASV)) *100
percent.t <- t(percent)

# To make minor phylum "Others"
taxonomy.minusp <- data.frame(lapply(taxonomy, function(x){gsub(pattern="f__", replacement = "", x)}),stringsAsFactors = FALSE)
rownames(taxonomy.minusp) <- rownames(taxonomy)
taxonomy <- taxonomy.minusp 
family <- aggregate(percent, by=list(taxonomy$Family),FUN = sum,na.rm=F) 
row.names(family)<-family[,1]
family <- family[,-1]
family <- data.frame(family)
rowMeans <- rowMeans(family) 
family <- cbind(family,rowMeans)
minor.family <- family[family[,"rowMeans"] < 2,] # Change
minor.family.list <- rownames(minor.family)

for (i in 1:length (minor.family.list)){
taxonomy$Family <- gsub(minor.family.list[i],"Others",taxonomy$Family)}

# Subset
NAME <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O")
bw.all <- NULL
bw.phylum <- NULL
bw.family <- NULL

for (k in 1:15){
bind <- cbind (percent.t,DESIGN)
subset <- subset(bind, bind$Farm==NAME[k]) # Change

subset <- subset[,1:(ncol(subset)-ncol(DESIGN))]

# Filter to pick up parts of ASVs
subset.t.filter <- subset[ ,colMeans(percent.t) >= 0.01] # To pick up >0.01% ASVs 
subset.t.filter <- subset.t.filter[,colSums(subset.t.filter)>0]
print(c(ncol(subset),"versus",ncol(subset.t.filter)))

# Calculate network
percent.cor <- rcorr(as.matrix(subset.t.filter), type="spearman")
percent.pval <- forceSymmetric(percent.cor$P) # Self-correlation as NA
#Select only the taxa for the filtered ASVs by using rownames of percent.pval
sel.tax <- taxonomy[rownames(percent.pval),,drop=FALSE]
#Sanity check --> should be "[1] TRUE"
all.equal(rownames(sel.tax), rownames(percent.pval))

p.yes <- percent.cor$P<0.05　
r.yes <- percent.cor$r>0　
r.high <- percent.cor$r>0.6　
r.val <- percent.cor$r # select all the correlation values 
p.r.yes = p.yes*r.yes*r.val*r.high
adjm<-p.r.yes 

net.grph=graph.adjacency(adjm,mode="undirected",weighted=TRUE,diag=FALSE)

# hs <- hub_score(net.grph, weights=NA)$vector　
# as <- authority_score(net.grph, weights=NA)$vector
# pr <- page.rank(net.grph,directed=F)$vector　
deg <- degree(net.grph, mode="all")　
deg.max <- max(deg)
pat <- average.path.length(net.grph, unconnected = F)
# tra <- transitivity(net.grph, type = "global")
tra <- transitivity(net.grph, type = "average")
cohesion <- vertex_connectivity(net.grph)
gsize <- gsize(net.grph)
edge_density <- round(edge_density(net.grph),digit=5)
betweenness.max <- max(betweenness(net.grph, directed=T, weights=NA))

# Align taxonomy names
sel.tax$Family <- factor(sel.tax$Family)
others.n <- 1
others.n <- which(levels(sel.tax$Family)=="Others")
levels <- NULL
if (length(others.n) == 0){
     for (i in 1:(length(levels(sel.tax$Family)))){
    levels <- c(levels, levels(sel.tax$Family)[i])}}
else if (others.n == 1){
    for (i in 1:(length(levels(sel.tax$Family)))){
    levels <- c(levels, levels(sel.tax$Family)[i])}
    levels <- c(levels, "Others")} else{
for (i in 1:(others.n-1)){
    levels <- c(levels, levels(sel.tax$Family)[i])}
for (i in (others.n+1):(length(levels(sel.tax$Family)))){
    levels <- c(levels, levels(sel.tax$Family)[i])}
levels <- c(levels, "Others")
levels(sel.tax$Family) <- levels}

# Illustrate
col=rainbow(length(levels(sel.tax$Family)))
plot.igraph(net.grph, vertex.size=deg*0.15,vertex.label=NA, vertex.color=col[unclass(sel.tax$Family)],layout=layout.kamada.kawai)
# plot(net.grph, vertex.size=deg*0.15,vertex.label=NA,vertex.color=col[unclass(sel.tax$Phylum)],layout=layout.random)
# plot(net.grph, vertex.size=deg*0.15,vertex.label=NA,vertex.color=col[unclass(sel.tax$Phylum)],layout=layout.fruchterman.reingold)
if (k==1){
legend(x = -2.5, y = 1.25, legend = levels(sel.tax$Family), cex=3, pch = 19, col = col, bty = "n",  y.intersp = 0.5, x.intersp = 0.75)}
# text(x=0,y=-0.4,paste("Edge: ", format(gsize,digit=3)),cex=3)
# text(x=0,y=-0.5,paste("Density: ", format(edge_density,digit=3)),cex=3)
title(NAME[k],cex.main=5) #Change

# Save 
if (k==1){
dev.copy(png, file=paste("Network/Network/Network.ITS.",NAME[k],".png",sep=""), height=800, width=1400)
dev.off()} else{
dev.copy(png, file=paste("Network/Network/Network.ITS.",NAME[k],".png",sep=""), height=800, width=800)
dev.off()}}
