### Comparison of chemical profile among workers from nest wood, BPC medium, and paper


## Copy and paste data from the "GCMS worker whole body" sheet in the file ("Datasets - Effects of colony and diet on RsAP.xlsx")

data <- read.delim(pipe("pbpaste"), header=T)
head(data)


### PCA plot ###

# Convert the amount data into proportional data (each compound amount / Total amount) for each sample (row)
d <- data.frame(data[6:123]/rowSums(data[, 6:123]))
d1 <- cbind(data[, 1:5], d)
d1$Material <- factor(d1$Material, levels=c("Brown-rotted wood", "BPC", "Paper"))

# PCA
rpca <- prcomp(d1[, 6:123], scale=T)
summary(rpca)
rpca$rotation

library(ggfortify)
dev.new(width=7, height=5)
autoplot(prcomp(d1[, 6:123], scale=T), data=d1, shape="Colony", colour="Material", size=4, loadings=F, loadings.label=F) + scale_shape_manual(values=c(0,1,2,3,6,15,16,17,18,4,5,7,8,9,10,11,12,13,14,19,20)) + theme_bw() 

# *After the above PCA plot was created, only the inverted triangle plots (Default shapes in R, No. 6) were filled in later using Adobe illustrator.


# Pairwise PERMANOVA

## Install from Github (https://github.com/pmartinezarbizu/pairwiseAdonis) if you don't have it ####
library(devtools)
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
####################################################################################################

library(pairwiseAdonis)

# Analysis using all workers (Benjamini-Hochberg correction, Method: Bray, Permutation: 999 times)
pairwise.adonis(d1[, 6:123], d1[, 4], p.adjust.m="BH")

# Analysis using workers immediately removed from nest wood - Cedar (Sugi) vs Pine (Akamatsu) (Benjamini-Hochberg correction, Method: Bray, Permutation: 999 times)
d1b <- d1[d1$Material=="Brown-rotted wood", ]
d1b2 <- d1b[, colSums(d1b != 0) > 0]
pairwise.adonis(d1b2[, 6:73], d1b2[, 5], p.adjust.m="BH")




### Heatmap ###

## Install "ComplexHeatmap" package if you don't have #######
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")
#############################################################

# Read the compound list
c.list <- read.csv("Compound list - Comparing chemical profile among workers from nest wood vs BPC vs paper.csv", header=T)
compounds <- c.list[, 1]


## Heatmap for all compounds (column: each colony for each material, row: compound)
d2 <- t(scale(data[, 6:123], center=F, scale=T))
dimnames(d2) <- list(compounds, paste(data[,1],data[,4]))

library(ComplexHeatmap)

# Define color gradation (Black: 0, brown~orange~yellow: > 0)
my.col <- colorRampPalette(c("brown4", "orange", "yellow"))(6999)
my.col2 <- c("#000000", my.col)

groups <- data.frame(Food=data[, 5])
rownames(groups) <- colnames(d2)
groups$Food <- factor(groups$Food, levels=c("Sugi", "Akamatsu", "BPC", "Paper"))
colony.names <- data[, 1]

dev.new(width=10, height=14)
ComplexHeatmap::pheatmap(d2, scale="none", color=my.col2, cluster_rows=T, cluster_cols=F, fontsize=8, 
				angle_col=c("90"), border_color="NA", legend=T, annotation_col=groups, name="Scaled value",
				annotation_colors=list(Food=c(Sugi="gold", Akamatsu="brown",BPC="green3", Paper="dodgerblue")),
				labels_col=colony.names)



## Heatmap for candidate aggregation pheromone components (column: each colony for each material, row: compound)
library(dplyr)
da <- select(data, X2.Phenylundecane, Palmitic.acid, Vaccenic.acid, n.Pentacosane, n.Heptacosane, Cholesterol)
d3 <- t(scale(da, center=F, scale=T))
dimnames(d3) <- list(c("2-Phenylundecane", "Palmitic acid", "Vaccenic acid", "n-Pentacosane", "n-Heptacosane", "Cholesterol"), paste(data[, 1], data[, 4]))

library(ComplexHeatmap)

# Define color gradation (Black: 0, brown~orange~yellow: > 0)
my.col <- colorRampPalette(c("brown4", "orange", "yellow"))(6999)
my.col2 <- c("#000000", my.col)

groups <- data.frame(Food=data[, 5])
rownames(groups) <- colnames(d3)
groups$Food <- factor(groups$Food, levels=c("Sugi", "Akamatsu", "BPC", "Paper"))
colony.names <- data[, 1]

dev.new(width=10, height=3)
ComplexHeatmap::pheatmap(d3, scale="none", color=my.col2, cluster_rows=T, cluster_cols=F, fontsize=8, 
				angle_col=c("90"), border_color="NA", legend=T, annotation_col=groups, name="Scaled value",
				annotation_colors=list(Food=c(Sugi="gold", Akamatsu="brown",BPC="green3", Paper="dodgerblue")),
				labels_col=colony.names)



## Calculate the mean and standard error of the content of each compound
library(tidyverse)
se <- function(x){sd(x)/sqrt(length(x));}

# Read the compound list
c.list <- read.csv("Compound list - Comparing chemical profile among workers from nest wood vs BPC vs paper.csv", header=T)
Compound <- rep(c.list[,1], 21*3)
No <- rep(1:length(c.list[,1]), 21*3)
# Rearrangement
d <- data[, -c(1,2,3,5)] %>% 
		pivot_longer(cols=-c("Material"), names_to="Compound", values_to="Amount")
d <- cbind(No, Compound, d[, c(1,3)]) 
# Calculation
d1 <- d %>%
		group_by(No, Material, Compound) %>% 
		summarise(Mean=round(mean(Amount),3), SEM=round(se(Amount), 3)) %>%
		unite("Amount", Mean, SEM, sep=" ± ") %>%
		pivot_wider(names_from=Material, values_from=Amount) %>%
		select(Compound, 'Brown-rotted wood', BPC, Paper) %>%
		arrange(No)
# Export the list
write.csv(d1, "Compound list with the mean content in each material.csv", row.names=F)


## Statistical analysis (ANOVA followed by Tukey's HSD test)
library(tidyverse)
library(multcomp)
se <- function(x){sd(x)/sqrt(length(x));}

# Read the compound list
c.list <- read.csv("Compound list - Comparing chemical profile among workers from nest wood vs BPC vs paper.csv", header=T)
Compound <- rep(c.list[,1], 21*3)
No <- rep(1:length(c.list[,1]), 21*3)
# Rearrangement
d <- data[, -c(1,2,3,5)] %>% 
		pivot_longer(cols=-c("Material"), names_to="Compound", values_to="Amount")
d <- cbind(No, Compound, d[, c(1,3)]) 
# Analysis
for(i in 1:length(c.list[,1])){
	d2 <- d[d$No==i, ]
	d2$Material <- factor(d2$Material)
	cat("[", d2[1, 1], "] ", d2[1, 2],"\n\n-- ANOVA --", sep="")
	r <- lm(Amount ~ Material, data=d2)
	res <- aov(r)
	print(summary(res))
	cat("\n\n-- Tukey's HSD test --\n")
	res2 <- glht(res, linfct=mcp(Material="Tukey"))
	print(summary(res2))
	print(cld(res2, level=0.05, decreasing=T))
	cat("\n\n")
}
