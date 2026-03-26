## Comparison of AI among an artificial aggregation pheromone (Mix) and aggregation pheromone without 2-phenylundecane

# Copy and paste from "AP without 2PhC11" sheet in "Datasets - Effects of colony and diet on RsAP.xlsx"

d <- read.delim(pipe("pbpaste"), header=T)
head(d)

library(tidyverse)
library(ggplot2)
se <- function(x){sd(x)/sqrt(length(x));}

# Calculate the mean and standard error of "aggregation index (AI)"

g <- d %>%
	group_by(Treatment) %>%
	summarize(mean = mean(AggregationIndex), se = se(AggregationIndex))
g

p <- ggplot(g, aes(x=Treatment, y = mean)) + 
		geom_bar(stat="identity", position="dodge", width=0.6) + 
		scale_y_continuous(breaks=seq(-0.2, 0.5, by=0.1), limits=c(-0.2, 0.5)) + 
		theme_classic() + 
		theme(axis.text.x = element_text(angle=45, hjust=1)) + 
		geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.3), position = position_dodge(width = 0.55)) + 
		labs(x="", y="Aggregation index") + geom_hline(yintercept=0, linewidth=0.2) + 
		scale_x_discrete(limits=c("Negative", "Mix", "Mix − 2PhC11", "Crude"))
dev.new(width=2, height=4)
plot(p)


# Statistical analyses

# Multiple Fisher's exact test (Bonferroni correction)
library(fmsb)
a1 <- d %>%
	group_by(Treatment) %>%
	summarize(SampleT=sum(Sample), SolventT=sum(Solvent))
a1
pairwise.fisher.test(a1$SampleT, a1$SampleT + a1$SolventT, p.adjust.method="bonferroni")
