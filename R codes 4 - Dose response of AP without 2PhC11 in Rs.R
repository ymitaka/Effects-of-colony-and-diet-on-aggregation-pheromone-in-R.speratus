## Comparison of AI among different doses of aggregation pheromone without 2-phenylundecane (Mix 2)

# Copy and paste from "Dose response" sheet in "Datasets - Effects of colony and diet on RsAP.xlsx"

d <- read.delim(pipe("pbpaste"), header=T)
head(d)

library(tidyverse)
library(ggplot2)
se <- function(x){sd(x)/sqrt(length(x));}

# Calculate the mean and standard error of "aggregation index (AI)"

g <- d %>%
	group_by(Treatment, Time) %>%
	summarize(mean = mean(AggregationIndex), se = se(AggregationIndex))
print(g, n=28)

p <- ggplot(g, aes(x=Treatment, y = mean)) + 
		geom_bar(stat="identity", position="dodge", width=0.6) + 
		scale_y_continuous(breaks=seq(-0.2, 0.3, by=0.1), limits=c(-0.2, 0.3)) + 
		theme_classic() + 
		theme(axis.text.x = element_text(angle=45, hjust=1)) + 
		geom_errorbar(aes(ymin = mean - se, ymax = mean + se, width = 0.3), position = position_dodge(width = 0.55)) + 
		labs(x="", y="Aggregation index") + geom_hline(yintercept=0, linewidth=0.2) + 
		scale_x_discrete(limits=c("Negative", "Mix 2 × 0.0001", "Mix 2 × 0.001", "Mix 2 × 0.01", "Mix 2 × 0.1", "Mix 2", "Crude")) +
		facet_wrap(~Time, ncol=4)
dev.new(width=10, height=4)
plot(p)


# Statistical analyses

# Multiple Fisher's exact test (Bonferroni correction)
library(fmsb)
a1 <- d %>%
	group_by(Treatment, Time) %>%
	summarize(SampleT=sum(Sample), SolventT=sum(Solvent)) %>%
	mutate(Total = SampleT + SolventT)

# 5 min later
a2 <- filter(a1, Time == 5)
a2
pairwise.fisher.test(a2$SampleT, a2$Total, p.adjust.method="bonferroni")

# 60 min later
a3 <- filter(a1, Time == 60)
a3
pairwise.fisher.test(a3$SampleT, a3$Total, p.adjust.method="bonferroni")

# 120 min later
a4 <- filter(a1, Time == 120)
a4
pairwise.fisher.test(a4$SampleT, a4$Total, p.adjust.method="bonferroni")

# 240 min later
a5 <- filter(a1, Time == 240)
a5
pairwise.fisher.test(a5$SampleT, a5$Total, p.adjust.method="bonferroni")
