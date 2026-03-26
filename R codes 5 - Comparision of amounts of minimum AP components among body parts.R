## Comparison of the amounts of minimum aggregation pheromone components among body parts in R. speratus
## (Unit: ng/worker eq.)


# Copy & paste data from the "GCMS worker parts" sheet in "Datasets - Effects of colony and diet on RsAP.xlsx"
data <- read.delim(pipe("pbpaste"), header=T)
head(data)

library(tidyverse)
library(multcomp)
library(ggplot2)
library(gplots)
se <- function(x){sd(x)/sqrt(length(x));}

summary_data <- data %>%
				group_by(Part, Compound) %>%
				summarize(Mean=mean(Amount), SE=se(Amount))
summary_data$Compound <- factor(summary_data$Compound, levels=c("Palmitic acid", "trans-Vaccenic acid", "n-Pentacosane", "n-Heptacosane", "Cholesterol"))
print(summary_data, n=35)


# Create bar plots
g1 <- ggplot(summary_data, aes(x=Part, y=Mean)) + 
		scale_x_discrete(limits=c("Surface", "Head", "Thorax", "Abdomen", "Foregut", "Midgut", "Hindgut")) + 
		geom_bar(width=0.8, fill="gray40", stat="identity") + 
		geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=0.4) + 
		labs(x="", y="Amount [ng/worker eq.] (Mean ± SEM)") + 
		theme_classic() + 
		theme(axis.title = element_text(size=10), 
				axis.text.x = element_text(angle=45, hjust=1, size=9), 
				axis.text.y = element_text(size=9)) + 
		facet_wrap(~Compound, ncol=3, scales="free_y")
dev.new(width=8, height=5)	# A new window opens
plot(g1)	# Show the bar plot


## Statistical analysis (ANOVA followed by Tukey's HSD test, P < 0.05)
g2 <- summary_data[, 1:3] %>% 
		pivot_wider(names_from=Part, values_from=Mean) %>%
		dplyr::select(Compound, Surface, Head, Thorax, Abdomen, Foregut, Midgut, Hindgut) %>%
		slice(2,5,4,3,1)
compound.names <- as.vector(g2$Compound)
for(i in 1:length(compound.names)){
	n <- grep(compound.names[i], data[,3])
	e <- data[n, c(2, 3, 4)]
	e$Part <- factor(e$Part)
	r <- lm(Amount ~ Part, data=e)
	res <- aov(r)
	cat(compound.names[i],"\n-- ANOVA --", sep="\n")
	print(summary(res))	# Show the result of ANOVA
	cat("\n\n-- Tukey's HSD test --\n")
	res2 <- glht(res, linfct=mcp(Part="Tukey"))
	print(summary(res2))	# Show the result of Tukey's HSD test
	print(cld(res2, level = 0.05, decreasing = T))	# Show significant differences with different letters
	cat("\n\n")
}

