## Calculation of the amount of each component in natural brown-rotted cedar and pine and matured brown-rotted pine


## Statistical analysis

# Japanese red pine wood (Pinus densiflora)

# Copy and paste from "Sheet1" in "GC-MS result summary - Natural brown-rotted pine (P densiflora).xlsx"
data <- read.delim(pipe("pbpaste"), header=T)
head(data)
compound.no <- nrow(data)
se <- function(x){sd(x)/sqrt(length(x));}

library(tidyverse)
r1 <- data.frame(Compound=data$Compound, Amount=" ")
for(i in 1:compound.no){
	d <- data[i, -2] %>% 
		pivot_longer(cols=c(-Compound), names_to="SampleID", values_to="Amount") %>% 
		mutate(Wood.type = "Brown-rotted pine", Replication = c(1,2,3,4,5))
	d1 <- d %>%
		summarize(Mean=round(mean(Amount), 4), SEM=round(se(Amount), 4))
	r1[i, 2] <- paste(d1[1, 1], " ± ", d1[1, 2])
}
write.csv(r1, "Mean amounts - Natural brown-rotted pine.csv", row.names=F)

########################################################################################################


# Japanese cedar (Cryptomeria japonica)

# Copy and paste from "Sheet1" in "GC-MS result summary - Natural brown-rotted cedar (C japonica).xlsx"
data <- read.delim(pipe("pbpaste"), header=T)
head(data)
compound.no <- nrow(data)
se <- function(x){sd(x)/sqrt(length(x));}

library(tidyverse)
r1 <- data.frame(Compound=data$Compound, Amount=" ")
for(i in 1:compound.no){
	d <- data[i, -2] %>% 
		pivot_longer(cols=c(-Compound), names_to="SampleID", values_to="Amount") %>% 
		mutate(Wood.type = "Brown-rotted cedar", Replication = c(1,2,3,4,5))
	d1 <- d %>%
		summarize(Mean=round(mean(Amount), 4), SEM=round(se(Amount), 4))
	r1[i, 2] <- paste(d1[1, 1], " ± ", d1[1, 2])
}
write.csv(r1, "Mean amounts - Natural brown-rotted cedar.csv", row.names=F)

########################################################################################################


# Matured brown-rotted pine

# Copy and paste from "Sheet1" in "GC-MS result summary - Matured brown-rotted pine (P densiflora).xlsx"
data <- read.delim(pipe("pbpaste"), header=T)
head(data)
compound.no <- nrow(data)
se <- function(x){sd(x)/sqrt(length(x));}

library(tidyverse)
r1 <- data.frame(Compound=data$Compound, Amount=" ")
for(i in 1:compound.no){
	d <- data[i, -2] %>% 
		pivot_longer(cols=c(-Compound), names_to="SampleID", values_to="Amount") %>% 
		mutate(Wood.type = "Matured brown-rotted pine", Replication = c(1,2,3,4,5))
	d1 <- d %>%
		summarize(Mean=round(mean(Amount), 4), SEM=round(se(Amount), 4))
	r1[i, 2] <- paste(d1[1, 1], " ± ", d1[1, 2])
}
write.csv(r1, "Mean amounts - Matured brown-rotted pine.csv", row.names=F)


