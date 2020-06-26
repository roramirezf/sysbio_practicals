# Author: Rico Ramirez
# roramirezf@uni-heidelberg.de

#' Analyze lung cancer data for Mr. Evil
#' 

library(tidyverse)
library(limma)

target_file = read_delim(file = "./day3_2020/Simulation_targets.txt",
                         delim = " ")

gex = read_delim(file = "./day3_2020/Simulation.txt", delim = "\t")


gene1 = gex[1,]
gene2 = gex[2,]

PCA_gex = prcomp(x = t(as.matrix(gex)))

# Principal components to plot

PC_df = tibble(PC1 = PCA_gex$x[,1], PC2 = PCA_gex$x[,2],
             sample_name = rownames(PCA_gex$x))

PC_plot = PC_df %>%
  left_join(target_file, 
            by = c("sample_name"="SampleNames")) %>%
  ggplot(., aes(x = PC1, y = PC2, 
                color = Conditions, shape = RNAlibrary)) +
  geom_point(size = 4)

plotMDS(as.matrix(gex),
        labels = target_file$Conditions)

plotMDS(as.matrix(gex),
        labels = target_file$RNAlibrary)  
  