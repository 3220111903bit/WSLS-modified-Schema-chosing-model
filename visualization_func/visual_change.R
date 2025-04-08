rm(list = ls())
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

confi = read.csv("D:/Users/Xin/Downloads/CMML_mini1/modify_v5_confichange/L/outputs_delta.csv")
confi$AC <- factor(confi$AC, levels = sort(unique(confi$AC)))

# 绘制按AC分组的boxplot
ggplot(confi, aes(x = AC, y = delta, fill = AC)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Schema Confidence changes",
       x = "AC",
       y = "Confidence Delta") +
  theme(legend.position = "none")

