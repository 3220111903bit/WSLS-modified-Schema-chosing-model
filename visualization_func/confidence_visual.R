rm(list = ls())
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

modify_l = read.csv("D:/Users/Xin/Downloads/CMML_mini1/modify_v4_try5/L/confidence.csv")
origin_l = read.csv("D:/Users/Xin/Downloads/CMML_mini1/origin/L/confidence.csv")
origin_h = read.csv("D:/Users/Xin/Downloads/CMML_mini1/origin/H/confidence.csv")
modify_h = read.csv("D:/Users/Xin/Downloads/CMML_mini1/modify_v4_try5/H/confidence.csv")

modify_l$Group <- "Modify_L"
origin_l$Group <- "Origin_L"
modify_h$Group <- "Modify_H"
origin_h$Group <- "Origin_H"

all_confidence <- bind_rows(modify_l, origin_l, modify_h, origin_h)

# 筛选每个Subject的最后一个时间点的数据
final_confidence <- all_confidence %>%
  group_by(Subject, Group) %>%
  filter(Time == max(Time))

# 绘制箱线图
ggplot(final_confidence, aes(x = Group, y = conN, fill = Group)) +
  geom_boxplot(alpha = 0.8, outlier.color = "red", outlier.size = 2) +
  labs(title = "Schema Confidence at Final Time Point",
       x = "Group",
       y = "Schema Confidence (conN)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

final_confidence <- all_confidence %>%
  group_by(Subject, Group) %>%
  filter(Time == max(Time))

# 绘制confidence分布图
ggplot(final_confidence, aes(x = conN, fill = Group)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Group, scales = "free") +
  labs(title = "Distribution of Schema Confidence at Final Time Point",
       x = "Schema Confidence (conN)",
       y = "Density") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

