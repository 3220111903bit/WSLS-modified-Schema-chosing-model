rm(list = ls())
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)


#origin = read.csv("D:/Users/Xin/Downloads/CMML_mini1/painting_summary.csv")
modify_l = read.csv("D:/Users/Xin/Downloads/CMML_mini1/modify_v5/L/allresult_processed.csv")
origin_l = read.csv("D:/Users/Xin/Downloads/CMML_mini1/origin/L/allresult_processed.csv")
origin_h = read.csv("D:/Users/Xin/Downloads/CMML_mini1/origin/H/allresult_processed.csv")
modify_h = read.csv("D:/Users/Xin/Downloads/CMML_mini1/modify_v5/H/allresult_processed.csv")

if (FALSE){
origin <- origin[,-1]
colnames(origin)[1] <- "Subject"
colnames(origin)[25] = 'AC'
colnames(origin)[2] = 'Round'
colnames(origin)[3] = 'Phase'
origin$new_category <- ifelse(origin$type %in% c("H", "HL", "Hc"), "H", 


ifelse(origin$type %in% c("L", "LH", "Lc"), "L", NA))
origin_h <- subset(origin, new_category == "H")
origin_l <- subset(origin, new_category == "L")
origin_h = origin_h[,-c(30, 31)]
origin_l = origin_l[,-c(30, 31)]
}
#vis#vis#visual 1: visual average performance
origin_l <- origin_l %>% mutate(Group = "Origin_L")
modify_l <- modify_l %>% mutate(Group = "Modify_L")
origin_h <- origin_h %>% mutate(Group = "Origin_H")
modify_h <- modify_h %>% mutate(Group = "Modify_H")

# 合并数据
all_data <- bind_rows(origin_l, modify_l, origin_h, modify_h)
all_data <- all_data %>%
  filter(Round <= 5)
#mean(all_data[,'RT_1'])
#mean(all_data[,'RT_2'])
#mean(all_data[,'RT_3'])
#mean(all_data[,'RT_4'])
#total mean = 39.92
# 计算汇总统计
summary_stats <- all_data %>%
  group_by(Group, Round) %>%
  summarise(
    avg_performance = mean(performance),
    sd = sd(performance),
    n = n(),
    sem = sd / sqrt(n),
    ci_lower = avg_performance - 1.96 * sem,
    ci_upper = avg_performance + 1.96 * sem,
    .groups = 'drop'
  )

# 绘图
ggplot(summary_stats, aes(x = factor(Round), y = avg_performance, color = Group, group = Group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
#  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(
    title = "Average Performance as experience increase",
    x = "Round",
    y = "Average Performance",
    color = "Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )

overall_perf <- all_data %>%
  group_by(Subject, Group) %>%
  summarise(avg_perf = mean(performance), .groups = 'drop')

ggplot(overall_perf, aes(x = Group, y = avg_perf, fill = Group)) +
  geom_boxplot() +
  labs(title = "Average Performance", y = "Performance") +
  theme_minimal()

mean_values <- all_data %>%
  filter(Group %in% c("Modify_L", "Modify_H")) %>%
  group_by(Group) %>%
  summarise(
    avg_performance = mean(performance),
    sd = sd(performance),
    n = n(),
    sem = sd / sqrt(n),
    ci_lower = avg_performance - 1.96 * sem,
    ci_upper = avg_performance + 1.96 * sem
  )

print(mean_values)

#visual 2: visual average AC

summary_stats <- all_data %>%
  group_by(Group, Round) %>%
  summarise(
    avg_performance = mean(AC),
    sd = sd(AC),
    n = n(),
    sem = sd / sqrt(n),
    ci_lower = avg_performance - 1.96 * sem,
    ci_upper = avg_performance + 1.96 * sem,
    .groups = 'drop'
  )

# 绘图
ggplot(summary_stats, aes(x = factor(Round), y = avg_performance, color = Group, group = Group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  #  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(
    title = "Average AC as experience increase",
    x = "Round",
    y = "Average AC",
    color = "Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )

overall_perf <- all_data %>%
  group_by(Subject, Group) %>%
  summarise(avg_perf = mean(AC), .groups = 'drop')

ggplot(overall_perf, aes(x = Group, y = avg_perf, fill = Group)) +
  geom_boxplot() +
  labs(title = "Average AC", y = "Performance") +
  theme_minimal()

mean_values2 <- all_data %>%
  filter(Group %in% c("Modify_L", "Modify_H")) %>%
  group_by(Group) %>%
  summarise(
    avg_performance = mean(AC),
    sd = sd(AC),
    n = n(),
    sem = sd / sqrt(n),
    ci_lower = avg_performance - 1.96 * sem,
    ci_upper = avg_performance + 1.96 * sem
  )

print(mean_values2)

#visual 3: visualize the shifting rate
all_data3 <- all_data %>% filter(Group %in% c('Modify_L', 'Modify_H'))
classify <- all_data3 %>%
  arrange(Group, Subject, Round, Phase) %>%  # 确保数据排序正确
  group_by(Group, Subject, Round) %>%  # 额外按 Group 进行分组
  mutate(Win_Stay = ifelse(lag(Schema, order_by = Phase) == Schema, 1, 0))
print(classify %>% group_by(Group) %>% summarise(mean_Win_Stay = mean(Win_Stay, na.rm = TRUE)))
# 统计每个组的 Win-Stay 概率
win_stay_summary <- classify %>%
  group_by(Group, Subject) %>%
  summarise(Win_Stay_Prob = mean(Win_Stay, na.rm = TRUE))

ggplot(win_stay_summary, aes(x=Group, y=Win_Stay_Prob, fill=Group)) +
  geom_violin(alpha=0.7) +
  geom_jitter(width=0.05, height=0, alpha=0.7, color='black') +
  theme_minimal()

schema_stay_rate <- all_data %>%
  arrange(Group, Subject, Round, Phase) %>%
  group_by(Group, Subject, Round) %>%
  mutate(
    Schema_prev = lag(Schema, order_by = Phase),
    AC_prev = lag(AC, order_by = Phase)
  ) %>%
  filter(!is.na(Schema_prev), !is.na(AC_prev)) %>%
  mutate(Stay = ifelse(Schema == Schema_prev, 1, 0)) %>%
  group_by(Group, AC_prev) %>%
  summarise(Stay_Rate = mean(Stay, na.rm = TRUE)) %>%
  ungroup()

# 绘制不同前一phase AC条件下的维持率
schema_stay_rate$AC_prev <- factor(schema_stay_rate$AC_prev, 
                                   levels = c(0, 0.5, 1), 
                                   labels = c("Prev AC = 0", "Prev AC = 0.5", "Prev AC = 1"))

ggplot(schema_stay_rate, aes(x = AC_prev, y = Stay_Rate, fill = AC_prev)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  facet_wrap(~Group) +
  labs(title = "Schema Stay Rate by Previous Phase Accuracy (AC) and Group",
       x = "Previous Phase Accuracy (AC)",
       y = "Schema Stay Rate") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")


#visual4: visualizing RT for each group
rt_data <- all_data %>%
  select(Group, RT_1, RT_2, RT_3, RT_4) %>%
  group_by(Group) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

rt_long <- rt_data %>%
  pivot_longer(cols = starts_with("RT_"), names_to = "RT_Type", values_to = "Mean_RT")

# Step 3: 绘图
ggplot(rt_long, aes(x = RT_Type, y = Mean_RT, group = Group, color = Group)) +
  geom_line(size = 0.5) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Mean Reaction Time (RT) by Group",
       x = "Reaction Time Type",
       y = "Mean Reaction Time") +
  scale_color_brewer(palette = "Set1")

# Statistical tests for L groups
origin_l_rt <- all_data %>% filter(Group == "Origin_L") %>% select(RT_1, RT_2, RT_3, RT_4)
modify_l_rt <- all_data %>% filter(Group == "Modify_L") %>% select(RT_1, RT_2, RT_3, RT_4)
l_test_results <- mapply(function(x, y) t.test(x, y)$p.value, origin_l_rt, modify_l_rt)

# Statistical tests for H groups
origin_h_rt <- all_data %>% filter(Group == "Origin_H") %>% select(RT_1, RT_2, RT_3, RT_4)
modify_h_rt <- all_data %>% filter(Group == "Modify_H") %>% select(RT_1, RT_2, RT_3, RT_4)
h_test_results <- mapply(function(x, y) t.test(x, y)$p.value, origin_h_rt, modify_h_rt)

# Overall RT statistical test (combined RT1-RT4)
overall_rt <- all_data %>% 
  mutate(Mean_RT = rowMeans(select(., RT_1, RT_2, RT_3, RT_4), na.rm = TRUE)) %>%
  select(Group, Mean_RT)
overall_test_result <- aov(Mean_RT ~ Group, data = overall_rt)

origin_l_mean <- overall_rt %>% filter(Group == "Origin_L") %>% pull(Mean_RT)
modify_l_mean <- overall_rt %>% filter(Group == "Modify_L") %>% pull(Mean_RT)
origin_h_mean <- overall_rt %>% filter(Group == "Origin_H") %>% pull(Mean_RT)
modify_h_mean <- overall_rt %>% filter(Group == "Modify_H") %>% pull(Mean_RT)

t_test_l_mean <- t.test(origin_l_mean, modify_l_mean)$p.value
t_test_h_mean <- t.test(origin_h_mean, modify_h_mean)$p.value


# Print test results
print("L group RT comparison p-values:")
print(l_test_results)
print("H group RT comparison p-values:")
print(h_test_results)
print("Overall RT ANOVA results (combined RT1-RT4):")
print(summary(overall_test_result))

print("Overall mean RT comparison L group (Origin vs Modify) p-value:")
print(t_test_l_mean)
print("Overall mean RT comparison H group (Origin vs Modify) p-value:")
print(t_test_h_mean)


group1 <- overall_perf %>% filter(Group == "Modify_L") %>% pull(avg_perf)
group2 <- overall_perf %>% filter(Group == "Origin_L") %>% pull(avg_perf)

t_res_l <- t.test(group1, group2)

group3 <- overall_perf %>% filter(Group == "Modify_H") %>% pull(avg_perf)
group4 <- overall_perf %>% filter(Group == "Origin_H") %>% pull(avg_perf)

t_res_h <- t.test(group3, group4)

# 合并显著性标签（可设定 p 值阈值）
p_label_l <- ifelse(t_res_l$p.value < 0.001, "***",
                    ifelse(t_res_l$p.value < 0.01, "**",
                           ifelse(t_res_l$p.value < 0.05, "*", "ns")))

p_label_h <- ifelse(t_res_h$p.value < 0.001, "***",
                    ifelse(t_res_h$p.value < 0.01, "**",
                           ifelse(t_res_h$p.value < 0.05, "*", "ns")))

# 绘图 + 手动标注
ggplot(overall_perf, aes(x = Group, y = avg_perf, fill = Group)) +
  geom_boxplot() +
  labs(title = "Average AC", y = "Performance") +
  theme_minimal() +
  geom_text(aes(x = 1.5, y = max(overall_perf$avg_perf) + 1, label = p_label_l)) +
  geom_text(aes(x = 3.5, y = max(overall_perf$avg_perf) + 1, label = p_label_h)) +
  geom_segment(aes(x = 1, xend = 2, y = max(overall_perf$avg_perf), yend = max(overall_perf$avg_perf))) +
  geom_segment(aes(x = 3, xend = 4, y = max(overall_perf$avg_perf), yend = max(overall_perf$avg_perf)))


#######
library(ggplot2)

# 创建数据框
df <- data.frame(
  modify_extent = factor(c(1, 2, 3, 4, 5)),
  performance = c(26.95, 26, 25.335, 24.987, 24.1),
  AC = c(0.434, 0.46, 0.43, 0.46, 0.483),
  win_stay_prob = c(0.111, 0.165, 0.1285, 0.167, 0.2)
)

# 绘图
ggplot(df, aes(x = modify_extent, group = 1)) +
  geom_line(aes(y = performance, color = "Performance"), size = 1) +
  geom_point(aes(y = performance, color = "Performance"), size = 3) +
  
  geom_line(aes(y = AC * 50, color = "AC"), size = 1, linetype = "dashed") +
  geom_point(aes(y = AC * 50, color = "AC"), size = 3) +
  
  geom_line(aes(y = win_stay_prob * 50, color = "Win-Stay Rate"), size = 1, linetype = "dotdash") +
  geom_point(aes(y = win_stay_prob * 50, color = "Win-Stay Rate"), size = 3) +
  
  scale_y_continuous(
    name = "Performance",
    sec.axis = sec_axis(~./50, name = "AC / Win-Stay Rate")
  ) +
  
  labs(
    title = "Performance, AC, and Win-Stay Rate Across Modify Extents",
    x = "Modify Extent",
    color = "Metrics"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  ) +
  scale_color_manual(values = c("Performance" = "blue", "AC" = "green", "Win-Stay Rate" = "red"))

library(ggplot2)
library(ggsignif)

# 创建数据
data <- data.frame(
  Condition = rep(c("Original", "Extreme WSLS"), each = 2),
  AC = factor(rep(c("0.5", "1"), times = 2)),
  Value = c(0.084, 0.13, 0.31, 0.38)
)

# 绘图 (调整Y轴范围)
ggplot(data, aes(x = Condition, y = Value, fill = AC)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = Value), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("steelblue", "orange")) +
  ylim(0, 0.45) +  # 将这里的上限改为0.45
  labs(title = "Win-stay prob", x = "Condition", y = "Value", fill = "AC") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  geom_signif(comparisons = list(c("Original", "Extreme WSLS")),
              annotations = "*",
              y_position = 0.42, tip_length = 0.03, textsize = 6)

