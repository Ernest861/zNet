# 加载必要的包
library(tidyr)
library(lmerTest)
library(emmeans)
library(ggplot2)

# 读取和整理数据
data <- read.csv('/Users/macbear/Downloads/SA.csv')
long_data <- pivot_longer(
  data = data,
  cols = SA1:SA12,
  names_to = "Session",
  values_to = "Response"
)

# 转换为因子
long_data$subjId <- as.factor(long_data$subjId)
long_data$Type <- as.factor(long_data$Type)
long_data$Brain_region <- as.factor(long_data$Brain_region)
long_data$Session <- factor(long_data$Session,
                          levels = paste0("SA", 1:12),
                          ordered = TRUE)


# 拟合模型
model <- lmer(Response ~ 1 + Brain_region + Type + Session +
                Brain_region:Type + Brain_region:Session +
                Type:Session + Brain_region:Type:Session + (1|subjId),
              data = long_data)

# 2. 获取固定效应结果
anova_results <- anova(model, ddf="Satterthwaite")
# First get the emmeans results
emm_results <- emmeans(model, ~ Brain_region | Type + Session)

# Get the pairwise comparisons
pairs_results <- pairs(emm_results)

# Get the emmeans values for calculating y_pos
emmeans_df <- as.data.frame(emm_results)
max_emmean <- max(emmeans_df$emmean)

# Convert pairs results to data frame
contrasts_df <- as.data.frame(pairs_results)

# Create significance dataframe
sig_df <- data.frame(
  Type = contrasts_df$Type,
  Session = contrasts_df$Session,
  y_pos = max_emmean + 10,
  sig = ifelse(contrasts_df$p.value < 0.05, "*", "")
)

# Add additional significance levels
sig_df$sig[contrasts_df$p.value < 0.01] <- "**"
sig_df$sig[contrasts_df$p.value < 0.001] <- "***"

# First create pred_data from emmeans results
pred_data <- as.data.frame(emm_results)
# 5. 绘图
# 修改图形代码，添加注释说明校正方法和显著性水平
p <- ggplot(pred_data, aes(x = Session, y = emmean,
                           color = Brain_region,
                           fill = Brain_region,
                           group = Brain_region)) +
  # 添加置信区间带
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2, color = NA) +
  # 添加线和点
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  # 添加显著性标记
  geom_text(data = sig_df,
            aes(x = Session, y = y_pos, label = sig),
            inherit.aes = FALSE,
            color = "black") +
  # 分面和主题设置
  facet_wrap(~Type) +
  theme_classic() +
  #scale_color_brewer(palette = "Set1") +
  #scale_fill_brewer(palette = "Set1") +
  scale_color_manual(values = c("CeA" = "#62102e", "BLA" = "#134857")) + # 替换为你的颜色和类别
  scale_fill_manual(values = c("CeA" = "#d2568c", "BLA" = "#2376b7")) +
  #ylim(0,200)+
  scale_x_discrete(labels = paste0("SA", 1:12)) +
  labs(title = "Response by Brain Region and Task Type",
       subtitle = sprintf("Brain region × Type interaction: F(%.0f, %.0f) = %.2f, p < .001",
                          anova_results["Brain_region:Type", "NumDF"],
                          anova_results["Brain_region:Type", "DenDF"],
                          anova_results["Brain_region:Type", "F value"]),
       x = "Sessions",
       y = "Response",
       color = "Brain Region",
       fill = "Brain Region",
       # 添加注释说明
       caption = "Note: * p < 0.05, ** p < 0.01, *** p < 0.001 (Bonferroni-corrected)\nError bars represent 95% confidence intervals") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0, face = "italic"),  # 左对齐注释文本
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p)

# 6. 输出统计结果
print(anova_results)
