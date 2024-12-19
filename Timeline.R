# 设置基础目录
base_dir <- '/Users/macbear/Downloads/abcd-extraction-main/abcd-data-release-5.1/substudies/social-development'
library(zNet)
setwd("/Users/macbear/Downloads/abcd-extraction-main/SocialDevelop/")
sd_p_vt <- read.csv(file.path(base_dir, "sd_p_vt.csv"))
str(sd_p_vt)

# 加载必要的包
library(ggplot2)
library(dplyr)
library(ggiraph)
library(scales)
library(lubridate)

# 数据预处理函数
process_visit_data <- function(data,
                               wave_pattern = "sd_wave(\\d+)_arm_3",  # 可自定义波次提取模式
                               date_format = "%Y-%m-%d %H:%M") {      # 可自定义日期格式
  # 向量化的日期转换函数
  safe_date_convert <- Vectorize(function(x) {
    if (is.na(x) || x == "") return(NA)
    tryCatch({
      as.POSIXct(x, format = date_format)
    }, error = function(e) {
      NA
    })
  })

  # 提取波次信息函数
  extract_wave <- function(eventname, pattern = wave_pattern) {
    wave_num <- as.numeric(gsub(pattern, "\\1", eventname))
    ifelse(!is.na(wave_num), paste("Wave", wave_num), NA)
  }

  # 首先处理Wave 1的数据，并根据访问时间排序
  wave1_order <- data %>%
    filter(grepl("wave1", eventname, ignore.case = TRUE)) %>%
    mutate(
      wave1_date = safe_date_convert(socialdev_visit_date)
    ) %>%
    filter(!is.na(wave1_date)) %>%
    arrange(wave1_date) %>%
    mutate(
      subject_index = row_number()
    ) %>%
    select(src_subject_id, subject_index, wave1_date)

  # 处理完整数据集
  processed_data <- data %>%
    mutate(
      # 转换所有日期
      visit_date_initial = safe_date_convert(socialdev_visit_date),
      visit_date_followup = safe_date_convert(socialdev_visit_date_l),
      visit_date = coalesce(visit_date_initial, visit_date_followup),

      # 动态提取Wave信息
      wave = sapply(eventname, extract_wave)
    ) %>%
    # 加入Wave 1排序后的subject_index
    left_join(wave1_order %>% select(src_subject_id, subject_index),
              by = c("src_subject_id","eventname")) %>%
    # 移除没有访问日期的记录
    filter(!is.na(visit_date))

  # 数据验证输出
  cat("\n数据处理统计：\n")
  cat("原始数据行数:", nrow(data), "\n")
  cat("处理后数据行数:", nrow(processed_data), "\n")
  cat("唯一受试者数:", length(unique(processed_data$src_subject_id)), "\n")
  cat("\n检测到的Wave：\n")
  print(sort(unique(processed_data$wave)))

  return(processed_data)
}

# 创建主要可视化
create_visit_timeline <- function(processed_data,
                                  custom_colors = NULL,        # 自定义颜色
                                  point_size = 2,             # 点的大小
                                  point_alpha = 0.8,          # 点的透明度
                                  line_alpha = 0.3,           # 线的透明度
                                  date_break = "3 months",    # 日期刻度间隔
                                  svg_width = 12,             # 图形宽度
                                  svg_height = 8) {           # 图形高度

  # 确保visit_date是POSIXct类型
  processed_data$visit_date <- as.POSIXct(processed_data$visit_date, origin = "1970-01-01")

  # 如果没有提供自定义颜色，则根据Wave数量自动生成
  if (is.null(custom_colors)) {
    n_waves <- length(unique(processed_data$wave))
    custom_colors <- colorRampPalette(c("#4B0082", "#1E90FF", "#32CD32", "#FFD700"))(n_waves)
  }

  # 创建基础图层
  p <- ggplot(processed_data,
              aes(x = visit_date,
                  y = subject_index,
                  color = wave)) +
    # 添加连线
    geom_line(aes(group = src_subject_id),
              alpha = line_alpha) +
    # 添加交互式点
    geom_point_interactive(
      aes(tooltip = paste("Subject:", src_subject_id,
                          "\nWave:", wave,
                          "\nDate:", format(visit_date, "%Y-%m-%d")),
          data_id = src_subject_id),
      size = point_size,
      alpha = point_alpha) +
    # 自定义主题
    theme_classic() +
    # 坐标轴标签
    labs(x = "Visit Date",
         y = "Subject Index (Ordered by Wave 1 Visit Date)",
         color = "Study Wave",
         title = "Longitudinal Study Visit Timeline") +
    # 自定义主题元素
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      #panel.grid.minor = element_line(color = "gray90"),# 不需要网格线
      #panel.grid.major = element_line(color = "gray85")
    ) +
    # 格式化日期轴
    scale_x_datetime(
      labels = date_format("%Y-%m-%d"),
      date_breaks = date_break
    ) +
    # 使用自定义颜色
    scale_color_manual(values = custom_colors)

  # 创建交互式图表
  interactive_plot <- girafe(
    ggobj = p,
    width_svg = svg_width,
    height_svg = svg_height,
    options = list(
      opts_hover(css = "fill-opacity:1;stroke-width:2;"),
      opts_hover_inv(css = "fill-opacity:0.2"),
      opts_selection(css = "fill-opacity:1;stroke-width:3;"),
      opts_zoom(max = 5)
    )
  )

  return(interactive_plot)
}

create_visit_interval_plot <- function(processed_data) {
  # 确保visit_date是POSIXct类型
  processed_data$visit_date <- as.POSIXct(processed_data$visit_date, origin = "1970-01-01")

  # 计算访问间隔
  intervals_data <- processed_data %>%
    group_by(src_subject_id) %>%
    arrange(visit_date) %>%
    mutate(
      interval = as.numeric(difftime(visit_date, lag(visit_date), units = "days")),
      interval_type = paste(lag(wave), "to", wave)
    ) %>%
    filter(!is.na(interval))

  # 确定interval_type的唯一值，并为每个值分配颜色
  interval_types <- unique(intervals_data$interval_type)
  colors <- c("#d62447", "#308cc5", "#cf3f7c") # 您想要的颜色列表
  color_map <- setNames(colors, interval_types) # 创建颜色映射

  # 创建直方图
  p_intervals <- ggplot(intervals_data,
                        aes(x = interval,
                            fill = interval_type)) +
    geom_histogram_interactive(
      aes(tooltip = paste("Interval:", round(interval), "days",
                          "\nTransition:", interval_type)),
      binwidth = 30,
      position = "identity",
      alpha = 0.7
    ) +
    labs(x = "Days Between Visits",
         y = "Count",
         title = "Distribution of Visit Intervals") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = color_map) # 使用fill而不是color

  # 转换为交互式图表
  interactive_intervals <- girafe(
    ggobj = p_intervals,
    width_svg = 10,
    height_svg = 6,
    options = list(
      opts_hover(css = "fill-opacity:1;"),
      opts_hover_inv(css = "fill-opacity:0.5")
    )
  )

  return(interactive_intervals)
}

# 使用示例
names(sd_p_vt)
#使用示例
processed_data <- process_visit_data(sd_p_vt)
#验证排序是否正确
verify_order <- processed_data %>%
  filter(wave == "Wave 1") %>%
  arrange(subject_index) %>%
  select(src_subject_id, subject_index, visit_date, wave) %>%
  head(10)
print(verify_order)

# 使用示例
# 自定义颜色方案
zcolor <- c("#1ba784","#63bbd0","#f87599","#fed71a",
            "#d1c2d3","#304fb0","#c6dfc8","#a8456b","#2486b9",# 洋葱紫，宝石蓝
            "#e16c96","#fc8c23","#280c1c",
            "#fbb957","#de1c31","#ee3f4d",
            "#c0c4c3","#c6e6e8",
            "#12a182","#eb3c70","#eaad1a","#45b787","#d11a2d",#虎皮黄
            "#eea08c","#cfccc9",
            "#2b1216","#61649f","#93b5cf","#c4cbcf",
            "#c4d7d6","#248067","#fbda41","#f1f0ed")

 my_colors <- c("Wave 1" = "#2d81be",
                "Wave 2" = "#a34c89",
                "Wave 3" = "#146f6e",
                "Wave 4" = "#f2c05e",
                "Wave 5" = "#4a5b66")

processed_data <- process_visit_data(sd_p_vt)
str(processed_data)
timeline_plot <- create_visit_timeline(processed_data,
                                     custom_colors = my_colors,
                                      point_size = 2.5,
                                      point_alpha = 0.9,
                                      line_alpha = 0.2)
print(timeline_plot)
saveWidget(timeline_plot, "interactive_plot_Vist_timeline.html", selfcontained = TRUE)
interval_plot <- create_visit_interval_plot(processed_data)
interval_plot
library(htmlwidgets)
saveWidget(interval_plot, "interactive_plot_Vist_intervals.html", selfcontained = TRUE)

# 人口学信息的读取
ABCD5.1_Demographics_MICE_MILDS <- readRDS('../ABCD5.1_TabulatedData/Res_3_IntermediateData/ABCD5.1_Demographics_MICE_MILDS.rds')
Demographics <- ABCD5.1_Demographics_MICE_MILDS$data
str(Demographics)
head(Demographics)

# 测量访问主要是每个时间点的年龄信息
abcd_y_lt <- read.csv('/Users/macbear/Downloads/abcd-extraction-main/abcd-data-release-5.1/core/abcd-general/abcd_y_lt.csv')
str(abcd_y_lt)
head(abcd_y_lt)

# 从 abcd_y_lt 提取每个受试者在不同时间点的年龄信息
age_info <- abcd_y_lt %>%
  select(src_subject_id, eventname, interview_date, interview_age) %>%
  # 统一日期格式，假设 abcd_y_lt 中的日期格式为 "mm/dd/yyyy"
  mutate(interview_date = as.POSIXct(interview_date, format = "%m/%d/%Y"))

# 物理测量的信息
ph_y_anthro <- read.csv('/Users/macbear/Downloads/abcd-extraction-main/abcd-data-release-5.1/core/physical-health/ph_y_anthro.csv')
head(ph_y_anthro)
anthro_info <- ph_y_anthro %>%
  select(src_subject_id, eventname, anthro_timestamp, anthroheightcalc, anthroweightcalc,anthro_waist_cm) %>%
  # 统一日期格式，假设 abcd_y_lt 中的日期格式为 "%Y-%m-%d %H:%M"
  mutate(interview_date = format(as.POSIXct(anthro_timestamp, format = "%Y-%m-%d %H:%M"), "%m/%d/%Y"))%>% # 抽时间处理下腰围数据异常
  mutate(height = anthroheightcalc * 2.54 )%>% #
  mutate(weight = anthroweightcalc * 0.453592)%>%
  mutate(waist = anthro_waist_cm * 2.54)
head(anthro_info)
age_info <- left_join(age_info,anthro_info,by = c("src_subject_id","eventname"))
str(age_info)
head(age_info)

# 创建年龄组（比如每1岁一组）
age_info <- age_info %>%
  mutate(age_group = cut(interview_age/12,
                         breaks = seq(8, 15, by = 1),
                         labels = paste(seq(8, 14), seq(9, 15), sep="-")))

# 加载必要的包
library(dplyr)
library(ggplot2)
library(zoo)

clean_anthropometric_data <- function(age_info) {
  # 1. 数据预处理和初步清理
  age_info_valid <- age_info %>%
    # 创建年龄组
    mutate(
      age_group = cut(interview_age/12,
                      breaks = seq(8, 15, by = 1),
                      labels = paste(seq(8, 14), seq(9, 15), sep="-"))
    )

  # 2. 计算年龄组的界限值
  outlier_bounds <- age_info_valid %>%
    filter(!is.na(age_group)) %>%  # 排除NA组
    group_by(age_group) %>%
    summarise(
      # 身高界限 (cm)
      height_mean = mean(height, na.rm = TRUE),
      height_sd = sd(height, na.rm = TRUE),
      height_lower = pmax(100, height_mean - 5 * height_sd),  # 最小100cm
      height_upper = pmin(220, height_mean + 5 * height_sd),  # 最大220cm

      # 体重界限 (kg)
      weight_mean = mean(weight, na.rm = TRUE),
      weight_sd = sd(weight, na.rm = TRUE),
      weight_lower = pmax(15, weight_mean - 5 * weight_sd),   # 最小15kg
      weight_upper = pmin(150, weight_mean + 5 * weight_sd),  # 最大150kg

      # 腰围界限 (cm)
      waist_mean = mean(waist, na.rm = TRUE),
      waist_sd = sd(waist, na.rm = TRUE),
      waist_lower = pmax(40, waist_mean - 5 * waist_sd),     # 最小40cm
      waist_upper = pmin(150, waist_mean + 5 * waist_sd)     # 最大150cm
    )

  # 3. 标记异常值
  age_info_cleaned <- age_info_valid %>%
    left_join(outlier_bounds, by = "age_group") %>%
    group_by(src_subject_id) %>%
    arrange(src_subject_id, interview_date.x) %>%
    mutate(
      # 基础异常值标记（只标记实际存在的值）
      height_outlier = case_when(
        is.na(height) ~ FALSE,
        height <= 0 | height < height_lower | height > height_upper ~ TRUE,
        TRUE ~ FALSE
      ),
      weight_outlier = case_when(
        is.na(weight) ~ FALSE,
        weight <= 0 | weight < weight_lower | weight > weight_upper ~ TRUE,
        TRUE ~ FALSE
      ),
      waist_outlier = case_when(
        is.na(waist) ~ FALSE,
        waist <= 0 | waist < waist_lower | waist > waist_upper ~ TRUE,
        TRUE ~ FALSE
      ),

      # 计算变化率（只对非NA值）
      height_change_pct = if_else(!is.na(height) & !is.na(lag(height)),
                                  (height - lag(height)) / lag(height) * 100,
                                  NA_real_),
      weight_change_pct = if_else(!is.na(weight) & !is.na(lag(weight)),
                                  (weight - lag(weight)) / lag(weight) * 100,
                                  NA_real_),
      waist_change_pct = if_else(!is.na(waist) & !is.na(lag(waist)),
                                 (waist - lag(waist)) / lag(waist) * 100,
                                 NA_real_),

      # 标记异常变化（只标记有变化率的记录）
      height_change_outlier = !is.na(height_change_pct) & height_change_pct < -5,
      weight_change_outlier = !is.na(weight_change_pct) & abs(weight_change_pct) > 50,
      waist_change_outlier = !is.na(waist_change_pct) & abs(waist_change_pct) > 50,

      # 综合异常标记
      height_any_outlier = height_outlier | height_change_outlier,
      weight_any_outlier = weight_outlier | weight_change_outlier,
      waist_any_outlier = waist_outlier | waist_change_outlier,

      # 清理后的数据（保留非异常的原始值）
      height_clean = if_else(height_any_outlier, NA_real_, height),
      weight_clean = if_else(weight_any_outlier, NA_real_, weight),
      waist_clean = if_else(waist_any_outlier, NA_real_, waist)
    ) %>%
    ungroup()

  # 4. 生成详细的数据质量报告
  data_quality_report <- list(
    original_summary = age_info_valid %>%
      summarise(
        total_records = n(),
        valid_height = sum(!is.na(height)),
        valid_weight = sum(!is.na(weight)),
        valid_waist = sum(!is.na(waist)),
        height_pct = round(valid_height/total_records*100, 1),
        weight_pct = round(valid_weight/total_records*100, 1),
        waist_pct = round(valid_waist/total_records*100, 1)
      ),

    outlier_summary = age_info_cleaned %>%
      summarise(
        # 基础异常值
        height_outliers = sum(height_outlier, na.rm = TRUE),
        weight_outliers = sum(weight_outlier, na.rm = TRUE),
        waist_outliers = sum(waist_outlier, na.rm = TRUE),

        # 变化率异常
        height_change_outliers = sum(height_change_outlier, na.rm = TRUE),
        weight_change_outliers = sum(weight_change_outlier, na.rm = TRUE),
        waist_change_outliers = sum(waist_change_outlier, na.rm = TRUE),

        # 总异常值
        total_height_outliers = sum(height_any_outlier, na.rm = TRUE),
        total_weight_outliers = sum(weight_any_outlier, na.rm = TRUE),
        total_waist_outliers = sum(waist_any_outlier, na.rm = TRUE)
      ),

    by_visit = age_info_cleaned %>%
      group_by(eventname) %>%
      summarise(
        total = n(),
        valid_height = sum(!is.na(height_clean)),
        valid_weight = sum(!is.na(weight_clean)),
        valid_waist = sum(!is.na(waist_clean)),
        height_pct = round(valid_height/total*100, 1),
        weight_pct = round(valid_weight/total*100, 1),
        waist_pct = round(valid_waist/total*100, 1)
      )
  )

  # 5. 创建可视化
  plots <- list(
    height = ggplot(age_info_cleaned, aes(x = interview_age/12, y = height)) +
      geom_point(aes(color = height_any_outlier), alpha = 0.5) +
      geom_line(aes(group = src_subject_id), alpha = 0.1) +
      facet_wrap(~age_group) +
      labs(title = "Height Measurements and Outliers",
           x = "Age (years)",
           y = "Height (cm)") +
      theme_minimal(),

    weight = ggplot(age_info_cleaned, aes(x = interview_age/12, y = weight)) +
      geom_point(aes(color = weight_any_outlier), alpha = 0.5) +
      geom_line(aes(group = src_subject_id), alpha = 0.1) +
      facet_wrap(~age_group) +
      labs(title = "Weight Measurements and Outliers",
           x = "Age (years)",
           y = "Weight (kg)") +
      theme_minimal(),

    waist = ggplot(age_info_cleaned, aes(x = interview_age/12, y = waist)) +
      geom_point(aes(color = waist_any_outlier), alpha = 0.5) +
      geom_line(aes(group = src_subject_id), alpha = 0.1) +
      facet_wrap(~age_group) +
      labs(title = "Waist Measurements and Outliers",
           x = "Age (years)",
           y = "Waist (cm)") +
      theme_minimal()
  )

  # 6. 返回结果
  return(list(
    data = age_info_cleaned,
    quality_report = data_quality_report,
    bounds = outlier_bounds,
    plots = plots
  ))
}

# 使用示例：
results <- clean_anthropometric_data(age_info)

# 查看数据质量报告
print("原始数据摘要：")
print(results$quality_report$original_summary)
print("\n异常值摘要：")
print(results$quality_report$outlier_summary)
print("\n按访问时间点的数据完整性：")
print(results$quality_report$by_visit)

# 查看图表
print(results$plots$height)
print(results$plots$weight)
print(results$plots$waist)


# 修改可视化部分
library(patchwork)  # 用于组合图表
create_measurement_plots <- function(cleaned_data) {
  # 通用的绘图主题
  plot_theme <- theme_classic() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 11),
      axis.title = element_text(size = 10),
      legend.title = element_blank()
    )

  # 身高轨迹图
  p1 <- ggplot(cleaned_data,
               aes(x = interview_age/12, y = height, group = src_subject_id)) +
    geom_line(alpha = 0.1) +
    geom_point(aes(color = height_any_outlier), size = 1) +
    scale_color_manual(values = c("TRUE" = "#af5497", "FALSE" = "#308cc5"),
                       labels = c("TRUE" = "Outlier", "FALSE" = "Normal")) +
    labs(title = "Height Trajectories",
         x = "Age (years)",
         y = "Height (cm)") +
    ylim(0, 200)+
    plot_theme

  # 体重轨迹图
  p2 <- ggplot(cleaned_data,
               aes(x = interview_age/12, y = weight, group = src_subject_id)) +
    geom_line(alpha = 0.1) +
    geom_point(aes(color = weight_any_outlier), size = 1) +
    scale_color_manual(values = c("TRUE" = "#af5497", "FALSE" = "#308cc5"),
                       labels = c("TRUE" = "Outlier", "FALSE" = "Normal")) +
    labs(title = "Weight Trajectories",
         x = "Age (years)",
         y = "Weight (kg)") +
    ylim(0, 200)+
    plot_theme

  # 腰围轨迹图
  p3 <- ggplot(cleaned_data,
               aes(x = interview_age/12, y = waist, group = src_subject_id)) +
    geom_line(alpha = 0.1) +
    geom_point(aes(color = waist_any_outlier), size = 1) +
    scale_color_manual(values = c("TRUE" = "#af5497", "FALSE" = "#308cc5"),
                       labels = c("TRUE" = "Outlier", "FALSE" = "Normal")) +
    labs(title = "Waist Trajectories",
         x = "Age (years)",
         y = "Waist (cm)") +
    ylim(0, 200)+
    plot_theme

  # 组合图表
  combined_plot <- p1 + p2 + p3 +
    plot_layout(ncol = 1, guides = "collect") &
    theme(legend.position = "bottom")

  return(combined_plot)
}

# 使用示例
measurement_plot <- create_measurement_plots(results$data)
print(measurement_plot)
ggsave("20241213_Trajectories_3index.pdf", width = 20, height = 20, units = "cm")
getwd()


# 获取清理后的数据
age_info_cleaned <- results$data[,c("src_subject_id","eventname",
                                    "interview_date.x","interview_age",
                                    "height_clean" ,"weight_clean","waist_clean")]
names(age_info_cleaned)[3] <- "interview_date"
str(age_info_cleaned)
names(processed_data)
processed_data1 <- processed_data[,c("src_subject_id","eventname",
                                     "visit_date","wave","subject_index")]
processed_data1$interview_date <- as.POSIXct(processed_data1$visit_date, origin = "1970-01-01")
str(processed_data1)
names(processed_data1)[2] <- 'eventname_sd'





###########################3
merge_anthropometric_data <- function(processed_data1, age_info_cleaned) {
  # 确保日期格式统一
  age_info_cleaned <- age_info_cleaned %>%
    mutate(interview_date = as.POSIXct(interview_date))

  # 对每个SD访问记录找到最接近的人体测量记录
  merged_data <- processed_data1 %>%
    # 确保日期格式一致
    mutate(
      interview_date = as.POSIXct(interview_date)
    ) %>%
    # 对每个记录进行处理
    group_by(src_subject_id, interview_date) %>%
    do({
      current_data <- .

      # 获取当前受试者的所有测量记录
      subject_measures <- age_info_cleaned %>%
        filter(src_subject_id == current_data$src_subject_id[1]) %>%
        mutate(
          # 计算与SD访问日期的时间差（以天为单位）
          date_diff = abs(difftime(interview_date,
                                   current_data$interview_date[1],
                                   units = "days"))
        ) %>%
        # 选择时间差最小的记录
        arrange(date_diff) %>%
        slice(1)

      # 合并数据
      current_data %>%
        mutate(
          anthro_date = subject_measures$interview_date,
          height = subject_measures$height_clean,
          weight = subject_measures$weight_clean,
          waist  = subject_measures$waist_clean,
          date_diff     = min(subject_measures$date_diff),
          interview_age = subject_measures$interview_age,
          eventname     = subject_measures$eventname
        )
    }) %>%
    ungroup()

  # 添加数据质量报告
  quality_report <- merged_data %>%
    summarise(
      total_records = n(),
      valid_height = sum(!is.na(height)),
      valid_weight = sum(!is.na(weight)),
      valid_waist = sum(!is.na(waist)),
      mean_date_diff = mean(date_diff, na.rm = TRUE),
      median_date_diff = median(date_diff, na.rm = TRUE),
      max_date_diff = max(date_diff, na.rm = TRUE)
    )

  # 按wave统计完整性
  completeness_by_wave <- merged_data %>%
    group_by(wave) %>%
    summarise(
      total = n(),
      height_complete = sum(!is.na(height)),
      weight_complete = sum(!is.na(weight)),
      waist_complete = sum(!is.na(waist)),
      height_pct = round(height_complete/total * 100, 1),
      weight_pct = round(weight_complete/total * 100, 1),
      waist_pct = round(waist_complete/total * 100, 1),
      mean_date_diff = round(mean(date_diff, na.rm = TRUE), 1)
    )

  return(list(
    data = merged_data,
    quality_report = quality_report,
    completeness_by_wave = completeness_by_wave
  ))
}

# 使用示例
merged_results <- merge_anthropometric_data(processed_data1, age_info_cleaned)

# 查看结果
print("数据质量报告：")
print(merged_results$quality_report)

print("\n按wave的完整性报告：")
print(merged_results$completeness_by_wave)

# 获取合并后的数据
final_data <- merged_results$data

str(final_data)
#############################

final_data <- left_join(final_data,
                        Demographics[, !(names(Demographics) %in% "interview_age")],
                        by = join_by(src_subject_id))
str(final_data)



library(dplyr)
library(gtsummary)
library(flextable)
library(officer)

create_wave_tables <- function(data) {
  # 为每个wave创建描述性统计表
  tables <- lapply(c("Wave 1", "Wave 2", "Wave 3"), function(wave_num) {
    wave_data <- data %>%
      filter(wave == wave_num) %>%
      select(
        # 人口统计学特征
        SexAssigned,
        `Age (years)` = interview_age,
        Race_PrntRep,
        Ethnicity_PrntRep,
        BirthCountry,
        YouthNativeLang,

        # 家庭特征
        ParentsHighEdu_5L,
        ParentsMarital_6L,
        ParentEmploy,
        FamilyIncome,
        HouseholdStructure,
        HouseholdSize,
        Relationship_3L,

        # 体格测量
        `Height (cm)` = height,
        `Weight (kg)` = weight,
        `Waist (cm)` = waist,
        BMI
      ) %>%
      # 处理年龄（从月龄转换为年龄）
      mutate(`Age (years)` = `Age (years)`/12)

    # 创建表格
    tbl <- wave_data %>%
      tbl_summary(
        missing = "no",
        statistic = list(
          all_continuous() ~ "{mean} ({sd})",
          all_categorical() ~ "{n} ({p}%)"
        ),
        digits = list(
          all_continuous() ~ 1,
          all_categorical() ~ c(0, 1)
        )
      ) %>%
      modify_header(label = "**Characteristic**") %>%
      modify_caption(paste("Table", substr(wave_num, 5, 5), ". Characteristics of", wave_num, "Participants"))

    return(tbl)
  })

  # 为每个表添加样本量信息
  wave_n <- data %>%
    group_by(wave) %>%
    summarise(
      total_n = n(),
      valid_height = sum(!is.na(height)),
      valid_weight = sum(!is.na(weight)),
      valid_waist = sum(!is.na(waist))
    )

  # 创建Word文档
  doc <- read_docx()

  # 添加每个wave的表格
  for(i in 1:3) {
    # 添加标题
    doc <- doc %>%
      body_add_par(paste("Wave", i, "Analysis"), style = "heading 1") %>%
      body_add_par(paste("Total participants:", wave_n$total_n[i]), style = "Normal") %>%
      body_add_par(paste("Valid measurements:"), style = "Normal") %>%
      body_add_par(paste("Height:", wave_n$valid_height[i],
                         "Weight:", wave_n$valid_weight[i],
                         "Waist:", wave_n$valid_waist[i]), style = "Normal")

    # 将gtsummary表格转换为flextable并添加到文档
    doc <- doc %>%
      body_add_flextable(as_flex_table(tables[[i]]))

    # 如果不是最后一个表，添加分页符
    if(i < 3) {
      doc <- doc %>% body_add_break()
    }
  }

  # 保存文档
  print(doc, target = "Wave_Characteristics_Tables.docx")

  # 返回表格对象以供预览
  return(tables)
}


# 创建纵向变化可视化
create_longitudinal_plots <- function(data) {
  interval_types <- unique(data$wave)
  colors <- c("#d62447", "#308cc5", "#cf3f7c") # 您想要的颜色列表
  color_map <- setNames(colors, interval_types) # 创建颜色映射
  # 1. 身高、体重、BMI的箱线图
  p1 <- ggplot(data, aes(x = wave, y = height)) +
    geom_boxplot(aes(fill = wave), alpha = 0.7) +
    geom_point(position = position_jitter(width = 0.2), alpha = 0.1) +
    labs(title = "Height by Wave",
         y = "Height (cm)") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_fill_manual(values = color_map)

  p2 <- ggplot(data, aes(x = wave, y = weight)) +
    geom_boxplot(aes(fill = wave), alpha = 0.7) +
    geom_point(position = position_jitter(width = 0.2), alpha = 0.1) +
    labs(title = "Weight by Wave",
         y = "Weight (kg)") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_fill_manual(values = color_map)

  p3 <- ggplot(data, aes(x = wave, y = BMI)) +
    geom_boxplot(aes(fill = wave), alpha = 0.7) +
    geom_point(position = position_jitter(width = 0.2), alpha = 0.1) +
    labs(title = "BMI by Wave",
         y = "BMI") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_fill_manual(values = color_map)

  interval_types <- unique(data$SexAssigned)
  colors <- c("#d62447", "#308cc5", "#cf3f7c") # 您想要的颜色列表
  color_map <- setNames(colors, interval_types) # 创建颜色映射
  # 2. 性别分层的发育轨迹
  p4 <- ggplot(data, aes(x = interview_age/12, y = height, color = SexAssigned)) +
    geom_smooth(method = "loess", se = TRUE) +
    geom_point(alpha = 0.1) +
    facet_wrap(~wave) +
    labs(title = "Height Trajectories by Sex",
         x = "Age (years)",
         y = "Height (cm)") +
    theme_minimal()+
    scale_color_manual(values = color_map)

  # 3. 完成率分析
  completion_data <- data %>%
    group_by(wave) %>%
    summarise(
      total = n(),
      height_complete = sum(!is.na(height)),
      weight_complete = sum(!is.na(weight)),
      waist_complete = sum(!is.na(waist)),
      height_rate = height_complete/total,
      weight_rate = weight_complete/total,
      waist_rate = waist_complete/total
    ) %>%
    pivot_longer(
      cols = ends_with("_rate"),
      names_to = "measure",
      values_to = "completion_rate"
    )

  interval_types <- unique(data$measure)
  colors <- c("#d62447", "#308cc5", "#cf3f7c") # 您想要的颜色列表
  color_map <- setNames(colors, interval_types) # 创建颜色映射
  p5 <- ggplot(completion_data,
               aes(x = wave, y = completion_rate, fill = measure)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Measurement Completion Rates",
         y = "Completion Rate") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_fill_manual(values = color_map)

  # 组合所有图表
  combined_plots <- (p1 + p2 + p3) / (p4 + p5) +
    plot_layout(heights = c(1, 1.2))

  return(combined_plots)
}

# 创建Wave间比较表
create_wave_comparison <- function(data) {
  wave_stats <- data %>%
    group_by(wave) %>%
    summarise(
      n = n(),
      age_mean = mean(interview_age/12, na.rm = TRUE),
      age_sd = sd(interview_age/12, na.rm = TRUE),
      height_mean = mean(height, na.rm = TRUE),
      height_sd = sd(height, na.rm = TRUE),
      weight_mean = mean(weight, na.rm = TRUE),
      weight_sd = sd(weight, na.rm = TRUE),
      waist_mean = mean(waist, na.rm = TRUE),
      waist_sd = sd(waist, na.rm = TRUE),
      bmi_mean = mean(BMI, na.rm = TRUE),
      bmi_sd = sd(BMI, na.rm = TRUE)
    ) %>%
    mutate(across(ends_with("_mean"), ~round(., 1)),
           across(ends_with("_sd"), ~round(., 1)))

  return(wave_stats)
}

# 使用示例
table1 <- create_wave_table(final_data)
plots <- create_longitudinal_plots(final_data)
wave_comparison <- create_wave_comparison(final_data)



tables <- create_wave_tables(final_data)
# 预览第一个表格
tables[[1]]
print(plots)
ggsave("20241213_3Wave_3index.pdf", width = 20, height = 20, units = "cm")
print(wave_comparison)
