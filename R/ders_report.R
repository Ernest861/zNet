#' Generate DERS Scale Statistics Report
#'
#' @title DERS量表统计报告
#' @description 生成DERS量表的总体信效度和分量表描述性统计
#' @export

ders_report <- function(data, prefix = "Cders") {

  # 1. 定义分量表结构
  subscales <- list(
    clarity = c(1,4,5,7,9),          # 情绪不清晰
    awareness = c(2,6,8,10,17,34),   # 缺乏情绪意识
    impulse = c(3,14,19,24,27,32),   # 冲动控制困难
    nonaccept = c(11,12,21,23,25,29),# 不接受情绪反应
    goals = c(13,18,20,26,33),       # 难以参与目标导向行为
    strategies = c(15,16,22,28,30,31,35,36) # 情绪调节策略有限
  )

  # 2. 计算总体信效度
  total_items <- paste0(prefix, sprintf("%02d", 1:36))
  reliability <- bruceR::Alpha(data[total_items],
                               vars = total_items)

  # 3. 计算各分量表的描述性统计
  subscale_stats <- lapply(names(subscales), function(scale_name) {
    items <- paste0(prefix, sprintf("%02d", subscales[[scale_name]]))
    scores <- rowMeans(data[items], na.rm = TRUE)

    c(
      mean = mean(scores, na.rm = TRUE),
      sd = sd(scores, na.rm = TRUE),
      min = min(scores, na.rm = TRUE),
      max = max(scores, na.rm = TRUE)
    )
  })
  names(subscale_stats) <- names(subscales)

  # 4. 准备报告
  return(list(
    reliability = reliability,
    subscales = subscale_stats
  ))
}
