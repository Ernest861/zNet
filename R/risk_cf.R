#' Calculate Firearm Risk Score
#'
#' @title 枪支风险评估分数计算
#' @description 基于携带枪支历史计算风险评分
#'
#' @param data 数据框
#' @param prefix 题目前缀，默认为"pfire_"
#'
#' @return 包含风险评分的数据框
#' @export
#'
risk_cf <- function(data, prefix = "pfire_") {
  
  processed_data <- data
  
  # 1. 验证所需列是否存在
  required_cols <- paste0(prefix, 1:3)
  missing_cols <- required_cols[!required_cols %in% names(processed_data)]
  
  if(length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # 2. 计算风险分数
  # 首先处理携带历史
  processed_data[[paste0(prefix, "score_carrying")]] <- 
    ifelse(processed_data[[paste0(prefix, "1")]] == 1, 1, 0)
  
  # 对于pfire_2和pfire_3，只在pfire_1为1时才考虑
  processed_data[[paste0(prefix, "score_age")]] <- 
    ifelse(processed_data[[paste0(prefix, "1")]] == 1,
           ifelse(!is.na(processed_data[[paste0(prefix, "2")]]), 1, 0),
           0)
  
  processed_data[[paste0(prefix, "score_frequency")]] <- 
    ifelse(processed_data[[paste0(prefix, "1")]] == 1,
           ifelse(!is.na(processed_data[[paste0(prefix, "3")]]), 1, 0),
           0)
  
  # 3. 计算总分
  score_cols <- paste0(prefix, c("score_carrying", "score_age", "score_frequency"))
  processed_data[[paste0(prefix, "total_risk_score")]] <- 
    rowSums(processed_data[score_cols], na.rm = TRUE)
  
  # 4. 计算风险等级
  processed_data[[paste0(prefix, "risk_level")]] <- 
    case_when(
      processed_data[[paste0(prefix, "total_risk_score")]] == 0 ~ "无风险",
      processed_data[[paste0(prefix, "total_risk_score")]] == 1 ~ "低风险",
      processed_data[[paste0(prefix, "total_risk_score")]] == 2 ~ "中等风险",
      processed_data[[paste0(prefix, "total_risk_score")]] == 3 ~ "高风险",
      TRUE ~ NA_character_
    )
  
  # 5. 添加指标说明
  attr(processed_data, "scoring_info") <- list(
    description = "枪支风险评分",
    components = c(
      "score_carrying: 是否携带过 (0-1)",
      "score_age: 有年龄记录 (0-1)",
      "score_frequency: 有频率记录 (0-1)"
    ),
    total_range = "0-3分",
    risk_levels = c(
      "无风险 = 0分",
      "低风险 = 1分",
      "中等风险 = 2分",
      "高风险 = 3分"
    )
  )
  
  return(processed_data)
}