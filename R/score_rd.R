#' Handle Scoring for Self-Reported Delinquency Scale
#'
#' @title 自报违法行为量表计分处理
#' @description 处理SRD量表的数据清理和维度得分计算
#'
#' @param data 数据框
#' @param prefix 题目前缀，默认为"prd_"
#' @param na_values 需要被转换为NA的值向量，默认为c(98, 99)
#'
#' @return 处理后的数据列表，包含清理后的数据和维度得分
#' @export
#'
score_rd <- function(data, prefix = "prd_", na_values = c(98, 99)) {
  
  # 1. 定义维度结构
  srd_structure <- list(
    vandalism = c(8, 9, 10, 11, 12),        # 破坏行为
    theft = 13:23,                          # 盗窃
    violence = c(24, 25, 26, 27, 29:33),    # 暴力
    police_contact = c(34, 35)              # 警察接触
  )
  
  # 2. 数据处理
  processed_data <- data
  report <- list()
  
  # 3. 检查数据中的列名前缀
  actual_prefixes <- unique(gsub("\\d+.*$", "", grep("^[a-zA-Z]+_\\d+", names(data), value = TRUE)))
  if (length(actual_prefixes) > 0 && !prefix %in% actual_prefixes) {
    warning(sprintf("Expected prefix '%s' not found. Available prefixes: %s", 
                    prefix, paste(actual_prefixes, collapse = ", ")))
  }
  
  # 4. 替换特定值为NA
  for(col in names(processed_data)) {
    if(grepl(paste0("^", prefix), col)) {
      processed_data[[col]][processed_data[[col]] %in% na_values] <- NA
    }
  }
  
  # 5. 计算每个维度的得分
  for(dimension_name in names(srd_structure)) {
    # 获取维度对应的题目编号
    items <- srd_structure[[dimension_name]]
    
    # 构建完整的列名
    dimension_cols <- paste0(prefix, sprintf("%02d", items))
    
    # 检查这个维度中哪些列存在
    available_cols <- dimension_cols[dimension_cols %in% names(processed_data)]
    
    # 创建新的维度得分列名
    dimension_col_name <- paste0(prefix, "dim_", dimension_name)
    
    if(length(available_cols) > 0) {
      # 计算维度平均分
      processed_data[[dimension_col_name]] <- rowMeans(
        processed_data[available_cols], 
        na.rm = TRUE
      )
      
      # 记录使用的列
      report[[paste0(dimension_name, "_columns_used")]] <- available_cols
    } else {
      report[[paste0(dimension_name, "_missing_columns")]] <- dimension_cols
      warning(sprintf("No valid columns available for dimension %s", dimension_name))
    }
  }
  
  # 6. 准备报告
  report$dimensions_processed <- names(srd_structure)
  report$prefix_used <- prefix
  report$na_values_replaced <- na_values
  
  return(list(
    data = processed_data,
    report = report
  ))
}