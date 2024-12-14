#' Handle Scoring for Coping Behavior Dimensions
#'
#' @title 应对行为维度计分处理
#' @description 处理应对行为的四个维度得分计算：积极学校、积极社交、消极学校、消极违法
#'
#' @param data 数据框
#' @param prefix 题目前缀，默认为"cpb_"
#'
#' @return 处理后的数据列表，包含原始数据和维度得分
#' @export
#'
score_pb <- function(data, prefix = "cpb_") {
  
  # 1. 定义维度结构
  cb_structure <- list(
    positive_school = c(1, 5, 9, 12, 16),    # 积极学校
    positive_social = c(3, 7, 14),           # 积极社交
    negative_school = c(2, 10, 13, 17),      # 消极学校
    negative_illegal = c(4, 6, 8, 11, 15)    # 消极违法
  )
  
  # 2. 数据处理
  processed_data <- data
  report <- list()
  
  # 3. 计算每个维度的分数
  for(dimension_name in names(cb_structure)) {
    # 获取维度对应的题目编号
    items <- cb_structure[[dimension_name]]
    
    # 构建完整的列名
    dimension_cols <- paste0(prefix, sprintf("%02d", items))
    
    # 创建新的维度得分列名
    dimension_col_name <- paste0(prefix, "dim_", dimension_name)
    
    # 检查所需的列是否都存在
    if(all(dimension_cols %in% names(processed_data))) {
      # 计算维度平均分
      processed_data[[dimension_col_name]] <- rowMeans(
        processed_data[dimension_cols], 
        na.rm = TRUE
      )
    } else {
      warning(paste("Some items for dimension", dimension_name, "are missing"))
    }
  }
  
  # 4. 准备报告
  report$dimensions_processed <- names(cb_structure)
  report$items_per_dimension <- lapply(cb_structure, length)
  
  return(list(
    data = processed_data,
    report = report
  ))
}