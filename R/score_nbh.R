#' Handle Scoring for Neighborhood Perception Scale
#'
#' @title 邻里感知量表计分处理
#' @description 处理邻里感知量表的反向计分和维度分计算
#'
#' @param data 数据框
#' @param prefix 题目前缀，默认为"socialdev_pneigh_"
#' @param time_col 时间列名，默认为"eventname"
#' @param subscales 是否计算分量表得分，默认TRUE
#'
#' @return 处理后的数据列表
#' @export
#'
#' @examples
#' \dontrun{
#' result <- score_nbh(data)
#' }

score_nbh <- function(data,
                      prefix = "pneigh_",
                      time_col = "eventname",
                      subscales = TRUE) {
  
  # 1. 定义量表结构
  nbh_structure <- list(
    # 需要反向计分的题目
    reverse_items = c(1, 2, 3, 4, 5, 6, 7, 10, 15, 16, 22, 23, 24, 25),
    
    # 维度定义
    subscales = list(
      lack_of_efficacy = 1:10,        # 缺乏效能感
      disorder = 11:25                 # 混乱无序
    )
  )
  
  # 2. 数据处理
  processed_data <- data
  report <- list()
  
  # 3. 反向计分
  for(item in nbh_structure$reverse_items) {
    col_name <- paste0(prefix, sprintf("%02d", item))
    col_name_l <- paste0(prefix, sprintf("%02d_l", item))
    
    # 处理基线数据
    if(col_name %in% names(processed_data)) {
      processed_data[[col_name]] <- 6 - processed_data[[col_name]]  # 5点量表反向计分
    }
    
    # 处理随访数据
    if(col_name_l %in% names(processed_data)) {
      processed_data[[col_name_l]] <- 6 - processed_data[[col_name_l]]
    }
  }
  
  # 4. 计算维度分数
  if(subscales) {
    for(scale_name in names(nbh_structure$subscales)) {
      items <- nbh_structure$subscales[[scale_name]]
      
      # 基线数据维度分计算
      base_cols <- paste0(prefix, sprintf("%02d", items))
      if(all(base_cols %in% names(processed_data))) {
        scale_col <- paste0(prefix, "scale_", scale_name)
        processed_data[[scale_col]] <- rowMeans(processed_data[base_cols], 
                                                na.rm = TRUE)
      }
      
      # 随访数据维度分计算
      follow_cols <- paste0(prefix, sprintf("%02d_l", items))
      if(all(follow_cols %in% names(processed_data))) {
        scale_col <- paste0(prefix, "scale_", scale_name, "_l")
        processed_data[[scale_col]] <- rowMeans(processed_data[follow_cols], 
                                                na.rm = TRUE)
      }
    }
  }
  
  # 5. 准备报告
  report$scales_processed <- "NBH"
  report$reverse_items <- nbh_structure$reverse_items
  report$subscales_computed <- if(subscales) {
    names(nbh_structure$subscales)
  } else NULL
  
  # 6. 添加维度说明
  report$scale_info <- list(
    scoring_range = c(1, 5),
    reverse_direction = "high scores indicate better neighborhood conditions",
    subscales = list(
      lack_of_efficacy = "Low scores indicate better collective efficacy",
      disorder = "Low scores indicate less neighborhood disorder"
    )
  )
  
  return(list(
    data = processed_data,
    report = report
  ))
}