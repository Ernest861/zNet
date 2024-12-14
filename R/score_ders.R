#' Handle Reverse Scoring for DERS Scale
#'
#' @title DERS量表反向计分处理
#' @description 处理情绪调节困难量表(DERS)的反向计分项目
#'
#' @param data 数据框，包含DERS量表数据
#' @param prefix 题目前缀，可以是"cders_"或"pders_"
#' @param reverse_items 需要反向计分的题号向量
#' @param subscales 是否计算分量表得分
#'
#' @return 处理后的数据列表，包含处理后的数据和量表信息
#' @export

score_ders <- function(data,
                         prefix = c("cders_", "pders_"),
                         reverse_items = c(1,2,6,7,8,10,17,20,22,24,34),
                         subscales = TRUE) {

  # 1. 输入检查
  prefix <- match.arg(prefix)

  # 2. 创建分量表信息
  subscale_info <- list(
    clarity = c(1,4,5,7,9),
    awareness = c(2,6,8,10,17,34),
    impulse = c(3,14,19,24,27,32),
    nonaccept = c(11,12,21,23,25,29),
    goals = c(13,18,20,26,33),
    strategies = c(15,16,22,28,30,31,35,36)
  )

  # 3. 反向计分处理
  processed_data <- data

  # 生成需要反向的列名并验证
  message("\nChecking column names in data:")
  print(head(names(processed_data)))

  # 检查所有题目的列名是否存在
  all_items <- 1:36
  expected_cols <- paste0(prefix, sprintf("%02d", all_items))
  missing_cols <- expected_cols[!expected_cols %in% names(processed_data)]

  if(length(missing_cols) > 0) {
    message("\nWarning: Some expected columns are missing:")
    print(missing_cols)
  }

  # 执行反向计分
  reverse_cols <- paste0(prefix, sprintf("%02d", reverse_items))
  message("\nProcessing reverse scoring for columns:")
  print(reverse_cols)

  for(col in reverse_cols) {
    if(col %in% names(processed_data)) {
      processed_data[[col]] <- 4 - processed_data[[col]]
      message(paste("Processed:", col))
    }
  }

  # 4. 计算分量表得分
  if(subscales) {
    message("\nCalculating subscale scores:")
    for(scale_name in names(subscale_info)) {
      scale_items <- paste0(prefix, sprintf("%02d", subscale_info[[scale_name]]))
      scale_col <- paste0(prefix, "_", scale_name)

      # 检查所需的列是否都存在
      missing_items <- scale_items[!scale_items %in% names(processed_data)]
      if(length(missing_items) > 0) {
        message(paste("\nWarning: Missing items for", scale_name, "subscale:"))
        print(missing_items)
        next
      }

      message(paste("\nCalculating", scale_name, "subscale score using columns:"))
      print(scale_items)

      processed_data[[scale_col]] <- rowMeans(processed_data[scale_items], na.rm = TRUE)
    }
  }

  # 5. 准备报告
  report <- list(
    reverse_items = reverse_items,
    reverse_cols_processed = reverse_cols[reverse_cols %in% names(processed_data)],
    subscales = subscale_info,
    subscales_computed = if(subscales) names(subscale_info) else NULL,
    missing_columns = missing_cols
  )

  return(list(
    data = processed_data,
    report = report
  ))
}
