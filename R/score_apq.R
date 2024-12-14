#' Handle Scoring for Alabama Parenting Questionnaire
#'
#' @title APQ量表计分处理
#' @description 处理Alabama Parenting Questionnaire (APQ)的维度分计算
#'
#' @param data 数据框
#' @param rater 评分者类型，"child"或"parent"
#' @param time_col 时间列名，默认为"eventname"
#' @param subscales 是否计算分量表得分，默认TRUE
#'
#' @return 处理后的数据列表
#' @export
#'
#' PI - Parental Involvement（父母参与）
#' PP - Positive Parenting（积极养育）
#' PMS - Poor Monitoring/Supervision（监管不足）
#' ID - Inconsistent Discipline（惩戒不一致）
#' CP - Corporal Punishment（体罚）

score_apq <- function(data,
                      rater = c("child", "parent"),
                      time_col = "eventname",
                      subscales = TRUE) {

  # 1. 输入检查
  rater <- match.arg(rater)

  # 2. 定义量表结构
  apq_structure <- list(
    child = list(
      prefix = "capq_",
      items = 1:42,
      reverse_items = NULL,  # APQ没有反向计分题目
      subscales = list(
        parental_involvement = c(1, 4, 7, 9, 11, 14, 15, 20, 23, 26),
        positive_parenting = c(2, 5, 13, 16, 18, 27),
        poor_monitoring = c(6, 10, 17, 19, 21, 24, 28, 29, 30, 32),
        inconsistent_discipline = c(3, 8, 12, 22, 25, 31),
        corporal_punishment = c(33, 35, 38)
      )
    ),

    parent = list(
      prefix = "papq_",
      items = 1:42,
      reverse_items = NULL,  # APQ没有反向计分题目
      subscales = list(
        parental_involvement = c(1, 4, 7, 9, 11, 14, 15, 20, 23, 26),
        positive_parenting = c(2, 5, 13, 16, 18, 27),
        poor_monitoring = c(6, 10, 17, 19, 21, 24, 28, 29, 30, 32),
        inconsistent_discipline = c(3, 8, 12, 22, 25, 31),
        corporal_punishment = c(33, 35, 38)
      )
    )
  )

  # 3. 数据处理
  processed_data <- data
  report <- list()

  # 获取当前评分者的结构
  current_structure <- apq_structure[[rater]]
  prefix <- current_structure$prefix

  # 4. 计算分量表分数
  if(subscales && !is.null(current_structure$subscales)) {
    for(sub_name in names(current_structure$subscales)) {
      items <- current_structure$subscales[[sub_name]]
      sub_cols <- paste0(prefix, sprintf("%02d", items))
      scale_col <- paste0(prefix, "scale_", sub_name)

      # 检查所需的列是否都存在
      if(all(sub_cols %in% names(processed_data))) {
        # 计算平均分
        processed_data[[scale_col]] <- rowMeans(processed_data[sub_cols],
                                                na.rm = TRUE)
      }
    }
  }

  # 5. 准备报告
  report$scales_processed <- "APQ"
  report$subscales_computed <- if(subscales) {
    names(current_structure$subscales)
  } else NULL

  # 6. 添加量表信息到报告
  report$scale_info <- list(
    n_items = length(current_structure$items),
    valid_range = c(0, 4),
    subscales = current_structure$subscales
  )

  return(list(
    data = processed_data,
    report = report
  ))
}
