#' Handle Scoring for Personality Disposition Scales
#'
#' @title 人格倾向量表计分处理
#' @description 处理YPI、TFQ-20、CADS、ICU和RPAQ的反向计分和维度分计算
#'
#' @param data 数据框
#' @param rater 评分者类型，"child"或"parent"
#' @param time_col 时间列名，默认为"eventname"
#' @param subscales 是否计算分量表得分，默认TRUE
#'
#' @return 处理后的数据列表
#' @export
#'
# ThS - Thrill Seeking（寻求刺激）
# Imp - Impulsiveness（冲动）
# Irr - Irresponsibility（不负责任）
# Uno - Unemotionality（缺乏情感）
# ReM - Remorselessness（无悔恨感）
# Cal - Callousness（冷酷）
# DCr - Dishonest Charm（欺诈性魅力）
# Ly - Lying（说谎）
# Grd - Grandiosity（夸大妄想）
# Mnp - Manipulation（操纵他人）
# IIR - Impulsive-Irresponsible（冲动-不负责任）
# CUE - Callous-Unemotional（冷酷-缺乏情感）
# GrM - Grandiose-Manipulative（夸大-操纵）
# Pso - Prosociality（亲社会行为）# 父母也有
# CUEp - Callous-Unemotional Prosocial（冷酷-缺乏情感亲社会）# 父母也有
# PAg - Proactive Aggression（主动攻击）# 父母也评
# RAg - Reactive aggession （反应性攻击）# 父母也评

reverse_pd <- function(data,
                     rater = c("child", "parent"),
                     time_col = "eventname",
                     subscales = TRUE) {

  # 1. 输入检查
  rater <- match.arg(rater)

  # 2. 定义量表结构
  pdstructure <- list(
    child = list(
      # YPI部分
      ypi = list(
        prefix = "Cpd",
        items = 1:50,
        reverse_items = c(23, 35, 49),
        subscales = list(
          thrill_seeking = c(1, 4, 22, 29, 42),
          impulsiveness = c(3, 9, 18, 26, 32),
          irresponsibility = c(5, 13, 16, 34, 40),
          unemotionality = c(2, 25, 36, 39, 45),
          remorselessness = c(8, 21, 28, 44, 48),
          callousness = c(12, 17, 23, 35, 49),
          dishonest_charm = c(6, 14, 27, 33, 38),
          lying = c(7, 24, 43, 47, 50),
          grandiosity = c(10, 19, 30, 37, 41),
          manipulation = c(11, 15, 20, 31, 46)
        ),
        dimensions = list(
          impulsive_irresponsible = c("thrill_seeking", "impulsiveness", "irresponsibility"),
          callous_unemotional = c("unemotionality", "remorselessness", "callousness"),
          grandiose_manipulative = c("dishonest_charm", "lying", "grandiosity", "manipulation")
        )
      ),

      # TFQ-20部分
      tfq = list(
        prefix = "Cpd",
        items = 78:97,
        reverse_items = c(78, 79, 84, 85, 86, 89, 96, 97)
      ),

      # CADS和ICU部分
      prosocial = list(
        prefix = "Cpd",
        items = 51:77,
        reverse_items = c(62, 64, 65, 66, 73, 74, 75),
        subscales = list(
          prosociality = c(51, 52, 53, 54, 59, 60, 61, 63, 68, 69, 71, 72),
          callous_unemotional = c(55, 56, 57, 58, 62, 64, 65, 66, 67, 70, 73, 74, 75, 76, 77)
        )
      ),

      # RPAQ部分
      aggression = list(
        prefix = "Cpd",
        items = 99:121,
        subscales = list(
          proactive = 99:110,
          reactive = 111:121
        )
      )
    ),

    parent = list(
      prosocial = list(
        prefix = "Ppd",
        items = 1:27,
        reverse_items = c(11, 13, 14, 15, 23, 24, 26),
        subscales = list(
          prosociality = c(1, 2, 3, 4, 8, 9, 10, 12, 18, 22, 20, 21),
          callous_unemotional = c(17, 6, 7, 5, 11, 13, 14, 15, 16, 19, 23, 24, 26, 25, 27)
        )
      ),

      aggression = list(
        prefix = "Ppd",
        items = 29:51,
        subscales = list(
          proactive = 29:40,
          reactive = 41:51
        )
      )
    )
  )

  # 3. 数据处理
  processed_data <- data
  report <- list()

  # 获取当前评分者的结构
  current_structure <- pdstructure[[rater]]

  # 4. 处理每个量表
  for(scale_name in names(current_structure)) {
    scale_info <- current_structure[[scale_name]]
    prefix <- scale_info$prefix

    # 4.1 反向计分
    if(!is.null(scale_info$reverse_items)) {
      reverse_cols <- paste0(prefix, sprintf("%02d", scale_info$reverse_items))

      for(col in reverse_cols) {
        if(col %in% names(processed_data)) {
          # YPI和Prosocial用0-3分
          if(scale_name %in% c("ypi", "prosocial", "tfq")) {
            processed_data[[col]] <- 3 - processed_data[[col]]
          } else { # RPAQ用0-2分
            processed_data[[col]] <- 2 - processed_data[[col]]
          }
        }
      }
    }

    # 4.2 计算分量表分数
    if(subscales && !is.null(scale_info$subscales)) {
      for(sub_name in names(scale_info$subscales)) {
        items <- scale_info$subscales[[sub_name]]
        sub_cols <- paste0(prefix, sprintf("%02d", items))
        scale_col <- paste0(prefix, scale_name, "_", sub_name)

        if(all(sub_cols %in% names(processed_data))) {
          processed_data[[scale_col]] <- rowMeans(processed_data[sub_cols],
                                                  na.rm = TRUE)
        }
      }
    }

    # 4.3 计算维度分数（仅YPI）
    if(scale_name == "ypi" && !is.null(scale_info$dimensions)) {
      for(dim_name in names(scale_info$dimensions)) {
        # 获取该维度包含的分量表
        sub_scales <- scale_info$dimensions[[dim_name]]
        # 创建一个数据框来存储分量表分数
        dim_scores <- sapply(sub_scales, function(sub) {
          scale_col <- paste0(prefix, scale_name, "_", sub)
          if(scale_col %in% names(processed_data)) {
            return(processed_data[[scale_col]])
          } else {
            # 如果分量表分数不存在，直接计算
            items <- scale_info$subscales[[sub]]
            item_cols <- paste0(prefix, sprintf("%02d", items))
            if(all(item_cols %in% names(processed_data))) {
              return(rowMeans(processed_data[item_cols], na.rm = TRUE))
            }
          }
        }, simplify = FALSE) # 确保返回的是一个列表

        # 过滤掉NULL值
        dim_scores <- dim_scores[!sapply(dim_scores, is.null)]

        # 确保 dim_scores 是数值矩阵
        if(length(dim_scores) > 0 && all(sapply(dim_scores, is.numeric))) {
          dim_col <- paste0(prefix, scale_name, "_dim_", dim_name)
          # 使用do.call和cbind来合并分数，确保结果是一个矩阵
          dim_matrix <- do.call(cbind, dim_scores)
          processed_data[[dim_col]] <- rowMeans(dim_matrix, na.rm = TRUE)
        }
      }
    }
  }

  # 5. 准备报告
  report$scales_processed <- names(current_structure)
  report$subscales_computed <- if(subscales) {
    lapply(current_structure, function(x) names(x$subscales))
  } else NULL

  return(list(
    data = processed_data,
    report = report
  ))
}
