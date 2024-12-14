#' Simple PD Score Calculator
#'
#' @param data 数据框
#' @param rater "child" 或 "parent"
#' @return 处理后的数据
score_pd <- function(data, rater) {
  result <- data

  if(rater == "child") {
    # YPI 10个分量表
    ypi_subscales <- list(
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
    )

    # 计算YPI分量表
    for(scale_name in names(ypi_subscales)) {
      items <- ypi_subscales[[scale_name]]
      cols <- paste0("cpd_", sprintf("%03d", items))
      if(all(cols %in% names(result))) {
        result[[paste0("cpd_ypi_", scale_name)]] <- rowMeans(result[cols], na.rm = TRUE)
      }
    }

    # YPI三个维度
    ypi_dimensions <- list(
      impulsive_irresponsible = c("thrill_seeking", "impulsiveness", "irresponsibility"),
      callous_unemotional = c("unemotionality", "remorselessness", "callousness"),
      grandiose_manipulative = c("dishonest_charm", "lying", "grandiosity", "manipulation")
    )

    # 计算YPI维度
    for(dim_name in names(ypi_dimensions)) {
      sub_scales <- ypi_dimensions[[dim_name]]
      scale_cols <- paste0("cpd_ypi_", sub_scales)
      if(all(scale_cols %in% names(result))) {
        result[[paste0("cpd_ypi_dim_", dim_name)]] <- rowMeans(result[scale_cols], na.rm = TRUE)
      }
    }

    # 反向计分项目
    reverse_items <- c(23, 35, 49)
    reverse_cols <- paste0("cpd_", sprintf("%03d", reverse_items))
    for(col in reverse_cols) {
      if(col %in% names(result)) {
        result[[col]] <- 3 - result[[col]]
      }
    }

    # Aggression分量表
    result$cpd_aggression_proactive <- rowMeans(result[paste0("cpd_", sprintf("%03d", 99:110))], na.rm = TRUE)
    result$cpd_aggression_reactive <- rowMeans(result[paste0("cpd_", sprintf("%03d", 111:121))], na.rm = TRUE)

  } else if(rater == "parent") {
    # Parent Prosocial分量表
    prosocial_items <- c(1:4, 8:10, 12, 18, 20:22)
    callous_items <- c(5:7, 11, 13:17, 19, 23:27)

    # 计算Parent分量表
    result$ppd_prosocial_prosociality <- rowMeans(result[paste0("ppd_", sprintf("%02d", prosocial_items))], na.rm = TRUE)
    result$ppd_prosocial_callous_unemotional <- rowMeans(result[paste0("ppd_", sprintf("%02d", callous_items))], na.rm = TRUE)

    # Parent Aggression分量表
    result$ppd_aggression_proactive <- rowMeans(result[paste0("ppd_", sprintf("%02d", 29:40))], na.rm = TRUE)
    result$ppd_aggression_reactive <- rowMeans(result[paste0("ppd_", sprintf("%02d", 41:51))], na.rm = TRUE)

    # Parent反向计分项目
    reverse_items <- c(11, 13, 14, 15, 23, 24, 26)
    reverse_cols <- paste0("ppd_", sprintf("%02d", reverse_items))
    for(col in reverse_cols) {
      if(col %in% names(result)) {
        result[[col]] <- 3 - result[[col]]
      }
    }
  }

  return(result)
}
