#' Prepare Longitudinal Survey Data
#'
#' @title Prepare ABCD Longitudinal Survey Data
#' @description Process longitudinal survey data from ABCD study, including data cleaning,
#'              renaming, and careless response detection
#'
#' @param data A data frame containing the survey data
#' @param id_col The name of the ID column (default: "src_subject_id")
#' @param time_col The name of the time column (default: "eventname")
#' @param item_prefix The prefix of item columns (default: "socialdev_")
#' @param remove_careless Logical, whether to remove careless responses
#' @param rename_items Logical, whether to rename variables
#'
#' @return A list containing:
#'   \itemize{
#'     \item data: processed data frame
#'     \item common_ids: IDs present in all waves
#'     \item report: processing report
#'   }
#' @import ufs
#' @export
#'
#' @examples
#' \dontrun{
#' result <- prepare_long_data(sd_p_nbh)
#' }

prepare_long_data <- function(data,
                              id_col = "src_subject_id",
                              time_col = "eventname",
                              item_prefix = "socialdev_",
                              remove_careless = TRUE,
                              rename_items = TRUE) {

  # 1. 检查输入
  if(!all(c(id_col, time_col) %in% names(data))) {
    stop("ID or time column not found in data")
  }

  # 2. 获取所有唯一的时间点并创建有序映射
  unique_waves <- unique(data[[time_col]])
  wave_patterns <- data.frame(
    wave = unique_waves,
    time = sapply(unique_waves, function(x) {
      as.numeric(gsub(".*wave(\\d+).*", "\\1", x))
    })
  )
  wave_patterns <- wave_patterns[order(wave_patterns$time), ]

  # 3. 识别所有item列
  all_items <- grep(item_prefix, names(data), value = TRUE)
  base_items <- all_items[!grepl("_l$", all_items)]
  follow_items <- all_items[grepl("_l$", all_items)]

  # 4. 创建变量重命名函数
  create_new_name <- function(old_name) {
    # 从完整变量名中提取类型（pneigh）和数字
    # 比如从 "socialdev_pneigh_01" 提取 "pneigh" 和 "01"
    type <- gsub(".*_([^_]+)_\\d+.*", "\\1", old_name)  # 提取 pneigh
    item_num <- gsub(".*_(\\d+).*", "\\1", old_name)    # 提取数字

    # 首字母大写
    type <- paste0(toupper(substring(type, 1, 1)),
                   substring(type, 2))

    # 组合新名称 (Pneigh01)
    paste0(type, sprintf("%02d", as.numeric(item_num)))
  }

  # 5. 分别处理每个时间点的数据
  data_list <- list()

  # 跟踪每个wave的处理过程
  wave_details <- list()

  # 分别处理每个时间点
  for(i in seq_along(wave_patterns$wave)) {
    wave <- wave_patterns$wave[i]
    time <- wave_patterns$time[i]

    wave_report <- list()
    message("\n=== Processing wave ", i, " ===")

    # 记录原始样本量
    wave_report$original_n <- sum(data[[time_col]] == wave)
    message(paste("Original n:", wave_report$original_n))

    # 选择当前时间点的数据
    if(i == 1) {
      curr_data <- data[data[[time_col]] == wave, c(id_col, time_col, base_items)]
      message("\nProcessing base items:")
      message("First 6 base items:")
      print(head(base_items))
      message(paste("Total base items:", length(base_items)))
    } else {
      curr_data <- data[data[[time_col]] == wave, c(id_col, time_col, follow_items)]
      message("\nProcessing follow-up items:")
      message("First 6 follow items:")
      print(head(follow_items))
      message(paste("Total follow items:", length(follow_items)))
    }

    message("\nCurrent column names (first 6):")
    print(head(names(curr_data)))
    message(paste("Total columns:", length(names(curr_data))))

    # 重命名列
    if(rename_items) {
      message("\nBefore renaming:")
      print(head(names(curr_data)))

      old_names <- names(curr_data)[-(1:2)]
      new_names <- sapply(if(i == 1) base_items else follow_items, create_new_name)

      message("\nName transformation examples:")
      for(j in 1:min(3, length(old_names))) {
        message(paste(old_names[j], "->", new_names[j]))
      }

      names(curr_data)[-(1:2)] <- new_names

      message("\nAfter renaming:")
      print(head(names(curr_data)))
    }

    # 处理不认真作答
    row.names(curr_data) <- NULL #重置行号
    if(remove_careless && nrow(curr_data) > 0) {
      require(ufs)
      item_cols <- names(curr_data)[-(1:2)]
      careless_obj <- carelessObject(data = curr_data, items = item_cols)
      suspect_rows <- suspectParticipants(careless_obj)
      curr_data <- curr_data[-as.numeric(rownames(suspect_rows)), ]

      # 记录删除的不认真作答数量
      wave_report$careless_removed <- length(rownames(suspect_rows))
    } else {
      wave_report$careless_removed <- 0
    }

    # 记录最终样本量
    wave_report$final_n <- nrow(curr_data)

    # 添加时间指标
    curr_data$time <- time

    data_list[[i]] <- curr_data
    wave_details[[wave]] <- wave_report
  }

  # 合并所有时间点的数据
  final_data <- do.call(rbind, data_list)
  row.names(final_data) <- NULL

  # 找出所有时间点的共同ID
  common_ids <- Reduce(intersect, lapply(data_list, function(x) unique(x[[id_col]])))

  # 准备详细报告
  report <- list(
    original_n = nrow(data),
    waves = wave_patterns,
    wave_details = wave_details,    # 每个wave的详细信息
    samples_by_wave = sapply(data_list, nrow),
    common_subjects = length(common_ids),
    final_n = nrow(final_data)
  )

  return(list(
    data = final_data,
    common_ids = common_ids,
    report = report
  ))
}
