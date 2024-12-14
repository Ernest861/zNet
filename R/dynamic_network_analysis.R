#' Dynamic Panel Network Analysis for Longitudinal Data
#' @param data 数据框，包含id_col、time_co和测量变量们
#' @param id_col ID列名
#' @param time_col 时间列名
#' @param vars 要分析的变量名向量
#' @param groups 分组列表，是对 vars 的分组，用以上色
#' @param threshold 网络阈值，三网络通用
#' @param plot_colors 绘图连边颜色的简便设置
#' @param output_dir 输出PDF的目录路径
#' @param file_prefix 输出文件名前缀
#' @return 返回动态网络分析结果列表
#' @export

dynamic_network_analysis <- function(data,
                                     id_col = "src_subject_id",
                                     time_col = "time",
                                     vars = NULL,
                                     groups = NULL,
                                     color = zcolor,
                                     threshold = 0.101,
                                     plot_colors = list(
                                       pos = c("#2376b7","#134857"),
                                       neg = c("#d2568c","#62102e")
                                     ),
                                     output_dir = getwd(),
                                     file_prefix = "dynamicNetwork_dlvm1") {

  # 1. 检查和创建输出目录
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # 1. 数据预处理
  ## 1.1 找出共同ID
  common_ids <- Reduce(intersect,
                       lapply(unique(data[[time_col]]),
                              function(t) data[data[[time_col]]==t, id_col]))

  ## 1.2 选择数据和重命名
  selected_data <- data[data[[id_col]] %in% common_ids,
                        c(id_col, time_col, vars)]

  ## 1.3 长宽转换
  long_data <- selected_data %>%
    pivot_longer(cols = all_of(vars),
                 names_to = "variable",
                 values_to = "value") %>%
    mutate(time = as.factor(!!sym(time_col))) %>%
    group_by(!!sym(id_col), variable, time) %>%
    summarise(value = first(value), .groups = 'drop') %>%
    mutate(across(everything(), ~replace_na(., 0)))

  wide_data <- long_data %>%
    pivot_wider(
      names_from = c(variable, time),
      values_from = value,
      names_glue = "{variable}_{time}"
    )

  # 2. 创建设计矩阵
  design <- matrix(NA,
                   nrow = length(vars),
                   ncol = length(unique(data[[time_col]])),
                   dimnames = list(vars,
                                   paste0("T", unique(data[[time_col]]))))

  for(i in seq_along(vars)) {
    for(j in seq_len(ncol(design))) {
      design[i,j] <- paste0(vars[i], "_", j)
    }
  }

  # 3. 拟合模型
  model <- dlvm1(data = wide_data,
                 vars = design,
                 within_latent = "ggm",
                 between_latent = "ggm",
                 within_residual = "chol",
                 between_residual = "chol",
                 estimator = "ML",
                 start = "version2",
                 verbose = TRUE) %>%
    runmodel

  # 4. 提取网络
  networks <- list(
    temporal = t(getmatrix(model, "PDC")),
    contemporaneous = t(getmatrix(model, "omega_zeta_within")),
    between = t(getmatrix(model, "omega_zeta_between"))
  )

  # 5. 计算节点大小
  node_sizes <- list(
    temporal = get_strength_node_size(Centrality(networks$temporal)) + 4.5,
    contemporaneous = get_strength_node_size(Centrality(networks$contemporaneous)) + 4.5,
    between = get_strength_node_size(Centrality(networks$between)) + 4.5
  )

  # 6. 绘图函数
  plot_networks <- function(filename = NULL) {
    plot_func <- function() {
      layout(t(1:3))

      # Temporal Network
      qgraph(networks$temporal,
             title = "Temporal network",
             threshold = threshold,
             label.cex = 0.8,
             vsize = node_sizes$temporal,
             asize = 10,
             groups = groups,
             color = color,
             labels = vars,
             edge.labels = TRUE,
             edge.label.cex = 2,
             posCol = plot_colors$pos,
             negCol = plot_colors$neg,
             legend = FALSE,
             GLratio = 5,
             layoutOffset = c(0.03,0))

      # Contemporaneous Network
      qgraph(networks$contemporaneous,
             title = "Contemporaneous network",
             threshold = threshold,
             label.cex = 0.8,
             vsize = node_sizes$contemporaneous,
             groups = groups,
             color = color,
             labels = vars,
             edge.labels = TRUE,
             edge.label.cex = 2,
             posCol = plot_colors$pos,
             negCol = plot_colors$neg,
             legend = FALSE,
             GLratio = 5,
             layoutOffset = c(0.03,0))

      # Between Network
      qgraph(networks$between,
             title = "Between network",
             threshold = threshold,
             label.cex = 0.8,
             vsize = node_sizes$between,
             groups = groups,
             color = color,
             labels = vars,
             edge.labels = TRUE,
             edge.label.cex = 2,
             posCol = plot_colors$pos,
             negCol = plot_colors$neg,
             legend = TRUE,
             GLratio = 5,
             layoutOffset = c(0.03,0))
    }

    if (!is.null(filename)) {
      # 构建完整的文件路径
      full_path <- file.path(output_dir, filename)
      pdf(full_path, height = 6, width = 16)
      plot_func()
      dev.off()
      message(paste("Plot saved to:", full_path))
    } else {
      plot_func()
    }
  }

  # 自动生成文件名并绘图
  default_filename <- sprintf("%s_%s.pdf", file_prefix, threshold)
  plot_networks(default_filename)

  # 7. 返回结果
  return(list(
    model = model,
    networks = networks,
    node_sizes = node_sizes,
    plot = plot_networks,
    fit = fit(model),
    output_path = file.path(output_dir, default_filename)
  ))
}

# 使用示例：
# result <- dynamic_network_analysis(
#   data = your_data,
#   vars = your_vars,
#   groups = your_groups,
#   output_dir = "output/networks",
#   file_prefix = "my_analysis"
# )
