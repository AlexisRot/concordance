library(tidyr)
library(dplyr)

get_lloq_list <- function(data_file, list_var_x, list_var_y, lloq_x_values, lloq_y_values, 
                          output_folder = "output/output_report/"){
  list_var_x <- gsub(" ", ".", list_var_x)
  list_var_y <- gsub(" ", ".", list_var_y)
  index_table <- data.frame("Col" = c(list_var_x, list_var_y),
                            "Values" = c(lloq_x_values,lloq_y_values))

  # Create a list of value name_var = value LOQ
  index_list <- as.list(index_table$Values)
  thresholds <- setNames(index_list, index_table$Col)
  return(thresholds)
}

get_data_lloq <- function(data_file_name, list_var_x, list_var_y, lloq_x_values, lloq_y_values, 
                          output_folder = "output/output_report/"){
  path_data <- paste0("data/",data_file_name,".csv")
  data <- read.csv2(path_data)
  data_num <- data %>% select(-1) %>% mutate_if(is.character, as.numeric)
  data_num <- data %>% mutate_if(is.character, as.numeric)
  # data <- cbind(data[identifier],data_num)
  
  index_table <- data.frame("Col" = c(list_var_x, list_var_y),
                            "Values" = c(lloq_x_values, lloq_y_values))
  
  # Create a list of value name_var = value LOQ
  index_list <- as.list(index_table$Values)
  thresholds <- setNames(index_list, index_table$Col)
  
  # Create a condition for filtering
  filter_condition <- Reduce(`&`, lapply(seq_along(thresholds), function(i) data_num[[i]] > thresholds[[i]]))
  
  # Filter rows based on the condition
  data_lloq <- data[!(filter_condition), ]
  
  #Remove lines with NA values
  data_lloq <- na.omit(data_lloq)
  
  write.csv(data_lloq, "output/output_report/data_lloq.csv")
}





#' Get Descriptive Statistics for LLOQ
#'
#' @param data : Dataset for the concordance
#'
#' @return : Table of the descriptive statistics
#'
lloq_stat_desc <- function(data, list_var_x, list_var_y, lloq_x_values, lloq_y_values){
  
  
  thresholds <- get_lloq_list(data, list_var_x, list_var_y, lloq_x_values, lloq_y_values)
  data_lloq <- read.csv("output/output_report/data_lloq.csv")
  
  count_values <- sapply(names(data_lloq), function(col_name) {
    sum(data_lloq[[col_name]] < thresholds[[col_name]])})
  count_values <- as.data.frame(count_values)
  count_values <- cbind(variables = rownames(count_values), count_values)
  print(count_values)

  data_lloq <- data %>%    
    select(-1) %>%  # Exclude the first column
    group_by(variables) %>%
    summarise(
      `Total number of results` = n(),
      `Missing` = sum(is.na(variables))) %>%
    rowwise()
  
  print("data_lloq")
  print(data_lloq)
  data_lloq <- merge(data_lloq, count_values, by = "variables")
  print("data_lloq")
  print(data_lloq)
  # data_lloq <- t(data_lloq)
  data_lloq <- as.data.frame(data_lloq)
  # names(data_lloq) <- data_lloq[1,]
  # data_lloq <- data_lloq[-1,]
  # data_lloq <- tibble::rownames_to_column(data_lloq, "Parameters")
  print(ncol(data_lloq))
  print(data_lloq)
  data_lloq["Number of analyzed results"] <- data_lloq[,2] - data_lloq[,3] - data_lloq[,4]
  
  
  return(data_lloq)
}





#' Get Descriptive Statistics of the long dataset
#'
#' @param df_long : Dataset used for the concordance in long format
#'
#' @return : Table with descriptive statistics for each variables selected
#'
desc_stat <- function(df_long) {
  desc <- df_long %>% 
    select(-1) %>%  # Exclude the first column
    group_by(variables) %>%
    summarise(
      `N samples` = n(),
      Mean = mean(titers, na.rm = TRUE),
      `Mean (log10)` = mean(log10(titers), na.rm = TRUE),
      `Variance (log10)` = var(log10(titers), na.rm = TRUE),
      `Standard deviation (log10)` = sd(log10(titers), na.rm = TRUE),
      Min = min(titers, na.rm = TRUE),
      Max = max(titers, na.rm = TRUE)
    ) %>%
    mutate(across(where(is.numeric), ~round(., 2)))  # Round all numeric columns
  
  return(desc)
}



#' Get Descriptive Statistics of the dataset used for the concordance
#'
#' @param data.file : Dataset in wide format (Sample ID, var X, var Y)
#' @param list_var.x : List of the variables labelled as X in the dataset. Ex: c(var1.x, var2.x)
#' @param list_var.y : List of the variables labelled as Y in the dataset. Ex: c(var1.y, var2.y)
#'
#' @return : 2 tables saved in /output_report/: descriptive_stat.csv & and descriptive_stat_lloq.csv
#'
get_desc_stat <- function(data_file, list_var_x, list_var_y, lloq_x_values, lloq_y_values,
                          output_folder = "output/output_report/") {
  # Read the dataset
  path_data <- paste0("data/", data_file, ".csv")
  data <- read.csv(path_data, sep =";")
  print(head(data))
  
  # Combine variable lists and convert data to long format
  list_var_x <- gsub(" ", ".", list_var_x)
  list_var_y <- gsub(" ", ".", list_var_y)
  list_var <- c(list_var_x, list_var_y)
  data_long <- gather(data, variables, titers, list_var)
  data_long <- data_long %>% select(-1, variables, titers)  # Exclude the first column directly
  data_long$titers <- as.numeric(data_long$titers)
  data_long$log_titers <- log10(data_long$titers)
  
  # Get descriptive statistics and write to CSV
  descriptive_table <- desc_stat(data_long)
  write.csv(descriptive_table, paste0(output_folder, "descriptive_stat.csv"), row.names = FALSE, sep = ";")
  
  # Get descriptive statistics for LLOQ and write to CSV
  stat_desc_lloq <- lloq_stat_desc(data_long, list_var_x, list_var_y, lloq_x_values, lloq_y_values)
  write.csv(stat_desc_lloq, paste0(output_folder, "descriptive_stat_lloq.csv"), row.names = FALSE, sep = ";")
}



#' Get tables of information and rho values for the #n concordance analysis executed
#'
#' @return : 2 Tables saved in  /output_report/: info.csv (n_samples) & rho.csv (rho values)
#'
get_rho <- function(){
  list_path <- list.files(path = output.folder,
                          full.names = FALSE, recursive = FALSE)
  i <- 1
  list_analysis <- c()
  list_rho <- c()
  list_pval <- c()
  list_n <- c()

  for (p in list_path){
    if (grepl(pattern = "concordance", x = p)){

      path_concordance <- paste0(output.folder,"/",p,"/table_concordance.csv")
      concordance_table <- read.csv(path_concordance)
      pvalue <- concordance_table$Estimates[4]
      list_pval <- c(list_pval,pvalue)

      path_rho <- paste0(output.folder,"/",p,"/table_GMT.csv")
      GMT <- read.csv(path_rho)
      rho <- round(GMT$Estimates[4],4)
      list_rho <- c(list_rho,rho)
      list_analysis <- c(list_analysis,p)

      path_info <- paste0(output.folder,"/",p,"/Qualification.Table.Info.csv")
      df.info <- read.csv(path_info)
      list_n <- df.info$Size.of.Data[1]
    }
  }

  df_rho <- data.frame(list_analysis, list_rho, list_pval)
  df_info <- data.frame(list_analysis, list_n)

  write.csv(df_rho,paste0(output.folder,"/output_report/rho.csv"), row.names = FALSE)
  write.csv(df_info,paste0(output.folder,"/output_report/info.csv"), row.names = FALSE)
}

#' Format tables in the RMD report
#'
#' @param df
#'
#' @return : Formatted tables that fits in the report
#'
format_table <- function(df){
  df_ft <- flextable(data = df,
                     col_keys = names(df))
  df_ft <- df_ft %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    theme_box()

  df_ft <- fontsize(x = df_ft, size = 9)
  df_ft <- font(x = df_ft, fontname = "arial")

  return(df_ft)
}



