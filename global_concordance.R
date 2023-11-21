
#' Creation of an empty folder
#'
#' @param main_dir : directory where the folder needs to be created
#' @param sub_dir : Name of the folder to create
#'
#' @return : creation of a folder in main_dir
#' @export
#'
#' @examples
#' \dontrun{
#' create_folder(main_dir = "", sub_dir = "new_folder")
#' }
create_folder <- function(main_dir,sub_dir){

  output_dir <- file.path(main_dir, sub_dir)

  if (!dir.exists(output_dir)){
    # print("Dir doesn't exist!")
  }
  else {
    # print("Dir already exists!")
    unlink(output_dir, recursive = TRUE)
    # print(paste0("Files in ",sub_dir," deleted !"))
  }

  dir.create(output_dir)
  # print(paste0("Files ",sub_dir," created !"))
}


#' Get results of the concordance
#'
#' @param output : specific table to export ("concordance_table", "SAT_table", "GMT_table", "ABE_table")
#' @param c_results : Results exported from QUALIFICATION.R file
#'
#' @return : Table output in format
concordance_tables <- function(output, c_results){
  # print("---------------- ## CONCORDANCE table results ## ----------------");
  t0 <- Sys.time()

  print(output)
  if (output == "concordance_table"){
    CT = c_results$Qualification.Table
    Concordance.table = data.frame(Parameter = c(CT[1,1],CT[5,1],CT[8,1],CT[12,1],CT[13,1]),
                                   'Estimates' = c(CT[1,2],CT[5,2],CT[8,2],CT[12,2],CT[13,2]),
                                   '95%LCL' = c(CT[1,3],CT[5,3],CT[8,3],CT[12,3],CT[13,3]),
                                   '95%UCL' = c(CT[1,4],CT[5,4],CT[8,4],CT[12,4],CT[13,4]))
    return(Concordance.table)
  }

  if (output == "SAT_table"){
    SAT = c_results$Serostatus.Agreement.Table
    print(SAT)

    if (dim(SAT)[1] == 7){
      Serostatus.Agreement.Table = data.frame('Dest Lab Negative' = SAT[1,],
                                              'Dest Lab Positive' = SAT[2,],
                                              'Sub Total' = SAT[3,],
                                              'PCT of sample in agreement' = SAT[3,])
      # names(Serostatus.Agreement.Table) <- names(SAT)
      print(t(Serostatus.Agreement.Table))
      Serostatus.Agreement.Table <- t(Serostatus.Agreement.Table)
      row.names(Serostatus.Agreement.Table) <- NULL
      return(Serostatus.Agreement.Table)
    }

  }

  if (output == "PCT_protected_table"){
    PCT = c_results$Serostatus.Agreement.Table

    if (dim(PCT)[1] == 7){
      Serostatus.Agreement.Table = data.frame('% Protected in Ref Lab(%)' = PCT[5,],
                                              '% Protected in Dest Lab(%)' = PCT[6,],
                                              'Diff protected (%)' = PCT[7,])
      Serostatus.Agreement.Table <- t(Serostatus.Agreement.Table)
      row.names(Serostatus.Agreement.Table) <- NULL
      return(Serostatus.Agreement.Table)
    }

  }

  if (output == "GMT_table"){
    GMT.Ratio.Table = c_results$GMT.Ratio.Table
    GMT.Ratio.Table = GMT.Ratio.Table[c(1,2,4,5),]
    GMT.Ratio.Table <- as.data.frame(GMT.Ratio.Table)
    return(GMT.Ratio.Table)
  }

  if (output == "ABE_table"){
    ABE.Table = c_results$AltmanBland.Table
    ABE.Table = ABE.Table[c(1,2),c("Parameter","Estimates","95%LCL","95%UCL")]
    ABE.Table <- as.data.frame(ABE.Table)
    return(ABE.Table)
  }

}



#' Execute 1 concordance analysis by calling QUALIFICATION.R
#'
#' @param dataframe_concordance : name of a data set without the extension (.csv, .xlsx)
#' @param logbase : log base to run the analysis (10)
#' @param var.x : Variable X of the concordance analysis ("Var.x")
#' @param var.y : Variable Y of the concordance analysis ("Var.y")
#' @param var.x.name : Name of the variable X of the concordance analysis ("Variable X")
#' @param var.y.name : Name of the variable Y of the concordance analysis ("Variable Y")
#' @param x.lloq : Numerical value of the LLOQ for var.x (40)
#' @param y.lloq : Numerical value of the LLOQ for var.y (40)
#' @param sub_folder.name : Name of the folder where the concordance will be exported
#' @param REG.Plot.step : Numerical
#' @param REG.Plot.title : Title the concordance graph ("Concordance plot)
#' @param AB.Plot.Log.Diff : Boolean
#' @param AB.Draw.Mean.STD : Boolean
#' @param AB.Draw.Perfect.Line : Boolean
#' @param AB.Draw.Regression.Line : Boolean
#' @param AB.Plot.step : Numerical
#' @param AB.Plot.title : Title the Altman-Bland graph ("Altman-Bland plot)
#' @param type_of_concordance : "ELISA" or ..
#'
#' @return : a folder with the results and graphs of the concordance
exe_concordance_results <- function(dataframe_concordance,
                                    logbase,
                                    var.x,
                                    var.y,
                                    var.x.name = NULL,
                                    var.y.name = NULL,
                                    x.lloq = 0,
                                    y.lloq = 0,
                                    x.cutoff = 0,
                                    y.cutoff = 0,
                                    sub_folder.name,

                                    REG.Plot.step = 1,
                                    REG.Plot.title = REG.Plot.title,

                                    AB.Plot.Log.Diff  = FALSE,
                                    AB.Draw.Mean.STD  = TRUE,
                                    AB.Draw.Perfect.Line = TRUE,
                                    AB.Draw.Regression.Line = FALSE,
                                    AB.Plot.step = 0.5,
                                    AB.Plot.title = AB.Plot.title,

                                    type_of_concordance = "ELISA"
){


  ## CREATE A FOLDER TO EXPORT THE CONCORDANCE
  LOCATION <<- "data/"
  LOCATIONout <<- ""
  output.folder <<- "output/"
  separator_excel <<- ';'
  create_folder(main_dir = output.folder, sub_dir = sub_folder.name)

  cat("Current working directory :",getwd())
  #print(dataframe_concordance)
  df <- read.csv2(paste0("data/",dataframe_concordance,".csv"), sep = separator_excel)
  df <- na.omit(df)
  print(head(df))
  #Change back space to dot :
  var.x <- gsub(" ", ".", var.x)
  var.y <- gsub(" ", ".", var.y)
  df[[var.x]] <- as.numeric(df[[var.x]])
  df[[var.y]] <- as.numeric(df[[var.y]])
  min.x <- round(log(min(na.omit(df[[var.x]])),logbase),0) - REG.Plot.step
  max.x <- round(log(max(na.omit(df[[var.x]])),logbase),0) + REG.Plot.step
  AB.max.y <- round(max(na.omit(df[[var.y]])/na.omit(df[[var.x]])),0) + AB.Plot.step
  AB.min.y <- round(min(na.omit(df[[var.y]])/na.omit(df[[var.x]])),0) - AB.Plot.step

  if (is.null(var.x.name)){
    var.x.name <- gsub('[[:punct:] ]+',' ',var.x)
  }
  if (is.null(var.y.name)){
    var.y.name <- gsub('[[:punct:] ]+',' ',var.y)
  }

  ## SET concordance.folder AS A GLOBAL VAR
  concordance.folder <<- sub_folder.name

  result.concordance <- QUALIFICATION (

    Data.File.Name        = dataframe_concordance,

    X.Var.Name            =  var.x,
    Y.Var.Name            =  var.y,

    logbase               = logbase,

    X.Concordance.Profile.Legend = 2,
    Y.Concordance.Profile.Legend = -2,

    X.AltmanBland.Profile.Legend = 2,
    Y.AltmanBland.Profile.Legend = 3,

    Concordance.X.Label = paste0(var.x.name),
    Concordance.Y.Label = paste0(var.y.name),
    AltmanBland.X.Label = paste0("Mean between ",var.x.name, " and ",var.y.name),
    AltmanBland.Y.Label = paste0("Ratio ",var.x.name, " and ",var.y.name),
    Alpha               = 0.05,
    Concordance.Range.Band = FALSE,

    Title               = "TEST",
    Postfix             = "",
    # Postfix             = paste0("Titer_",var.x.name,"_VS_",var.y.name),

    Title.concordance   = REG.Plot.title,
    Title.altman        = AB.Plot.title,

    X.Min               = min.x,
    X.Max               = max.x,
    X.Ticks             = log2(2^c(seq(min.x,max.x, REG.Plot.step))), ## LOGBASE UPADTE 10, log10
    X.Tick.Label        = 2^c(seq(min.x,max.x, REG.Plot.step)),
    AltmanBland.Y.Min   = AB.min.y,
    AltmanBland.Y.Max   = AB.max.y,

    AltmanBland.Y.Ticks              =log2(2^c(seq(AB.min.y, AB.max.y, AB.Plot.step))),
    AltmanBland.Plot.Log.Diff        = AB.Plot.Log.Diff,
    AltmanBland.Draw.Mean.STD        = AB.Draw.Mean.STD,
    AltmanBland.Draw.Perfect.Line    = AB.Draw.Perfect.Line,
    AltmanBland.Draw.Regression.Line = AB.Draw.Regression.Line,
    X.Cutoff       =  x.cutoff, # 100 sero status agreement done at this value of LLOQ <100,
    Y.Cutoff       =  y.cutoff,
    Test.Type      = type_of_concordance, # "ELISA" or "FUNCTIONNAL" log2 or log10
    X.LLOQ         = x.lloq, # If LLOQ = 0: All samples are kept, NOT DONE OFTEN
    Y.LLOQ         = y.lloq

  )
  #print(result.concordance)

  ## EXPORT TABLES WITH CONCORDANCE RESULTS
  concordance <- concordance_tables(output = "concordance_table", c_results = result.concordance)
  GMT <- concordance_tables(output = "GMT_table", c_results = result.concordance)
  ABE <- concordance_tables(output = "ABE_table", c_results = result.concordance)
  SAT <- concordance_tables(output = "SAT_table", c_results = result.concordance)


  write.csv(concordance,paste0(output.folder,"/",sub_folder.name,"/table_concordance.csv"),
            row.names = FALSE)
  write.csv(GMT, paste0(output.folder,"/",sub_folder.name,"/table_GMT.csv"),
            row.names = FALSE)
  write.csv(ABE, paste0(output.folder,"/",sub_folder.name,"/table_ABE.csv"),
            row.names = FALSE)
  write.csv(result.concordance$Qualification.Table.Info,
            paste0(output.folder,"/",sub_folder.name,"/Qualification.Table.Info.csv"),
            row.names = FALSE)
  write.csv(SAT,
            paste0(output.folder,"/",sub_folder.name,"/table_SAT.csv"),
            row.names = FALSE)

  return(result.concordance)
}



#' Format Results of ELISA concordance
#'
#' @return : table with formatted results saved at: /output_report/results_ELISA.csv
get_format  <- function(output.folder = "output/") {

  list_path <- list.files(path = output.folder,
                          full.names = FALSE, recursive = FALSE)

  res_global <- data.frame(matrix(ncol=5,
                                  nrow=0,
                                  dimnames=list(NULL, c("a","b","c","d","e"))))

  for (p in list_path){

    ## EXPORT NAME OF CONCORDANCE VARIABLES
    name_concordance <- strsplit(p, "__")[[1]][2]
    name_concordance <- gsub("_", " ", name_concordance)
    name_concordance <- as.data.frame(name_concordance)

    ## LOAD RESULTS OF CONCORDANCE
    path_temp <- paste0(output.folder,p,"/table_concordance.csv")
    df <- read.csv(path_temp)
    df[,2:4] <- sapply(df[,2:4], as.numeric)
    df[1:3,2:4] <- round(df[1:3,2:4],2)
    print(df)
    print("------------")

    ## RENAME COL
    names(df) <- c("Parameter","Concordance slope [90%CI]")

    ## SELECT VALUES
    df[1,2] <- paste0(df[1,2], " [",df[1,3]," ; ", df[1,4],"]")
    df[2,2] <- paste0(df[2,2], " [",df[2,3]," ; ", df[2,4],"]")
    df[3,2] <- paste0(df[3,2], " [",df[3,3]," ; ", df[3,4],"]")
    df <- t(df)
    df <- as.data.frame(df)
    names(df) <- df[1,]
    df <- df[-1,]
    rownames(df) <- NULL
    df <- df[1,1:3]

    ## ADD INFO
    res_n.sample <- read.csv(paste0(output.folder,p,"/Qualification.Table.Info.csv"))
    names(res_n.sample) <- c("Table","N samples")
    df <- cbind(res_n.sample[,2],df)
    names(df) <- c("a","b","c","d")

    ## ADD NAMES VARIABLES
    df <- cbind(name_concordance[1],df)

    res_global <- rbind(res_global,df[1,])
  }

  names(res_global) <- c("Analysis (Var Y VS Var X)", "N samples","Conc. Slope [90%CI]","Average %Diff. [90%CI]" , "GMR Y/X [90%CI]")

  create_folder(main_dir = output.folder, sub_dir = "output_report")
  create_folder(main_dir = paste0(output.folder,"/output_report"), sub_dir = "graphs")
  create_folder(main_dir = paste0(output.folder,"/output_report"), sub_dir = "SAT")

  write.csv(res_global, paste0(output.folder, "output_report/results_.csv"),
            row.names = FALSE)
  return(res_global)
}

#' Copy graphs from each concordance folder to the /output_report
#'
#' @return : save graph in /output_report
copy_graphs  <- function() {

  list_path <- list.files(path = output.folder,
                          full.names = FALSE, recursive = FALSE)

  i <- 1

  for (p in list_path){
    if (grepl(pattern = "concordance", x = p)){
      file.copy(from = paste0(output.folder,"/",p,"/Concordance_plot.png"),   # Copy files
                to = paste0(output.folder,"/","output_report/graphs/",i,"_Concordance_plot.png"))

      file.copy(from = paste0(output.folder,"/",p,"/AltmanBland_plot.png"),   # Copy files
                to = paste0(output.folder,"/","output_report/graphs/",i,"_AltmanBland_plot.png"))

      file.copy(from = paste0(output.folder,"/",p,"/table_SAT.csv"),   # Copy files
                to = paste0(output.folder,"/","output_report/SAT/",p,"__table_SAT.csv"))

      file.copy(from = paste0(output.folder,"/",p,"/PCT_table.csv"),   # Copy files
                to = paste0(output.folder,"/","output_report/SAT/",p,"__PCT_table.csv"))

      i <- i+1
      }
  }
}



#' Format graphs export
#'
#' @return : 2 images .png with all the graphs for regression and Altman-Bland
get_format_plots <- function(){

  list_path <- list.files(path = output.folder,
                          full.names = FALSE, recursive = FALSE)

  n_analysis <- length(list_path)-1

  concordance_plots = lapply(sprintf(paste0(output.folder,"output_report/graphs/%i_Concordance_plot.png"), 1:n_analysis), png::readPNG)
  g_concordance_plots = lapply(concordance_plots, grid::rasterGrob)
  g_concordance_plots <- gridExtra::grid.arrange(grobs=g_concordance_plots, ncol =2)
  ggsave(file= paste0(output.folder,"output_report/concordance_plots.png"), g_concordance_plots)

  AB_plots = lapply(sprintf(paste0(output.folder,"output_report/graphs/%i_AltmanBland_plot.png"), 1:n_analysis), png::readPNG)
  g_AB_plots = lapply(AB_plots, grid::rasterGrob)
  g_AB_plots <- gridExtra::grid.arrange(grobs=g_AB_plots, ncol =2 )
  ggsave(file= paste0(output.folder,"output_report/altman_plots.png"), g_AB_plots)
}

data_management <- function(data_file_name = "data_temp") {
  # Read the dataset
  dataset <- read.csv2(file = paste0("data/", data_file_name, ".csv"))
  print(head(dataset))

  # Convert character columns to numeric where possible
  dataset <- dataset %>% mutate_if(is.character, as.numeric)

  # Calculate mean for each column except the first one
  dataset_summary <- dataset %>%
    select(-1) %>%  # Exclude the first column directly
    group_by_at(vars(-group_cols())) %>%
    summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')

  # Write the summarised dataset back to a file
  write.table(dataset_summary, paste0("data/", data_file_name, ".csv"), sep = separator_excel, row.names = FALSE)
}




#' Global Concordance Function
#'
#' @param data.file : name of a data set without the extension (.csv, .xlsx)
#' @param list_var.x List of variables X c("var1.x", "var2.x", ...)
#' @param list_var.y List of variables Y c("var1.x", "var2.x", ...)
#' @param list_var.x.name List of the names of variables X c("Variable 1", "Variable 2", ...)
#' @param list_var.y.name List of the names of variables Y c("Variable 1", "Variable 2", ...)
#' @param list_x.lloq List of LLOQ of variables X c(40, 40, ...)
#' @param list_y.lloq List of LLOQ of variables Y c(40, 40, ...)
#' @param list_x.cutoff List of cutoff of variables X c(40, 40, ...) to display the agreement serostatus table
#' @param list_y.cutoff List of cutoff of variables Y c(40, 40, ...) to display the agreement serostatus table
#' @param logbase 2 or 10.
#' @param REG.Plot.step : Numerical
#' @param REG.Plot.title : Title the concordance graph ("Concordance plot)
#' @param AB.Plot.Log.Diff : Boolean
#' @param AB.Draw.Mean.STD : Boolean
#' @param AB.Draw.Perfect.Line : Boolean
#' @param AB.Draw.Regression.Line : Boolean
#' @param AB.Plot.step : Numerical
#' @param AB.Plot.title : Title the Altman-Bland graph ("Altman-Bland plot)
#' @param type_of_concordance : "ELISA" or ..
#'
#' @return : Concordance results in /output/
#' @export
concordance_function <- function(data.file = "data_temp",
                                 list_var.x,
                                 list_var.y,
                                 list_var.x.name,
                                 list_var.y.name,
                                 list_x.lloq,
                                 list_y.lloq,
                                 list_x.cutoff,
                                 list_y.cutoff,
                                 logbase,

                                 REG.Plot.step = 1,
                                 REG.Plot.title = "Concordance plot",

                                 AB.Plot.Log.Diff  = FALSE,
                                 AB.Draw.Mean.STD  = TRUE,
                                 AB.Draw.Perfect.Line = TRUE,
                                 AB.Draw.Regression.Line = FALSE,
                                 AB.Plot.step = 0.5,
                                 AB.Plot.title = "Altman-Bland plot",

                                 type_of_concordance = "ELISA"
) {


  ## CREATE FOLDER TO STORE CONCORDANCE RESULTS
  LOCATION <<- "data/"
  LOCATIONout <<- ""
  output.folder <<- "output/"
  separator_excel <<- ';'
  cat("Current working directory :",getwd())
  create_folder(main_dir = getwd(), sub_dir = output.folder)
  print("Folder output created!")

  ## Data management
  data_management()

  for (i in seq(1,length(list_var.x))){
    output.folder <- paste0(i,"_concordance__",list_var.x.name[i], "_VS_", list_var.y.name[i])

    ## GET RESUTLS CONCORDANCE FOR I
    res <- exe_concordance_results(dataframe_concordance = data.file,
                                   logbase = logbase,
                                   var.x = list_var.x[i],
                                   var.y = list_var.y[i],
                                   var.x.name = list_var.x.name[i],
                                   var.y.name = list_var.y.name[i],
                                   x.lloq = list_x.lloq[i],
                                   y.lloq = list_y.lloq[i],
                                   x.cutoff = list_x.cutoff[i],
                                   y.cutoff = list_y.cutoff[i],
                                   sub_folder.name = output.folder,

                                   REG.Plot.step = REG.Plot.step,
                                   REG.Plot.title =  paste(REG.Plot.title, "between",list_var.x.name[i], "and", list_var.y.name[i], sep = " "),

                                   AB.Plot.Log.Diff  = AB.Plot.Log.Diff,
                                   AB.Draw.Mean.STD  = AB.Draw.Mean.STD,
                                   AB.Draw.Perfect.Line = AB.Draw.Perfect.Line,
                                   AB.Draw.Regression.Line = AB.Draw.Regression.Line,
                                   AB.Plot.step = AB.Plot.step,
                                   AB.Plot.title = paste(AB.Plot.title, "between",list_var.x.name[i], "and", list_var.y.name[i], sep = " "),

                                   type_of_concordance = type_of_concordance)


  }

  ## GATHER ALL THE RESULTS IN ONE TABLE
  ## REFORMAT - ELISA REPORT
  res <- get_format()
  print("get_format good")
  # if (type_of_concordance == "ELISA"){
  #   res <- get_format()
  # }

  ## GATHER ALL THE GRAPHS IN ONE GRAPH FOR REG AND AB
  copy_graphs()
  print("copy_graphs good")
  get_format_plots()
  print("get_format_plots good")

  # Get data with values < LLOQ
  get_data_lloq(data_file_name = data.file,
                list_var_x = list_var.x, list_var_y = list_var.y,
                lloq_x_values = list_x.lloq, lloq_y_value = list_y.lloq)
  print("get_data_lloq good")

  ## FORMATTING TABLES FOR THE REPORT
  get_desc_stat(data_file = data.file,
                list_var_x = list_var.x, list_var_y = list_var.y,
                lloq_x_values = list_x.lloq, lloq_y_value = list_y.lloq)
  print("get_desc_stat good")
  get_rho()
  print("get_rho good")

  print("concordance results exported in : /output/")
  ## EXPORT REPORT FOR THE DIFFERENT CONCORDANCE
  # rmarkdown::render(system.file("rmd", "test.Rmd", package = "concordance"))
  #
  # rmarkdown::render(system.file("rmd", "report_concordance.Rmd", package = "concordance"),
  #                   params = list(
  #                     data = data.file,
  #                     var.x = list_var.x.name,
  #                     var.y = list_var.y.name,
  #                     LLOQ = list_x.lloq_example[1],
  #                     log_transfo = "log10"))


}


#' Concordance Plot Function
#'
#' @param data.file : name of a data set without the extension (.csv, .xlsx)
#' @param var.x : Variable X of the concordance analysis ("Var.x")
#' @param var.y : Variable Y of the concordance analysis
#' @param log : type of transformation of the variables (log10 or log2)
#'
#' @return : Concordance regression plot
#' @export
concordance_plot_function <- function(data.frame, var.x, var.y, log =10){
  print("--------------- ## CONCORDANCE regression plot ## ---------------")
  t0 <- Sys.time()

  data.frame[[var.x]] <- as.numeric(data.frame[[var.x]])
  data.frame[[var.y]] <- as.numeric(data.frame[[var.y]])

  if (log == 10) {
    data.frame[[var.x]] <- log10(data.frame[[var.x]])
    data.frame[[var.y]] <- log10(data.frame[[var.y]])
  }
  else{
    data.frame[[var.x]] <- log2(data.frame[[var.x]])
    data.frame[[var.y]] <- log2(data.frame[[var.y]])
  }

  x <- rlang::sym(var.x)
  y <- rlang::sym(var.y)


  # Define the limits and create a data frame for the lines to be added to the ggplot
  limits <- data.frame(slope = c(1),
                       intercept = c(0),
                       label = factor(c("Perfect Concordance"),
                                      levels = c("Perfect Concordance")))

  plot <- ggplot(data.frame, aes(x= !!x, y= !!y)) +

    geom_point(shape = 16, color = "black", size = 2) +

    # Add perfect concordance line
    geom_abline(data = limits,
                aes(intercept = intercept, slope = slope, color = label, linetype = label),
                size = 1) +
    # Add observed concordance line
    geom_smooth(method = lm, se = FALSE,
                aes(color = "Observed Concordance Line", linetype = "Observed Concordance Line"),
                size = 0.75) +

    # Define the colors for the legend
    scale_color_manual(values = c("Perfect Concordance" = "blue",
                                  "Observed Concordance Line" = "red")) +

    scale_linetype_manual(values = c("Perfect Concordance" = "dashed",
                                     "Observed Concordance Line" = "solid")) +

    ggtitle(paste0("Concordance regression for ", var.x, " and ", var.y)) +

    xlab(paste0(var.x)) +
    ylab(paste0(var.y)) +
    theme_light() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 15),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.position = "bottom"
    )

  print(Sys.time() - t0)
  return(plot)
}






#' Altman Plot Function
#'
#' @param data.file : name of a data set without the extension (.csv, .xlsx)
#' @param var.x : Variable X of the concordance analysis ("Var.x")
#' @param var.y : Variable Y of the concordance analysis
#'
#' @return : Altman-Bland plot
#' @export
altman_plot_function <- function(data.frame, var.x, var.y, log = 10){
  print("---------------- ## ALTMAN-BLAND plot ## ----------------")
  t0 <- Sys.time()

  data.frame[[var.x]] <- as.numeric(data.frame[[var.x]])
  data.frame[[var.y]] <- as.numeric(data.frame[[var.y]])

  if (log == 10) {
    data.frame[[var.x]] <- log10(data.frame[[var.x]])
    data.frame[[var.y]] <- log10(data.frame[[var.y]])
  } else {
    data.frame[[var.x]] <- log2(data.frame[[var.x]])
    data.frame[[var.y]] <- log2(data.frame[[var.y]])
  }

  # Calculate the average and difference for Bland-Altman
  data.frame$average <- (data.frame[[var.x]] + data.frame[[var.y]]) / 2
  data.frame$difference <- data.frame[[var.x]] - data.frame[[var.y]]

  GMR <- mean(data.frame$difference, na.rm = TRUE)
  STD <- sd(data.frame$difference, na.rm = TRUE)

  # Define limits and create a data frame for the lines to be added to the ggplot
  limits <- data.frame(yintercept = c(GMR, GMR + 2 * STD, GMR - 2 * STD, 0),
                       label = factor(c("Geometric Mean Ration (GMR)", "GMR + 2STD", "GMR - 2STD", "Perfect Agreement"),
                                      levels = c("Geometric Mean Ration (GMR)", "GMR + 2STD", "GMR - 2STD", "Perfect Agreement")))

  # Create the plot
  plot <- ggplot(data.frame, aes(x = average, y = difference)) +
    geom_point(shape = 16, color = "black", size = 2) +
    geom_hline(data = limits, aes(yintercept = yintercept, color = label, linetype = label), show.legend = TRUE) +
    scale_color_manual(values = c("Geometric Mean Ration (GMR)" = "red",
                                  "GMR + 2STD" = "green",
                                  "GMR - 2STD" = "green",
                                  "Perfect Agreement" = "blue")) +

    scale_linetype_manual(values = c("Geometric Mean Ration (GMR)" = "solid",
                                     "GMR + 2STD" = "dashed",
                                     "GMR - 2STD" = "dashed",
                                     "Perfect Agreement" = "dotted")) +

    ggtitle(paste0("Altman-Bland plot for ", var.x, " and ", var.y)) +
    xlab(paste("Mean of ", var.x, " and ", var.y, sep = "")) +
    ylab(paste("Ratio of ", var.x, " and ", var.y, sep = "")) +
    theme_light() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 15),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.position = "bottom"
    )

  # Print the processing time and return the plot
  print(Sys.time() - t0)
  return(plot)
}






