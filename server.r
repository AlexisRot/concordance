size_text_group=10

function(input, output, session) {

  ## Descriptive analysis
  data <- reactive({
    req(input$upload)

    ext <- tools::file_ext(input$upload$name)
    df <- switch(ext,
                 csv = vroom::vroom(input$upload$datapath, delim = ",", show_col_types = FALSE),
                 tsv = vroom::vroom(input$upload$datapath, delim = "\t", show_col_types = FALSE),
                 validate("Invalid file; Please upload a .csv or .tsv file")
    )

    # Ensure the first column remains as character and convert others to numeric
    df <- df %>%
      mutate_at(vars(-one_of(names(df)[1])), list(~ as.numeric(as.character(.)))) %>%
      mutate_at(vars(one_of(names(df)[1])), as.character)

    write.csv2(df,"data/data_temp.csv")

    return(df)
  })


  #observe(print(data()))

  # # Update Var.y
  # observeEvent(data(), {
  #   # req(input$input.var.y, data())
  #   freezeReactiveValue(input, "input.var.y")
  #   updateSelectInput(session = session, inputId = "input.var.y", choices = names(data()),
  #                     selected = tail(names(data()),7))
  #
  #   freezeReactiveValue(input, "input.var.x")
  #   updateSelectInput(session = session, inputId = "input.var.x", choices = names(data()),
  #                     selected = tail(names(data()),2))
  #
  #   freezeReactiveValue(input, "input.ID")
  #   updateSelectInput(session = session, inputId = "input.ID", choices = names(data()),
  #                     selected = tail(names(data()),2))
  #
  #   freezeReactiveValue(input, "input.group")
  #   updateSelectInput(session = session, inputId = "input.group", choices = names(data()),
  #                     selected = tail(names(data()),2))
  #
  #   updateCheckboxGroupInput(session = session, inputId = "box_choice.timepoint", choices = unique(data()[[input$input.var.x]]),
  #                            inline = TRUE)
  # })
  #
  # # # # Update Timepoint & group choices
  # observeEvent(input$refresh, {
  #   print("Observe event for choices timepoint and group name ")
  #
  #   req(data(),input$input.var.x,input$input.group)
  #
  #   freezeReactiveValue(input, "box_choice.timepoint")
  #   updateCheckboxGroupInput(session = session, inputId = "box_choice.timepoint", choices = unique(data()[[input$input.var.x]]),
  #                            inline = TRUE, selected = unique(data()[[input$input.var.x]]) )
  #
  #   freezeReactiveValue(input, "box_choice.group_name")
  #   updateCheckboxGroupInput(session = session, inputId = "box_choice.group_name", choices = unique(data()[[input$input.group]]),
  #                            inline = TRUE, selected = unique(data()[[input$input.group]]) )
  # })


  output$head <- renderDT(
    data(), options = list(
      columnDefs = list(list(className = 'dt-center', targets = 5)),
      pageLength = 5,
      lengthMenu = c(5, 10, 15, 20),
      lengthChange = FALSE,
      scrollX = T
    )
  )

  output$summary <- renderPrint({
    req(data())
    summary(data())
  })

  output$NbRows <- renderValueBox({
    valueBox(
      nrow(data()), "Rows", icon = icon("list"),
      color = "orange"
    )
  })

  output$NbColumns <- renderValueBox({
    req(data())
    valueBox(
      ncol(data()), "Columns", icon = icon("list"),
      color = "purple"
    )
  })

  # Reactive expression that returns the names of numeric columns
  numeric_columns <- reactive({
    req(data())
    names(data())[sapply(data(), is.numeric)]
  })

  ## Concordance
  # Update Var.x.
  observeEvent(data(), {
    freezeReactiveValue(input, "var.x.concordance")
    updateSelectInput(session = session, inputId = "var.x.concordance",
                      choices = c("", numeric_columns()))
  })

  # Update Var.y
  observeEvent(data(), {
    freezeReactiveValue(input, "var.y.concordance")
    updateSelectInput(session = session, inputId = "var.y.concordance",
                      choices = c("", numeric_columns()))
  })


  # Display results TABLES + GRAPHS
  output$concordance_table <- renderTable({
    # Avoid issues ERROR, check missing values
    req(input$var.x.concordance,input$var.y.concordance)



    res_concordance <<- exe_concordance_results(dataframe_concordance= "data_temp", logbase = 2,
                                                var.x= input$var.x.concordance,var.y = input$var.y.concordance,
                                                sub_folder.name = "temp_concordance")
    concordance_table.table <- concordance_tables(output = "concordance_table", c_results = res_concordance)

  })
  output$GMT_table <- renderTable({
    req(input$var.x.concordance,input$var.y.concordance)
    # res_concordance <- exe_concordance(dataframe_concordance= "data_rshiny",var.x= input$var.x.concordance,var.y = input$var.y.concordance)
    concordance_table.table <- concordance_tables(output = "GMT_table", c_results = res_concordance)

  })
  output$ABE_table <- renderTable({
    req(input$var.x.concordance,input$var.y.concordance)
    # res_concordance <- exe_concordance(dataframe_concordance= "data_rshiny",var.x= input$var.x.concordance,var.y = input$var.y.concordance)
    concordance_table.table <- concordance_tables(output = "ABE_table", c_results = res_concordance)

  })
  output$concordance_plot <- renderPlotly({
    print("CONCORDANCE PLOT DYNAMIC -------------------------------------------")
    req(data(),input$var.x.concordance,input$var.y.concordance)

    concordance_plot_function(data.frame = data(),
                              var.x = input$var.x.concordance,
                              var.y = input$var.y.concordance,
                              log = input$log)
  })

  output$concordance_plot_fix <- renderPlot({
    print("CONCORDANCE PLOT FIX -------------------------------------------------")
    req(data(),input$var.x.concordance,input$var.y.concordance,input$log)
    concordance_plot_function(data.frame = data(),
                              var.x = input$var.x.concordance,
                              var.y = input$var.y.concordance,
                              log = input$log)
  })

  output$altman_bland_plot <- renderPlotly({
    print("ALTMAN-BLAND PLOT DYNAMIC -------------------------------------------")
    req(data(),input$var.x.concordance,input$var.y.concordance,input$log)

    altman_plot_function(data.frame = data(),
                               var.x = input$var.x.concordance,
                               var.y = input$var.y.concordance)
  })

  output$altman_bland_plot_fix <- renderPlot({
    print("ALTMAN-BLAND PLOT FIX -------------------------------------------------")
    req(data(),input$var.x.concordance,input$var.y.concordance,input$log)

    altman_plot_function(data.frame = data(),
                               var.x = input$var.x.concordance,
                               var.y = input$var.y.concordance)
  })


  # Report pdf & html
  # output$showfile.html <- renderUI({
  #   req(input$var.x.concordance,input$var.y.concordance)
  #   includeHTML("www/concordance_DASHBOARD_V2.html")
  # })
  #
  # output$pdf_viewer <- renderUI( tags$iframe(src = "www/concordance_DASHBOARD_V2.pdf", height = "1000px", width="100%"))

  # Report RMD
  # output$file_name <- renderPrint({input$upload$name})
  output$RMD_report <- downloadHandler(
    filename = function() {
      report_format <- input$report_format
      title_report <- input$title_report
      title <- paste("RMD_",title_report, sep="")
      if (report_format == "Word"){
        paste(title,".docx", sep="")
      }
      else{
        paste(title, ".pdf", sep ="")
      }
    },

    content = function(file) {
      req(data()) # Make sure data is available

      # Other parameters (update as necessary)
      log_transfo <- as.integer(input$log_base)
      units <- input$unity
      type_criteria <- input$concordance_type
      assay_info <- input$assay_info
      percent_diff <- as.integer(input$percentage_difference)
      fold_diff <- as.integer(input$fold)
      name_project <- input$name_project
      title_report <- input$title_report


      report_format <- input$report_format
      if (report_format == "Word"){
        report_format = "word_document"
      }
      else{
        report_format ="pdf_document"
      }


      #Deal with empty informations
      si <- summary_info()
      si$LOQ_X[si$LOQ_X == ""] <- 0
      si$LOQ_Y[si$LOQ_Y == ""] <- 0
      si$Cutoff_X[si$Cutoff_X ==""] <- 0
      si$Cutoff_Y[si$Cutoff_Y ==""] <- 0
      if (is.null(units)){
        units = "UNITS"
      }

      # Extract data from summary_info
      concordance_name <- si$Concordance_Name
      variable_x <- si$Variable_X
      variable_y <- si$Variable_Y
      loq_x <- as.integer(si$LOQ_X)
      loq_y <- as.integer(si$LOQ_Y)
      cutoff_x <- si$Cutoff_X
      cutoff_y <- si$Cutoff_Y



      #Make the concordance using concordance_function
      concordance_function("data_temp",
                           variable_x,
                           variable_y,
                           variable_x, #TBC
                           variable_y,
                           loq_x,
                           loq_y,
                           cutoff_x,
                           cutoff_y,
                           log_transfo
      )

      # File rendering
      #src <- normalizePath("report_concordance.Rmd")
      #owd <- setwd(tempdir())
      #on.exit(setwd(owd))
      #file.copy(src, "report_concordance.Rmd", overwrite = TRUE)


      out <- rmarkdown::render("report_concordance.Rmd",
                               params = list(
                                 var.x = variable_x,
                                 var.y = variable_y,
                                 var.x.name = variable_x,
                                 var.y.name = variable_y,
                                 LLOQ.x = loq_x,
                                 LLOQ.y = loq_y,
                                 cutoff.x = cutoff_x,
                                 cutoff.y = cutoff_y,
                                 log_transfo = log_transfo,
                                 percent_diff = percent_diff,
                                 fold_diff = fold_diff,
                                 units = units,
                                 type_criteria = type_criteria,
                                 assay_info = assay_info,
                                 project_name = name_project,
                                 title = title_report # Update if needed
                               ),
                               output_format = report_format,
                               output_file = file
      )

      #file.rename(out, file)
    }
  )


  #Add button :
  dynamic_limit <- reactive({
    tryCatch({
      req(data())
      n <- ncol(data()) - 1
      if (n <= 1) {
        return(1)
      } else {
        return(n * (n - 1) / 2)
      }
    }, shiny.silent.error = function(e) {
      1
    })
  })

  counter <- reactiveVal(0)

  output$current_count <- reactive({
    counter()
  })
  outputOptions(output, "current_count", suspendWhenHidden = FALSE)


  output$conditional_message <- renderUI({
    tryCatch({
      req(data())

      if (counter() == 0) {
        return(tags$div("Please add at least one concordance", style = "font-weight: bold; color: red;"))
      }
      if (counter() == 1) {
        return(tags$div(paste("You have 1 concordance (", dynamic_limit() ," possible concordances)"), style = "font-weight: bold;"))
      }
      else {
        return(tags$div(paste("You have", counter(),"concordances (" ,dynamic_limit() , " possible concordances)"), style = "font-weight: bold;"))
      }

    }, shiny.silent.error = function(e) {
      return(tags$div("Please upload a dataset first", style = "font-weight: bold; color: red;"))
    })
  })

  output$dynamic_buttons <- renderUI({

    req(data())

    limit <- dynamic_limit()
    current_count <- counter()

    fluidRow(
      column(width = 6,
             conditionalPanel(
               condition = paste0("output.current_count < ", limit),
               actionButton("add_block", "Add Concordance", icon = icon("plus"))
             )
      ),
      column(width = 6,
             conditionalPanel(
               condition = "output.current_count > 0",
               actionButton("remove_block", "Delete Concordance", icon = icon("minus"))
             )
      )
    )
  })


  # Create a reactiveValues object to store the state
  rv <- reactiveValues(blocks = list())


  summary_info <- reactiveVal(data.frame(Concordance_Name = character(0),
                                         Variable_X = character(0),
                                         Variable_Y = character(0),
                                         LOQ_X = character(0),
                                         LOQ_Y = character(0),
                                         Cutoff_X = character(0),
                                         Cutoff_Y = character(0),
                                         stringsAsFactors = FALSE))

  output$summary_table <- renderUI({
    tableOutput("summary_table_output")
  })
  output$summary_table_output <- renderTable({
    summary_info()
  })


  observeEvent(input$add_block, {
    current_count <- counter()

    # Dynamic limit
    if (current_count >= dynamic_limit()) {
      return(NULL)
    }

    new_count <- current_count + 1
    counter(new_count)

    # Generate a new block ID
    new_block_id <- paste0("concordance_block_", new_count)

    # Determine initial choices
    initial_choices <- tryCatch({
      req(data())
      c("",numeric_columns())
    }, shiny.silent.error = function(e) {
      NULL
    })

    # Insert the new block
    insertUI(
      selector = "#sci_param_container",
      where = "beforeEnd",
      ui = div(id = new_block_id,
               box(title = paste("Concordance", new_count,'/',dynamic_limit()), width = "100%",
                   fluidRow(
                     column(6,
                            h4("Parameters of Variable X"),
                            selectInput(inputId = paste0("var.x.concordance_", new_count), label = "Choose Variable X (Mandatory)", choices = initial_choices),
                            textInput(inputId = paste0("rename_x_", new_count), label = "Rename Variable X (Optional)", value = ""),
                            textInput(inputId = paste0("loq_x_", new_count), label = "LOQ Value for Variable X (Optional)", value = ""),
                            conditionalPanel(
                              condition = "input.analysis_type == 'Cutoff'",
                              textInput(inputId = paste0("cutoff_x_", new_count), label = "Cutoff Parameter for Variable X (Optional)", value = "")
                            )
                     ),
                     column(6,
                            h4("Parameters of Variable Y"),
                            selectInput(inputId = paste0("var.y.concordance_", new_count), label = "Choose Variable Y (Mandatory)", choices = initial_choices),
                            textInput(inputId = paste0("rename_y_", new_count), label = "Rename Variable Y (Optional)", value = ""),
                            textInput(inputId = paste0("loq_y_", new_count), label = "LOQ Value for Variable Y (Optional)", value = ""),
                            conditionalPanel(
                              condition = "input.analysis_type == 'Cutoff'",
                              textInput(inputId = paste0("cutoff_y_", new_count), label = "Cutoff Parameter for Variable Y (Optional)", value = "")
                            )
                     )
                   ),
                   div(style = "text-align: center;",
                       actionButton(inputId = paste0("save_concordance_", new_count),
                                    label = paste("Save Concordance", new_count),
                                    icon = icon("save"))  # Save Concordance button added here
                   )
               )
      )
    )

    # Save the state of all blocks
    rv$blocks <- lapply(1:counter(), function(i) {
      list(
        var_x = input[[paste0("var.x.concordance_", i)]],
        var_y = input[[paste0("var.y.concordance_", i)]]
      )
    })

    # Update summary_info to add a new row
    isolate({
      new_summary_info <- data.frame(
        Concordance_Name = paste("Concordance_", new_count, sep = ""),
        Variable_X = NA,
        Variable_Y = NA,
        LOQ_X = NA,
        LOQ_Y = NA,
        Cutoff_X = NA,
        Cutoff_Y = NA,
        stringsAsFactors = FALSE
      )
      updated_summary_info <- rbind(summary_info(), new_summary_info)
      summary_info(updated_summary_info)
    })

  })

  observe({
    # Ensure data is available before proceeding
    req(data())

    # Get the current number of blocks
    cnt <- counter()

    # Loop through each block and update the selectInput choices
    if(cnt > 0 && cnt <= dynamic_limit()) {
      for (i in 1:cnt) {
        updateSelectInput(session,
                          inputId = paste0("var.x.concordance_", i),
                          choices = c("", numeric_columns()),
                          selected = rv$blocks[[i]]$var_x)
        updateSelectInput(session,
                          inputId = paste0("var.y.concordance_", i),
                          choices = c("", numeric_columns()),
                          selected = rv$blocks[[i]]$var_y)
      }
    }

  })

  # Create a reactive expression that concatenates the values of all "Save Concordance" buttons
  concatenated_values <- reactive({
    vals <- sapply(1:counter(), function(i) {
      input[[paste0("save_concordance_", i)]]
    })
    paste(vals, collapse = "_")
  })

  # Use observeEvent to watch for changes in the concatenated_values
  observeEvent(concatenated_values(), {
    # The code here will run whenever any "Save Concordance" button is clicked

    current_count <- counter()
    if (current_count <= 0) {
      return(NULL)
    }

    # Update the corresponding row in summary_info
    isolate({
      new_summary_info_list <- lapply(1:counter(), function(i) {
        # Check if the inputs exist and are not NULL
        var_x <- input[[paste0("var.x.concordance_", i)]]
        var_y <- input[[paste0("var.y.concordance_", i)]]
        LOQ_x <- input[[paste0("loq_x_", i)]]
        LOQ_y <- input[[paste0("loq_y_", i)]]
        Cutoff_X <- input[[paste0("cutoff_x_", i)]]
        Cutoff_Y <- input[[paste0("cutoff_y_", i)]]

        if (!is.null(var_x) && !is.null(var_y)) {
          # If the inputs are not NULL, create the data frame
          data.frame(
            Concordance_Name = paste("Concordance_", i, sep = ""),
            Variable_X = var_x,
            Variable_Y = var_y,
            LOQ_X = LOQ_x,
            LOQ_Y = LOQ_y,
            Cutoff_X = Cutoff_X,
            Cutoff_Y = Cutoff_Y
          )
        } else {
          # If either input is NULL, return NULL (or substitute with a default value)
          NULL
        }
      })
      new_summary_info_list <- Filter(Negate(is.null), new_summary_info_list)
      combined_data <- do.call(rbind, new_summary_info_list)
      summary_info(combined_data)
    })
  })


  #Remove button :
  observeEvent(input$remove_block, {
    current_count <- counter()

    # Don't remove if no blocks are present
    if (current_count <= 0) {
      return(NULL)
    }

    # Generate the ID of the block to remove
    remove_block_id <- paste0("#concordance_block_", current_count)

    # Remove the block
    removeUI(
      selector = remove_block_id
    )

    # Save the state of all blocks
    rv$blocks <- lapply(1:(current_count - 1), function(i) {  # Adjusted the loop to reflect the removal of a block
      list(
        var_x = input[[paste0("var.x.concordance_", i)]],
        var_y = input[[paste0("var.y.concordance_", i)]]
      )
    })

    # If the first block is being removed, replace its values with empty strings in summary_info
    if (current_count == 1) {
      summary_info(data.frame(Concordance_Name = character(0),
                              Variable_X = character(0),
                              Variable_Y = character(0),
                              LOQ_X = character(0),
                              LOQ_Y = character(0),
                              Cutoff_X = character(0),
                              Cutoff_Y = character(0),
                              stringsAsFactors = FALSE))
    } else {
      # Otherwise, update summary_info to reflect the removal of the block
      new_summary_info_list <- lapply(1:(current_count - 1), function(i) {
        data.frame(
          Concordance_Name = paste("Concordance_", i, sep = ""),
          Variable_X = input[[paste0("var.x.concordance_", i)]],
          Variable_Y = input[[paste0("var.y.concordance_", i)]],
          LOQ_x <- input[[paste0("loq_x_", i)]],
          LOQ_y <- input[[paste0("loq_y_", i)]],
          Cutoff_X <- input[[paste0("cutoff_x_", i)]],
          Cutoff_Y <- input[[paste0("cutoff_y_", i)]]
        )
      })
      combined_data <- do.call(rbind, new_summary_info_list)
      summary_info(combined_data)
    }

    # Update the counter
    new_count <- current_count - 1
    counter(new_count)

  })


}
