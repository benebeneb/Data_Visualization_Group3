library(shiny)
library(DBI)
library(RSQLite)
library(tidyverse)
library(ggridges)
library(gridExtra)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)

# Define score columns for analysis (updated to include all available columns)
score_columns <- c(
  "flesch_kincaid_grade", "flesch_reading_ease", "gunning_fog",
  "smog_index", "dale_chall", "avg_sentence_length",
  "avg_word_length", "lexical_density", "type_token_ratio",
  "passive_ratio", "subordinate_clauses", "errors", 
  "transitions", "subjectivity", "formality", "complexity"
)

# Pretty labels for scores
score_labels <- c(
  "Flesch-Kincaid Grade", "Flesch Reading Ease", "Gunning Fog",
  "SMOG Index", "Dale-Chall", "Avg Sentence Length",
  "Avg Word Length", "Lexical Density", "Type-Token Ratio",
  "Passive Ratio", "Subordinate Clauses", "Errors",
  "Transitions", "Subjectivity", "Formality", "Complexity"
)

# Define a color palette for all scores
score_colors <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5",
  "#c49c94"
)

#standardize data turns Max value = 1 and Min value = 0
standardize_data <- function( de_file ) {
  MaxVal <- 0.0
  MinVal <- 0.0

  # loops over all the columns in db_file
  for ( col in seq_len( ncol( db_file.data ) ) ) {

      MaxVal <- 0.0
      MinVal <- 0.0

    # Finds the minimum and maximum values in each column for part 2
    for ( row in seq_len( nrow( db_file.data ) ) ) {
      item <- db_file.data[ row, col ]

      if( item > MaxVal ){
        MaxVal <- item
      }else if( item < minVal ){
        MinVal <- item
      }
    }

    # does the following ( item - MinVal ) / ( MaxVal - MinVal ) = new data
    divisor <- ( MaxVal - MinVal )
    if (divisr != 0) {
      for ( row in seq_len( nrow( db_file.data ) ) ) {
        item <- db_file.data[ row, col ]
        db_file.data[ row, col ] <- (( item - MinVal ) / divisor )
      }
    }
  }

  return ( db_file )
}

# Load and prepare data
load_data <- function(db_file, include_text_type = TRUE) {
  con <- dbConnect(RSQLite::SQLite(), db_file)
  data <- dbReadTable(con, "text_analysis")
  dbDisconnect(con)
  
  # Clean data and handle outliers
  for(col in score_columns) {
    if(col %in% names(data)) {
      q1 <- quantile(data[[col]], 0.01, na.rm = TRUE)
      q99 <- quantile(data[[col]], 0.99, na.rm = TRUE)
      data[[col]] <- pmin(pmax(data[[col]], q1), q99)
    }
  }
  
  # Filter for responses only if text_type exists
  if(include_text_type) {
    data <- data %>% filter(text_type == "response")
  }
  
  return(data)
}

# Load all datasets
data_4o <- load_data("analysis_4o-responces-plain copy.db")
data_4o_mini <- load_data("analysis_4o-mini-responces-plain copy.db")
data_news <- load_data("analysis_corpus-news-articles copy.db", include_text_type = FALSE)

# UI Definition - Updated with box layout
ui <- fluidPage(
  # Enable shinyjs
  useShinyjs(),
  
  # Add custom CSS
  tags$head(
    tags$style(HTML("
      .btn-primary { 
        background-color: #337ab7;
        color: white;
      }
      .btn-default {
        background-color: #f8f8f8;
        color: #333;
      }
      .action-button {
        margin: 5px;
      }
      .score-buttons {
        display: flex;
        flex-wrap: wrap;
        gap: 5px;
        margin-bottom: 15px;
      }
      .box {
        border: 1px solid #ddd;
        border-radius: 4px;
        padding: 15px;
        margin-bottom: 20px;
        background-color: white;
        box-shadow: 0 1px 3px rgba(0,0,0,0.12);
      }
    "))
  ),
  
  titlePanel("Interactive Readability Analysis"),
  
  # Single box containing both sections
  div(class = "box",
      # Model Selection Section
      fluidRow(
        column(12,
               h3("Model Selection"),
               div(class = "score-buttons",
                   actionButton("btn_4o", "4O Responses", class = "btn-primary action-button"),
                   actionButton("btn_4o_mini", "4O Mini Responses", class = "btn-default action-button"),
                   actionButton("btn_news", "News Articles", class = "btn-default action-button")
               ),
               plotOutput("ridgeline_plot_1", height = "400px")
        )
      ),
      
      # Score Selection Section
      fluidRow(
        column(12,
               h3("Score Selection"),
               div(class = "score-buttons",
                   lapply(1:length(score_columns), function(i) {
                     cls <- if(i == 1) "btn-primary action-button" else "btn-default action-button"
                     actionButton(paste0("score_", i), score_labels[i], class = cls)
                   })
               ),
               plotOutput("ridgeline_plot_2", height = "400px")
        )
      )
  )
)

# Server Definition - Completely rewritten with simpler approach
server <- function(input, output, session) {
  # Reactive values to store current selections
  current_model <- reactiveVal("btn_4o")
  current_score <- reactiveVal("score_1")
  
  # Handle model button clicks
  observeEvent(input$btn_4o, {
    updateButton("btn_4o")
    current_model("btn_4o")
  })
  
  observeEvent(input$btn_4o_mini, {
    updateButton("btn_4o_mini")
    current_model("btn_4o_mini")
  })
  
  observeEvent(input$btn_news, {
    updateButton("btn_news")
    current_model("btn_news")
  })
  
  # Helper function to update button styles
  updateButton <- function(active_id) {
    model_buttons <- c("btn_4o", "btn_4o_mini", "btn_news")
    
    for (btn in model_buttons) {
      if (btn == active_id) {
        shinyjs::addClass(btn, "btn-primary")
        shinyjs::removeClass(btn, "btn-default")
      } else {
        shinyjs::removeClass(btn, "btn-primary")
        shinyjs::addClass(btn, "btn-default")
      }
    }
  }
  
  # Handle score button clicks - create separate observers for each
  lapply(1:length(score_columns), function(i) {
    btn_id <- paste0("score_", i)
    observeEvent(input[[btn_id]], {
      updateScoreButton(btn_id)
      current_score(btn_id)
    })
  })
  
  # Helper function to update score button styles
  updateScoreButton <- function(active_id) {
    score_buttons <- paste0("score_", 1:length(score_columns))
    
    for (btn in score_buttons) {
      if (btn == active_id) {
        shinyjs::addClass(btn, "btn-primary")
        shinyjs::removeClass(btn, "btn-default")
      } else {
        shinyjs::removeClass(btn, "btn-primary")
        shinyjs::addClass(btn, "btn-default")
      }
    }
  }
  
  # First ridgeline plot - Selected Model
  output$ridgeline_plot_1 <- renderPlot({
    # Get selected dataset
    selected_model <- current_model()
    
    selected_data <- switch(selected_model,
                          "btn_4o" = data_4o,
                          "btn_4o_mini" = data_4o_mini,
                          "btn_news" = data_news)
    
    # Prepare data for visualization
    data_long <- selected_data %>%
      select(all_of(score_columns)) %>%
      pivot_longer(cols = everything(), names_to = "Score", values_to = "Value")
    
    data_long$Score <- factor(data_long$Score, 
                            levels = score_columns,
                            labels = score_labels)
    
    # Generate ridgeline plot with different colors for each score
    ggplot(data_long, aes(x = Value, y = Score, fill = Score)) +
      geom_density_ridges_gradient(
        aes(fill = Score),
        scale = 3,
        gradient_lwd = 1,
        calc_ecdf = TRUE,
        alpha = 0.7
      ) +
      scale_fill_manual(values = score_colors) +
      scale_x_continuous(limits = c(-5, 40)) +  # Set x-axis limits
      labs(title = paste("Ridgeline Plot for", 
                        switch(selected_model,
                               "btn_4o" = "4O Responses",
                               "btn_4o_mini" = "4O Mini Responses",
                               "btn_news" = "News Articles")),
           x = "Score Value",
           y = "Score Type") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
  })
  
  # Second ridgeline plot - Score Comparison
  output$ridgeline_plot_2 <- renderPlot({
    # Get selected score
    selected_score_id <- current_score()
    score_idx <- as.numeric(str_extract(selected_score_id, "\\d+"))
    
    selected_score <- score_columns[score_idx]
    selected_score_label <- score_labels[score_idx]
    
    # Prepare data for comparison
    data_comparison <- bind_rows(
      data_4o %>% mutate(Dataset = "4O Responses"),
      data_4o_mini %>% mutate(Dataset = "4O Mini Responses"),
      data_news %>% mutate(Dataset = "News Articles")
    ) %>%
      select(Dataset, !!selected_score)
    
    # Generate comparison ridgeline plot with different colors
    ggplot(data_comparison, aes(x = !!sym(selected_score), y = Dataset, fill = Dataset)) +
      geom_density_ridges_gradient(
        scale = 3,
        gradient_lwd = 1,
        calc_ecdf = TRUE,
        alpha = 0.7
      ) +
      scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
      labs(title = paste("Comparison of", selected_score_label, "Across Datasets"),
           x = "Score Value",
           y = "Dataset") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server) 
