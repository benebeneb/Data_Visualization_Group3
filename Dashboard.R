############################################################
#            AI Readability Dashboard (bs4Dash)            #
############################################################
# install.packages(c("bs4Dash","shiny","shinyWidgets",
#                    "shinyjs","DBI","RSQLite","tidyverse",
#                    "ggridges"))

library(bs4Dash)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(ggridges)
library(readr)

## ---------- 1.  HELPERS -------------------------------------------------- ##
score_columns <- c(
  "flesch_kincaid_grade","flesch_reading_ease","gunning_fog",
  "smog_index","dale_chall","avg_sentence_length","avg_word_length",
  "lexical_density","type_token_ratio","passive_ratio",
  "subordinate_clauses","errors","transitions","subjectivity",
  "formality","complexity"
)

score_labels <- c(
  "F‑K Grade","F‑K Ease","Gunning Fog","SMOG",
  "Dale‑Chall","Avg Sent Len","Avg Word Len",
  "Lexical Density","TTR","Passive Ratio",
  "Sub‑Clauses","Errors","Transitions",
  "Subjectivity","Formality","Complexity"
)

# quick palette
score_colors <- RColorBrewer::brewer.pal(9,"Set1") %>% 
  append(RColorBrewer::brewer.pal(7,"Set2"))

load_data <- function(csv_file){
  data <- read_csv(csv_file, show_col_types = FALSE)
  
  # clip extreme outliers
  for(col in score_columns){
    if(col %in% names(data)){
      q1  <- quantile(data[[col]], .01, na.rm = TRUE)
      q99 <- quantile(data[[col]], .99, na.rm = TRUE)
      data[[col]] <- pmax(pmin(data[[col]], q99), q1)
    }
  }
  data
}

data_4o      <- load_data("4o.csv")
data_4o_mini <- load_data("4o-mini.csv")
data_corpus  <- load_data("news_articles.csv")

# data_deep   <- load_data("deepseek.csv")   # example if you add more


## ---------- 2.  UI ------------------------------------------------------- ##
ui <- bs4DashPage(
  title  = "AI Readability Dashboard",
  dark   = NULL,
  header = bs4DashNavbar(
    title = span("Dashboard"),
    skin  = "light",
    status= "primary"
  ),
  # no global sidebar – the sketch uses an in‑body column
  sidebar = bs4DashSidebar(disable = TRUE),
  
  body = bs4DashBody(
    useShinyjs(),
    tags$head(tags$style(HTML("
        /* dashed separator between inner sidebar & main panel */
        .left-panel{border-right:2px dashed #d3d3d3;height:100%;}
        .logo-img{max-width:80px;display:block;margin:0 auto 10px;}
    "))),
    br(),
    
    # ---- Model selector pill‑buttons  ------------------------------------
    fluidRow(
      column(
        width = 12,
        radioGroupButtons(
          inputId   = "model_sel",
          choices   = c(`4O Responses`   = "4o",
                        `4O‑Mini`       = "4o_mini",
                        `Corpus`        = "corpus"),
          # `DeepSeek v2`   = "deep"),   # uncomment if added
          selected  = "4o",
          status    = "primary",
          justified = TRUE,
          size      = "lg"
        )
      )
    ),
    
    # ---- Whole dashboard box --------------------------------------------
    bs4Card(
      width = 12, headerBorder = FALSE,
      fluidRow(
        # -------- Left column (logo + filter buttons) --------------------
        column(
          width = 2,
          div(
            class = "left-panel",
            # YOUR logo; put file in ./www -------------
            tags$img(src = "logo.png", class = "logo-img"),
            h4(class = "text-center", "Bounty Hunters"),
            hr(),
            h5("Click to filter"),
            radioGroupButtons(
              inputId  = "txt_filter",
              choices  = c(Both = "both", Prompts = "prompt", Responses = "response"),
              selected = "response",      # or "both" if you prefer as default
              status   = "secondary",
              direction= "vertical",
              size     = "sm"
            )
          )
        ),
        
        # -------- Right main content ------------------------------------
        column(
          width = 10,
          fluidRow(
            bs4ValueBoxOutput("box_sent", width = 3),
            bs4ValueBoxOutput("box_word", width = 3),
            bs4ValueBoxOutput("box_ttr",  width = 3),
            bs4ValueBoxOutput("box_sub",  width = 3)
          ),
          fluidRow(
            column(
              width = 6,
              plotOutput("ridge_plot", height = "400px")
            ),
            column(
              width = 6,
              plotOutput("hist_plot",   height = "195px"),
              plotOutput("scatter_plot",height = "195px")  # placeholder
            )
          )
        )
      )
    )
  ),
  footer = bs4DashFooter()
)

## ---------- 3.  SERVER --------------------------------------------------- ##
server <- function(input, output, session){
  
  # ------- dataset reactive ---------------------------------------------
  current_data <- reactive({
    dat <- switch(input$model_sel,
                  "4o"      = data_4o,
                  "4o_mini" = data_4o_mini,
                  "corpus"  = data_corpus)
    
    if("text_type" %in% names(dat)){
      dat <- dplyr::case_when(
        input$txt_filter == "prompt"   ~ dat$text_type == "prompt",
        input$txt_filter == "response" ~ dat$text_type == "response",
        TRUE                           ~ TRUE                    # "both"
      ) |> {\(keep) dat[keep, , drop = FALSE]}()
    }
    dat
  })
  
  
  # ------- VALUE BOXES ---------------------------------------------------
  output$box_sent <- renderbs4ValueBox({
    bs4ValueBox(
      value    = round(mean(current_data()$avg_sentence_length, na.rm = TRUE), 1),
      subtitle = "Avg Sentence Length",
      icon     = icon("align-left"),
      color    = "primary"
    )
  })
  
  output$box_word <- renderbs4ValueBox({
    bs4ValueBox(
      value    = round(mean(current_data()$avg_word_length, na.rm = TRUE), 1),
      subtitle = "Avg Word Length",
      icon     = icon("sort-alpha-down"),
      color    = "secondary"
    )
  })
  
  output$box_ttr <- renderbs4ValueBox({
    bs4ValueBox(
      value    = round(mean(current_data()$type_token_ratio, na.rm = TRUE), 2),
      subtitle = "Type‑Token Ratio",
      icon     = icon("font"),
      color    = "success"
    )
  })
  
  output$box_sub <- renderbs4ValueBox({
    bs4ValueBox(
      value    = round(mean(current_data()$subordinate_clauses, na.rm = TRUE), 2),
      subtitle = "Subordinate Clauses",
      icon     = icon("code-branch"),
      color    = "danger"
    )
  })
  
  # ------- RIDGELINE -----------------------------------------------------
  output$ridge_plot <- renderPlot({
    data_long <- current_data() %>%
      select(all_of(score_columns)) %>% 
      pivot_longer(everything(),
                   names_to  = "Score",
                   values_to = "Value") %>% 
      mutate(Score = factor(Score,
                            levels = score_columns,
                            labels = score_labels))
    ggplot(data_long,
           aes(x = Value, y = Score, fill = Score)) +
      geom_density_ridges_gradient(
        scale         = 2.8,
        gradient_lwd  = 0.8,
        calc_ecdf     = TRUE,
        alpha         = 0.75) +
      scale_fill_manual(values = score_colors) +
      labs(
        title = paste("Ridgeline –", names(input$model_sel)[
          which(c("4o","4o_mini","corpus")==input$model_sel)]),
        x     = "Score",
        y     = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none",
            plot.title     = element_text(hjust = .5, face = "bold"))
  })
  
  # ------- HISTOGRAM (Complexity) ---------------------------------------
  output$hist_plot <- renderPlot({
    ggplot(current_data(), aes(complexity))+
      geom_histogram(bins = 25, alpha = .85, fill = "#2ca02c")+
      labs(title = "Complexity", x = NULL, y = NULL)+
      theme_minimal(base_size = 13)+
      theme(plot.title = element_text(hjust = .5, face = "bold"))
  })
  
  # ------- SCATTER PLACEHOLDER ------------------------------------------
  output$scatter_plot <- renderPlot({
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1,1,"(scatter‑plot placeholder)", cex = 1.1)
  })
}

shinyApp(ui, server)

