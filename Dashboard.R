############################################################
#                 AI Readability Dashboard                 #
############################################################
# install.packages(c("bs4Dash","shiny","shinyWidgets",
#                    "shinyjs","tidyverse","ggridges",
#                    "RColorBrewer","readr"))

library(bs4Dash)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(ggridges)
library(readr)
library(RColorBrewer)
library(stringr)

## ---------- 1.  HELPERS -------------------------------------------------- ##
score_columns <- c(
  "flesch_kincaid_grade","flesch_reading_ease","gunning_fog",
  "smog_index","dale_chall","avg_sentence_length","avg_word_length",
  "lexical_density","type_token_ratio","passive_ratio",
  "subordinate_clauses","errors","transitions","subjectivity",
  "formality","complexity"
)
score_labels  <- c(
  "F‑K Grade","F‑K Ease","Gunning Fog","SMOG","Dale‑Chall",
  "Avg Sent Len","Avg Word Len","Lexical Density","TTR",
  "Passive Ratio","Sub‑Clauses","Errors","Transitions",
  "Subjectivity","Formality","Complexity"
)
score_colors  <- c(brewer.pal(9,"Set1"), brewer.pal(7,"Set2"))

load_data <- function(csv){
  dat <- read_csv(csv, show_col_types = FALSE)
  for(col in score_columns){
    if(col %in% names(dat)){
      q1  <- quantile(dat[[col]], .01, na.rm = TRUE)
      q99 <- quantile(dat[[col]], .99, na.rm = TRUE)
      dat[[col]] <- pmax(pmin(dat[[col]], q99), q1)
    }
  }
  dat
}

data_4o      <- load_data("4o.csv")
data_4o_mini <- load_data("4o-mini.csv")
data_corpus  <- load_data("news_articles.csv")

safe_mean <- function(x, d){ m <- mean(x, na.rm = TRUE); if(is.nan(m)) "--" else round(m, d) }

filter_by_type <- function(df, sel = c("both","prompt","response")){
  sel <- match.arg(sel)
  if(sel == "both" || !"text_type" %in% names(df)) return(df)
  dplyr::filter(df, text_type == sel)
}

## ---------- 2.  UI ------------------------------------------------------- ##
ui <- bs4DashPage(
  title = "AI Readability Dashboard",
  dark  = FALSE,
  header = bs4DashNavbar(title = span("Dashboard"), skin = "light", status = "primary"),
  sidebar = bs4DashSidebar(disable = TRUE),
  
  body = bs4DashBody(
    useShinyjs(),
    tags$head(tags$style(HTML("
      .left-panel{border-right:2px dashed #d3d3d3;height:100%;}
      .logo-img{max-width:80px;display:block;margin:0 auto 10px;}
      .score-buttons .btn{margin:0 2px 4px 0;}
    "))),
    br(),
    
    ## model selector ------------------------------------------------------
    fluidRow(
      column(12,
             radioGroupButtons(
               "model_sel",
               choices  = c(`4‑o` = "4o", `4‑o Mini` = "4o_mini", Corpus = "corpus"),
               selected = "4o",
               status   = "primary",
               justified= TRUE,
               size     = "lg"
             )
      )
    ),
    
    ## main card -----------------------------------------------------------
    bs4Card(width = 12, headerBorder = FALSE,
            fluidRow(
              ## LEFT mini‑sidebar
              column(width = 2,
                     div(class = "left-panel",
                         tags$img(src = "logo.png", class = "logo-img"),
                         h4(class = "text-center", "Bounty Hunters"), hr(),
                         h5("Filter text type"),
                         radioGroupButtons(
                           "txt_filter",
                           choices  = c(Both = "both", Prompts = "prompt", Responses = "response"),
                           selected = "response",
                           status   = "secondary",
                           direction= "vertical",
                           size     = "sm"
                         )
                     )
              ),
              
              ## RIGHT main content
              column(width = 10,
                     
                     ## value boxes ----------------------------------------------------
                     fluidRow(
                       bs4ValueBoxOutput("box_sent", 3),
                       bs4ValueBoxOutput("box_word", 3),
                       bs4ValueBoxOutput("box_ttr",  3),
                       bs4ValueBoxOutput("box_sub",  3)
                     ),
                     
                     ## plots & controls ----------------------------------------------
                     fluidRow(
                       # LEFT: ridgeline + histogram
                       column(width = 7,
                              plotOutput("ridge_all", height = "420px"),
                              plotOutput("hist_plot",  height = "220px")
                       ),
                       
                       # RIGHT: buttons + compare ridge + scatter
                       column(width = 5,
                              h4("Score Selection"),
                              div(class = "score-buttons",
                                  lapply(seq_along(score_columns), function(i){
                                    actionBttn(
                                      paste0("score_", i), score_labels[i],
                                      style = "material-flat", size = "xs",
                                      color = if(i == 1) "primary" else "default"
                                    )
                                  })
                              ),
                              plotOutput("ridge_compare", height = "260px"),
                              plotOutput("scatter_plot", height = "220px")
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
  
  current_data <- reactive({
    dat <- switch(input$model_sel,
                  "4o"      = data_4o,
                  "4o_mini" = data_4o_mini,
                  "corpus"  = data_corpus)
    filter_by_type(dat, input$txt_filter)
  })
  
  ## score‑button state ----------------------------------------------------
  current_score <- reactiveVal("score_1")
  lapply(seq_along(score_columns), function(i){
    id <- paste0("score_", i)
    observeEvent(input[[id]], {
      current_score(id)
      lapply(seq_along(score_columns), function(j){
        btn <- paste0("score_", j)
        if(j == i){
          shinyjs::addClass(btn, "bttn-primary")
          shinyjs::removeClass(btn, "bttn-default")
        } else {
          shinyjs::removeClass(btn, "bttn-primary")
          shinyjs::addClass(btn, "bttn-default")
        }
      })
    }, ignoreInit = TRUE)
  })
  
  ## value boxes -----------------------------------------------------------
  output$box_sent <- renderbs4ValueBox({
    bs4ValueBox(
      safe_mean(current_data()$avg_sentence_length, 1),
      "Avg Sentence Length", icon("align-left"), color = "primary")
  })
  output$box_word <- renderbs4ValueBox({
    bs4ValueBox(
      safe_mean(current_data()$avg_word_length, 1),
      "Avg Word Length", icon("sort-alpha-down"), color = "secondary")
  })
  output$box_ttr <- renderbs4ValueBox({
    bs4ValueBox(
      safe_mean(current_data()$type_token_ratio, 2),
      "Type‑Token Ratio", icon("font"), color = "success")
  })
  output$box_sub <- renderbs4ValueBox({
    bs4ValueBox(
      safe_mean(current_data()$subordinate_clauses, 2),
      "Subordinate Clauses", icon("code-branch"), color = "danger")
  })
  
  ## ridgeline (all scores) -------------------------------------------------
  # ---- RIDGELINE : all scores (robust, no unused levels) ------------------
  output$ridge_all <- renderPlot({
    
    dat <- current_data(); req(nrow(dat) > 0)
    
    ## 1. keep metrics that have ≥2 distinct numeric values
    valid_cols <- score_columns[
      vapply(dat[score_columns], function(v){
        v <- v[!is.na(v)]
        length(unique(v)) >= 2
      }, logical(1))
    ]
    if(length(valid_cols) == 0){
      plot.new(); text(.5,.5,"No numeric variation to plot"); return()
    }
    
    ## 2. corresponding labels + colours (same order)
    valid_labels <- score_labels[match(valid_cols, score_columns)]
    valid_colors <- score_colors[match(valid_labels, score_labels)]
    
    ## 3. long format with *only* those metrics, factor rebuilt
    dat_long <- dat |>
      select(all_of(valid_cols)) |>
      pivot_longer(everything(), names_to = "Score", values_to = "Value") |>
      mutate(Score = factor(Score, levels = valid_cols, labels = valid_labels))
    
    ## 4. plot
    ggplot(dat_long, aes(Value, Score, fill = Score)) +
      geom_density_ridges(scale = 2.8, alpha = 0.75) +
      scale_fill_manual(values = valid_colors, drop = FALSE) +
      labs(
        title = paste(
          "All readability scores –",
          c(`4o` = "4‑o", `4o_mini` = "4‑o‑mini", `corpus` = "Corpus")[input$model_sel]
        ),
        x = "Score value", y = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = .5, face = "bold"))
  })
  
  
  ## ridgeline (selected score across datasets) ----------------------------
  output$ridge_compare <- renderPlot({
    idx  <- as.integer(str_remove(current_score(), "score_"))
    sel  <- score_columns[idx]
    lab  <- score_labels[idx]
    keep <- input$txt_filter
    mk   <- function(df, lbl){ filter_by_type(df, keep) %>% mutate(Dataset = lbl) }
    
    bind_rows(
      mk(data_4o,      "4‑o"),
      mk(data_4o_mini, "4‑o‑mini"),
      mk(data_corpus,  "Corpus")
    ) |>
      ggplot(aes(.data[[sel]], Dataset, fill = Dataset)) +
      geom_density_ridges_gradient(scale = 3, gradient_lwd = 1, calc_ecdf = TRUE, alpha = 0.75) +
      scale_fill_manual(values = c("4‑o" = "#1f77b4",
                                   "4‑o‑mini" = "#ff7f0e",
                                   "Corpus" = "#2ca02c")) +
      labs(title = paste(lab, "across datasets"), x = "Score value", y = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = .5, face = "bold"))
  })
  
  ## histogram -------------------------------------------------------------
  output$hist_plot <- renderPlot({
    d <- current_data()
    if(nrow(d) == 0){ plot.new(); text(.5,.5,"No data"); return() }
    ggplot(d, aes(complexity)) +
      geom_histogram(bins = 25, alpha = .85, fill = "#2ca02c") +
      labs(title = "Complexity", x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(hjust = .5, face = "bold"))
  })
  
  ## scatter: prompt vs response ------------------------------------------
  output$scatter_plot <- renderPlot({
    
    idx       <- as.integer(str_remove(current_score(), "score_"))
    met_name  <- score_columns[idx]
    met_label <- score_labels[idx]
    model_lbl <- c(`4o`="4‑o", `4o_mini`="4‑o‑mini", `corpus`="Corpus")[input$model_sel]
    
    df <- switch(input$model_sel,
                 "4o" = data_4o, "4o_mini" = data_4o_mini, "corpus" = data_corpus)
    
    if(!all(c("text_type", met_name) %in% names(df))){
      plot.new(); text(.5,.5,"Required columns missing"); return()
    }
    
    prompts   <- df %>% filter(text_type == "prompt",   !is.na(.data[[met_name]])) %>%
      mutate(rid = row_number())
    responses <- df %>% filter(text_type == "response", !is.na(.data[[met_name]])) %>%
      mutate(rid = row_number())
    
    pair_n <- min(nrow(prompts), nrow(responses))
    if(pair_n == 0){ plot.new(); text(.5,.5,"No paired values"); return() }
    
    scatter_df <- tibble(
      prompt   = prompts[[met_name]][seq_len(pair_n)],
      response = responses[[met_name]][seq_len(pair_n)]
    )
    
    ggplot(scatter_df, aes(prompt, response)) +
      geom_point(alpha = .7, size = 2.2, colour = score_colors[idx]) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
      labs(
        title = paste(model_lbl, "–", met_label, "(Prompt vs Response)"),
        x = "Prompt value", y = "Response value"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(hjust = .5, face = "bold"))
  })
}

shinyApp(ui, server)

