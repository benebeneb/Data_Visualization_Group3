# ----------------- 1. Libraries --------------------------
library(bs4Dash)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(ggridges)
library(readr)
library(stringr)
library(RColorBrewer)
library(fresh)
library(fontawesome)

# ----------------- 2. Theme + CSS ------------------------
my_theme <- create_theme(
  bs4dash_vars(
    font_family_base = "'Inter', sans-serif",
    navbar_light_color = "#003049",
    navbar_light_active_color = "#f77f00",
    navbar_light_hover_color = "#d62828"
  ),
  
  # --- CORRECTED bs4dash_color block ---
  bs4dash_color(
    blue      = "#003049", 
    cyan      = "#0077b6", 
    yellow    = "#fcbf49", 
    red       = "#d62828", 
    green     = "#38b000",
    lightblue = "#f1faee", 
    gray_900  = "#212529"
  ),
  
  # -------------------------------------
  bs4dash_yiq(
    contrasted_threshold = 180,
    text_dark  = "#003049",
    text_light = "#ffffff"
  )
)

custom_css <- "
body { font-family:'Inter',sans-serif; }
.model-buttons .bttn, .score-buttons .bttn {
  border-radius: 8px !important;
  font-weight: 500;
  padding: 8px 16px;
  margin: 4px 6px 6px 0 !important;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  transition: all 0.2s ease;
  height: auto !important;
  line-height: 1.5 !important;
  display: inline-block;
  white-space: nowrap;
  vertical-align: middle;
  text-align: center;
}
.model-buttons .bttn:active, .score-buttons .bttn:active {
  transform: scale(.98);
}
.model-buttons .bttn-primary, .score-buttons .bttn-primary {
  background-image: linear-gradient(135deg, #fcbf49, #d62828) !important;
  color: #fff !important;
  border: none;
}
.model-buttons .bttn-primary:hover, .score-buttons .bttn-primary:hover {
  background-image: linear-gradient(135deg, #fca949, #c62828) !important;
  box-shadow: 0 4px 12px rgba(0,0,0,0.15);
}
.model-buttons .bttn-default, .score-buttons .bttn-default {
  background: #f8f9fa !important;
  color: #003049 !important;
  border: 1px solid #dee2e6;
}
.model-buttons .bttn-default:hover, .score-buttons .bttn-default:hover {
  background: #e9ecef !important;
}
.card-header { background: #f8f9fa !important; border-bottom: 1px solid #dee2e6; }
.card-title { color: #003049; font-weight: 600; font-size: 1.1rem; }

/* Style Value Boxes */
.value-box {
  margin-bottom: 10px !important;
  border-radius: 8px !important;
  transition: transform 0.2s ease, box-shadow 0.2s ease;
  background: #FFFFFF !important;
  border: 1px solid #E5E7EB !important;
}
.value-box:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 12px rgba(0,0,0,0.05) !important;
}
.value-box .inner h3 { 
  font-weight: 600 !important;
  font-size: 1.8rem !important;
  font-family: 'Inter', sans-serif !important;
  color: #1F2937 !important;
}
.value-box .inner p { 
  font-weight: 500 !important;
  font-size: 0.85rem !important;
  color: #4B5563 !important;
}
.value-box .icon { 
  opacity: 0.7;
  right: 15px;
  color: #6B7280 !important;
}

/* Modern color scheme for value boxes - subtle gradients */
.bg-primary {
  background: linear-gradient(135deg, #F3F4F6, #E5E7EB) !important;
  border-left: 4px solid #4B5563 !important;
}
.bg-info {
  background: linear-gradient(135deg, #F3F4F6, #E5E7EB) !important;
  border-left: 4px solid #6B7280 !important;
}
.bg-warning {
  background: linear-gradient(135deg, #F3F4F6, #E5E7EB) !important;
  border-left: 4px solid #9CA3AF !important;
}
.bg-danger {
  background: linear-gradient(135deg, #F3F4F6, #E5E7EB) !important;
  border-left: 4px solid #D1D5DB !important;
}

/* Style for left column content - completely redesigned */
.left-column-content { 
  padding: 24px !important; 
  height: 100% !important;
  background: #FAFAFA;
  border-right: 1px solid #EEEEEE;
}

/* Logo styling - simplified and fixed */
.logo-container {
  margin-bottom: 15px;
  padding: 10px;
  height: 120px;
  display: flex;
  align-items: center;
  justify-content: center;
}

.logo-container img {
  width: 100%;
  max-height: 120px;
  object-fit: contain;
}

/* Model title styling */
.model-title {
  font-weight: 600 !important;
  font-size: 1.2rem !important;
  color: #333333;
  margin-bottom: 4px !important;
  font-family: 'Inter', sans-serif !important;
}

/* Model subtitle styling */
.model-subtitle {
  font-size: 0.85rem !important;
  color: #666666;
  margin-bottom: 10px !important;
  line-height: 1.5 !important;
}

/* Model tag - moved below description */
.model-tag {
  margin-bottom: 15px;
}

/* Metric cards - completely new design */
.metric-card {
  background: white;
  border-radius: 6px;
  padding: 16px;
  margin-bottom: 12px;
  border: 1px solid #EEEEEE;
  transition: all 0.2s ease;
}

.metric-card:hover {
  border-color: #DDDDDD;
  box-shadow: 0 2px 8px rgba(0,0,0,0.04);
}

.metric-title {
  font-size: 0.85rem;
  color: #666666;
  margin-bottom: 2px;
  font-weight: 500;
}

.metric-value {
  font-size: 1.8rem !important;
  font-weight: 600 !important;
  color: #333333;
  margin-bottom: 0;
}

.metric-icon {
  color: #999999;
  float: right;
  margin-top: -30px;
  font-size: 1.1rem;
}

.metric-bar-container {
  height: 4px;
  background: #F0F0F0;
  width: 100%;
  margin-top: 12px;
  border-radius: 2px;
  overflow: hidden;
}

.metric-bar {
  height: 100%;
  border-radius: 2px;
}

.metric-bar-1 { background: #9E9E9E; }
.metric-bar-2 { background: #757575; }
.metric-bar-3 { background: #616161; }
.metric-bar-4 { background: #424242; }

/* Tag styling */
.model-tag {
  display: inline-block;
  padding: 5px 10px;
  background: #EEEEEE;
  border-radius: 4px;
  font-size: 0.75rem;
  font-weight: 500;
  color: #333333;
  margin-bottom: 24px;
}

/* Hide header completely */
.main-header { display: none !important; }
.content-wrapper { margin-top: 0 !important; }

.value-box { margin-bottom: 10px !important; }
.value-box .inner h3 { font-size: 1.8rem !important; }
.value-box .inner p { font-size: 0.85rem !important; }
.card { margin-bottom: 10px !important; }
.card-body { padding: 10px !important; }
h4 { font-size: 1.1rem !important; margin-bottom: 8px !important; }
.model-buttons .bttn, .score-buttons .bttn { padding: 6px 12px !important; margin: 2px 4px 4px 0 !important; }
hr { margin-top: 0.5rem !important; margin-bottom: 0.5rem !important; }
br { display: none !important; }

.card-title { font-size: 0.9rem !important; }
.plot-container { margin: 0 !important; padding: 0 !important; }

/* Right column spacing improvements */
.right-column {
  padding-top: 15px;
  padding-bottom: 20px;
}

.section-header {
  margin-top: 20px;
  margin-bottom: 15px;
}

.plot-row {
  margin-bottom: 25px !important;
}

.card {
  margin-bottom: 20px !important;
}

.card-body {
  padding: 15px !important;
}

hr {
  margin-top: 1rem !important;
  margin-bottom: 1.5rem !important;
}

h4 {
  font-size: 1.1rem !important;
  margin-top: 5px !important;
  margin-bottom: 12px !important;
}

.model-buttons, .score-buttons {
  margin-bottom: 10px;
}

/* Restore br elements that we previously hid */
br { display: block !important; }

/* Keep other existing CSS properties */
.value-box { margin-bottom: 10px !important; }
.value-box .inner h3 { font-size: 1.8rem !important; }
.value-box .inner p { font-size: 0.85rem !important; }
.plot-container { margin: 0 !important; padding: 5px !important; }
"

# ----------------- 3. Helpers & Data ---------------------
score_columns <- c(
  "flesch_kincaid_grade","flesch_reading_ease","gunning_fog",
  "smog_index","dale_chall","lexical_density","passive_ratio",
  "errors","transitions","subjectivity","formality","complexity"
)
score_labels <- c(
  "F‑K Grade","F‑K Ease","Gunning Fog","SMOG","Dale‑Chall",
  "Lexical Density","Passive Ratio","Errors","Transitions",
  "Subjectivity","Formality","Complexity"
)

score_colors <- setNames(
  c("#219EBC","#FB8500","#FFB703","#8ECAE6","#023047",
    "#FFB703","#FB8500","#219EBC","#8ECAE6","#023047",
    "#FFB703","#FB8500"),
  score_labels
)

load_data <- function(csv){
  if(!file.exists(csv)){
    warning(paste("File",csv,"not found – returning empty tibble")); return(tibble())
  }
  dat <- readr::read_csv(csv, show_col_types = FALSE)
  
  metric_cols <- c("avg_sentence_length", "avg_word_length", "type_token_ratio", "subordinate_clauses")
  cols_to_cap <- unique(c(score_columns, metric_cols))
  
  for(col in cols_to_cap){
    if(col %in% names(dat) && is.numeric(dat[[col]])){
      qs <- quantile(dat[[col]], c(.01,.99), na.rm = TRUE)
      if (!is.na(qs[1]) && !is.na(qs[2])) {
        dat[[col]] <- pmax(pmin(dat[[col]], qs[2]), qs[1])
      }
    }
  }
  dat
}

# ---- CSV PATHS ------------------------------------------
data_4o       <- load_data("4o.csv") 
data_4o_mini  <- load_data("4o-mini.csv")
data_corpus   <- load_data("news_articles.csv")
data_deepseek <- load_data("deepseek.csv")
# ---------------------------------------------------------

filter_by_type <- function(df, sel = c("both","prompt","response")){
  sel <- match.arg(sel)
  if(sel=="both" || !"text_type" %in% names(df)) return(df)
  dplyr::filter(df, text_type == sel)
}

# ----------------- 4. UI --------------------------------
ui <- bs4DashPage(
  title = "AI Readability Dashboard",
  dark  = NULL,
  header = bs4DashNavbar(
    skin = "light",
    status = "white",
    border = FALSE,
    compact = TRUE,
    fixed = FALSE,
    tags$style(".main-header { display: none !important; }") 
  ),
  sidebar = bs4DashSidebar(disable = TRUE),
  body = bs4DashBody(
    useShinyjs(), use_theme(my_theme),
    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = TRUE),
      tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap", rel = "stylesheet"),
      tags$style(HTML(custom_css))
    ),
    
    # ------- Main Row for 20/80 Split --------
    fluidRow(
      
      # ------- Left Column (20% width) --------
      column(width = 2,
             div(class = "left-column-content",
                 div(class = "logo-container",
                     shiny::imageOutput("logo", height = "auto")
                 ),
                 
                 h3(textOutput("model_title"), class = "model-title"),
                 p("Linguistic metrics provide insights into model output characteristics", 
                   class = "model-subtitle"),
                 
                 div(class = "model-tag", textOutput("model_tag")),
                 
                 div(class = "metric-card",
                     div(class = "metric-title", "Sentence Length"),
                     div(class = "metric-value", textOutput("metric_sentence_length")),
                     div(class = "metric-icon", icon("ruler-horizontal"))
                 ),
                 
                 div(class = "metric-card",
                     div(class = "metric-title", "Word Length"),
                     div(class = "metric-value", textOutput("metric_word_length")),
                     div(class = "metric-icon", icon("text-width"))
                 ),
                 
                 div(class = "metric-card",
                     div(class = "metric-title", "Type-Token Ratio"),
                     div(class = "metric-value", textOutput("metric_ttr")),
                     div(class = "metric-icon", icon("book-reader"))
                 ),
                 
                 div(class = "metric-card",
                     div(class = "metric-title", "Subordinate Clauses"),
                     div(class = "metric-value", textOutput("metric_clauses")),
                     div(class = "metric-icon", icon("sitemap"))
                 )
             )
      ), 
      
      # ------- Right Column (80% width) - Contains original dashboard content --------
      column(width = 10, style="padding-left:15px;padding-right:15px;",
             
             div(class="right-column",
                 # ---- model selector --------------------------------
                 div(class="section-header",
                     h4("Select Model", style="margin-left:4px;color:#003049;font-weight:600;"),
                     div(class="model-buttons",
                         actionBttn("model_4o",       "GPT‑4o",      style="material-flat", size="sm", color="primary"),
                         actionBttn("model_4o_mini",  "GPT‑4o Mini", style="material-flat", size="sm", color="default"),
                         actionBttn("model_corpus",   "News Corpus", style="material-flat", size="sm", color="default"),
                         actionBttn("model_deepseek", "DeepSeek",    style="material-flat", size="sm", color="default")
                     )
                 ),
                 hr(),
                 
                 # ---- row 1 : ridgeline + histogram ----------------
                 div(class="plot-row",
                     fluidRow(
                       bs4Card(width = 7, title = textOutput("ridge_all_title", inline=TRUE),
                               status="lightblue", solidHeader=FALSE, plotOutput("ridge_all", height="320px")),
                       bs4Card(width = 5, title = textOutput("hist_plot_title",  inline=TRUE),
                               status="lightblue", solidHeader=FALSE, plotOutput("hist_plot",  height="320px"))
                     )
                 ),
                 
                 # ---- score selector -------------------------------
                 div(class="section-header",
                     h4("Select Score Metric", style="margin-left:4px;color:#003049;font-weight:600;"),
                     div(class="score-buttons",
                         lapply(seq_along(score_columns), function(i){
                           actionBttn(paste0("score_",i), score_labels[i],
                                      style="material-flat", size="sm",
                                      color=if(i==1) "primary" else "default")
                         })
                     )
                 ),
                 hr(),
                 
                 # ---- row 2 : compare ridge + scatter --------------
                 div(class="plot-row",
                     fluidRow(
                       bs4Card(width = 7, title = textOutput("ridge_compare_title", inline=TRUE),
                               status="lightblue", solidHeader=FALSE, plotOutput("ridge_compare", height="320px")),
                       bs4Card(width = 5, title = textOutput("scatter_plot_title",  inline=TRUE),
                               status="lightblue", solidHeader=FALSE, plotOutput("scatter_plot",  height="320px"))
                     )
                 )
             )
      ) 
    ) 
  ) 
) 

# ----------------- 5. Server -----------------------------
server <- function(input, output, session){
  message("Static file directory (www): ", getwd(), "/www")
  if(dir.exists("www")) {
    message("Contents of www directory:")
    message(paste(list.files("www"), collapse="\n"))
  } else {
    message("Warning: www directory not found!")
  }
  
  # -- reactive bookkeeping -------------------------------
  current_model_id   <- reactiveVal("4o")
  current_model_name <- reactiveVal("GPT‑4o")
  current_score_index<- reactiveVal(1)
  
  model_map <- list("4o"="GPT‑4o","4o_mini"="GPT‑4o Mini",
                    "corpus"="News Corpus","deepseek"="DeepSeek")
  
  lapply(names(model_map), function(id){
    observeEvent(input[[paste0("model_",id)]], {
      current_model_id(id); current_model_name(model_map[[id]])
      lapply(names(model_map), function(other_id){
        btn_id <- paste0("model_", other_id)
        if(other_id == id){
          runjs(sprintf("$('#%s').removeClass('bttn-default').addClass('bttn-primary');", btn_id))
        } else {
          runjs(sprintf("$('#%s').removeClass('bttn-primary').addClass('bttn-default');", btn_id))
        }
      })
    }, ignoreInit=TRUE)
  })
  
  lapply(seq_along(score_columns), function(i){
    observeEvent(input[[paste0("score_",i)]], {
      current_score_index(i)
      lapply(seq_along(score_columns), function(other_i){
        btn_id <- paste0("score_", other_i)
        if(other_i == i){
          runjs(sprintf("$('#%s').removeClass('bttn-default').addClass('bttn-primary');", btn_id))
        } else {
          runjs(sprintf("$('#%s').removeClass('bttn-primary').addClass('bttn-default');", btn_id))
        }
      })
    }, ignoreInit=TRUE)
  })
  
  # -- data reactive ---------------------------------------
  current_data_filtered <- reactive({
    dat <- switch(current_model_id(),
                  "4o"       = data_4o,
                  "4o_mini"  = data_4o_mini,
                  "corpus"   = data_corpus,
                  "deepseek" = data_deepseek,
                  tibble()) 
    
    if (current_model_id() != "corpus" && "text_type" %in% names(dat)) {
      dat_filtered <- dplyr::filter(dat, text_type == "response")
      if (nrow(dat_filtered) == 0) {
        warning(paste("No rows with text_type='response' found for model:", current_model_id(), "- Using original data if available."))
        if (nrow(dat) > 0) dat else tibble() 
      } else {
        dat_filtered
      }
    } else {
      dat
    }
  })
  
  
  current_data <- reactive({
    dat <- current_data_filtered()
    validate(need(is.data.frame(dat) && nrow(dat) > 0,
                  paste("No displayable data available for", current_model_name(), "(check filtering and data source).")))
    dat
  })
  
  # ========================================================
  # NEW: Value Box Outputs
  # ========================================================
  output$avg_sent_len_box <- renderValueBox({
    dat <- current_data()
    col_name <- "avg_sentence_length" 
    avg_val <- if(col_name %in% names(dat) && is.numeric(dat[[col_name]])) {
      mean(dat[[col_name]], na.rm = TRUE)
    } else { NA }
    
    bs4ValueBox(
      value = if(is.na(avg_val) || !is.finite(avg_val)) "N/A" else format(round(avg_val, 1), nsmall = 1),
      subtitle = "Avg Sentence Length",
      icon = icon("ruler-horizontal"),
      color = "primary", 
      elevation = 2
    )
  })
  
  output$avg_word_len_box <- renderValueBox({
    dat <- current_data()
    col_name <- "avg_word_length" 
    avg_val <- if(col_name %in% names(dat) && is.numeric(dat[[col_name]])) {
      mean(dat[[col_name]], na.rm = TRUE)
    } else { NA }
    
    bs4ValueBox(
      value = if(is.na(avg_val) || !is.finite(avg_val)) "N/A" else format(round(avg_val, 1), nsmall = 1),
      subtitle = "Avg Word Length",
      icon = icon("text-width"),
      color = "info", 
      elevation = 2
    )
  })
  
  output$ttr_box <- renderValueBox({
    dat <- current_data()
    col_name <- "type_token_ratio" 
    avg_val <- if(col_name %in% names(dat) && is.numeric(dat[[col_name]])) {
      mean(dat[[col_name]], na.rm = TRUE)
    } else { NA }
    
    bs4ValueBox(
      value = if(is.na(avg_val) || !is.finite(avg_val)) "N/A" else format(round(avg_val, 3), nsmall = 3),
      subtitle = "Type-Token Ratio",
      icon = icon("book-reader"),
      color = "warning", 
      elevation = 2
    )
  })
  
  output$sub_clause_box <- renderValueBox({
    dat <- current_data()
    col_name <- "subordinate_clauses" 
    avg_val <- if(col_name %in% names(dat) && is.numeric(dat[[col_name]])) {
      mean(dat[[col_name]], na.rm = TRUE)
    } else { NA }
    
    bs4ValueBox(
      value = if(is.na(avg_val) || !is.finite(avg_val)) "N/A" else format(round(avg_val, 1), nsmall = 1),
      subtitle = "Subordinate Clauses (Avg)",
      icon = icon("sitemap"),
      color = "danger", 
      elevation = 2
    )
  })
  
  
  # ========================================================
  # Plot Titles
  # ========================================================
  output$ridge_all_title <- renderText({ paste("Score Distributions:", current_model_name()) })
  output$hist_plot_title <- renderText({ paste("Complexity Distribution:", current_model_name()) })
  output$ridge_compare_title <- renderText({
    idx <- current_score_index()
    paste(score_labels[idx], "Across All Datasets")
  })
  output$scatter_plot_title <- renderText({
    idx <- current_score_index()
    paste(score_labels[idx],"– Prompt vs. Response:", current_model_name())
  })
  
  # ========================================================
  # PLOTS
  # ========================================================
  # ---- 1. Ridgeline of all scores (normalized) ---------
  output$ridge_all <- renderPlot({
    dat <- current_data();
    available_score_cols <- score_columns[score_columns %in% names(dat)]
    validate(need(length(available_score_cols) > 0, "No score columns found in the data."))
    valid_cols <- available_score_cols[
      vapply(dat[available_score_cols], function(v){
        is.numeric(v) && length(unique(v[!is.na(v)])) >= 2
      }, logical(1))
    ]
    validate(need(length(valid_cols)>0, "No numeric variation in available scores to plot"))
    valid_labels <- score_labels[match(valid_cols, score_columns)]
    valid_colors <- score_colors[valid_labels]
    
    dat_scaled <- dat %>%
      select(all_of(valid_cols)) %>%
      mutate(across(everything(),
                    ~ {
                      min_val <- min(.x, na.rm = TRUE)
                      max_val <- max(.x, na.rm = TRUE)
                      range_val <- max_val - min_val
                      if (range_val == 0 || !is.finite(range_val)) { 0 }
                      else { (.x - min_val) / range_val }
                    })) %>%
      mutate(across(everything(), ~ ifelse(is.finite(.x), .x, 0)))
    
    dat_long <- dat_scaled %>%
      pivot_longer(everything(), names_to="Score", values_to="Value") %>%
      mutate(Score = factor(Score, levels = valid_cols, labels = valid_labels))
    
    ggplot(dat_long, aes(Value, Score, fill=Score)) +
      geom_density_ridges_gradient(scale=3, gradient_lwd=1, calc_ecdf=TRUE, alpha=.75, rel_min_height=.01, na.rm = TRUE) +
      scale_fill_manual(values=valid_colors, drop=FALSE) +
      labs(title=NULL, x="Normalized Score Value", y=NULL) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position="none",
        plot.title=element_text(hjust=.5, face="bold", size=11),
        plot.margin = margin(10, 10, 10, 10),
        axis.title = element_text(size=10),
        axis.text = element_text(size=9),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  }, res = 110)
  
  # ---- 2. Histogram of Complexity ------------------------
  output$hist_plot <- renderPlot({
    d <- current_data()
    validate(
      need("complexity" %in% names(d), "Data is missing the 'complexity' column."),
      need(is.numeric(d$complexity), "'complexity' column is not numeric."),
      need(length(unique(na.omit(d$complexity))) > 1, "No variation in 'complexity' values.")
    )
    
    bin_width <- diff(range(d$complexity, na.rm = TRUE)) / 30
    
    ggplot(d, aes(complexity)) +
      geom_histogram(
        binwidth = bin_width,
        boundary = 0,
        color = "white",  
        fill = "#FFB703",
        alpha = 0.85,
        na.rm = TRUE
      ) +
      labs(title=NULL, x="Complexity Score", y="Count") +
      theme_minimal(base_size = 11) +
      theme(
        legend.position="none",
        plot.title=element_text(hjust=.5, face="bold", size=11),
        plot.margin = margin(10, 10, 10, 10),
        axis.title = element_text(size=10),
        axis.text = element_text(size=9),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      )
  }, res = 110)
  
  
  # ---- 3. Ridgeline of selected score across datasets ----
  output$ridge_compare <- renderPlot({
    idx       <- current_score_index()
    sel_col   <- score_columns[idx]
    sel_label <- score_labels[idx]
    
    mk <- function(df, df_name, col_name){
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 || !col_name %in% names(df)) {
        warning(paste("Skipping", df_name, "- invalid data or missing column:", col_name))
        return(NULL)
      }
      
      temp_df <- df
      if(df_name != "News Corpus" && "text_type" %in% names(df)) {
        filtered_df <- dplyr::filter(df, text_type == "response")
        if(nrow(filtered_df) > 0) {
          temp_df <- filtered_df
        } else {
          warning(paste("Skipping", df_name, "for comparison - no 'response' data found."))
          return(NULL)
        }
      }
      
      if (is.numeric(temp_df[[col_name]]) && length(unique(na.omit(temp_df[[col_name]]))) > 1) {
        temp_df %>% select(all_of(col_name)) %>% mutate(Dataset = df_name)
      } else {
        warning(paste("Skipping", df_name, "- column", col_name, "is not numeric or has no variation after filtering."))
        return(NULL)
      }
    }
    
    all_data_list <- list(
      mk(data_4o,       "GPT‑4o",      sel_col),
      mk(data_4o_mini,  "GPT‑4o Mini", sel_col),
      mk(data_corpus,   "News Corpus", sel_col), 
      mk(data_deepseek, "DeepSeek",    sel_col)
    )
    all_data_list <- all_data_list[!sapply(all_data_list, is.null)] 
    
    validate(need(length(all_data_list) > 0, paste("No datasets contain valid, comparable data for the selected score:", sel_label)))
    
    combined_data <- bind_rows(all_data_list)
    validate(need(nrow(combined_data) > 0, paste("No combined data available for:", sel_label, "after processing.")))
    validate(need(sel_col %in% names(combined_data), "Selected score column missing from combined data (internal error)."))
    
    compare_colors <- c("GPT‑4o"="#FFB703", "GPT‑4o Mini"="#219EBC", "News Corpus"="#FB8500", "DeepSeek"="#8ECAE6")
    datasets_present <- unique(combined_data$Dataset)
    plot_colors <- compare_colors[names(compare_colors) %in% datasets_present]
    
    ggplot(combined_data, aes(.data[[sel_col]], Dataset, fill=Dataset)) +
      geom_density_ridges_gradient(scale=3, gradient_lwd=1, calc_ecdf=TRUE, alpha=.75, na.rm = TRUE) +
      scale_fill_manual(values=plot_colors, breaks=datasets_present) +
      labs(title=NULL, x=paste(sel_label, "Score Value"), y=NULL) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position="none",
        plot.title=element_text(hjust=.5, face="bold", size=11),
        plot.margin = margin(10, 10, 10, 10),
        axis.title = element_text(size=10),
        axis.text = element_text(size=9)
      )
  }, res = 110)
  
  
  # ---- 4. Scatter Prompt vs Response ---------------------
  output$scatter_plot <- renderPlot({
    idx       <- current_score_index()
    met_name  <- score_columns[idx]
    met_label <- score_labels[idx]
    df <- switch(current_model_id(),
                 "4o"      = data_4o, "4o_mini" = data_4o_mini,
                 "corpus"  = data_corpus, "deepseek"= data_deepseek, tibble())
    
    validate(
      need(is.data.frame(df) && nrow(df) > 0, "No data loaded for this model."),
      need(if(current_model_id() != "corpus") "text_type" %in% names(df) else TRUE, "Column 'text_type' missing for prompt/response plot."),
      need(met_name %in% names(df), paste("Required score column ('", met_name, "') missing.", sep="")),
      need(is.numeric(df[[met_name]]), paste("Score '", met_name, "' is not numeric.", sep=""))
    )
    
    if(current_model_id() == "corpus") {
      plot.new(); text(0.5, 0.5, "Prompt vs Response plot not applicable\nfor News Corpus data.", cex = 1.2); return()
    }
    
    prompts   <- df %>% filter(text_type=="prompt",   !is.na(.data[[met_name]]))
    responses <- df %>% filter(text_type=="response", !is.na(.data[[met_name]]))
    validate(need(nrow(prompts) > 0, "No 'prompt' data found."), need(nrow(responses) > 0, "No 'response' data found."))
    pair_n <- min(nrow(prompts), nrow(responses))
    validate(need(pair_n > 0, "No matching prompt/response pairs found (or data is missing)."))
    
    scatter_df <- tibble(
      prompt   = df %>% filter(text_type=="prompt") %>% slice(seq_len(pair_n)) %>% pull(.data[[met_name]]),
      response = df %>% filter(text_type=="response") %>% slice(seq_len(pair_n)) %>% pull(.data[[met_name]])
    ) %>% filter(!is.na(prompt), !is.na(response)) 
    
    validate(need(nrow(scatter_df) > 0, "No valid numeric pairs after filtering NAs."))
    validate(need(length(unique(scatter_df$prompt)) > 1 || length(unique(scatter_df$response)) > 1,
                  "Not enough variation in paired values to plot scatter."))
    
    plot_color <- score_colors[met_label]
    if (is.na(plot_color) || length(plot_color) == 0) { plot_color <- "#0077b6" } 
    
    ggplot(scatter_df, aes(prompt, response)) +
      geom_point(alpha=.2, shape=20, size=1.5, colour=plot_color) +
      geom_abline(slope=1, intercept=0, linetype="dashed", colour="grey50") +
      geom_smooth(method="lm", se=FALSE, color="black", linetype="dotted", linewidth=0.5) + 
      labs(title=NULL, x=paste("Prompt", met_label, "Value"), y=paste("Response", met_label, "Value")) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position="none",
        plot.title=element_text(hjust=.5, face="bold", size=11),
        plot.margin = margin(10, 10, 10, 10),
        axis.title = element_text(size=10),
        axis.text = element_text(size=9)
      )
  }, res = 110)
  
  output$model_tag <- renderText({
    toupper(current_model_name())
  })
  
  output$model_title <- renderText({
    paste("Language Metrics")
  })
  
  output$metric_sentence_length <- renderText({
    dat <- current_data()
    col_name <- "avg_sentence_length"
    avg_val <- if(col_name %in% names(dat) && is.numeric(dat[[col_name]])) {
      mean(dat[[col_name]], na.rm = TRUE)
    } else { NA }
    
    if(is.na(avg_val) || !is.finite(avg_val)) "N/A" else format(round(avg_val, 1), nsmall = 1)
  })
  
  output$metric_word_length <- renderText({
    dat <- current_data()
    col_name <- "avg_word_length"
    avg_val <- if(col_name %in% names(dat) && is.numeric(dat[[col_name]])) {
      mean(dat[[col_name]], na.rm = TRUE)
    } else { NA }
    
    if(is.na(avg_val) || !is.finite(avg_val)) "N/A" else format(round(avg_val, 1), nsmall = 1)
  })
  
  output$metric_ttr <- renderText({
    dat <- current_data()
    col_name <- "type_token_ratio"
    avg_val <- if(col_name %in% names(dat) && is.numeric(dat[[col_name]])) {
      mean(dat[[col_name]], na.rm = TRUE)
    } else { NA }
    
    if(is.na(avg_val) || !is.finite(avg_val)) "N/A" else format(round(avg_val, 3), nsmall = 3)
  })
  
  output$metric_clauses <- renderText({
    dat <- current_data()
    col_name <- "subordinate_clauses"
    avg_val <- if(col_name %in% names(dat) && is.numeric(dat[[col_name]])) {
      mean(dat[[col_name]], na.rm = TRUE)
    } else { NA }
    
    if(is.na(avg_val) || !is.finite(avg_val)) "N/A" else format(round(avg_val, 1), nsmall = 1)
  })
  
  output$logo <- renderImage({
    list(
      src = file.path(getwd(), "www", "actual_logo.png"),
      contentType = "image/png",
      width = "100%",
      height = "120px",
      style = "object-fit: contain;",
      alt = "Logo"
    )
  }, deleteFile = FALSE)
} 

# ----------------- 6. Run app ----------------------------
shinyApp(ui, server)

