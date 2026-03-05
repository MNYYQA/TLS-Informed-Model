options(encoding = "UTF-8")
library(shiny)
library(ggplot2)
required_rfsrc <- "3.3.1"

get_rfsrc_ver <- function() {
  if (!requireNamespace("randomForestSRC", quietly = TRUE)) return(NULL)
  as.character(packageVersion("randomForestSRC"))
}

v <- get_rfsrc_ver()
if (is.null(v) || v != required_rfsrc) {
  message("This app requires randomForestSRC == ", required_rfsrc,
          ". Current: ", ifelse(is.null(v), "not installed", v))
  install.packages("remotes", repos="https://cloud.r-project.org")
  remotes::install_version("randomForestSRC", version = required_rfsrc,
                           repos="https://cloud.r-project.org")
  stop("randomForestSRC version fixed. Please restart R and re-run the app.")
}

library(randomForestSRC)
Random_Forest_t <- readRDS("./1rf_model.rds")
stopifnot(inherits(Random_Forest_t, "rfsrc"))
if (is.null(Random_Forest_t$forest)) stop("Model forest is NULL. The model was saved without forest=TRUE, cannot predict.")
TLS_CUTOFF <- 0.060   # TODO: replace with your TLS cutoff
IBI_CUTOFF <- 18.8   # TODO: replace with your IBI cutoff
lvl <- list(
  Group  = c("High", "Low"),
  IBIG   = c("High", "Low"),
  N      = c("N0", "N1-3"),
  Grade  = c("I-II", "III"),
  T      = c("High", "Low"),
  Lauren = c("DI", "IN", "MI")
)
convert_high_low <- function(x, cutoff, high_label = "High", low_label = "Low", high_if = ">=") {
  if (is.null(x) || is.na(x)) return(NA_character_)
  if (high_if == ">=") {
    ifelse(x >= cutoff, high_label, low_label)
  } else {
    ifelse(x > cutoff, high_label, low_label)
  }
}
convert_tls_group <- function(tls_value) {
    convert_high_low(tls_value, TLS_CUTOFF, high_label = "High", low_label = "Low", high_if = ">=")
}
convert_ibi_group <- function(ibi_value) {
  convert_high_low(ibi_value, IBI_CUTOFF, high_label = "High", low_label = "Low", high_if = ">=")
}
convert_grade <- function(g) {
  if (g %in% c("I", "II")) "I-II"
  else if (g == "III") "III"
  else NA_character_
}
convert_T <- function(t) {
  if (t %in% c("T2")) "Low"
  else if (t %in% c("T3", "T4")) "High"
  else NA_character_
}
convert_N <- function(n) {
  if (n == "N0") "N0"
  else if (n %in% c("N1", "N2", "N3")) "N1-3"
  else NA_character_
}
convert_Lauren <- function(l) {
  if (l == "Diffuse") "DI"
  else if (l == "Intestinal") "IN"
  else if (l == "Mixed/others") "MI"
  else NA_character_
}
S_at <- function(times, Svec, t0) {
  as.numeric(approx(x = times, y = Svec, xout = t0, rule = 2)$y)
}
ui <- fluidPage(
  titlePanel("TLS-Informed Model Predicts Survival Calculator"),
    fluidRow(
    column(1, offset = 0.5),
    column(3,
           numericInput("TLS_val",
                        "TLS density",
                        value = TLS_CUTOFF, min = 0, step = 0.0001, width = "100%")),
    
    column(3,
           numericInput("IBI_val",
                        "Inflammatory Burden Index",
                        value = IBI_CUTOFF, min = 0, step = 0.0001, width = "100%")),
    column(3,
           selectInput("Lauren_in",
                       "Lauren classification",
                       choices = c("Diffuse", "Intestinal", "Mixed/others"),
                       selected = "Diffuse",
                       width = "100%")),
    column(1, offset = 0.5)
  ),
  fluidRow(
    column(1, offset = 0.5),
    column(3,
           selectInput("T_in",
                       "T stage",
                       choices = c("T2", "T3", "T4"),
                       selected = "T2",
                       width = "100%")),
    column(3,
           selectInput("N_in",
                       "N stage",
                       choices = c("N0", "N1", "N2", "N3"),
                       selected = "N3",
                       width = "100%")),
    
    column(3,
           selectInput("Grade_in",
                       "Histological Grade",
                       choices = c("I", "II", "III"),
                       selected = "II",
                       width = "100%")),
    
    column(1, offset = 0.5)
  ),
  fluidRow(
    column(1, offset = 0.5),
    column(9, align = "center",
           actionButton("go", "Predict", class = "btn-primary")),
    column(1, offset = 0.5)
  ),
  br(),
  fluidRow(
    column(1, offset = 0.5),
    column(9,
           h3("Results"),
           verbatimTextOutput("res_text"),
           plotOutput("surv_plot", height = 430)),
    column(1, offset = 0.5)
  )
)
server <- function(input, output, session) {
     newdata <- reactive({
    
    Group_val  <- convert_tls_group(as.numeric(input$TLS_val))  
    IBIG_val   <- convert_ibi_group(as.numeric(input$IBI_val)) 
    N_val      <- convert_N(input$N_in)                        
    Grade_val  <- convert_grade(input$Grade_in)                
    T_val      <- convert_T(input$T_in)                        
    Lauren_val <- convert_Lauren(input$Lauren_in)              
    
    data.frame(
      Group  = factor(Group_val,  levels = lvl$Group),
      IBIG   = factor(IBIG_val,   levels = lvl$IBIG),
      N      = factor(N_val,      levels = lvl$N),
      Grade  = factor(Grade_val,  levels = lvl$Grade),
      T      = factor(T_val,      levels = lvl$T),
      Lauren = factor(Lauren_val, levels = lvl$Lauren)
    )
  })
  
  pred_obj <- eventReactive(input$go, {
    nd <- newdata()
    if (any(is.na(nd))) {
      stop("Some inputs could not be mapped to model levels (NA). Please check your cutoffs or selections.")
    }
    pr <- predict(Random_Forest_t, newdata = nd)
    times <- pr$time.interest
    Svec  <- pr$survival[1, ]
    t3 <- 3
    t5 <- 5
    
    S3 <- S_at(times, Svec, t3)
    S5 <- S_at(times, Svec, t5)
    
    list(times = times, Svec = Svec, t3 = t3, t5 = t5, S3 = S3, S5 = S5, nd = nd)
  })
  
  output$res_text <- renderText({
    req(pred_obj())
    po <- pred_obj()
    
    paste0(
      "3-year OS (S(", po$t3, ")) = ", round(po$S3, 4),
      "  (Event risk = ", round(1 - po$S3, 4), ")\n",
      "5-year OS (S(", po$t5, ")) = ", round(po$S5, 4),
      "  (Event risk = ", round(1 - po$S5, 4), ")\n\n",
      "Note: OS probabilities are extracted from the individualized survival function predicted by RSF."
    )
  })
  
  output$surv_plot <- renderPlot({
    
    req(pred_obj())
    po <- pred_obj()
    
    df <- data.frame(time = po$times, S = po$Svec)
    
    ggplot(df, aes(time, S)) +
      
      # 生存曲线
      geom_line(color = "#1F4E79", linewidth = 1.3) +
      
      # 曲线下方填充
      geom_area(fill = "#A6C8E1", alpha = 0.25) +
      
      # 3年 / 5年垂直虚线
      geom_vline(xintercept = po$t3, linetype = "dashed",
                 color = "#2C7FB8", linewidth = 0.8) +
      
      geom_vline(xintercept = po$t5, linetype = "dashed",
                 color = "#D95F02", linewidth = 0.8) +
      
      # 标记点
      geom_point(aes(x = po$t3, y = po$S3),
                 size = 4, color = "#2C7FB8") +
      
      geom_point(aes(x = po$t5, y = po$S5),
                 size = 4, color = "#D95F02") +
      
      # 标注框
      annotate("label",
               x = po$t3,
               y = po$S3,
               label = paste0("3-year OS\n", sprintf("%.3f", po$S3)),
               fill = "white",
               color = "#2C7FB8",
               size = 4,
               label.size = 0.3) +
      
      annotate("label",
               x = po$t5-0.03,
               y = po$S5,
               label = paste0("5-year OS\n", sprintf("%.3f", po$S5)),
               fill = "white",
               color = "#D95F02",
               size = 4,
               label.size = 0.3) +
      
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      
      coord_cartesian(ylim = c(0, 1)) +
      
      labs(
        x = "Time (Years)",
        y = "Overall Survival Probability",
        title = "Individualized Survival Prediction"
      ) +
      
      # ⭐ 核心修改在这里 ⭐
      theme_classic(base_size = 15) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        axis.text  = element_text(color = "black"),
        axis.line  = element_line(color = "black", linewidth = 0.8),
        panel.grid = element_blank(),          # 去掉灰色网格线
        panel.background = element_blank(),
        plot.background = element_blank()
      )
    
  })
 }

shinyApp(ui, server)