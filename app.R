# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
# Dummy data
sales=data.frame(
  bulan=c("Jan","Feb","Mar","Apr","Mei","Jun","Jul","Ags","Sept","Okt","Nov","Des"),
  x1=c(150000,160000,170000,180000,190000,200000,210000,220000,230000,240000,250000,260000),
  x2=c(8000,9500,10000,10500,11000,9000,11500,12000,12500,13000,14000,15000),
  x3=c(5.0,4.5,4.8,4.6,5.1,4.7,4.9,5.0,5.2,5.3,5.4,5.5),
  x4=c(8.5,8.2,8.4,8.5,8.6,8.7,8.8,8.9,8.7,8.8,8.9,9.0),
  x5=c(20000,22000,25000,23000,30000,28000,27000,35000,40000,45000,50000,60000),
  y=c(120,150,160,165,180,170,190,210,230,250,300,350)
)
# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Sales Volume Estimation Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary Model (Full)", tabName = "summary_model_full", icon = icon("info")),
      menuItem("Sales Prediction (FullModel)", tabName = "sales_prediction_full", icon = icon("calculator")),
      menuItem("Summary Model (Significant)", tabName = "summary_model_significant", icon = icon("info")),
      menuItem("Sales Prediction (Significant)", tabName = "sales_prediction_significant", icon = icon("calculator")),
      menuItem("Data E-Commerce", tabName = "Data_ECommerce", icon = icon("table")),  # Tab baru untuk data penjualan
      menuItem("Assumptions Test", tabName = "assumptions", icon = icon("info"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "summary_model_full",
        h2("Summary Model (Full)"),
        verbatimTextOutput("model_summary_full")
      ),
      
      tabItem(
        tabName = "sales_prediction_full",
        h2("Sales Prediction with Full Model"),
        fluidRow(
          box(
            title = "Input Predictors (Full Model)",
            width = 6,
            solidHeader = TRUE,
            numericInput("x1_full", "X1 ( Website Visitors):", value = 0),
            numericInput("x2_full", "X2 (Monthly Transactions):", value = 0),
            numericInput("x3_full", "X3 (Items/Transactions):", value = 0),
            sliderInput("x4_full", "X4 (Rating):", min = 1, max = 10, value = 1, step = 0.5),
            numericInput("x5_full", "X5 (Online Advertisements):", value = 0),
            actionButton("predict_button_full", "Predict Sales")
          ),
          box(
            title = "Sales Prediction (Full Model)",
            width = 6,
            solidHeader = TRUE,
            verbatimTextOutput("sales_prediction_full")
          )
        )
      ),
      
      tabItem(
        tabName = "summary_model_significant",
        h2("Summary Model (Significant)"),
        verbatimTextOutput("model_summary_significant")
      ),
      
      tabItem(
        tabName = "sales_prediction_significant",
        h2("Sales Prediction with Significant Model"),
        fluidRow(
          box(
            title = "Input Predictors (Significant Model)",
            width = 6,
            solidHeader = TRUE,
            numericInput("x2_significant", "X2 (Monthly Transactions):", value = 0),
            numericInput("x5_significant", "X5 (Online Advertisements):", value = 0),
            actionButton("predict_button_significant", "Predict Sales")
          ),
          box(
            title = "Sales Prediction (Significant Model)",
            width = 6,
            solidHeader = TRUE,
            verbatimTextOutput("sales_prediction_significant")
          )
        )
      ),
      tabItem(
        tabName = "Data_ECommerce",  # Tab baru untuk data penjualan
        h2("Data E-Commerce"),
        DTOutput("Data_ECommerce_table")  # Menampilkan tabel data penjualan menggunakan DT
      ),
      tabItem(tabName = "assumptions",
              fluidRow(
                column(width = 6,
                       style = "display: flex; flex-wrap: wrap;",
                       box(verbatimTextOutput("dw_test"), title = "Autocorrelation Durbin-Watson Test", width = 12),
                       box(verbatimTextOutput("bp_test"), title = "Homogeneity Breusch-Pagan Test", width = 12)
                ),
                column(width = 6,
                       style = "display: flex; flex-wrap: wrap;",
                       box(verbatimTextOutput("norm_test"), title = "Kolmogorov-Smirnov Normality Test", width = 12),
                       box(verbatimTextOutput("vif_test"), title = "Multicollinearity VIF Test", width = 12)
                )))
    )
  )
)
# Define server
server <- function(input, output) {
  
  # Deklarasikan fit_full dan fit_significant di tingkat atas server
  fit_full <- reactive({
    lm(y ~ x1 + x2 + x3 + x4 + x5, data = sales)
  })
  
  fit_significant <- reactive({
    lm(y ~ x2 + x5, data = sales)
  })
  
  # Render Summary Model (Full)
  output$model_summary_full <- renderPrint({
    summary(fit_full())
  })
  
  # Perform sales prediction with full model on button click
  observeEvent(input$predict_button_full, {
    predictors_full <- data.frame(
      x1 = input$x1_full,
      x2 = input$x2_full,
      x3 = input$x3_full,
      x4 = input$x4_full,
      x5 = input$x5_full
    )
    
    if(all(!is.na(predictors_full))) {
      sales_prediction_full <- predict(fit_full(), newdata = predictors_full)
      output$sales_prediction_full <- renderText(paste("Predicted Sales : $", round(sales_prediction_full, 2),"K"))
    } else {
      output$sales_prediction_full <- renderText("Please enter valid predictor values.")
    }
  })
  
  # Render Summary Model (Significant)
  output$model_summary_significant <- renderPrint({
    summary(fit_significant())
  })
  
  # Perform sales prediction with significant model on button click
  observeEvent(input$predict_button_significant, {
    predictors_significant <- data.frame(
      x2 = input$x2_significant,
      x5 = input$x5_significant
    )
    
    if (all(!is.na(predictors_significant))) {
      sales_prediction_significant <- predict(fit_significant(), newdata = predictors_significant)
      output$sales_prediction_significant <- renderText(paste("Predicted Sales : $", round(sales_prediction_significant, 2), "K"))
    } else {
      output$sales_prediction_significant <- renderText("Please enter valid predictor values.")
    }
  })
  
  # Menampilkan tabel data penjualan di tab "Sales Data"
  output$Data_ECommerce_table <- renderDT({
    datatable(sales, options = list(pageLength = 10))
  })
  
  # Assumptions Test
  output$dw_test <- renderPrint({
    model_reg <- lm(y ~ ., data = sales[, -1])
    dw_test <- lmtest::dwtest(model_reg)
    dw_test
  })
  
  output$bp_test <- renderPrint({
    model_reg <- lm(y ~ ., data = sales[, -1])
    bp_test <- lmtest::bptest(model_reg, studentize = TRUE)
    bp_test
  })
  
  output$norm_test <- renderPrint({
    model_reg <- lm(y ~ ., data = sales[, -1])
    norm_test <- nortest::lillie.test(model_reg$residuals)
    norm_test
  })
  
  output$vif_test <- renderPrint({
    model_reg <- lm(y ~ ., data = sales[, -1])
    vif_test <- car::vif(model_reg)
    vif_test
  })
}

# Run the application
shinyApp(ui, server)

