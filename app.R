library(shiny)
library(shinythemes)
library(plotly)
library(tibble)
library(dplyr)

ui <- fluidPage(
  title = "Linear Regression",
  theme = shinytheme("darkly"),
  h1("Linear Regression"),              
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "dv",label = "Dependent Variable",choices = colnames(mtcars)),
      selectInput(inputId = "iv",label = "Independent Variable",multiple = T,choices = colnames(mtcars)),
      hr(),
      actionButton(inputId = "run",label = "Run"),
      hr(),
      tags$a(href = "https://github.com/paladinic/shiny_lm","Code")
    ),
    mainPanel(
      textOutput(outputId = "r2"),
      tableOutput(outputId = "result")
    )
  )
)

server <- function(input, output, session) {
  
  get_result = eventReactive(input$run,{
    
    ivs = input$iv
    dv = input$dv
    
    formula = as.formula(
      paste0(
        dv,
        "~",
        paste0(ivs,collapse = " + ")
        )
      )
    
    model = lm(formula = formula,data = mtcars)
    
  })
  get_r2 = reactive({
    
    model = get_result()
    
    summary(model)$adj.r.squared %>% 
      round(2)
    
  })
  
  output$result = renderTable({
    summary(get_result())$coefficients %>% 
      data.frame() %>% 
      rownames_to_column("variable")
  })
  output$r2 = renderText({
    paste0("Adj R2: ",get_r2())
  })
  
}

shinyApp(ui, server)