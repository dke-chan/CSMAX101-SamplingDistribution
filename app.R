library(shiny); library(ggplot2)

president.df = read.csv("Sampling-Presidents-2022.csv")

colnames(president.df)[3] = "Height (inches)"
colnames(president.df)[4] = "Age (years)"

ui <- fluidPage(
  titlePanel("CSMAX101-22B Worksheet 1"),

  sidebarLayout(
    ##
    sidebarPanel(
      sliderInput("nInput", "Sample size (n):", min = 1, max = 46, value = 5),
      actionButton("singlePlay", "Sample!", class = "btn-success"),
      hr(),
      strong(HTML("Sample mean (x&#772;): ")), textOutput("xbar", inline = TRUE), br(),
    ),
  
    ##
    mainPanel(
      tabsetPanel(type = "tabs", id = "viewport",
        ##
        tabPanel("Simple Random Sampler",
          plotOutput("samplePlot", height = "300px", width = "500px"),
          plotOutput("populationPlot", height = "300px", width = "500px"),
        ),
            
        ##
        tabPanel("Full Data Set",
          dataTableOutput("data")
        )
    ))
  )
)

server <- function(input, output) {
  RAM = reactiveValues(sampleData.df = data.frame())
  
  ##
  # sampleData.df <- observe({
  #   president.df[sample(1:nrow(president.df), input$nInput), ]
  #   }) %>%
  #   bindEvent(input$singlePlay)
  
  # reactiveA = bindEvent(input$singlePlay, reactive({
  #   RAM$sampleData.df = president.df[sample(1:nrow(president.df), input$nInput), ]
  # }))
  
  observer = observe({
    RAM$sampleData.df = president.df[sample(1:nrow(president.df), input$nInput), ]
  }) |>
    bindEvent(input$singlePlay)

  ##
  output$samplePlot = renderPlot({
    req(nrow(RAM$sampleData.df) > 0)
    ggplot(data = RAM$sampleData.df, aes(x = `Height (inches)`)) +
      geom_histogram(aes(y = ..density..), binwidth = 1, fill = "tomato", col = "black") +
      ggtitle("Sample Distribution of US Presidents' Heights") +
      xlim(range(president.df[, 3])) +
      ylab("Density") +
      scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
      theme_bw() +
      theme(title = element_text(size = 14))
  })
  
  ##
  output$xbar = renderText({
    req(nrow(RAM$sampleData.df) > 0)
    mean(RAM$sampleData.df[, 3])
  })
  
  ##
  output$populationPlot = renderPlot({
    ggplot(data = president.df, aes(x = `Height (inches)`)) +
      geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightblue", col = "black") + 
      ggtitle("Population Distribution of US Presidents' Heights") +
      ylab("Density") +
      scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
      xlim(range(president.df[, 3])) +
      theme_bw() +
      theme(title = element_text(size = 14))
  })

  ##
  output$data = renderDataTable(president.df, options = list(pageLength = 10))
}

# Run the application 
shinyApp(ui = ui, server = server)
