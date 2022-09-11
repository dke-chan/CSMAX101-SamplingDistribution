library(shiny); library(ggplot2)

president.df = read.csv("Sampling-Presidents-2022.csv")
old.faithful.df = faithful

ui <- fluidPage(
  titlePanel("CSMAX101-22B Lesson 7"),

  sidebarLayout(
    ##
    sidebarPanel(
      selectInput("dropdownSelect", label = NULL, choices = c(
        "Presidents.df", "Old_Faithful.df", "Uniform", "Exponential", "Normal", "Binomial"
      )),
      hr(),
      strong("Options"),
      numericInput("sampleSize", HTML("Sample size, <i>n</i>"), value = 10, min = 1, max = 999),
      uiOutput("dropdownBonusOptions"),
      hr(),
      actionButton("sim_1", "1 Sample", class = "btn-primary"),
      actionButton("sim_100", "100 Samples", class = "btn-primary"),
      actionButton("reset", HTML("&#8634;"), class = "btn-warning", width = "15%"),
      hr(),
      strong("Population Data"),
      plotOutput("populationPlot", height = "300px")
    ),
  
    ##
    mainPanel(
      plotOutput("samplingDistribution", height = "400px"),
      hr(),
      splitLayout(cellWidths = c("50%", "50%"),
                  tagList(
                    span(strong("# of samples: "), textOutput("currentNumOfSamples", inline = TRUE)),
                    hr(),
                    actionButton("normal.Approx.View", "View Normal Approx.", class = "btn-default"),
                    sliderInput("normal.Approx.Mu", HTML("&mu;"), min = -250, max = 250, value = 5, step = 0.001),
                    sliderInput("normal.Approx.Sigma", HTML("&sigma;"), min = 1e-10, max = 100, value = 5, step = 0.001),
                    actionButton("normal.Approx.Exact", "Exact Normal Approx.", class = "btn-info")
                  ),
                  tagList(
                    strong("Sample Data"),
                    plotOutput("finalSamplePlot", height = "300px")
                  )
      )
    ))
  )

server <- function(input, output, session) {
  RAM = reactiveValues(popn.df = data.frame(), backstage.df = data.frame(), backstage.sample.size = 0,
                       current.sample.df = data.frame(), sample.stat.df = data.frame(Stat = numeric(0)),
                       current.dataset = "", backstage.dataset = "", toggle = FALSE)

  # Update output$dropdownBonusOptions based on the "dataset" selected ----
  output$dropdownBonusOptions <- renderUI({
    if (input$dropdownSelect == "Presidents.df") {
      radioButtons("president.variable", NULL, choices = c("Age", "Height", "Proportion of Democrats", "Proportion of Republicans"))
    } else if (input$dropdownSelect == "Old_Faithful.df") {
      
    } else if (input$dropdownSelect == "Uniform") {
      sliderInput("uniform.range", "Minimum and Maximum:", min = -50, max = 50, value = c(0, 25))
    } else if (input$dropdownSelect == "Exponential") {
      sliderInput("exponential.waiting", "Average Waiting Time", value = 1, min = 1e-10, max = 5, step = 0.1)
    } else if (input$dropdownSelect == "Normal") {
      tagList(
        sliderInput("normal.mu", HTML("Population Mean, &mu;"), value = 0, min = -250, max = 250),
        sliderInput("normal.sd", HTML("Population Standard Deviation, &sigma;"), value = 5, min = 1e-10, max = 50, step = 1)
      )
    } else if (input$dropdownSelect == "Binomial") {
      sliderInput("binomial.proportion", HTML("Population proportion, <i>p</i>"), value = 0.5, min = 0, max = 1, step = 0.01)
    }
  })
  
  # Update input$sampleSize's maximum value based on the "dataset" selected ----
  observeEvent(input$dropdownSelect, {
    if (input$dropdownSelect == "Presidents.df") {
      updateNumericInput(session, "sampleSize", max = 46)
    } else if (input$dropdownSelect == "Old_Faithful.df") {
      updateNumericInput(session, "sampleSize", max = 272)
    } else {
      updateNumericInput(session, "sampleSize", max = 1000)
    }
  })
  
  # Update input$uniform.range to always be a range ----
  observeEvent(input$uniform.range, {
    if (input$uniform.range[1] == input$uniform.range[2]) {
      if (input$uniform.range[1] - 1 < -500) {
        updateSliderInput(session, "uniform.range", value = c(input$uniform.range[1], input$uniform.range[2] + 1, 500))
      } else {
        updateSliderInput(session, "uniform.range", value = c(input$uniform.range[1] - 1, input$uniform.range[2], 500))
      }
    }
  })
  
  # Update RAM$popn.df based on input values ----
  observeEvent(input$dropdownSelect, {
    if (input$dropdownSelect == "Presidents.df") {
      req(!is.na(input$president.variable))
      if (input$president.variable == "Age") {
        RAM$popn.df = data.frame(Age = president.df[, 4])
        RAM$current.dataset = "Age"
      } else if (input$president.variable == "Height") {
        RAM$popn.df = data.frame(Height = president.df[, 3])
        RAM$current.dataset = "Height"
      } else if (input$president.variable == "Proportion of Democrats") {
        RAM$popn.df = data.frame(Party = ifelse(president.df$Party == "Democratic", "Democrat", "Not Democrat"))
        RAM$current.dataset = "Democrats"
      } else if (input$president.variable == "Proportion of Republicans") {
        RAM$popn.df = data.frame(Party = ifelse(president.df$Party == "Republican", "Republican", "Not Republican"))
        RAM$current.dataset = "Republicans"
      }
    } else if (input$dropdownSelect == "Old_Faithful.df") {
      RAM$popn.df = data.frame("Waiting Times" = old.faithful.df$waiting)
      RAM$current.dataset = "Faithful"
    } else if (input$dropdownSelect == "Uniform") {
      req(!is.na(input$uniform.range[1]))
      RAM$popn.df = data.frame(X = runif(10000, min = input$uniform.range[1], max = input$uniform.range[2]))
      RAM$current.dataset = "Uniform_A"
    } else if (input$dropdownSelect == "Exponential") {
      req(!is.na(input$exponential.waiting))
      RAM$popn.df = data.frame(X = rexp(10000, rate = 1 / input$exponential.waiting))
      RAM$current.dataset = "Exponential_A"
    } else if (input$dropdownSelect == "Normal") {
      req(!is.na(input$normal.mu))
      RAM$popn.df = data.frame(X = rnorm(10000, mean = input$normal.mu, sd = input$normal.sd))
      RAM$current.dataset = "Normal_A"
    } else if (input$dropdownSelect == "Binomial") {
      req(!is.na(input$binomial.proportion))
      RAM$popn.df = data.frame(X = c(rep("A", 10000 * input$binomial.proportion), rep("B", 10000 - 10000 * input$binomial.proportion)))
      RAM$current.dataset = "Binomial_A"
    }
  })

  observeEvent(input$president.variable, {
    if (input$president.variable == "Age") {
      RAM$popn.df = data.frame(Age = president.df[, 4])
      RAM$current.dataset = "Age"
    } else if (input$president.variable == "Height") {
      RAM$popn.df = data.frame(Height = president.df[, 3])
      RAM$current.dataset = "Height"
    } else if (input$president.variable == "Proportion of Democrats") {
      RAM$popn.df = data.frame(Party = ifelse(president.df$Party == "Democratic", "Democrat", "Not Democrat"))
      RAM$current.dataset = "Democrats"
    } else if (input$president.variable == "Proportion of Republicans") {
      RAM$popn.df = data.frame(Party = ifelse(president.df$Party == "Republican", "Republican", "Not Republican"))
      RAM$current.dataset = "Republicans"
    }
  })
  
  observeEvent(input$uniform.range, {
    RAM$popn.df = data.frame(X = runif(10000, min = input$uniform.range[1], max = input$uniform.range[2]))
    RAM$current.dataset = "Uniform_A"
  })
  
  observeEvent(input$exponential.waiting, {
    RAM$popn.df = data.frame(X = rexp(10000, rate = 1 / input$exponential.waiting))
    RAM$current.dataset = "Exponential_A"
  })
  
  observeEvent(input$normal.mu, {
    RAM$popn.df = data.frame(X = rnorm(10000, mean = input$normal.mu, sd = input$normal.sd))
    RAM$current.dataset = "Normal_A"
  })
  
  observeEvent(input$normal.sd, {
    RAM$popn.df = data.frame(X = rnorm(10000, mean = input$normal.mu, sd = input$normal.sd))
    RAM$current.dataset = "Normal_A"
  })

  observeEvent(input$binomial.proportion, {
    RAM$popn.df = data.frame(X = c(rep("A", 10000 * input$binomial.proportion), rep("B", 10000 - 10000 * input$binomial.proportion)))
    RAM$current.dataset = "Binomial_A"
  })

  # Update output$populationPlot based on RAM$popn.df ----
  output$populationPlot = renderPlot({
    req(nrow(RAM$popn.df) > 0)
    plotObj = ggplot(data = RAM$popn.df, aes(x = RAM$popn.df[, 1])) + theme_bw() + xlab(colnames(RAM$popn.df))
    if (is.numeric(RAM$popn.df[1, 1])) {
      plotObj + 
        geom_histogram(aes(y = ..density..), binwidth = 1, fill = sample(2:8, 1), col = "black") + ylab("Density")
    } else {
      plotObj + 
        geom_bar(aes(y = (..count..)/sum(..count..), fill = RAM$popn.df[, 1]), col = "black", show.legend = FALSE) +
        scale_y_continuous(labels = scales::percent) +
        ylab("Percentage")
    }
  })
  
  # input$normal.Approx.Exact updates to input$normal.Approx.Mu and input$normal.Approx.Sigma ----
  observeEvent(input$normal.Approx.Exact, {
    req(nrow(RAM$popn.df) > 0)
    if (is.numeric(RAM$popn.df[1, 1])) {
      if (input$dropdownSelect == "Normal") {
        updateSliderInput(session, "normal.Approx.Mu", value = input$normal.mu)
        updateSliderInput(session, "normal.Approx.Sigma", value = input$normal.sd / sqrt(input$sampleSize))  
      } else if (input$dropdownSelect == "Old_Faithful.df") {
        updateSliderInput(session, "normal.Approx.Mu", value = 0.3609 * 54.6149 + (1 - 0.3609) * 80.0911)
        updateSliderInput(session, "normal.Approx.Sigma", value = sqrt(0.3609 * 5.8712^2 + (1 - 0.3609) * 5.8677^2 + (0.3609 * 54.6149^2 + (1 - 0.3609) * 80.0911^2 - (.3609 * 54.6149 + (1 - 0.3609) * 80.0911)^2)) / sqrt(input$sampleSize)) 
      } else if (input$dropdownSelect == "Exponential") {
        updateSliderInput(session, "normal.Approx.Mu", value = input$exponential.waiting)
        updateSliderInput(session, "normal.Approx.Sigma", value = input$exponential.waiting / sqrt(input$sampleSize)) 
      } else if (input$dropdownSelect == "Uniform") {
        updateSliderInput(session, "normal.Approx.Mu", value = sum(input$uniform.range)/2)
        updateSliderInput(session, "normal.Approx.Sigma", value = sqrt((input$uniform.range[2] - input$uniform.range[1])^2 / 12) / sqrt(input$sampleSize)) 
      } else {
        updateSliderInput(session, "normal.Approx.Mu", value = mean(RAM$popn.df[, 1]))
        updateSliderInput(session, "normal.Approx.Sigma", value = sd(RAM$popn.df[, 1]) / sqrt(input$sampleSize))  
      }
    } else if (input$dropdownSelect == "Presidents.df") {
      if (input$president.variable == "Proportion of Democrats") {
        updateSliderInput(session, "normal.Approx.Mu", value = 17/46)
        updateSliderInput(session, "normal.Approx.Sigma", value = sqrt(17/46 * (1 - 17/46) / input$sampleSize))      
      } else {
        updateSliderInput(session, "normal.Approx.Mu", value = 19/46)
        updateSliderInput(session, "normal.Approx.Sigma", value = sqrt(19/46 * (1 - 19/46) / input$sampleSize))      
      }
    } else {
      updateSliderInput(session, "normal.Approx.Mu", value = input$binomial.proportion)
      updateSliderInput(session, "normal.Approx.Sigma", value = sqrt(input$binomial.proportion * (1 - input$binomial.proportion) / input$sampleSize))      
    }
  })
  
  # input$normal.Approx.View to toggle on/off the normal curve ----
  bindEvent(input$normal.Approx.View, x = observe({
    RAM$toggle = !RAM$toggle
  }))

  # input$sim_1 to simulate one dataset ----
  bindEvent(input$sim_1, x = observe({
    if (RAM$current.dataset != RAM$backstage.dataset | RAM$backstage.sample.size != input$sampleSize) {
      RAM$backstage.df <- RAM$popn.df
      RAM$backstage.sample.size <- input$sampleSize
      if (substr(RAM$current.dataset, nchar(RAM$current.dataset) - 1, nchar(RAM$current.dataset)) == "_A") {
        RAM$backstage.dataset <- substr(RAM$current.dataset, 0, nchar(RAM$current.dataset) - 2)
        RAM$current.dataset <- RAM$backstage.dataset
      } else {
        RAM$backstage.dataset <- RAM$current.dataset
      }
      RAM$sample.stat.df <- data.frame(Stat = numeric(0))
    }
    
    req(input$sampleSize <= nrow(RAM$popn.df))

    RAM$current.sample.df = data.frame(X = RAM$backstage.df[sample(1:nrow(RAM$backstage.df), size = RAM$backstage.sample.size), 1])
    colnames(RAM$current.sample.df) = colnames(RAM$backstage.df)
    
    if (is.numeric(RAM$current.sample.df[1, 1])) {
      RAM$sample.stat.df <- rbind(RAM$sample.stat.df, data.frame(mean(RAM$current.sample.df[, 1])))
    } else {
      if (RAM$current.dataset == "Democrats") {
        z = sum(RAM$current.sample.df[, 1] == "Democrat")
      } else if (RAM$current.dataset == "Republicans") {
        z = sum(RAM$current.sample.df[, 1] == "Republican")
      } else if (RAM$current.dataset == "Binomial") {
        z = sum(RAM$current.sample.df[, 1] == "A")
      }

      RAM$sample.stat.df <- rbind(RAM$sample.stat.df, data.frame(z / RAM$backstage.sample.size))
    }
  }))
  
  # input$sim_100 to simulate 100 datasets ----
  bindEvent(input$sim_100, x = observe({
    if (RAM$current.dataset != RAM$backstage.dataset | RAM$backstage.sample.size != input$sampleSize) {
      RAM$backstage.df <- RAM$popn.df
      RAM$backstage.sample.size <- input$sampleSize
      if (substr(RAM$current.dataset, nchar(RAM$current.dataset) - 1, nchar(RAM$current.dataset)) == "_A") {
        RAM$backstage.dataset <- substr(RAM$current.dataset, 0, nchar(RAM$current.dataset) - 2)
        RAM$current.dataset <- RAM$backstage.dataset
      } else {
        RAM$backstage.dataset <- RAM$current.dataset
      }
      RAM$sample.stat.df <- data.frame(Stat = numeric(0))
    }
    
    req(input$sampleSize <= nrow(RAM$popn.df))
    
    for (i in 1:100) {
      RAM$current.sample.df = data.frame(X = RAM$backstage.df[sample(1:nrow(RAM$backstage.df), size = RAM$backstage.sample.size), 1])
      colnames(RAM$current.sample.df) = colnames(RAM$backstage.df)
      
      if (is.numeric(RAM$current.sample.df[1, 1])) {
        RAM$sample.stat.df <- rbind(RAM$sample.stat.df, data.frame(mean(RAM$current.sample.df[, 1])))
      } else {
        if (RAM$current.dataset == "Democrats") {
          z = sum(RAM$current.sample.df[, 1] == "Democrat")
        } else if (RAM$current.dataset == "Republicans") {
          z = sum(RAM$current.sample.df[, 1] == "Republican")
        } else if (RAM$current.dataset == "Binomial") {
          z = sum(RAM$current.sample.df[, 1] == "A")
        }
        
        RAM$sample.stat.df <- rbind(RAM$sample.stat.df, data.frame(z / RAM$backstage.sample.size))
      }   
    }
  }))

  # input$reset to reset the process ----
  bindEvent(input$reset, x = observe({
    if (RAM$current.dataset != RAM$backstage.dataset | RAM$backstage.sample.size != input$sampleSize) {
      RAM$backstage.df <- RAM$popn.df
      RAM$backstage.sample.size <- input$sampleSize
      if (substr(RAM$current.dataset, nchar(RAM$current.dataset) - 1, nchar(RAM$current.dataset)) == "_A") {
        RAM$backstage.dataset <- substr(RAM$current.dataset, 0, nchar(RAM$current.dataset) - 2)
        RAM$current.dataset <- RAM$backstage.dataset
      } else {
        RAM$backstage.dataset <- RAM$current.dataset
      }
      RAM$sample.stat.df <- data.frame(Stat = numeric(0))
    }
    RAM$sample.stat.df <- data.frame(Stat = numeric(0))
  }))

  # Visualise the sample statistic ----
  output$samplingDistribution = renderPlot({
    req(nrow(RAM$sample.stat.df) > 0)
    plotObj = ggplot(data = RAM$sample.stat.df, aes(x = RAM$sample.stat.df[, 1])) + theme_bw() + 
      xlab("Sample Statistic") +
      geom_histogram(aes(y = ..density..), fill = "tomato", col = "black",
                     bins = ifelse(is.numeric(RAM$current.sample.df[1, 1]), 25, 20)) + ylab("Density")
    if (RAM$toggle) {
      plotObj = plotObj + 
        stat_function(fun = dnorm, args = list(mean = input$normal.Approx.Mu, sd = input$normal.Approx.Sigma),
                      size = 1.25)
    }
    
    plotObj
  })

  # Visualise the final sample data ----
  output$finalSamplePlot = renderPlot({
    req(nrow(RAM$current.sample.df) > 0)
    plotObj = ggplot(data = RAM$current.sample.df, aes(x = RAM$current.sample.df[, 1])) + theme_bw() + xlab(colnames(RAM$popn.df))
    if (is.numeric(RAM$current.sample.df[1, 1])) {
      plotObj + 
        geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "lightblue", col = "black") + ylab("Density")
    } else {
      plotObj + 
        geom_bar(aes(y = (..count..)/sum(..count..), fill = RAM$current.sample.df[, 1]), col = "black", show.legend = FALSE) +
        scale_y_continuous(labels = scales::percent) +
        ylab("Percentage")
    }
  })
  
  # Misc. ----
  output$currentNumOfSamples = renderText({
    nrow(RAM$sample.stat.df)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
