library(here)
library(shiny)
library(shinyWidgets)
library(stopwords)
library(tidyverse)

to_span_string <- function(x){
  if (x[1] == x[2]) {
    result <- paste0(x[1], "LR")
  } else
  {
    result <- paste0(x[1], "L", x[2], "R")
  }
  return(result)
}

results <- read_csv(here("results.csv"))

corpora <- list(
  "19C"    = "19C",
  "AAW"    = "AAW",
  "DNov"   = "DNov",
  "ArTs"   = "ArTs",
  "ChiLit" = "ChiLit"
)

left_default <- "AAW"
right_default <- "ChiLit"

body_parts <- c('back', 'eye', 'eyes', 'forehead', 'hand', 'hands', 'head', 'shoulder')

stop_words <- stopwords("en", source = "nltk")

ui <- fluidPage(

  titlePanel("Corpus Co-occurrence Explorer"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId  = "left_corpus",
        label    = "Left corpus", 
        choices  = corpora, 
        selected = left_default
      ),
      selectInput(
        inputId  = "right_corpus",
        label    = "Right corpus", 
        choices  = corpora, 
        selected = right_default
      ),
      pickerInput(
        inputId = "nodes", 
        label = "Nodes", 
        choices = body_parts,
        options = list(
          `actions-box` = TRUE, 
          size = 10,
          `selected-text-format` = "count > 3"
        ), 
        multiple = TRUE,
        selected = body_parts
      ),
      sliderInput(
        inputId = "span",
        label   = "Span",
        min     = -10, 
        max     = 10,
        value   = c(-5, 5)
      ),
      numericInput(
        inputId = "fdr",
        label   = "False Discovery Rate",
        min     = 0.01,
        max     = 1,
        step    = 0.01,
        value   = 0.01
      ),
      pickerInput(
        inputId = "stop_words", 
        label = "Stop Words", 
        choices = stop_words,
        options = list(
          `actions-box` = TRUE, 
          size = 10,
          `selected-text-format` = "count > 3"
        ), 
        multiple = TRUE,
        selected = stop_words
      ),
    ),
    mainPanel(
      plotOutput("cocoPlot", height = "1000px")
    )
  )
)

server <- function(input, output) {
  
  results_ <- reactive({
    results %>%
      filter(!(y %in% req(input$stop_words))) %>%
      filter(left == req(input$left_corpus)) %>%
      filter(right == req(input$right_corpus)) %>%
      filter(x %in% req(input$nodes)) %>%
      filter(span == to_span_string(abs(req(input$span)))) %>%
      filter(fdr == req(input$fdr)) %>%
      filter(!is.infinite(effect_size)) %>%
      arrange(x, effect_size) %>%
      mutate(
        p = paste(x, y),
        n = row_number()
      )
  })

  output$cocoPlot <- renderPlot({
    results_() %>%
      group_by(x) %>%
      ggplot(aes(reorder(p, n), effect_size)) +
      geom_point(colour = "skyblue4") +
      geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                    colour = "seashell4") +
      coord_flip() +
      labs(
        x = "",
        y = "Effect size"
      ) +
      geom_hline(
        yintercept = 0,
        colour     = "grey",
        linetype   = 2,
        size       = 0.5
      ) +
      facet_wrap(~ x, ncol = 1, scales = "free_y") +
      theme_light()
  })
    
}

shinyApp(ui = ui, server = server)
