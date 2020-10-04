library(here)
library(shiny)
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
  "19th Century Novels" = "19C",
  "AAW"                 = "AAW",
  "Charles Dickens"     = "DNov",
  "ArTs"                = "ArTs",
  "ChiLit"              = "ChiLit"
)

left_default <- "AAW"
right_default <- "ChiLit"

ui <- fluidPage(

  titlePanel("Corpus Co-occurrence Explorer"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId  = "left_corpus",
        label    = h3("Left corpus"), 
        choices  = corpora, 
        selected = left_default
      ),
      selectInput(
        inputId  = "right_corpus",
        label    = h3("Right corpus"), 
        choices  = corpora, 
        selected = right_default
      ),
      radioButtons(
        inputId = "nodes",
        label   = h3("Nodes"),
        choices = list(
          "Body parts (back, eye, eyes, forehead, hand, hands, head, shoulder)" = c('back', 'eye', 'eyes', 'forehead', 'hand', 'hands', 'head', 'shoulder')
        )
      ),
      sliderInput(
        inputId = "span",
        label   = h3("Span"),
        min     = -10, 
        max     = 10,
        value   = c(-5, 5)
      ),
      numericInput(
        inputId = "fdr",
        label   = h3("False Discovery Rate"),
        min     = 0.01,
        max     = 1,
        step    = 0.01,
        value   = 0.01
      )
    ),

    mainPanel(
      plotOutput("cocoPlot", height = "800px")
    )
  )
)

server <- function(input, output) {
  
  results_ <- reactive({
    results %>%
      filter(left == input$left_corpus) %>%
      filter(right == input$right_corpus) %>%
      filter(span == to_span_string(abs(input$span))) %>%
      filter(fdr == input$fdr) %>%
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
        colour    = "grey",
        linetype  = 2,
        size      = 0.5
      ) +
      facet_wrap(~ x, ncol = 1, scales = "free_y") +
      theme_light()
  })
    
}

shinyApp(ui = ui, server = server)
