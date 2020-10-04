library(here)
library(plotly)
library(shiny)
library(tidyverse)

results <- read_csv(here("results.csv"))

corpora <- list(
  "19th Century Novels" = "19C",
  "AAW" = "AAW",
  "Charles Dickens" = "DNov",
  "ArTs" = "ArTs",
  "ChiLit" = "ChiLit"
)

left_default <- "AAW"
right_default <- "ChiLit"

ui <- fluidPage(

  titlePanel("Corpus Co-occurrence Explorer"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "left_corpus",
        label = h3("Left corpus"), 
        choices = corpora, 
        selected = left_default
      ),
      selectInput(
        inputId = "right_corpus",
        label = h3("Right corpus"), 
        choices = corpora, 
        selected = right_default
      ),
      radioButtons(
        inputId = "nodes",
        label = h3("Nodes"),
        choices = list(
          "Body parts (back, eye, eyes, forehead, hand, hands, head, shoulder)" = c('back', 'eye', 'eyes', 'forehead', 'hand', 'hands', 'head', 'shoulder')
        )
      ),
      sliderInput(
        inputId = "span",
        label = h3("Span"),
        min = -10, 
        max = 10,
        value = c(-5, 5)
      ),
      numericInput(
        inputId = "fdr",
        label = h3("False Discovery Rate"),
        min = 0.01,
        max = 1,
        step = 0.01,
        value = 0.01
      )
    ),

    mainPanel(
      plotlyOutput("cocoPlot")
    )
  )
)

server <- function(input, output) {
  
  results_ <- reactive({
    results %>%
      filter(left == input$left_corpus) %>%
      filter(right == input$right_corpus) %>%
      arrange(x, effect_size) %>%
      mutate(n = row_number())
  })

  output$cocoPlot <- renderPlotly({
    results_() %>%
      ggplot(aes(reorder(paste(x, y), n), effect_size)) +
      geom_point(colour = "skyblue4") +
      geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), colour = "seashell4") +
      coord_flip() +
      labs(
        x = "",
        y = "Effect size"
      ) +
      theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y  = element_text(family = "monospace")
      ) +
      geom_hline(yintercept = 0, colour = "grey", linetype = 2, size = 0.5) -> p
    ggplotly(p)
  })
    
}

shinyApp(ui = ui, server = server)
