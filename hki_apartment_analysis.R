library(tidyverse)
library(gridlayout)
library(stringr)

path <- "./apartment_data/"
data <- pins::pin_read(pins::board_folder(path = path),
              name = list.files(path) %>% max())

trend <- sapply(list.files(path), function(x) {
  pins::pin_read(pins::board_folder(path = path),
                       name = x) %>%
  pluck("number_of_results")
})

date_of_data <- sapply(names(trend), function(x){
  str_flatten(unlist(str_extract_all(x, "\\d+")),collapse = "-")
})

df <- data$results_df 

df %>% count(barri) %>%
  ggplot(aes(x = fct_reorder(barri,n),
             y = n)) +
  geom_col() +
  coord_flip()

ggplot(df, aes(x = fct_reorder(barri, preu_metre),
               y = preu_metre)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = .3) +
  coord_flip()

library(shiny)

page_layout <- gridlayout::new_gridlayout(
  c(" 1fr 1fr 1fr",
    "120px barri barri superficie",
    "500px mainplot mainplot mainplot",
    "500px table table table"),container_height = "viewport")

ui <- grid_page(layout = page_layout,
  grid_card("barri",
            selectInput("barri_filter",label = "Barri",
                        choices = unique(df$barri),selectize = T, 
                        multiple = T, 
                        selected = unique(df$barri), width = "100%")
            ),
  grid_card("superficie",
            sliderInput(inputId = "superficie_filter",label = "Superficie",
                        min = min(df$superficie), max = max(df$superficie), 
                        value = c(min(df$superficie),max(df$superficie)), step = 1)),
  grid_card("mainplot",
            plotOutput("scatter_plot",brush = "plot_brush")
            ),
  grid_card("table", scrollable = T,
            DT::dataTableOutput("main_table")
            )
)

server <- function(input, output, session) {
  
  data <- reactive({
    df %>%
      dplyr::filter(barri %in% input$barri_filter) %>%
      dplyr::filter(between(superficie, input$superficie_filter[1], input$superficie_filter[2]))
  })
  
  output$scatter_plot <- renderPlot({
    data() %>%
    ggplot(aes(x = preu_metre,
                       y = preu,
               color = any)) +
      geom_point() +
      scale_x_continuous(labels = scales::label_dollar(suffix = "€/m2",prefix = NULL)) +
      scale_y_continuous(labels = scales::label_dollar(suffix = "€", prefix = NULL)) +
      coord_cartesian(ylim = c(1.5e5, 4e5)) +
      labs(x = NULL, y = NULL) +
      ggtitle("Apartaments per preu per metre quadrat i preu total") +
      theme_minimal(base_size = 14)
  })
  
  output$main_table <- DT::renderDataTable({
    data <- brushedPoints(data(), input$plot_brush,allRows = T) %>%
      mutate(id = paste0("<a href = \"",id, "\"> ",id,"</a>"))
    
    if(sum(data$selected_) == 0){
      out <- data %>% select(-selected_)
    } else { 
      out <- filter(data, selected_==T) %>% select(-selected_)
    }
    DT::datatable(out, escape = F,rownames = F)})
  
}

shinyApp(ui, server)


