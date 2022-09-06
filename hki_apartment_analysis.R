library(tidyverse)
library(gridlayout)
library(stringr)
library(shiny)
library(patchwork)

path <- "./apartment_data/"

count_barris_progressio <- function(path){
  all_data <- lapply(list.files(path), function(x) 
    pins::pin_read(pins::board_folder(path = path),
                   name = x) %>% pluck("results_df"))
  names(all_data) <- list.files(path)
  out <- all_data %>% bind_rows(.id = "date") %>%
    count(date, barri)
  return(out)
}
tendencia_general <- function(path){
  trend <- sapply(list.files(path), function(x) {
    pins::pin_read(pins::board_folder(path = path),name = x) %>%
    pluck("number_of_results")
  })
  out <- tibble(date = names(trend), general_trend = trend)
  return(out)
}
ultimes_dades_disponibles <- function(path, minim_barri = 2, minim_preu = 4000){
  all_data <- pins::pin_read(pins::board_folder(path = path),
                   name = max(list.files(path))) %>%
    pluck("results_df")
  
  out <- all_data %>%
    group_by(barri) %>%
    mutate(count_barri = n()) %>%
    filter(count_barri > minim_barri &
             preu_metre >= minim_preu)
  return(out)
}

barris <- count_barris_progressio(path)
tendencia <- tendencia_general(path)
df <- ultimes_dades_disponibles(path)

plot <- function(){

a <- ggplot(df, aes(x = fct_reorder(barri, count_barri),
               y = preu_metre)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = .3) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format(accuracy = .1,
                                                    scale = .001,
                                                    prefix = NULL),
                     breaks = seq(0,20e3, by = 1.5e3)) +
  labs(y = "k€ / m2") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "darkgrey",linetype = 2),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank()
        
  )

b <- ggplot(select(df, barri, count_barri) %>% unique, 
       aes(x = fct_reorder(barri, count_barri),
               y = 0)) +
  geom_text(aes(label = count_barri)) +
  coord_flip() +
  labs(y = NULL) + 
  theme(text = element_text(size = 16),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()
        )

c <- b + a + plot_layout(widths = c(1,10)) &
  labs(x = NULL)

c

return(c)
}

page_layout <- gridlayout::new_gridlayout(
  c(" 1fr 1fr",
    "300px distplot trend",
    "500px mainplot mainplot",
    "500px table table"),container_height = "viewport")

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


