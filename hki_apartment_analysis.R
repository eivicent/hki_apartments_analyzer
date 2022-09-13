library(tidyverse)
library(stringr)
library(shiny)
library(bs4Dash)
library(patchwork)
library(lubridate)

path <- "./apartment_data/"
dim_barris <- read_csv("dim_barris.csv", show_col_types = F)

count_barris_progressio <- function(path){
  all_data <- lapply(list.files(path), function(x) 
    pins::pin_read(pins::board_folder(path = path),
                   name = x) %>% pluck("results_df"))
  names(all_data) <- list.files(path)
  out <- all_data %>% bind_rows(.id = "date") %>%
    group_by(date, barri)  %>%
    summarise(n = n(),
              minpreu = min(preu_metre),
              maxpreu = max(preu_metre),
              avgpreu = mean(preu_metre))
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
clean_barris_apartments_df <- function(apartments_df, dim_barris = NULL){
  apartments_df %>% 
    `if`(!is.null(dim_barris),
         dplyr::left_join(.,dim_barris, c("barri" = "original")) %>%
           dplyr::mutate(actualitzat = coalesce(actualitzat, barri)) %>%
           dplyr::select(-barri) %>%
           dplyr::rename(barri = actualitzat),.) %>%
    dplyr::distinct()
}

ultimes_dades_disponibles <- function(path, minim_barri = 2, minim_preu = 4000){
  all_data <- pins::pin_read(pins::board_folder(path = path),
                   name = max(list.files(path))) %>%
    pluck("results_df")
  
  out <- all_data %>%
    group_by(barri) %>%
    mutate(count_barri = n()) %>%
    filter(count_barri > minim_barri &
             preu_metre >= minim_preu) %>%
    ungroup()
  return(out)
}
pinta_distribucio_barri <- function(df){
  df %>%
    ggplot(aes(x = fct_reorder(barri, desc(barri)),
               y = preu_metre)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(alpha = .1) +
    coord_flip() +
    geom_text(data = . %>% select(barri, count_barri) %>% unique, 
              aes(x = barri,  y = 3.6e3, label = count_barri)) +
    scale_y_continuous(breaks = seq(4.5,20e3, by = 1.5e3),
                       labels = scales::dollar_format(accuracy = .1,
                                                      scale = .001,
                                                      prefix = NULL,)) +
    labs(y = "k€ / m2", x = NULL) +
    theme(axis.text.y = element_text(size = 14),
          axis.ticks.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(colour = "darkgrey",linetype = 2),
          panel.grid.major.y = element_blank(),
          panel.background = element_blank()
    )
}

last_date <- list.files(path) %>% max()
barris <- count_barris_progressio(path) %>% 
  clean_barris_apartments_df(dim_barris)
tendencia <- tendencia_general(path)
df <- ultimes_dades_disponibles(path, minim_barri = 4) %>%
  clean_barris_apartments_df(., dim_barris)

tendencia_barris <- barris %>% group_by(barri) %>% arrange(date) %>%
  mutate(diff_n = (n - coalesce(lag(n),n))/n,
         diff_preu = (avgpreu-coalesce(lag(avgpreu),avgpreu))/avgpreu) %>%
  slice_tail(n = 1)

tendencia_barris %>% 
  ggplot(aes(x = 1, y = 1, label = diff_n)) +
  geom_text() +
  facet_wrap("barri")


ui <- fluidPage(
  fluidRow(
    valueBox(mean(df$preu_metre),subtitle = "")
  ),
  fluidRow(
    plotOutput("distplot_plot", dblclick = "f_barri")
    ),
  fluidRow(
    plotOutput("scatter_plot",brush = "plot_brush")
          ),
  fluidRow(
    DT::dataTableOutput("main_table")
    )
)

server <- function(input, output, session) {


  output$distplot_plot <- renderPlot({
    pinta_distribucio_barri(df)
    })
  
  output$trendplot_plot <- renderPlot({
    ggplot(df %>% filter(barri == input$f_barri),
           aes(x = preu_metre)) +
      geom_bar(stat = "count")
    
  })
  
  data <- reactive({
    df %>%
      dplyr::filter(barri %in% input$f_barri)
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


