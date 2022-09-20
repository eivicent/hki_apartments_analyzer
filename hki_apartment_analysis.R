library(tidyverse)
library(stringr)
library(shiny)
library(bs4Dash)
library(patchwork)
library(lubridate)

path <- "./apartment_data/"
dim_barris <- read_csv("dim_barris.csv", show_col_types = F)
clean_barris_apartments_df <- function(apartments_df, dim_barris = NULL){
  apartments_df %>% 
    `if`(!is.null(dim_barris),
         dplyr::left_join(.,dim_barris, c("barri" = "original")) %>%
           dplyr::mutate(actualitzat = coalesce(actualitzat, barri)) %>%
           dplyr::select(-barri) %>%
           dplyr::rename(barri = actualitzat),.) %>%
    dplyr::distinct()
}
count_barris_progressio <- function(path, dim_barris = dim_barris){
  all_data <- lapply(list.files(path), function(x) 
    pins::pin_read(pins::board_folder(path = path),
                   name = x) %>% pluck("results_df"))
  names(all_data) <- list.files(path)
  out <- all_data %>% bind_rows(.id = "date") %>%
    clean_barris_apartments_df(dim_barris) %>%
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
ultimes_dades_disponibles <- function(path, minim_barri = 2, minim_preu = 4000, dim_barris = dim_barris){
  all_data <- pins::pin_read(pins::board_folder(path = path),
                   name = max(list.files(path))) %>%
    pluck("results_df") %>%
    clean_barris_apartments_df(., dim_barris)
  
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
    geom_boxplot(outlier.shape = NA, coef = 1, linetype = 1, 
                 color = "blue") +
    geom_boxplot(outlier.shape = NA, fatten = 5, coef = 0) +
    geom_jitter(alpha = .1) +
    coord_flip() +
    geom_text(data = df %>% select(barri, count_barri) %>% unique, 
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
barris <- count_barris_progressio(path, dim_barris)
tendencia <- tendencia_general(path)
df <- ultimes_dades_disponibles(path, minim_barri = 4, dim_barris = dim_barris) 

tendencia_barris <- barris %>% 
  group_by(barri) %>% arrange(date) %>%
  mutate(diff_n = (n - coalesce(lag(n),n))/n,
         diff_preu = (avgpreu-coalesce(lag(avgpreu),avgpreu))/avgpreu) %>%
  slice_tail(n = 1) %>% ungroup()

tendencia_barris %>% arrange(desc(n)) %>%
  slice_head(n=4) %>%
  ggplot(aes(x = avgpreu, y = 1, label = round(avgpreu,1))) +
    geom_linerange(aes(xmin = minpreu, xmax = maxpreu), alpha =.3) +
    geom_text() +
    facet_wrap("barri") +
    theme_void(base_size = 16)



### SHINY APP

ui <- fluidPage(
  # fluidRow(
  #   plotOutput("distplot_plot", dblclick = "f_barri_2")
  #   ),
  # fluidRow(
  #   tableOutput("dobleclick"),
  #   checkboxInput(inputId = "s_barri",label = "Select all", value = T),
  #   uiOutput("ui_f_barri"),
  #   plotOutput("scatter_plot",brush = "plot_brush")
  #         ),
  fluidRow(
    column(width = 3,
           actionButton(inputId = "interested_button",label = "Interested")
    ),
    column(width = 3,
           actionButton(inputId = "not_interested_button",label = "Not interested")
    ),
    column(width = 3,
           actionButton(inputId = "seen_liked_button",label = "Seen-and-liked")
    ),
    column(width = 3,
           actionButton(inputId = "seen_not_liked_button",label = "Seen-NO")
    )
  ),
  fluidRow(
    DT::dataTableOutput("main_table")
    ),
  fluidRow(
    DT::dataTableOutput("text")
  )
)

server <- function(input, output, session) {
  output$ui_f_barri <- renderUI({
    if(!input$s_barri){
      selectInput(inputId = "f_barri", label = "barri", choice = unique(df$barri),
                  selected = nearPoints(df,input$f_barri_2, yvar= "barri") %>%
                    unique() %>%
                    pull(barri))
    }
  })
  output$distplot_plot <- renderPlot({
    pinta_distribucio_barri(df)
    })
  data <- reactive({
    if(!input$s_barri){
    df %>%
      dplyr::filter(barri %in% input$f_barri)
    } else {df}
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
  
  brushed_data <- reactive({
    data <- brushedPoints(data(), input$plot_brush,allRows = T) %>%
      mutate(id = paste0("<a href = \"",id, "\"> ",id,"</a>"))
     
     if(sum(data$selected_) == 0){
     out <- data %>% select(-selected_)
    } else {
      out <- filter(data, selected_==T) %>% select(-selected_)
    }
      return(data)
  })
  
  output$main_table <- DT::renderDataTable({
    DT::datatable(df, escape = F,rownames = F)},server = T)
  
  output$text <- DT::renderDataTable(data_to_update())
  
  # observeEvent(input$interested_button,{
  #              aux <- df[input$main_table_rows_selected,]  %>%
  #                select(id) %>%
  #                mutate(status = "interested")
  #              
  #              out <- rows_update(df, aux)
  #               } 
  #              )
  # observeEvent(input$intersted_button, {
  #   
  #   aux <- df[input$main_table_rows_selected,]  %>%
  #     select(id) %>%
  #     mutate(status = "interested")
  #   
  #   out <- left_join(update_categorized_df, aux) %>%
  #     filter(!is.na(status))
  #   
  #   pins::pin_write(pins::board_folder(path = path),
  #                  name = "categorized", x = out,versioned = F,type = "csv",
  #                  title = "Categorized apartments")
  #   return(out)
  # })
  # observeEvent(input$intersted_button, {
  #   
  #   aux <- df[input$main_table_rows_selected,]  %>%
  #     select(id) %>%
  #     mutate(status = "not_interested_button")
  #   
  #   out <- left_join(update_categorized_df, aux) %>%
  #     filter(!is.na(status))
  #   
  #   pins::pin_write(pins::board_folder(path = path),
  #                   name = "categorized", x = out,versioned = F,type = "csv",
  #                   title = "Categorized apartments")
  #   return(out)
  # })
  # observeEvent(input$intersted_button, {
  #   
  #   aux <- df[input$main_table_rows_selected,]  %>%
  #     select(id) %>%
  #     mutate(status = "interested")
  #   
  #   out <- left_join(df, aux) %>%
  #     filter(!is.na(status))
  #   
  #   pins::pin_write(pins::board_folder(path = path),
  #                   name = "categorized", x = out,versioned = F,type = "csv",
  #                   title = "Categorized apartments")
  #   return(out)
  # })
  # observeEvent(input$intersted_button, {
  #   
  #   aux <- df[input$main_table_rows_selected,]  %>%
  #     select(id) %>%
  #     mutate(status = "interested")
  #   
  #   out <- left_join(df, aux) %>%
  #     filter(!is.na(status))
  #   
  #   pins::pin_write(pins::board_folder(path = path),
  #                   name = "categorized", x = out,versioned = F,type = "csv",
  #                   title = "Categorized apartments")
  #   return(out)
  # })
  
}

shinyApp(ui, server)



