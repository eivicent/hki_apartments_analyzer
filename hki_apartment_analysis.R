library(tidyverse)
library(shiny)
library(shinydashboard)
library(lubridate)
library(pins)

path <- "./apartment_data/"
path2 <- "./data/"
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
                   name = max(as.Date(list.files(path)),na.rm=T)) %>%
    pluck("results_df") %>%
    clean_barris_apartments_df(dim_barris)
  
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
               y = mensualitat)) +
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

# pins::pin_write(board = pins::board_folder(path2),
#                            x = df %>% slice(1) %>% select(id) %>% mutate(status = "test"),
#                            name = "favorits")

tendencia_barris <- barris %>% 
  group_by(barri) %>% arrange(date) %>%
  mutate(diff_n = (n - coalesce(lag(n),n))/n,
         diff_preu = (avgpreu-coalesce(lag(avgpreu),avgpreu))/avgpreu) %>%
  slice_tail(n = 1) %>% ungroup()

ggplot(tendencia, 
       aes(x = as_date(date),
           y = general_trend)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(x = NULL, y = NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(n.breaks = 2)
  
### SHINY APP
header <- dashboardHeader(
  title = "Apartments in Helsinki"
)
body <- dashboardBody(
  # Header for summary
  fluidRow(
    column(width = 6,
          infoBox(round(mean(df$preu_metre),2),width = 6, fill=  T,
                                   subtitle = "Average price", icon = icon("arrow-up"))
          
           ),
    column(width = 6,
           splitLayout(cellWidths = c("50%","50%"),
           )
    )
  ),
  # Distribution plot
  fluidRow(column(width = 12, plotOutput("distplot_plot"))),
  # Filter for price scatter plot
  fluidRow(column(width = 12,
                  checkboxInput(inputId = "s_barri", label = "Select all", value = T),
                  uiOutput("ui_f_barri")
    )),
  # Scatter plot
  fluidRow(
    column(width = 12,
    plotOutput("scatter_plot", brush = "plot_brush"))
  ),
  # Table and buttons  
  fluidRow(
    column(width = 12,
    sidebarLayout(position = "right",
    mainPanel = mainPanel(
      DT::dataTableOutput("main_table"),width = 10),
    sidebarPanel = sidebarPanel(width = 2,
          fixedRow(
           actionButton(inputId = "interested_button",label = "Interested")
           ),
          fixedRow(
           actionButton(inputId = "not_interested_button",label = "Not interested")
           ),
          fixedRow(
           actionButton(inputId = "seen_liked_button",label = "Seen-and-liked")
           ),
          fixedRow(
           actionButton(inputId = "seen_not_liked_button",label = "Seen-NO")
          ),
          fixedRow(
            actionButton(inputId = "contacted_not_seen_button",label = "Contacted-not-seen")
          )
         )
      )
    )
    )
  )

ui <- dashboardPage(
  header, 
  dashboardSidebar(disable = T),
  body
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
  output$distplot_plot <- renderPlot(pinta_distribucio_barri(df))
  
  favorits <- pins::pin_reactive_read(board = board_folder(path2),
                                      name = "favorits",interval = 100)
  
  data <- reactive(
    df %>%
      `if`(!input$s_barri,
           df %>% filter(barri %in% input$f_barri),.) %>%
      left_join(favorits()) %>%
      mutate(status = ifelse(is.na(status),"unassigned",status))
    )
  
  output$scatter_plot <- renderPlot({
    data() %>%
    ggplot(aes(x = preu_metre,
                       y = preu,
               color = status)) +
      geom_point() +
      scale_x_continuous(labels = scales::label_dollar(suffix = "€/m2",prefix = NULL)) +
      scale_y_continuous(labels = scales::label_dollar(suffix = "€", prefix = NULL)) +
      coord_cartesian(ylim = c(1.5e5, 4e5)) +
      labs(x = NULL, y = NULL) +
      ggtitle("Apartaments per preu per metre quadrat i preu total") +
      theme_minimal(base_size = 14)
  })
  
  
  brushed_data <- reactive(brushedPoints(data(), input$plot_brush, allRows = T) %>%
      mutate(id_out = paste0("<a href = \"",id, "\"> ",id,"</a>")) %>%
        relocate(id_out, .before = carrer) %>%
      `if`(sum(.$selected_) >0,
               filter(., selected_==T),.) %>%
             select(-selected_, -count_barri)
      )
  
  output$main_table <- DT::renderDataTable({
    DT::datatable(brushed_data() %>% select(-id), escape = F,rownames = F)},server = T)
  
  observeEvent(input$interested_button, {

    aux <- brushed_data()[input$main_table_rows_selected,]  %>%
      select(id) %>%
      mutate(status = "interested")
    
    out <- favorits() %>% rows_upsert(aux) %>%
      unique()

    pins::pin_write(pins::board_folder(path = path2),
                   name = "favorits", x = out, versioned = F)
    return(out)
  })
  
  observeEvent(input$not_interested_button, {
    
    aux <- brushed_data()[input$main_table_rows_selected,]  %>%
      select(id) %>%
      mutate(status = "not_interested")
    
    out <- favorits() %>% rows_upsert(aux) %>%
      unique()
    
    pins::pin_write(pins::board_folder(path = path2),
                    name = "favorits", x = out, versioned = F)
    return(out)
  })
  
  observeEvent(input$seen_liked_button, {
    
    aux <- brushed_data()[input$main_table_rows_selected,]  %>%
      select(id) %>%
      mutate(status = "seen_and_liked")
    
    out <- favorits() %>% rows_upsert(aux) %>%
      unique()
    
    pins::pin_write(pins::board_folder(path = path2),
                    name = "favorits", x = out, versioned = F)
    return(out)
  })
  
  observeEvent(input$seen_not_liked_button, {
    
    aux <- brushed_data()[input$main_table_rows_selected,]  %>%
      select(id) %>%
      mutate(status = "seen_not_liked")
    
    out <- favorits() %>% rows_upsert(aux) %>%
      unique()
    
    pins::pin_write(pins::board_folder(path = path2),
                    name = "favorits", x = out, versioned = F)
    return(out)
  })
  
  observeEvent(input$contacted_not_seen_button, {
    
    aux <- brushed_data()[input$main_table_rows_selected,]  %>%
      select(id) %>%
      mutate(status = "contacted_not_seen")
    
    out <- favorits() %>% rows_upsert(aux) %>%
      unique()
    
    pins::pin_write(pins::board_folder(path = path2),
                    name = "favorits", x = out, versioned = F)
    return(out)
  })
}


shinyApp(ui, server)



