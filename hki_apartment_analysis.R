library(tidyverse)
library(shiny)
library(shinydashboard)
library(lubridate)
library(pins)
library(patchwork)

path <- "./apartment_data/"
path2 <- "./data/"
dim_barris <- read_csv("dim_barris.csv", show_col_types = F)

refactor_barris <- function(apartments_df, dim_barris_df = NULL){
  apartments_df %>% 
    `if`(!is.null(dim_barris_df),
         dplyr::left_join(.,dim_barris_df, c("barri" = "original")) %>%
           dplyr::mutate(actualitzat = coalesce(actualitzat, barri)) %>%
           dplyr::select(-barri) %>%
           dplyr::rename(barri = actualitzat),.) %>%
    dplyr::distinct()
}
tendencia_disponibilitat <- function(paths){
  trend <- sapply(list.files(paths), function(x) {
    pins::pin_read(pins::board_folder(path = paths),name = x) %>%
    pluck("number_of_results")
  })
  out <- tibble(date = names(trend), general_trend = trend)
  return(out)
}
llegeix_pin_apartaments <- function(paths, dates, minim_disponible_barri = 4, minim_preu = 4000, dim_barris_df = dim_barris){
  all_data <- pins::pin_read(pins::board_folder(path = paths),
                   name = dates) %>%
    pluck("results_df") %>%
    refactor_barris(dim_barris_df)
  
  out <- all_data %>%
    group_by(barri) %>%
    mutate(total_disponible_barri = n()) %>%
    filter(total_disponible_barri > minim_disponible_barri &
             preu_metre >= minim_preu) %>%
    ungroup()
  return(out)
}
boxplot_preu_per_barri <- function(df){
  df %>%
    ggplot(aes(x = fct_reorder(barri, desc(barri)),
               y = preu_metre)) +
    geom_boxplot(outlier.shape = NA, coef = 1, linetype = 1, 
                 color = "blue") +
    geom_boxplot(outlier.shape = NA, fatten = 5, coef = 0) +
    geom_jitter(alpha = .1) +
    coord_flip() +
    geom_text(data = df %>% select(barri, total_disponible_barri) %>% unique, 
              aes(x = barri,  y = 3.6e3, label = total_disponible_barri)) +
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


ultima_data_disponible <- list.files(path) %>% max()
primera_data_disponible <- list.files(path) %>% min()
tendencia <- tendencia_disponibilitat(path)

df <- llegeix_pin_apartaments(paths = path, dates = primera_data_disponible)

evolucio_preu <- function(){
ups <- df %>% mutate(date = ultima_data_disponible) %>%
  union(df_old %>% mutate(date = primera_data_disponible)) %>%
  group_by(id) %>%
  mutate(disponibilitat = n())

a <- ggplot(ups, aes(x = preu_metre,
                color = date)) +
  geom_density() +
  geom_vline(aes(xintercept = median(preu_metre)),
             linetype = 2, alpha = .6) + 
  theme_void() +
  theme(axis.text.x = element_text(color = "black"),
        legend.title = element_blank(),
        legend.position = "bottom")

b <- group_by(ups, date) %>% summarise(overlap = sum(disponibilitat==2)/n()) %>%
  filter(date == ultima_data_disponible) %>%
  ggplot(aes(x = as.factor(1),
         y = overlap,
         label = paste0("Overlap of: \n", round(overlap*100,2),"%")))  +
  geom_text() +
  theme_void()

out <- a + inset_element(b, left = 0.6, bottom = .5, right = 1, top = 1,
                  align_to = "plot",on_top = T)

return(out)
}

# pins::pin_write(board = pins::board_folder(path2),
#                            x = df %>% slice(1) %>% select(id) %>% mutate(status = "test"),
#                            name = "favorits")

tendencia_apartamentes_plot <- ggplot(tendencia, 
       aes(x = as_date(date),
           y = general_trend)) + 
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(x = NULL, y = NULL) +
  scale_x_date(expand = c(0,1)) +
  scale_y_continuous(n.breaks = 3)
  
### SHINY APP
header <- dashboardHeader(
  title = "Apartments in Helsinki"
)
body <- dashboardBody(
  # Header for summary
  fluidRow(
    column(width = 6,
          plotOutput("tendencia_plot")
          ),
    column(width = 6,
           plotOutput("tendencia_preu")
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
  output$tendencia_plot <- renderPlot(tendencia_apartamentes_plot)
  output$tendencia_preu <- renderPlot(evolucio_preu())
  output$distplot_plot <- renderPlot(boxplot_preu_per_barri(df))
  
  favorits <- pins::pin_reactive_read(board = board_folder(path2),
                                      name = "favorits", interval = 100)
  
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
                       y = mensualitat,
               color = status)) +
      geom_point() +
      scale_x_continuous(labels = scales::label_dollar(suffix = "€/m2",prefix = NULL)) +
      # scale_y_continuous(labels = scales::label_dollar(suffix = "€", prefix = NULL)) +
      # coord_cartesian(ylim = c(1e5, 4e5)) +
      labs(x = NULL, y = NULL) +
      ggtitle("Apartaments per preu per metre quadrat i preu total") +
      theme_minimal(base_size = 14)
  })
  
  
  brushed_data <- reactive(brushedPoints(data(), input$plot_brush, allRows = T) %>%
      mutate(id_out = paste0("<a href = \"",id, "\"> ",id,"</a>")) %>%
        relocate(id_out, .before = carrer) %>%
      `if`(sum(.$selected_) >0,
               filter(., selected_==T),.) %>%
             select(-selected_, -total_disponible_barri)
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



