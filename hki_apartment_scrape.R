library(tidyverse)
library(rvest)
library(RSelenium)
library(glue)
library(pins)

get_max_pages <- function(url_search, rd){
  results <- rd$findElements(using = "class", value = "pagination__pages")
  pages <- unlist(sapply(results, function(x) x$getElementText()))
  max_page_number <- stringr::str_split(pages, pattern = "\n") %>%
    purrr::keep(.!="") %>% purrr::reduce(.f = c) %>%
    as.numeric() %>% max(na.rm =T)
  return(max_page_number)
}
scrape_results <- function(url_search, max_page_number, rd){
  # input is the url of the search with the pagination component in it
  # and all the max pages to iterate with
  # output is list of links
  all_links <- list()
  for(ii in 1:max_page_number){
    page_number <- ii
    new_page <- glue(url_search)
    rd$navigate(new_page)
    links <- rd$findElements(using = "xpath", value = "//*[@class = 'ot-card ng-scope']")
    df <- data.frame(link = unlist(sapply(links, function(x){x$getElementAttribute('href')})))
    Sys.sleep(30)
    all_links[[ii]] <- df
    cat(ii, "/", max_page_number, "\n")
  }
  df_all <- reduce(all_links,bind_rows) %>% pull(link)
  driver$server$stop()
  return(df_all)
}
scrape_apartments <- function(all_links){
  apartment_details <- list()
  lost_links <- list()
  kk <- 1
  for(jj in 1:length(all_links)){
    
    apartment_html <-  tryCatch(read_html(all_links[jj]),
                                error = function(e) NA)
    Sys.sleep(30)
    if(is.na(apartment_html)) {
      apartment_details[[jj]] <- NA
      lost_links[[kk]] <- all_links[jj]
      kk <- kk + 1
      next
    }
    
    aux <- apartment_html %>%
      html_elements(".listing-details") %>% 
      html_element(".info-table") %>% 
      html_children()
    
    headers <- aux %>% html_element(".info-table__title") %>% html_text2
    values <- aux %>% html_element(".info-table__value") %>% html_text2
    
    apartment_details[[jj]] <- tibble(headers, values)
    cat(jj, "/", length(all_links), "\n")
  }
  names(apartment_details) <- all_links
  apartment_details_clean <- purrr::keep(apartment_details, is_tibble) %>%
    bind_rows(.id = "id") %>%
    distinct()
  return(list(apartment_df = apartment_details_clean,
              lost_links = lost_links))
}
clean_apartments_df <- function(apartment_df, info, dim_barris = NULL){
  aux <- apartment_df %>% 
    filter(headers %in% info$headers) %>%
    inner_join(info, by = "headers")
  
  aux2 <- aux %>%
    dplyr::filter(headers %in% 
                    c("Velaton hinta", "Kerros","Asuinpinta-ala","Hoitovastike","Neliöhinta")) %>%
    dplyr::mutate(values = str_extract(str_replace(values,pattern = " ", ""),"[0-9]*"))
  
  apartment_details_df_clean <- aux %>% 
    rows_update(aux2, by = c("id", "catala","headers")) %>%
    select(-headers) %>%
    pivot_wider(names_from = catala, values_from = values) %>%
    mutate(across(altura:any, as.numeric)) 
  return(apartment_details_df_clean)
  
}

##### SCRAPE #####
page_number <- 1
max_price <- 400000L
url_search <- ("https://asunnot.oikotie.fi/myytavat-asunnot?pagination={page_number}&locations=%5B%5B1642,4,%22Kamppi,%20Helsinki%22%5D,%5B5695443,4,%22J%C3%A4tk%C3%A4saari,%20Helsinki%22%5D,%5B1643,4,%22Punavuori,%20Helsinki%22%5D,%5B1644,4,%22Eira,%20Helsinki%22%5D,%5B1645,4,%22Ullanlinna,%20Helsinki%22%5D,%5B1724,4,%22T%C3%B6%C3%B6l%C3%B6,%20Helsinki%22%5D,%5B1669,4,%22Lauttasaari,%20Helsinki%22%5D,%5B5695451,4,%22Kalasatama,%20Helsinki%22%5D,%5B1649,4,%22Kallio,%20Helsinki%22%5D,%5B1660,4,%22Vallila,%20Helsinki%22%5D,%5B335403,4,%22Alppila,%20Helsinki%22%5D,%5B335080,4,%22Lapinlahti,%20Helsinki%22%5D,%5B335078,4,%22Ruoholahti,%20Helsinki%22%5D,%5B335075,4,%22Siltasaari,%20Helsinki%22%5D,%5B335073,4,%22Hakaniemi,%20Helsinki%22%5D,%5B11820666,4,%22Sompasaari,%20Helsinki%22%5D,%5B1655,4,%22Pasila,%20Helsinki%22%5D,%5B1680,4,%22Kulosaari,%20Helsinki%22%5D%5D&price%5Bmax%5D={max_price}&roomCount%5B%5D=1&roomCount%5B%5D=2&roomCount%5B%5D=3&buildingType%5B%5D=1&buildingType%5B%5D=256&cardType=100")

driver <- rsDriver(browser = c("chrome"))
rd <- driver[["client"]]
rd$navigate(glue::glue(url_search))

## This is where you need to accept cookies manually before scraping

max_pages <- get_max_pages(url_serach, rd)
all_links <- scrape_results(url_search, max_pages, rd)
apartment_list <- scrape_apartments(all_links)


##### CLEAN RESULTS #####
info <- tibble(headers = c("Sijainti","Kaupunginosa", "Asuinpinta-ala",
                           "Kerros", "Velaton hinta", "Hoitovastike", "Neliöhinta", "Rakennusvuosi"),
               catala = c("carrer", "barri", "superficie", "altura", "preu", "mensualitat", "preu_metre", "any"))

apartment_df_clean <-  clean_apartments_df(apartment_list$apartment_df, info)

pin_write(board_folder(path = "./apartment_data"), 
          list("url_search" = glue(url_search),
               "number_of_results" = length(unique(apartment_df_clean$id)),
               "results_df" = apartment_df_clean,
               "lost_links" = apartment_list$lost_links), 
          paste0(Sys.Date()),
          versioned = F, type = "rds")
