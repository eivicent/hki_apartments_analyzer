library(tidyverse)
library(rvest)
library(RSelenium)
library(glue)

page_number <- 1
url_search <- ("https://asunnot.oikotie.fi/myytavat-asunnot?pagination={page_number}&locations=%5B%5B1642,4,%22Kamppi,%20Helsinki%22%5D,%5B5695443,4,%22J%C3%A4tk%C3%A4saari,%20Helsinki%22%5D,%5B1643,4,%22Punavuori,%20Helsinki%22%5D,%5B1644,4,%22Eira,%20Helsinki%22%5D,%5B1645,4,%22Ullanlinna,%20Helsinki%22%5D,%5B1724,4,%22T%C3%B6%C3%B6l%C3%B6,%20Helsinki%22%5D,%5B1669,4,%22Lauttasaari,%20Helsinki%22%5D,%5B5695451,4,%22Kalasatama,%20Helsinki%22%5D,%5B1649,4,%22Kallio,%20Helsinki%22%5D,%5B1660,4,%22Vallila,%20Helsinki%22%5D,%5B335403,4,%22Alppila,%20Helsinki%22%5D,%5B335080,4,%22Lapinlahti,%20Helsinki%22%5D,%5B335078,4,%22Ruoholahti,%20Helsinki%22%5D,%5B335075,4,%22Siltasaari,%20Helsinki%22%5D,%5B335073,4,%22Hakaniemi,%20Helsinki%22%5D,%5B11820666,4,%22Sompasaari,%20Helsinki%22%5D,%5B11820669,4,%22Hanasaari,%20Helsinki%22%5D,%5B1655,4,%22Pasila,%20Helsinki%22%5D,%5B1680,4,%22Kulosaari,%20Helsinki%22%5D,%5B1668,4,%22Munkkiniemi,%20Helsinki%22%5D%5D&price%5Bmax%5D=350000&size%5Bmin%5D=25&roomCount%5B%5D=1&roomCount%5B%5D=2&buildingType%5B%5D=1&buildingType%5B%5D=256&cardType=100")

driver <- rsDriver(browser = c("chrome"), chromever="104.0.5112.79")
rd <- driver[["client"]]
rd$navigate(glue(url_search))
results <- rd$findElements(using = "class", value = "pagination__pages")
pages <- unlist(sapply(results, function(x) x$getElementText()))
max_page_number <- stringr::str_split(pages, pattern = "\n") %>%
  purrr::keep(.!="") %>% purrr::reduce(.f = c) %>%
  as.numeric() %>% max(na.rm =T)

all_links <- list()
for(ii in 1:max_page_number){
  page_number <- ii
  new_page <- glue(url_search)
  rd$navigate(new_page)
  Sys.sleep(5)
  links <- rd$findElements(using = "xpath", value = "//*[@class = 'ot-card ng-scope']")
  df <- data.frame(link = unlist(sapply(links, function(x){x$getElementAttribute('href')})))
  Sys.sleep(5)
  all_links[[ii]] <- df
  cat(ii, "/", max_page_number, "\n")
}

df_all <- reduce(all_links,bind_rows)
number_of_results <- nrow(df_all)
driver$server$stop()

apartment_details <- list()
for(jj in 1:number_of_results){
  
  apartment_html <-  tryCatch(read_html(df_all[jj,1]),
                              error = function(e) NA)
  
  if(is.na(apartment_html)) {
    apartment_details[[jj]] <- NA
    next
  }

  aux <- apartment_html %>%
    html_elements(".listing-details") %>% 
    html_element(".info-table") %>% 
    html_children()
  
  headers <- aux %>% html_element(".info-table__title") %>% html_text2
  values <- aux %>% html_element(".info-table__value") %>% html_text2
  
  apartment_details[[jj]] <- tibble(headers, values)
  cat(jj, "/", number_of_results, "\n")
}
  
  
  
