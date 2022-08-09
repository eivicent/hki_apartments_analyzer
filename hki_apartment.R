library(tidyverse)
library(rvest)
library(RSelenium)

apartment_html <- read_html("https://asunnot.oikotie.fi/myytavat-asunnot/helsinki/16929304")

aux <- apartment_html %>%
  html_elements(".listing-details") %>% 
  html_element(".info-table") %>% 
  html_children()

headers <- aux %>% html_element(".info-table__title") %>% html_text2
values <- aux %>% html_element(".info-table__value") %>% html_text2

apartment_data <- tibble(headers, values)

url_search <- ("https://asunnot.oikotie.fi/myytavat-asunnot?pagination=2&locations=%5B%5B%2264%22,%226%22,%22Helsinki,%20Uusimaa%22%5D%5D&price%5Bmax%5D=300000&size%5Bmin%5D=25&roomCount%5B%5D=1&roomCount%5B%5D=2&buildingType%5B%5D=1&buildingType%5B%5D=256&cardType=100")

favourites %>% html_elements(".cards__cards")

driver <- rsDriver(browser = c("chrome"),chromever="104.0.5112.79")
rd <- driver[["client"]]

  remDr$open()
  remDr$navigate(url_search)
  links <- remDr$findElements(using = "xpath", value = "//*[@class = 'ot-card ng-scope']")
  df <- data.frame(link = unlist(sapply(links, function(x){x$getElementAttribute('href')})))
  Sys.sleep(1)
  df_all <- rbind(df_all, df)
}

driver$server$stop()


