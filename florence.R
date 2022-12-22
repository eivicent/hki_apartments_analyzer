library(tidyverse)
library(rvest)
library(telegram.bot)

flor_live <- rvest::read_html("https://florenceandthemachine.net/live/")
bot = Bot(token = bot_token("eivicent_r_bot"))

concerts <- flor_live %>% 
  html_elements(".umg_live_table") %>%
  html_table() %>%
  pluck(1) %>%
  select(-c(4,5))

names(concerts) <- c("date", "place","city")

aux <- concerts %>%
  separate(col = city, into = c("city", "country"), sep =  ", ") 

plot <- aux %>%
  count(country) %>%
  ggplot(aes(x = fct_reorder(country,desc(n)), y = n)) +
  geom_col() +
  scale_y_continuous(breaks = seq(1,100,2),
                     limits = c(0,NA)) +
  labs(x = NULL, 
       y = NULL, 
       title = "Florence Concerts by Country") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

spain_concerts <- aux %>% 
  filter(country %in% c("ES", "Spain", "España"))

msg1 <- paste0("Florence + The Machine concerts in Spain: ",nrow(spain_concerts))
msg2 <- paste0("Cities: ",paste0(spain_concerts$city, collapse = ", "))



bot$sendMessage(chat_id = 64108692, text = msg1)
bot$sendMessage(chat_id = 64108692, text = msg2)



