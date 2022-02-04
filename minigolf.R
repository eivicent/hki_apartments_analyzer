library(googlesheets4)
library(tidyverse)
library(gganimate)
library(gifski)
library(ggthemes)

s <- read_sheet("https://docs.google.com/spreadsheets/d/1Q41tpQhCHqdA0e7hl9Qd13DeNu-niKf_6-ypSuNqN-A/edit#gid=0")

stats <- s %>% group_by(Player) %>%
  mutate(track_id = row_number(),
         cumScore = cumsum(Score),
         avgScore = cummean(Score)) %>%
  group_by(track_id) %>%
  mutate(winner = if_else(Score == min(Score),1,0)) %>%
  ungroup %>%
  mutate(Track = factor(Track,levels = unique(s$Track)))

graph <- ggplot(stats, aes(x = track_id,
           y = cumScore,
           color = Player)) +
  geom_line(size = 1.1) +
  geom_point(size = 1) +
  scale_color_tableau() +
  labs() +
  scale_x_continuous(labels = unique(stats$Track),
                     breaks = seq(1:15),
                     guide = guide_axis(angle = 30))

graph

ggplot(stats, 
       aes(x = track_id,
           y = winner,
           fill = Player)) +
  geom_col(position = "stack")

graph.animation <- graph +
  transition_reveal(track_id) +
  view_follow()

# animate(graph.animation, height = 500, width = 800, fps = 30, duration = 10,
        # end_pause = 60, res = 100)
# anim_save("ps3 game sales.gif")