library(tidyverse)
library(stringr)
library(rvest)


links <- list(
  STROKES.GAINED.TEE.TO.GREEN = "http://www.pgatour.com/stats/stat.02674.html",
  DRIVING.DISTANCE = "http://www.pgatour.com/stats/stat.101.2016.html",
  STROKES.GAINED.PUTTING = "http://www.pgatour.com/stats/stat.02564.2016.html",
  CLUB.HEAD.SPEED = "http://www.pgatour.com/stats/stat.02401.html",
  BALL.SPEED = "http://www.pgatour.com/stats/stat.02402.html",
  LAUNCH.ANGLE = "http://www.pgatour.com/stats/stat.02404.html",
  SPIN.RATE = "http://www.pgatour.com/stats/stat.02405.html",
  CARRY.DISTANCE = "http://www.pgatour.com/stats/stat.02409.html"
)

link <- "http://www.pgatour.com/stats/stat.02674.html"

link <- "http://www.pgatour.com/stats/stat.101.2016.html"

link <- "http://www.pgatour.com/stats/stat.02401.html"

link <- "http://www.pgatour.com/stats/stat.186.html"

stats <- map(links, function(link) {

  html_xml <- read_html(link)
  
  player_name_nodes <- html_nodes(html_xml, ".player-name")
  
  players <- html_text(player_name_nodes, trim = TRUE)[-which(html_name(player_name_nodes) == "th")]
  
  stats_nodes <- html_nodes(html_xml, ".hidden-medium")
  
  col_headers_index <- which(html_name(stats_nodes) == "th")
  
  stats_headers <- html_text(stats_nodes)[col_headers_index]
  
  stats <- as.numeric(
    html_text(stats_nodes, trim = TRUE)[-col_headers_index] %>%
    str_replace_all(",", "")
  )

  stats_df <- map(seq_along(stats_headers),
      ~ stats[seq(.x, length(stats), by = length(stats_headers))]
  ) %>% set_names(stats_headers) %>% as_data_frame(.)
  
  bind_cols(tibble(PLAYER.NAME = players), stats_df) %>%
    set_names(make.names(names(.)))
  
})

stats_df <- map_df(stats, ~ gather(.x, STAT, VAL, -PLAYER.NAME), .id = "STAT_GROUP") %>%
  filter(!STAT == "ROUNDS") %>%
  filter(!STAT == "MEASURED.ROUNDS") %>%
  filter(!STAT == "EVENTS") %>%
  filter(str_detect(STAT, "TOTAL") == FALSE) %>%
  unite(STAT, STAT_GROUP, STAT) %>%
  spread(STAT, VAL)

View(stats_df[complete.cases(stats_df), ])

