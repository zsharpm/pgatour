

links <- list(
  THIS_EVENT = "https://golf.fantasysports.yahoo.com/golf/24831"
)

link <- "https://golf.fantasysports.yahoo.com/golf/24831"

stats <- map(links, function(link) {
  
  html_xml <- read_html(link)
  
  dat <- html_nodes(html_xml, "#yui_3_18_1_1_1484613330683_1192 , td")
  
  dat2 <- html_text(dat)
  
  
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