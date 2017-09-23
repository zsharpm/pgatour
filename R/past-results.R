

links <- list(
  CAREER.BUILDER = "http://www.pgatour.com/tournaments/careerbuilder-challenge/past-results.html"
)

link <- "http://www.golfstats.com/search/?yr=2016&tour=PGA&tournament=CareerBuilder+Challenge&submit=go"

stats <- map(links, function(link) {
  
  html_xml <- read_html(link)
  
  stats_nodes <- html_nodes(html_xml, "td")
  
  stats <- html_text(stats_nodes, trim = TRUE)[-c(1:4)] %>%
      str_replace_all(",", "") %>%
      str_replace_all("$", "")
  
  player <- stats[seq(1, length(stats), by = 16)]
  
  
  player[1:5] %>% str_extract("^.+?(?=Playoff)")
  
  player[1:5] %>% str_replace("Playoff.+", "")
  

  
  map_chr(player, ~str_replace(.x, "Playoff.+", ""))
  

  
  place <- stats[seq(3, length(stats), by = 16)] %>%
    str_replace_all("Win", 1L) %>%
    str_replace_all("^T", "")
  
  topar <- stats[seq(11, length(stats), by = 16)]
  
  tbl <- tibble(player, place, topar)
  
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