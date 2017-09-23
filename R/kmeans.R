

pgatour <- stats_df[complete.cases(stats_df), ]

train_df <- pgatour %>%
  select(
    BALL.SPEED_AVG.,
    CARRY.DISTANCE_AVG.,
    STROKES.GAINED.PUTTING_AVERAGE,
    STROKES.GAINED.TEE.TO.GREEN_SG.APR,
    STROKES.GAINED.TEE.TO.GREEN_SG.ARG,
    STROKES.GAINED.TEE.TO.GREEN_SG.OTT
  )
    

clusters <- kmeans(scale(train_df), 10)$cluster

pgatour %>% 
  select(PLAYER.NAME) %>% 
  bind_cols(train_df, tibble(clusters)) %>%
  arrange(clusters) %>%
  write_excel_csv(., "data/clusters.csv")
