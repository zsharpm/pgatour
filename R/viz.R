ggplot(stat_loop, aes(x = fct_reorder(player_name, driving_distance), y = driving_distance)) + 
  geom_point() + 
  coord_flip()

ggplot(stat_loop, aes(x = driving_distance, y = strokes_gained_approach_to_green, label = player_name)) + 
  #geom_point() + 
  geom_text(check_overlap = TRUE, size = 2) +
  ggtitle("2016 Stats", subtitle = "Strokes Gained (Approach) vs. Driving Distance") +
  labs(x = "Driving Distance", y = "Strokes Gained")

ggplot(stat_loop, aes(x = strokes_gained_putting, y = strokes_gained_approach_to_green, label = player_name)) + 
  #geom_point() + 
  geom_text(check_overlap = TRUE)