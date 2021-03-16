draw9 <- function(data, position, title) { 
  data <- data %>%
    tidyr::separate_rows(actor_roles_all) %>%
    filter(actor_roles_all == "publisher") %>%
    distinct(actor_id, publication_decade, .keep_all = TRUE) %>%
    group_by(publication_decade) %>%
    dplyr::summarise(n=n())  %>%
    filter(publication_decade < 1800)
  
  figure <- ggplot(data = data, aes(publication_decade, y = n)) +
    geom_bar(position=position, stat="identity") +
    geom_smooth(fill=NA) +
    ggtitle(title)
  
  return(figure)
}

fig9.1 <- draw9(spectator, "stack", "Plain data")

fig9.2 <- draw9(estc_only, "stack", "ESTC only")

fig9.3 <- draw9(estc_and_bernard, "stack", "ESTC and Bernard")

fig9.4 <- draw9(pure_only, "stack", "Pure Spectator")

final9 <- fig9.1 + fig9.2 + fig9.3 + fig9.4

png(file="../../../output/figures/spectator/fig9_distinct_publishers_by_decade.png",
    width=1200, height=700)

print(final9)

dev.off()

