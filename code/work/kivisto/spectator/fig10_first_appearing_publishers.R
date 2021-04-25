draw10 <- function(data, position, title) { 
  data <- data %>%
    tidyr::separate_rows(actor_roles_all) %>%
    filter(actor_roles_all == "publisher") %>%
    distinct(actor_id, publication_decade, .keep_all = TRUE) %>%
    group_by(actor_id) %>% 
    filter(publication_decade == min(publication_decade)) %>% 
    slice(1) %>%
    ungroup() %>%
    group_by(publication_decade) %>%
    dplyr::summarise(n=n()) %>%
    filter(publication_decade > 1700) %>%
    filter(publication_decade < 1800)

  figure <- ggplot(data = data, aes(publication_decade, y = n)) +
    geom_bar(position=position, stat="identity") +
    geom_smooth(fill=NA) +
    ggtitle(title)
  
  return(figure)
}

fig10.1 <- draw10(spectator, "stack", "Plain data")

fig10.2 <- draw10(estc_only, "stack", "ESTC only")

fig10.3 <- draw10(estc_and_bernard, "stack", "ESTC and Bernard")

fig10.4 <- draw10(pure_only, "stack", "Pure Spectator")

fig10.5 <- draw10(tatler, "stack", "Tatler")

fig10.6 <- draw10(allData, "stack", "Whole ESTC")

final10 <- fig10.1 + fig10.2 + fig10.3 + fig10.4 + fig10.5 + fig10.6

png(file="../../../output/figures/spectator/fig10_new_publishers_by_decade.png",
    width=1200, height=700)

print(final10)

dev.off()

