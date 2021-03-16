draw12 <- function(data, position, title) { 
  
  publishers <- data %>%
    tidyr::separate_rows(actor_roles_all) %>%
    filter(actor_roles_all == "publisher") %>%
    group_by(publication_decade) %>%
    distinct(actor_id, .keep_all = TRUE) %>%
    dplyr::summarise(pub = n()) %>%
    #mutate(type = "pub") %>%
    filter(!is.na(publication_decade)) %>%
    ungroup()
  
  entries <- data %>%
    distinct(id, .keep_all = TRUE) %>%
    group_by(publication_decade) %>%
    dplyr::summarise(ent = n()) %>%
    #mutate(type = "ent") %>%
    filter(!is.na(publication_decade)) %>%
    ungroup()

  alt <- merge(publishers, entries, by="publication_decade")
  alt$entries_per_publisher <- alt$ent / alt$pub
  
  figure <- ggplot(data = alt, aes(publication_decade, y = entries_per_publisher)) +
    geom_point() +
    geom_smooth(fill=NA) +
    ggtitle(title)
  
  return(figure)
}

fig12.1 <- draw12(spectator, "stack", "Plain data")

fig12.2 <- draw12(estc_only, "stack", "ESTC only")

fig12.3 <- draw12(estc_and_bernard, "stack", "ESTC and Bernard")

fig12.4 <- draw12(pure_only, "stack", "Pure Spectator")

final12 <- fig12.1 + fig12.2 + fig12.3 + fig12.4

png(file="../../../output/figures/spectator/fig12_entries_per_publisher.png",
    width=1200, height=700)

print(final12)

dev.off()

