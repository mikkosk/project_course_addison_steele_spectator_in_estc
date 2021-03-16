draw11 <- function(data, position, title) { 
  
  publishers <- data %>%
    tidyr::separate_rows(actor_roles_all) %>%
    filter(actor_roles_all == "publisher") %>%
    group_by(publication_decade) %>%
    distinct(actor_id, .keep_all = TRUE) %>%
    dplyr::summarise(n = n()) %>%
    mutate(type = "pub") %>%
    ungroup()
  
  entries <- data %>%
    distinct(id, .keep_all = TRUE) %>%
    group_by(publication_decade) %>%
    dplyr::summarise(n = n()) %>%
    mutate(type = "ent") %>%
    ungroup()
  
  
  data <- publishers %>% rbind(entries) %>% filter(publication_decade < 1800)
  
  
  figure <- ggplot(data = data, aes(publication_decade, y = n, color = type)) +
    geom_point() +
    geom_smooth(fill=NA) +
    ggtitle(title)
  
  return(figure)
}

fig11.1 <- draw11(spectator, "stack", "Plain data")

fig11.2 <- draw11(estc_only, "stack", "ESTC only")

fig11.3 <- draw11(estc_and_bernard, "stack", "ESTC and Bernard")

fig11.4 <- draw11(pure_only, "stack", "Pure Spectator")

final11 <- fig11.1 + fig11.2 + fig11.3 + fig11.4

png(file="../../../output/figures/spectator/fig11_publishers_per_entries.png",
    width=1200, height=700)

print(final11)

dev.off()

