draw11 <- function(data, position, title) { 
  data <- data %>%
    filter(publication_decade > 1700) %>%
    filter(publication_decade < 1800)
  
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
  
  
  data <- publishers %>% rbind(entries)
  
  
  figure <- ggplot(data = data, aes(publication_decade, y = n, color = type)) +
    geom_point() +
    geom_smooth(fill=NA) +
    ylim(0, NA) +
    ggtitle(title)
  
  return(figure)
}

fig11.1 <- draw11(spectator, "stack", "Plain data")

fig11.2 <- draw11(estc_only, "stack", "ESTC only")

fig11.3 <- draw11(estc_and_bernard, "stack", "ESTC and Bernard")

fig11.4 <- draw11(pure_only, "stack", "Pure Spectator")

fig11.5 <- draw11(tatler, "stack", "Tatler")

fig11.6 <- draw11(allData, "stack", "Whole ESTC")

final11 <- fig11.1 + fig11.2 + fig11.3 + fig11.4 + fig11.5 + fig11.6 

png(file="../../../output/figures/spectator/fig11_publishers_per_entries.png",
    width=1200, height=700)

print(final11)

dev.off()

