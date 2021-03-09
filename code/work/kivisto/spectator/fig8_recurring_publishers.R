draw8 <- function(data, position, title) { 
  data <- data %>%
    tidyr::separate_rows(actor_roles_all) %>%
    filter(actor_roles_all == "publisher") %>%
    group_by(publication_year, publication_decade, actor_id) %>%
    dplyr::summarise(n=n(), dup=duplicated(actor_id)) %>%
    group_by(publication_decade, dup) %>%
    dplyr::summarise(sum = sum(n))

  figure <- ggplot(data = data, aes(fill=dup, y = sum, x = publication_decade)) +
    geom_bar(position=position, stat="identity") +
    scale_fill_manual(values = palette)+
    ggtitle(title)
  
  return(figure)
}

fig8.1 <- draw8(spectator, "stack", "Plain data")

fig8.2 <- draw8(estc_only, "stack", "ESTC only")

fig8.3 <- draw8(estc_and_bernard, "stack", "ESTC and Bernard")

fig8.4 <- draw8(pure_only, "stack", "Pure Spectator")

fig8.5 <- draw8(spectator, "fill", "Plain data")

fig8.6 <- draw8(estc_only, "fill", "ESTC only")

fig8.7 <- draw8(estc_and_bernard, "fill", "ESTC and Bernard")

fig8.8 <- draw8(pure_only, "fill", "Pure Spectator")

final8 <- fig8.1 + fig8.2 + fig8.3 + fig8.4 + fig8.5 + fig8.6 + fig8.7 + fig8.8

png(file="../../../output/figures/spectator/fig8_spectator_recurring.png",
    width=1200, height=700)

print(final8)

dev.off()
