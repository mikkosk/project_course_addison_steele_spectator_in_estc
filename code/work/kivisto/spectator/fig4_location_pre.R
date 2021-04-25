draw4 <- function(data, title) {
  data <- data %>%
    distinct(id, .keep_all = TRUE) %>%
    group_by(publication_place_752, publication_decade) %>%
    dplyr::summarise(n = n()) %>%
    arrange(n)
  
  return(ggplot(data = data, aes(y = n, x = publication_decade, fill = publication_place_752)) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_manual(values = createLocationPalatte(data)) +
    ggtitle(title))
}

fig4.1 <- draw4(spectator, "Plain ESTC")

fig4.2 <- draw4(estc_only, "ESTC only")

fig4.3 <- draw4(estc_and_bernard, "ESTC and Bernard")

fig4.4 <- draw4(pure_only, "Pure Spectator")

fig4.5 <- draw4(tatler, "Tatler")

final4 <- fig4.1 + fig4.2 + fig4.3 + fig4.4 + fig4.5

png(file="../../../output/figures/spectator/fig4_spectator_location_pre.png",
    width=1200, height=700)

print(final4)

dev.off()

