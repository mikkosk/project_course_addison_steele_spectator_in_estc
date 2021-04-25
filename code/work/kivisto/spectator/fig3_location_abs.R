draw3 <- function(data, title) {
  data <- data %>% distinct(id, .keep_all = TRUE) %>%
    group_by(publication_place_752, publication_decade) %>%
    dplyr::summarise(n = n()) %>%
    arrange(n)
  return(ggplot(data = data, aes(y = n, x = publication_decade, fill = publication_place_752)) +
           geom_bar(position="stack", stat="identity") +
           scale_fill_manual(values = createLocationPalatte(data)) +
           ggtitle(title))
}

fig3.1 <- draw3(spectator, "Plain ESTC")

fig3.2 <- draw3(estc_only, "ESTC only")

fig3.3 <- draw3(estc_and_bernard, "ESTC and Bernard")

fig3.4 <- draw3(pure_only, "Pure Spectator")

fig3.5 <- draw3(tatler, "Tatler")

final3 <- fig3.1 + fig3.2 + fig3.3 + fig3.4 + fig3.5

png(file="../../../output/figures/spectator/fig3_spectator_location_abs.png",
    width=1200, height=700)

print(final3)

dev.off()

