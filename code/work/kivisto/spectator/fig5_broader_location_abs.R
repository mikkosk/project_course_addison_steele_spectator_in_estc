jsbl <- broader_location(spectator) %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(broader_location, publication_decade) %>%
  dplyr::summarise(n = n()) %>%
  arrange(n)

eonbl <- broader_location(estc_only) %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(broader_location, publication_decade) %>%
  dplyr::summarise(n = n()) %>%
  arrange(n)

eabnbl <- broader_location(estc_and_bernard) %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(broader_location, publication_decade) %>%
  dplyr::summarise(n = n()) %>%
  arrange(n)

pobl <- broader_location(pure_only) %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(broader_location, publication_decade) %>%
  dplyr::summarise(n = n()) %>%
  arrange(n)

draw5 <- function(data, title) {
  return(ggplot(data = data, aes(fill=broader_location, y = n, x = publication_decade)) +
           geom_bar(position="stack", stat="identity") +
           scale_fill_manual(values = palette)+
           ggtitle(title))
}

fig5.1 <- draw5(jsbl, "Just ESTC")

fig5.2 <- draw5(eonbl, "ESTC only")

fig5.3 <- draw5(eabnbl, "ESTC and Bernard")

fig5.4 <- draw5(pobl, "Pure Spectator")

final5 <- fig5.1 + fig5.2 + fig5.3 + fig5.4

png(file="../../../output/figures/spectator/fig5_spectator_broader_location_abs.png",
    width=1200, height=700)

print(final5)

dev.off()
