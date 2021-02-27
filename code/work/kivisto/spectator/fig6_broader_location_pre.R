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

draw6 <- function(data, title){
  return(ggplot(data = data, aes(fill=broader_location, y = n, x = publication_decade)) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_manual(values = palette)+
    ggtitle(title))
}

fig6.1 <- draw6(jsbl, "Just ESTC")

fig6.2 <- draw6(eonbl, "ESTC only")

fig6.3 <- draw6(eabnbl, "ESTC and Bernard")

fig6.4 <- draw6(pobl, "Pure Spectator")

final6 <- fig6.1 + fig6.2 + fig6.3 + fig6.4

png(file="../../../output/figures/spectator/fig6_spectator_broader_location_pre.png",
    width=1200, height=700)

print(final6)

dev.off()

