jsp <- spectator %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(pure, publication_decade) %>%
  dplyr::summarise(n = n()) %>%
  arrange(n)

eop <- estc_only %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(pure, publication_decade) %>%
  dplyr::summarise(n = n()) %>%
  arrange(n)

eabp <- estc_and_bernard %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(pure, publication_decade) %>%
  dplyr::summarise(n = n()) %>%
  arrange(n)

draw7 <- function(data, position, title) {
  figure <- ggplot(data = data, aes(fill=pure, y = n, x = publication_decade)) +
    geom_bar(position=position, stat="identity") +
    scale_fill_manual(values = palette)+
    ggtitle(title)
  return(figure)
}

fig7.1 <- draw7(jsp, "fill", "Plain data")

fig7.2 <- draw7(eop, "fill", "ESTC only")

fig7.3 <- draw7(eabp, "fill", "ESTC & Bernard")

fig7.4 <- draw7(jsp, "stack", "Plain data")

fig7.5 <- draw7(eop, "stack", "ESTC only")

fig7.6 <- draw7(eabp, "stack", "ESTC & Bernard")

final7 <- fig7.1 + fig7.2 + fig7.3 + fig7.4 + fig7.5 + fig7.6


png(file="../../../output/figures/spectator/fig7_spectator_pure.png",
    width=1200, height=700)

print(final7)

dev.off()
