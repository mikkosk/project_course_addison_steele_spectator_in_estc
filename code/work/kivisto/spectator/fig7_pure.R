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

draw7 <- function(data, position) {
  figure <- ggplot(data = data, aes(fill=pure, y = n, x = publication_decade)) +
    geom_bar(position=position, stat="identity") +
    scale_fill_manual(values = palette)+
    ggtitle("Plain Data %")
  return(figure)
}

fig7.1 <- draw7(jsp, "fill")

fig7.2 <- draw7(eop, "fill")

fig7.3 <- draw(eabp, "fill")

fig7.4 <- draw7(jsp, "stack")

fig7.5 <- draw7(eop, "stack")

fig7.6 <- draw(eabp, "stack")

final7 <- fig7.1 + fig7.2 + fig7.3 + fig7.4 + fig7.5 + fig7.6


png(file="../../../output/figures/spectator/fig7_spectator_pure.png",
    width=1200, height=700)

print(final7)

dev.off()
