draw5 <- function(data, title) {
  
  data <- broader_location(data) %>%
    filter(publication_decade > 1700) %>%
    filter(publication_decade <  1800) %>%
    distinct(id, .keep_all = TRUE) %>%
    group_by(broader_location, publication_decade) %>%
    dplyr::summarise(n = n()) %>%
    arrange(n)
  
  return(ggplot(data = data, aes(fill=broader_location, y = n, x = publication_decade)) +
           geom_bar(position="stack", stat="identity") +
           scale_fill_manual(values = palette)+
           ggtitle(title))
}

fig5.1 <- draw5(spectator, "Plain ESTC")

fig5.2 <- draw5(estc_only, "ESTC only")

fig5.3 <- draw5(estc_and_bernard, "ESTC and Bernard")

fig5.4 <- draw5(pure_only, "Pure Spectator")

fig5.5 <- draw5(tatler, "Tatler")

fig5.6 <- draw5(allData, "Whole ESTC")

final5 <- fig5.1 + fig5.2 + fig5.3 + fig5.4 + fig5.5 + fig5.6

png(file="../../../output/figures/spectator/fig5_spectator_broader_location_abs.png",
    width=1200, height=700)

print(final5)

dev.off()
