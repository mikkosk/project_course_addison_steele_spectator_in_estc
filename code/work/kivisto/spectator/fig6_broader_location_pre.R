draw6 <- function(data, title) {
  
  data <- broader_location(data) %>%
    filter(publication_decade > 1700) %>%
    filter(publication_decade <  1800) %>%
    distinct(id, .keep_all = TRUE) %>%
    group_by(broader_location, publication_decade) %>%
    dplyr::summarise(n = n()) %>%
    arrange(n)
  
  return(ggplot(data = data, aes(fill=broader_location, y = n, x = publication_decade)) +
           geom_bar(position="fill", stat="identity") +
           scale_fill_manual(values = palette)+
           ggtitle(title))
}

fig6.1 <- draw6(spectator, "Plain ESTC")

fig6.2 <- draw6(estc_only, "ESTC only")

fig6.3 <- draw6(estc_and_bernard, "ESTC and Bernard")

fig6.4 <- draw6(pure_only, "Pure Spectator")

fig6.5 <- draw6(tatler, "Tatler")

fig6.6 <- draw6(allData, "Whole ESTC")

final6 <- fig6.1 + fig6.2 + fig6.3 + fig6.4 + fig6.5 + fig6.6

png(file="../../../output/figures/spectator/fig6_spectator_broader_location_pre.png",
    width=1200, height=700)

print(final6)

dev.off()

