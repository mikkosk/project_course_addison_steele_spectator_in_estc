jsl <- spectator %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(publication_place_752, publication_year) %>%
  dplyr::summarise(n = n()) %>%
  arrange(n)

eonl <- estc_only %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(publication_place_752, publication_year) %>%
  dplyr::summarise(n = n()) %>%
  arrange(n)

eabnl <- estc_and_bernard %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(publication_place_752, publication_year) %>%
  dplyr::summarise(n = n()) %>%
  arrange(n)
  
pol <- pure_only %>%
  distinct(id, .keep_all = TRUE) %>%
  group_by(publication_place_752, publication_year) %>%
  dplyr::summarise(n = n()) %>%
  arrange(n)

draw3 <- function(data, title) {
  return(ggplot(data = data, aes(y = n, x = publication_year, fill = publication_place_752)) +
           geom_bar(position="stack", stat="identity") +
           scale_fill_manual(values = createLocationPalatte(data)) +
           ggtitle(title))
}

fig3.1 <- draw3(jsl, "Just ESTC")

fig3.2 <- draw3(eonl, "ESTC only")

fig3.3 <- draw3(eabnl, "ESTC and Bernard")

fig3.4 <- draw3(pol, "Pure Spectator")

final3 <- fig3.1 + fig3.2 + fig3.3 + fig3.4

png(file="../../../output/figures/spectator/fig3_spectator_location_abs.png",
    width=1200, height=700)

print(final3)

dev.off()

