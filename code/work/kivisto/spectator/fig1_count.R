js <- distinct(spectator, .keep_all = TRUE) %>% count(publication_year) 
eon <- distinct(estc_only, .keep_all = TRUE) %>% count(publication_year)
eabn <- distinct(estc_and_bernard, .keep_all = TRUE) %>% count(publication_year)
pon <- distinct(pure_only, .keep_all = TRUE) %>% count(publication_year)
combined_data <- js %>% mutate(set = "Plain ESTC") %>% 
  rbind(eon %>% mutate(set = "Only ESTC")) %>% 
  rbind(eabn %>% mutate(set = "ESTC and Bernard"))  %>% 
  rbind(pon %>% mutate(set = "Pure Spectator"))

draw1 <- function(data, title) {
  return(ggplot(data=data, aes(publication_year, y = n)) +
           geom_smooth(fill=NA) +
           geom_point() + ggtitle(title))
}
fig1.1 <- draw1(js, "Just ESTC")

fig1.2 <- draw1(eon, "ESTC entries not in Bernard")

fig1.3 <- draw1(eabn, "Matching entries ESTC & Bernard")

fig1.4 <- draw1(pon, "Pure Spectator only")

combined1 <- ggplot(combined_data, aes(publication_year, y = n, color = set)) + 
  geom_smooth(fill=NA)

final1 <- fig1.1 + fig1.2 + fig1.3 + fig1.4

png(file="../../../output/figures/spectator/fig1_count.png",
    width=600, height=350)

print(final1)

dev.off()

png(file="../../../output/figures/spectator/fig1_count_combined.png",
    width=600, height=350)

print(combined1)

dev.off()
