js <- distinct(spectator, .keep_all = TRUE) %>% mutate(set = "Plain ESTC")
eon <- distinct(estc_only, .keep_all = TRUE) %>% mutate(set = "Only ESTC")
eabn <- distinct(estc_and_bernard, .keep_all = TRUE) %>% mutate(set = "ESTC and Bernard")
pon <- distinct(pure_only, .keep_all = TRUE) %>% mutate(set = "Pure Spectator")
combined_data <- js %>% 
  rbind(eon) %>% 
  rbind(eabn) %>% 
  rbind(pon)

draw1 <- function(data, title) {
  data <- data %>% group_by(publication_decade, set) %>% dplyr::summarise(n=n()) %>% filter(publication_decade < 1800)
  return(ggplot(data=data, aes(publication_decade, y = n, color=set)) +
           geom_smooth(fill=NA) +
           geom_point() + ggtitle(title))
}
fig1.1 <- draw1(js, "Just ESTC")

fig1.2 <- draw1(eon, "ESTC entries not in Bernard")

fig1.3 <- draw1(eabn, "Matching entries ESTC & Bernard")

fig1.4 <- draw1(pon, "Pure Spectator only")

combined1 <- draw1(combined_data, "Combined")

final1 <- fig1.1 + fig1.2 + fig1.3 + fig1.4

png(file="../../../output/figures/spectator/fig1_count.png",
    width=600, height=350)

print(final1)

dev.off()

png(file="../../../output/figures/spectator/fig1_count_combined.png",
    width=600, height=350)

print(combined1)

dev.off()
