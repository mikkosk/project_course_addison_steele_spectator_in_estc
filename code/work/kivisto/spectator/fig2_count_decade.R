jsd <- distinct(spectator, .keep_all = TRUE) %>% count(publication_decade) 
eond <- distinct(estc_only, .keep_all = TRUE) %>% count(publication_decade)
eabnd <- distinct(estc_and_bernard, .keep_all = TRUE) %>% count(publication_decade)
pond <- distinct(pure_only, .keep_all = TRUE) %>% count(publication_decade)

combined_data2 <- jsd %>% mutate(set = "Plain ESTC") %>% 
  rbind(eond %>% mutate(set = "Only ESTC")) %>% 
  rbind(eabnd %>% mutate(set = "ESTC and Bernard"))  %>% 
  rbind(pond %>% mutate(set = "Pure Spectator"))

draw2 <- function(data, title) {
  data <- data %>% filter(publication_decade < 1800)
  return(ggplot(data=data, aes(publication_decade, y = n)) +
           geom_line() +
           geom_point() + ggtitle(title))
}
fig2.1 <- draw2(jsd, "Just ESTC")

fig2.2 <- draw2(eond, "ESTC entries not in Bernard")

fig2.3 <- draw2(eabnd, "Matching entries ESTC & Bernard")

fig2.4 <- draw2(pond, "Pure Spectator only")

combined2 <- ggplot(combined_data2, aes(publication_decade, y = n, color = set)) + 
  geom_line()

final2 <- fig2.1 + fig2.2 + fig2.3 + fig2.4

png(file="../../../output/figures/spectator/fig2_decade_count.png",
    width=600, height=350)

print(final2)

dev.off()

png(file="../../../output/figures/spectator/fig2_decade_count_combined.png",
    width=600, height=350)

print(combined2)

dev.off()
