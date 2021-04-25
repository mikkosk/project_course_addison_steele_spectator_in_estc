# Step 1
js <- spectator %>% distinct(id, .keep_all = TRUE) %>% mutate(set = "Plain ESTC")
eon <- estc_only %>% distinct(id, .keep_all = TRUE) %>% mutate(set = "Only ESTC")
eabn <- estc_and_bernard %>% distinct(id, .keep_all = TRUE) %>% mutate(set = "ESTC and Bernard")
pon <- pure_only %>% distinct(id, .keep_all = TRUE) %>% mutate(set = "Pure Spectator")
tat <- tatler %>% distinct(id, .keep_all = TRUE) %>% mutate(set = "Tatler")
all <- allData %>% distinct(id, .keep_all = TRUE) %>% mutate(set = "Whole ESTC")

#Step 2

combined_data <- js %>% 
  rbind(eon) %>% 
  rbind(eabn) %>% 
  rbind(pon)

# Step 3
draw1 <- function(data, title) {
  data <- data %>% filter(publication_decade > 1700) %>% group_by(publication_decade, set) %>% dplyr::summarise(n=n()) %>% filter(publication_decade < 1800)
  return(ggplot(data=data, aes(publication_decade, y = n, color=set)) +
           geom_smooth(fill=NA) +
           geom_point() + ggtitle(title))
}

# Step 4

fig1.1 <- draw1(js, "Just ESTC")

fig1.2 <- draw1(eon, "ESTC entries not in Bernard")

fig1.3 <- draw1(eabn, "Matching entries ESTC & Bernard")

fig1.4 <- draw1(pon, "Pure Spectator only")

fig1.5 <- draw1(tat, "Tatler")

fig1.6 <- draw1(all, "Whole ESTC")

combined1 <- draw1(combined_data, "Combined")

final1 <- fig1.1 + fig1.2 + fig1.3 + fig1.4 + fig1.5 + fig1.6

# Step 5
png(file="../../../output/figures/spectator/fig1_count.png",
    width=1200, height=700)

print(final1)

dev.off()

png(file="../../../output/figures/spectator/fig1_count_combined.png",
    width=600, height=350)

print(combined1)

dev.off()

