#FIG. 10 - How much do we know about other actors in Steele publications

steeleIds <- steele$id
worksWithSteele <- filter(allData, id %in% steeleIds) %>% 
  filter(actor_id != 22167754) %>%
  tidyr::separate_rows(actor_roles_all) %>%
  group_by(publication_year, actor_roles_all) %>%
  dplyr::summarise(n = n())

fig10 <- ggplot(data = worksWithSteele, aes(publication_year, y = n, color = actor_roles_all)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = palette)+
  ggtitle("How much do we know about other actors in Steele publications")

png(file="../../../output/figures/fig10_actors_steele.png",
    width=1200, height=700)

print(fig10)

dev.off()