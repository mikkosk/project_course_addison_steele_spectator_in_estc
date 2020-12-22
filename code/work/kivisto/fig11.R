#FIG. 11 - How much do we know about other actors in Addison publications

addisonIds <- addison$id
worksWithAddison <- filter(allData, id %in% addisonIds) %>% 
  filter(actor_id != 7413288) %>%
  tidyr::separate_rows(actor_roles_all) %>%
  group_by(publication_year, actor_roles_all) %>%
  dplyr::summarise(n = n())

fig11 <- ggplot(data = worksWithAddison, aes(publication_year, y = n, color = actor_roles_all)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = palette)+
  ggtitle("How much do we know about other actors in Addison publications")

png(file="../../../output/figures/fig11_actors_addison.png",
    width=1200, height=700)

print(fig11)

dev.off()