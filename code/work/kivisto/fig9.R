#FIG. 9 - Publish types

groupType <- adSt %>% group_by(document_type, publication_year, actor_id) %>% dplyr::summarise(n = n())

fig9 <- ggplot(data = groupType, aes(publication_year, y = n, color = actor_id)) +
  geom_line(aes(linetype=document_type)) +
  geom_point() +
  scale_color_hue(labels = c("Steele", "Addison"))+
  ggtitle("Publish types - Addison & Steele")

png(file="../../../output/figures/fig9_pub_types.png",
    width=1200, height=700)

print(fig9)

dev.off()