#FIG. 8 - Addison most published works

groupWorkA <- addison %>% group_by(finalWorkField) %>% dplyr::summarise(n = n())

groupWorkA <- arrange(groupWorkA, n)

groupWorkA$finalWorkField <- factor(groupWorkA$finalWorkField, levels = groupWorkA$finalWorkField)

fig8 <- ggplot(data = groupWorkA, aes(x = finalWorkField, y = n)) +
  geom_bar(position="stack", stat="identity") +
  coord_flip() +
  theme(axis.text.y  = element_text(angle=0,vjust=0.5, size=6,hjust=1))+
  ggtitle("Addison most published works")

png(file="../../../output/figures/fig8_addison_most_published.png",
    width=1200, height=700)

print(fig8)

dev.off()