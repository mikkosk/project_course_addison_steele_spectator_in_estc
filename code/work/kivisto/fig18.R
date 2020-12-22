#FIG. 18 - Works where Spectator was used

spectatorFinal <- spectator %>% distinct(id, .keep_all = TRUE) %>% group_by(finalWorkField) %>% dplyr::summarise(n = n())

finalCount <- arrange(spectatorFinal, n)

finalCount$finalWorkField <- factor(finalCount$finalWorkField, levels =finalCount$finalWorkField)

fig18 <- ggplot(data = finalCount, aes(x = finalWorkField, y = n)) +
  geom_bar(position="stack", stat="identity") +
  coord_flip() +
  theme(axis.text.y  = element_text(angle=0,vjust=0.5, size=6,hjust=1))+
  ggtitle("Works where Spectator was used")

png(file="../../../output/figures/fig18_works_specator.png",
    width=1200, height=700)

print(fig18)

dev.off()
