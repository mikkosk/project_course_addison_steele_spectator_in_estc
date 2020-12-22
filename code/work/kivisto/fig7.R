#FIG. 7 - Steele most published works
groupWorkS <- steele %>% group_by(finalWorkField) %>% dplyr::summarise(n = n())

groupWorkS <- arrange(groupWorkS, n)

groupWorkS$finalWorkField <- factor(groupWorkS$finalWorkField, levels = groupWorkS$finalWorkField)

fig7 <- ggplot(data = groupWorkS, aes(x = finalWorkField, y = n)) +
  geom_bar(position="stack", stat="identity") +
  coord_flip() +
  theme(axis.text.y  = element_text(angle=0,vjust=0.5, size=6,hjust=1)) +
  ggtitle("Steele - most published works")


png(file="../../../output/figures/fig7_steele_pub.png",
    width=1200, height=700)

print(fig7)

dev.off()