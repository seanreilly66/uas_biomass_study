set.seed(3527)
subjects <- sample(1:20, size = 80, replace = TRUE)
table(subjects)



lineup <- tibble(
  x = c(-1, -2, 8, 7, -12, -15, -13, 15, 21, 12, -25, 26), 
  y = c(1, -3, 6, -8, 8, 0, -10, 16, 2, -15, 1, 0), 
  id = 1:12)

ggplot(lineup, aes(x = x, y = y)) + 
  geom_point() +
  geom_text(aes(label=id, x = x + 0.5)) +
  # Assuming a 40x60 field
  lims(x = c(-30,30), y = c(-20, 20))

m_dist <- dist(lineup)

clust <- hclust(m_dist)

dend <- as.dendrogram(clust)
plot(dend)

cut <- cutree(clust, h = 10)

c_lineup <- mutate(lineup, cluster = cut)

ggplot(c_lineup, aes(x = x, y = y, color = factor(cluster))) +
  geom_point()
