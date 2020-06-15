library(tidyverse)
library(ggplot2)
data = readr::read_tsv('/Users/eloncha/Documents/GitHub/Elon/python/testSet_kmeans.txt', col_names = FALSE)

set.seed(6)
km = kmeans(data, centers = 5, iter.max = 20, nstart = 1, algorithm = 'Lloyd')
data['cluster'] = km['cluster']
centers = as.data.frame(km['centers'])
centers = centers %>% mutate(X1 = centers.X1, X2 = centers.X2)

ggplot() + 
  geom_point(data = data, aes(X1,X2, color = as.factor(cluster))) +
  geom_point(data = centers,aes(X1, X2), size = 5)