library(tidyverse)
library(factoextra)

data = readr::read_tsv('http://web.stanford.edu/~hastie/ElemStatLearn/datasets/prostate.data')
oriDataMat = data %>% select(.,c(-1,-10,-11)) %>% scale()
oriLabelMat = data %>% select(.,10) %>% scale()

getPrinCompTable = function (oriDataMat, oriLabelMat) {
  PCA = prcomp(oriDataMat)
  pcdata = as.data.frame(t(as.matrix(get_pca_var(PCA)$coord) %*% t(oriDataMat)))
  for (i in 1:length(pcdata)) {
    colnames(pcdata)[i] = paste0('Dim',as.character(i))
  }
  pctable = cbind(pcdata, oriLabelMat)
  return(pctable)
}

PCTable = getPrinCompTable(oriDataMat, oriLabelMat)

# PCA visualization
fviz_eig(PCA)
fviz_pca_ind(PCA,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(PCA,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Useful Info
get_eigenvalue(PCA)
get_pca_var(PCA)$coord
get_pca_var(PCA)$cos2
get_pca_var(PCA)$contrib
