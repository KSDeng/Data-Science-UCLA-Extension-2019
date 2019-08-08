# Learn to do PCA in R.
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
# Currently I don't know the theory behind it.

rm(list = ls())

#install.packages("factoextra")
library(factoextra)
# Import data
data(decathlon2)
decathlon2.active = decathlon2[1:23, 1:10]
# PCA calculation
res.pca = prcomp(decathlon2.active, scale = TRUE)
# Get eig values of the components.
get_eig(res.pca)
# Visualize PCA result.
fviz_eig(res.pca)



# Get the dimensions of PCA.
res.var = get_pca_var(res.pca)
res.var$coord
res.var$contrib # Contributions to the PCs
res.var$cos2 # Quality of representation

var_coord = as.data.frame(res.var$coord[,1:6])
var_coord$Variables <- row.names(var_coord)
str(var_coord)
var_coord = gather(data = var_coord, key = "Component", value = "Weight", - Variables)
var_coord$Color = var_coord$Weight > 0

# Plot the components and the weights of original variables.
library(ggplot2)
ggplot(var_coord, aes(x = Variables, y = Weight, fill = Color)) +
    geom_bar(stat = 'identity', position = "identity", width = .75) +
    facet_grid(Component ~ ., scales = 'free_y') +
    guides(fill = FALSE) +
    ylab('Component Loading') +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5))


# Results for individuals
res.ind = get_pca_ind(res.pca)
res.ind$coord

# Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_ind(res.pca, col.ind = "cos2",             # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),    # Gradient colors
             repel = TRUE)  # Avoid text overlapping

# Graph of variables. Positive correlated variables point to the same side of the plot. 
# Negative correlated variables point to opposite sides of the graph.
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_pca_var(res.pca, col.var = "contrib",
             axes = c(3,4),     # Specify the dimensions to be plotted
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Biplot of individuals and variables
fviz_pca_biplot(res.pca, reple = TRUE,
                col.var = "#2E9FDF",
                col.ind = "#696969")


ind.sup = decathlon2[24:27, 1:10]

# Use PCA to predict.
# The prediction results are new variables created by PCA.
ind.sup.coord = predict(res.pca, newdata = ind.sup)

