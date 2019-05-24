# functions ---------------------------------------------------------------


extract_function_genes <- function(names, data) {

  # read the genes function Pfam
  function_gene <- fread("../Genes_Pfam_GO.txt")

  names_function_gene <- sapply(names, function(x) {
    x <- strsplit(x, "\\.")[[1]][1]
  }, USE.NAMES = F)


  data_remix <- data[
    name_gene %chin% names,
    .(
      Gene = name_gene,
      condition = condition,
      logFC = logFC
    )
  ]

  data_remix <- dcast(data_remix, Gene ~ condition, value.var = "logFC", fill = 0)
  data_remix[, Gene := tstrsplit(Gene, "\\.", keep = 1)]

  output <- merge(function_gene, data_remix, by = "Gene", all.y = T)
  setorder(output, "pfam", na.last = T)

  return(output)
}



coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end = 4 / 6, alpha = alpha)[n:1]
}


# create an hypersphere inside the cloud points and retrive all the genes outside this sphere
sphere <- function(x, radius) {
  center <- apply(x, 2, mean)

  vec_dist <- rowSums(sweep(x, 2, center, "-")^2) >= radius^2

  return(names(which(vec_dist)))
}



heatmap_with_gene <- function(x, name = NULL, title = NULL) {
  if (!is.null(name)) {
    x <- x[name, ]
  }

  # order the label
  h <- hclust(dist(x))
  x <- x[h$order, ]


  # plot
  plot_ly(
    x = rownames(x), y = colnames(x), z = t(as.matrix(x)),
    type = "heatmap",
    colors = colorRamp(c("royalblue4", "darkolivegreen", "firebrick3"))
  ) %>%
    layout(title = title)
}


data_method <- data[, .N, by = name_gene]



namescall <- function(x, probs = 0.95, sup = T) {
  if (sup) {
    rownames(matrix[x > quantile(x, probs), ])
  } else {
    rownames(matrix[x < quantile(x, probs), ])
  }
}

# condition activation genes ----------------------------------------------

data_tri <- data[, .(.N, mean = mean(logFC), sd = sd(logFC)), by = condition]

ggplotly(ggplot(data_tri, aes(x = condition, y = N)) +
  geom_point(aes(color = mean, size = sd)) +
  ylab("number of differential genes who are activated"))

dodge <- position_dodge(width = 0.9)
ggplotly(ggplot(data_tri, aes(x = condition, y = mean, fill = N)) +
  geom_col(position = dodge) +
  geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = dodge))




# ACP ---------------------------------------------------------------------

acp <- dudi.pca(matrix, scale = F, center = F, nf = 5, scannf = F)
cumsum(100 * acp$eig / sum(acp$eig))
# j'ai pris 5 axes




# petites visualisations en 3d
plot_ly(acp$l1,
  x = ~RS1, y = ~RS2, z = ~RS3, type = "scatter3d",
  marker = list(symbol = "circle", sizemode = "diameter", color = "blue"),
  sizes = 0.2, mode = "markers", text = paste("Gene:", rownames(acp$l1))
) %>%
  layout(
    paper_bgcolor = "rgb(243, 243, 243)",
    plot_bgcolor = "rgb(243, 243, 243)"
  )


# plot3d(a$l1, type = "s", size = 0.2, col = "blue", alpha = 0.7)
# spheres3d(apply(a$l1, 2, mean), radius = 2, col = "red", alpha = 0.3)




# just an things to have a better performance latter
center <- apply(acp$l1, 2, mean)
vec_dist <- sqrt(rowSums(sweep(acp$l1, 2, center)^2))

# plot the number of value outside the sphere in function of the radius of that sphere
x <- seq(0, 19, by = 0.1)
num <- sapply(x, function(i) {
  sum(vec_dist >= i)
})
plot(x, num,
  type = "l", # log = "y",
  xlab = "radius",
  ylab = "number of genes outside the sphere"
)

data_method[name_gene %chin% sphere(acp$l1, 6), ACP := T]


heatmap_with_gene(acp$tab, sphere(acp$l1, 6))








# kmeans ------------------------------------------------------------------

wss <- sapply(1:100, function(k) {
  kmeans(matrix, k)$tot.withinss
})

plot(wss, type = "l")




# self organising map -----------------------------------------------------


# the model
set.seed(5)
som_grid <- somgrid(xdim = 10, ydim = 10, topo = "hexagonal", toroidal = F, neighbourhood.fct = "gaussian")
som_model <- som(matrix, grid = som_grid, rlen = 1000)



# plot
plot(som_model, codeRendering = "lines", shape = "straight")

par(mfrow = c(2, 2))
plot(som_model, type = "quality", palette.name = coolBlueHotRed)
plot(som_model, type = "changes")
plot(som_model, type = "counts", palette.name = coolBlueHotRed)
plot(som_model, type = "dist.neighbours", palette.name = coolBlueHotRed)
par(mfrow = c(1, 1))


# get the mean of the distances between the nodes
nhbrdist <- unit.distances(som_model$grid)
cddist <- as.matrix(object.distances(som_model, type = "codes"))
cddist[abs(nhbrdist - 1) > 0.001] <- NA
neigh.dists <- colMeans(cddist, na.rm = TRUE)

# predict the nodes of each rows of the dataset
pred <- predict(som_model)

# the names of the strange values
names <- rownames(pred$predictions[[1]])[ pred$unit.classif %in% which(neigh.dists > quantile(neigh.dists, probs = 0.95)) ]

# heatmap with them
heatmap_with_gene(matrix, names)


# som with repetition -----------------------------------------------------

list_som_iter <- sapply(seq(100, 3000, by = 100), function(x) {
  set.seed(5)
  print(x)
  unlist(replicate(50, {
    som_grid <- somgrid(xdim = 10, ydim = 10, topo = "hexagonal", toroidal = F)
    som_model <- som(matrix, grid = som_grid, rlen = x)

    # get the mean of the distances between the nodes
    nhbrdist <- unit.distances(som_model$grid)
    cddist <- as.matrix(object.distances(som_model, type = "codes"))
    cddist[abs(nhbrdist - 1) > 0.001] <- NA
    neigh.dists <- colMeans(cddist, na.rm = TRUE)

    # predict the nodes of each rows of the dataset
    pred <- predict(som_model)

    # the names of the strange values
    names <- rownames(pred$predictions[[1]])[ pred$unit.classif %in% which(neigh.dists > quantile(neigh.dists, probs = 0.95)) ]

    names
  }))
})

Names <- names(Filter(function(x) x > length(list_som_iter), table(unlist(list_som_iter))))

data_method[name_gene %chin% Names, SOM := T]



fwrite(extract_function_genes(names), "gene_explication.txt", sep = "\t")




# t SNE -------------------------------------------------------------------
set.seed(2)
tsne <- Rtsne(matrix, pca = T, normalize = F, pca_center = F, theta = 0, max_iter = 10000)

ggplot(NULL, aes(x = tsne$Y[, 1], y = tsne$Y[, 2])) +
  geom_point(aes(color = dbscan(tsne$Y, 1.5, 5)$cluster == 0)) +
  scale_color_discrete(name = "outliers")



data_method[, TSNE := ifelse(name_gene %chin% rownames(matrix[dbscan(tsne$Y, 1.5, 4)$cluster == 0, ]), T, F)]

x = seq(0.01, 1, by = 0.01)
y = sapply(x, function(i){ sum(dbscan(tsne$Y, eps = i)$cluster == 0) })
plot(x, y)








# dbscan ------------------------------------------------------------------
dbscan_mat <- dbscan(matrix, 2)
names_gene_dbscan <- rownames(matrix[which(dbscan_mat$cluster == 0), ])

x <- seq(0.1, 10, by = 0.1)
y <- sapply(x, function(i) {
  dbs <- dbscan(matrix, eps = i)
  sum(dbs$cluster == 0)

})
plot(x, y, type = "l")


data_method[name_gene %chin% names_gene_dbscan, DBSCAN := T]






# isolation forest --------------------------------------------------------
iso_for <- iForest(matrix, 10000, 2^8)
# pairs(matrix, col = ifelse(predict(iso_for, matrix) > quantile(predict(iso_for, matrix), 0.95), "red", "blue"))
Names <- namescall(predict(iso_for, matrix))

data_method[name_gene %chin% Names, IFOR := T]

# Angle-Based Outlier Detection -------------------------------------------
abod_mat <- abod(as.data.frame(matrix))
Names <- namescall(abod_mat, probs = 0.05, sup = F)

data_method[name_gene %chin% Names, ABOD := T]





# all the method ----------------------------------------------------------
data_method[ rowSums(data_method[, -(1:2)]) != 0 ]

func_gene <- extract_function_genes(data_method[ rowSums(data_method[, -(1:2)]) != 0, name_gene ], data = data)

data_method2 = data_method[, .(
  name_gene,
  methods = rowSums(.SD) / ncol(.SD)
),
.SDcols = patterns("[A-Z]{2,}")
][, name_gene := tstrsplit(name_gene, "\\.", keep = 1)]

fwrite(setorder(
  merge(func_gene, data_method2,
        by.x = "Gene",
        by.y = "name_gene",
        all.x = T),
  "pfam", na.last = T), "gene_explanation_all_methods2.csv")





ggplot(NULL, aes(x = tsne$Y[,1], y = tsne$Y[,2])) +
  geom_point(aes(col = data_method[, as.character(rowSums(.SD)),
                                   .SDcols = patterns("[A-Z]{2,}")])) +
  scale_color_discrete(name = "methods")



