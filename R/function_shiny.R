# ajoute un id a un box de maniere a pouvoir le montrer/cacher
boxWithId <- function(..., title = NULL, footer = NULL, status = NULL,
                      solidHeader = FALSE, background = NULL, width = 6, height = NULL,
                      collapsible = FALSE, collapsed = FALSE, id = NULL) {
  b <- match.call(expand.dots = TRUE)
  bid <- id
  b$id <- NULL
  b[[1]] <- as.name("box")
  b <- eval(b, parent.frame())
  b$attribs$id <- bid
  b
}

hideMenuItem <- function(tabName) {
  shinyjs::hide(selector = sprintf("a[data-value='%s']", tabName))
}

showMenuItem <- function(tabName) {
  shinyjs::show(selector = sprintf("a[data-value='%s']", tabName))
}



# for the color and shape on the true false graph
color_true_false = c("TRUE" = "blue", "FALSE" = "red")
shape_true_false = c("TRUE" = 19, "FALSE" = 3)


coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end = 4 / 6, alpha = alpha)[n:1]
}

# to do a heatmap with ggplot
heatmap_ggplot <- function(x, title = NULL) {
  mat = dcast(x, rn ~ comp_name, fill = 0, value.var = "logFC")
  mat = as.matrix(mat, rownames = "rn")

  order_gene = rownames(mat)[ hclust(dist(mat))$order ]
  order_cond = colnames(mat)[ hclust(dist(t(mat)))$order ]

  ggplot(x, aes(x = factor(rn, levels =  order_gene, ordered = T), y = factor(comp_name, levels = order_cond, ordered = T))) +
    geom_tile(aes(fill = logFC)) +
    scale_fill_gradientn(colors = coolBlueHotRed(200)) +
    xlab(NULL) + ylab(NULL) + theme_gray() + theme(axis.text.x = element_text(angle = -90)) + ggtitle(title)
}



# to print the the number of outliers
outliers_number = function(nb){
  sprintf("There is %d outliers", nb)
}


numericInp_danger_NA = function(x, session, input){

  inp = input$

  x %>% walk(~ feedbackDanger(paste0(session$ns(""), .), condition = is.na(input[[.]]), text = "It return a NA value"))

  return(invisible(x))
}




