#' produce_pca_df
#'
#' This function produce a dataframe based on a 2 components PCA sample projection.
#'
#' @param .data a matrix
#'
#' @return a list
#'
#' @examples
#' data('iris')
#' X <- iris
#' projection.obj <- produce_pca_df(X)
#'
produce_pca_df <- function(.data, ...){

  # produce pca
  pca.res <- mixOmics::pca(.data, ...)

  # extract explained var.
  labs <- c(pca.res$prop_expl_var$X[1], pca.res$prop_expl_var$X[2])
  labs <- paste0(c("PC1: ", "PC22: "), round(labs, digits = 3)*100,  "% expl. var")

  # produce df
  pca.df <- data.frame("sample" = rownames(pca.res$variates$X),
                       "Dim1" = unname(pca.res$variates$X[,1]),
                       "Dim2" = unname(pca.res$variates$X[,2]))

  projectionObj <- list()
  projectionObj[["coord"]] <- pca.df
  projectionObj[["labs"]] <- labs

  return(list("coord" = pca.df, "labs" = labs))
}



#' produce_umap_df
#'
#' This function produce a dataframe based on a 2 components umap sample projection.
#'
#' @param .data a matrix
#'
#' @return a list
#'
#' @examples
#' data('iris')
#' X <- iris
#' projection.obj <- produce_umap(X)

produce_umap_df <- function(.data, n_components = 2, random_state = 1, ...){

  checkmate::assert_integer(as.integer(n_components))
  checkmate::assert_integer(as.integer(random_state))

  umap.res <- umap(.data, n_components = 2, random_state = 1, ...)
  umap.df <- data.frame(sample = rownames(.data),
                        Dim1 = umap.res[["layout"]][,1],
                        Dim2 = umap.res[["layout"]][,2])

  return(list("coord" = umap.df, "labs" = c("X1", "X2")))
}
