# Improve PCA visualisation

require(mixOmics)
require(tidyverse)
require(checkmate)
require(ggrepel)

#' produce_pca_df
#'
#' This function produce a dataframe based on a 2 components PCA sample projection.
#' From metadata, it includes all other information for colors, shapes, ...
#'
#' @param .data a matrix
#' @param metadata a data.frame
#' @param sample_col (default: "sample") sample column in metadata to join PCA variates
#'
#' @return a list
#' @export
#'
#' @examples
#' data('iris')
#' X <- iris
#' X$sample <- paste0("sample_", seq_along(rownames(iris)))
#' metadata <- X[, c("Species", "sample")]
#' X <- X[,c(1,2,3,4)]
#' rownames(X) <- metadata$sample
#'
#' projection.obj <- produce_pca_df(X, metadata)
#'
old_produce_pca_df <- function(.data, metadata = NULL, sample_col = "sample"){

  # check
  checkmate::assert_data_frame(.data, types = "numeric")

  checkmate::assert_data_frame(metadata, null.ok = TRUE)
  if(!is.null(metadata)){
    checkmate::assert_choice(sample_col, names(metadata), null.ok = TRUE)
  }
  # produce pca
  pca.res <- mixOmics::pca(.data, ncomp = 2)

  # extract explained var.
  labs <- c(pca.res$prop_expl_var$X[1], pca.res$prop_expl_var$X[2])
  labs <- paste0(c("PC1: ", "PC22: "), round(labs, digits = 3)*100,  "% expl. var")

  # produce df
  pca.df <- data.frame("sample" = rownames(pca.res$variates$X),
                       "Dim1" = unname(pca.res$variates$X[,1]),
                       "Dim2" = unname(pca.res$variates$X[,2]))
  if(!is.null(metadata)){
    pca.df <- dplyr::left_join(pca.df, metadata, by = c("sample" = sample_col))
  }

  return(list("coord" = pca.df, "labs" = labs))
}


#' produce_plsda_df
#'
#' This function produce a dataframe based on a 2 components PLS-DA sample projection.
#' From metadata, it includes all other information for colors, shapes, ...
#'
#' @param X a matrix
#' @param Y a vector
#' @param metadata a data.frame
#' @param sample_col (default: "sample") sample column in metadata to join PCA variates
#'
#' @return a list
#' @export
#'
old_produce_plsda_df <- function(.data, Y, metadata = NULL, sample_col = "sample", ...){

  # check
  checkmate::assert_data_frame(.data, types = "numeric")
  checkmate::assert_atomic(Y, len = nrow(.data))

  checkmate::assert_data_frame(metadata, null.ok = TRUE)
  if(!is.null(metadata)){
    checkmate::assert_choice(sample_col, names(metadata), null.ok = TRUE)
  }

  # produce pca
  plsda.res <- mixOmics::plsda(X = .data, Y = Y, ncomp = 2, ...)

  # extract explained var.
  labs <- c(plsda.res$prop_expl_var$X[1], plsda.res$prop_expl_var$X[2])
  labs <- paste0(c("X-variate 1: ", "X-variate 2: "), round(labs, digits = 3)*100,  "% expl. var")

  # produce df
  plsda.df <- data.frame("sample" = rownames(plsda.res$variates$X),
                         "Dim1" = unname(plsda.res$variates$X[,1]),
                         "Dim2" = unname(plsda.res$variates$X[,2]),
                         "Y" = Y)
  if(!is.null(metadata)){
    plsda.df <- dplyr::left_join(plsda.df, metadata, by = c("sample" = sample_col))
  }
  res <- list("coord" = plsda.df, "labs" = labs)
  class(res) <- "projection"
  return(res)
}

old_produce_tSNE_df <- function(.data, seed = 1, initial_dims = 2, metadata = NULL, sample_col = "sample",  ...){
  checkmate::assert_integer(as.integer(seed))
  checkmate::assert_integer(as.integer(initial_dims))


  # check
  checkmate::assert_data_frame(.data, types = "numeric")

  checkmate::assert_data_frame(metadata, null.ok = TRUE)
  if(!is.null(metadata)){
    checkmate::assert_choice(sample_col, names(metadata), null.ok = TRUE)
  }

  set.seed(seed)
  tsne.res <- tsne(.data, initial_dims = initial_dims, ...)
  tsne.df <- data.frame(sample = rownames(.data),
                        Dim1 = tsne.res[,1],
                        Dim2 = tsne.res[,2])
  if(!is.null(metadata)){
    tsne.df <- dplyr::left_join(tsne.df, metadata, by = c("sample" = sample_col))
  }
  return(list("coord" = tsne.df, "labs" = c("X1", "X2")))
}

old_produce_umap_df <- function(.data, n_components = 2, random_state = 1,
                            metadata = NULL, sample_col = "sample",  ...){

  checkmate::assert_integer(as.integer(n_components))
  checkmate::assert_integer(as.integer(random_state))


  # check
  checkmate::assert_data_frame(.data, types = "numeric")

  checkmate::assert_data_frame(metadata, null.ok = TRUE)
  if(!is.null(metadata)){
    checkmate::assert_choice(sample_col, names(metadata), null.ok = TRUE)
  }

  umap.res <- umap(.data, n_components = 2, random_state = 1, ...)
  umap.df <- data.frame(sample = rownames(.data),
                        Dim1 = umap.res[["layout"]][,1],
                        Dim2 = umap.res[["layout"]][,2])
  if(!is.null(metadata)){
    umap.df <- dplyr::left_join(umap.df, metadata, by = c("sample" = sample_col))
  }
  return(list("coord" = umap.df, "labs" = c("X1", "X2")))
}
