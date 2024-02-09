#' @export
projection <- function(.data,
                       method,
                       #engine,
                       ...){

  # check .data
  checkmate::assert_data_frame(.data, types = "numeric")

  # check method
  current_methods <- c("pca", "plsda", "umap", "tsne")
  checkmate::assert_choice(x = method, choices = current_methods)

  # available_engine <- list("pca" = c("mixOmics", "factomineR", umap = c("umap", ...)))
  # checkmate::assert_choice(x = engine, choices = available_engine[[method]])

  # other argument are checked during specific function evaluation

  # projection
  if(method == "pca"){
    produce_pca_df(.data - .data,  ...)
  } else if (method == "umap"){
    produce_umap_df(.data = .data,  ...)
  }
}
