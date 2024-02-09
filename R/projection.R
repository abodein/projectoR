#' projection
#'
#' This function can be used to call different projection methods on a data.frame
#' using the same interface.
#'
#' @param .data a numeric data.frame
#' @param method character, available methods: pca
#' @param .metadata (optional) a data.frame with sample metadata
#' @param .metadata_join_col (optional) if .metadata is given, .metadata_join_col must be a column of .metadata. Otherwise, .metadata_join_col <- "sample".
#' @param ... other argument to pass to projection method
#'
#' @export
projection <- function(.data,
                       method = "pca",
                       #engine,
                       .metadata = NULL,
                       .metadata_join_col = NULL, # default: "sample"
                       ...){

  # check .data
  checkmate::assert_data_frame(.data, types = "numeric")

  # check method
  current_methods <- c("pca") #TODO: COMPLETE
  checkmate::assert_choice(x = method, choices = current_methods)

  # available_engine <- list("pca" = c("mixOmics", "factomineR", umap = c("umap", ...)))
  # checkmate::assert_choice(x = engine, choices = available_engine[[method]])

  # check .metadata and .metadata_join_col
  if(!is.null(.metadata)){
    checkmate::assert_data_frame(.metadata, null.ok = FALSE)
    if(!is.null(.metadata_join_col)){
      checkmate::assert_string(.metadata_join_col, null.ok = FALSE)
    } else {
      .metadata_join_col <- "sample"
    }
    checkmate::check_subset(.metadata_join_col, choices = colnames(.metadata))
  }

  # projection
  if(method == "pca"){
    projectionObj <- produce_pca_df(.data = .data,  ...)
  } #TODO: COMPLETE else if ... umap ...

  if(!is.null(.metadata)){
    projectionObj <- add_metadata(projectionObj = projectionObj, .metadata = .metadata, .metadata_join_col = .metadata_join_col)
  }

  class(projectionObj) <- "projection"
  return(projectionObj)
}
