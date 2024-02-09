#'
add_metadata <- function(projectionObj, .metadata, .metadata_join_col){
  # test
  checkmate::check_class(projectionObj, "projectionObj")
  checkmate::check_data_frame(projectionObj$coord, min.cols = 3)
  checkmate::check_data_frame(.metadata_join_col)
  checkmate::assert_choice(x = .metadata_join_col, choices = colnames(projectionObj$coord))

  projectionObj$coord <- dplyr::left_join(projectionObj$coord, .metadata, by = c("sample" = .metadata_join_col))
  return(projectionObj)
}
