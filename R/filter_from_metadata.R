#' Title
#'
#' @param .data data.frame
#' @param Y Y
#' @param metadata metadata
#' @param filter_column column in metadata
#' @param filter_value value in filter_column
#'
#' @return a filter list of dataframe
#' @export
#'
#' @examples
#' data(iris)
#' .data <- iris[1:4]
#' rownames(.data) <- paste0("sample_", 1:nrow(.data))
#' Y = iris$Species %>% as.factor() %>% as.numeric()
#' metadata <- data.frame(sample =  paste0("sample_", 1:nrow(.data)), species = iris$Species)
#' filter_column <- "species"
#' filter_value = "versicolor"
#' filter_from_metadata(.data = .data, metadata = metadata,
#' filter_column = "species", filter_value = "versicolor")
#'
#' filter_from_metadata(.data = .data, Y = Y, metadata = metadata,
#' filter_column = "species", filter_value = "versicolor")
filter_from_metadata <- function(.data,
                                 Y = NULL,
                                 metadata,
                                 filter_column,
                                 filter_value,
                                 sample_col = "sample"){
  # validate .data
  # # validate Y
  # validate metadata

  filter_guide_df <- data.frame(sample = rownames(.data))
  if(!is.null(Y)){
    filter_guide_df[,"Y"] <- Y
  }
  filter_guide_df <- left_join(filter_guide_df, metadata, by = c("sample" = sample_col))

  # filter()
  filter_guide_df_filtered <- filter(filter_guide_df,
                                     filter_guide_df[[filter_column]] == filter_value)

  res_filtered <- list()
  res_filtered[[".data"]] <- .data[filter_guide_df_filtered$sample,]

  metadata_filtered <- filter_guide_df_filtered
  metadata_filtered[, sample_col] <- metadata_filtered$sample
  if(!is.null(Y)){
    metadata_filtered <- dplyr::select(metadata_filtered, -Y)
    res_filtered[["Y"]] <- filter_guide_df_filtered$Y
  }
  res_filtered[[".metadata"]] <- metadata_filtered

  return(res_filtered)
}
