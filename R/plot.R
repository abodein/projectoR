#' plot_projection
#'
#' a short description
#'
#' @param projection
#' @param color The name of the column in \code{projection$coord} to use to color the
#' points in the plot. If \code{NULL}, won't add color. Default: \code{NULL}.
#' @param shape The name of the column in \code{res_pca$coord} to use to define
#' the shape of the points in the PCA. If \code{NULL}, won't add change the shape.
#' Default: \code{NULL}.
#' @param size The size of the points. Default: 3.
#' @param show_names Try to add the labels of each point? Will only add the
#' labels if the points are not too closely clustered together in the PCA.
#' Default: \code{TRUE}.
#' @param title Add a title to the graph? If \code{NULL}, no title is added to
#' the graph. Otherwise this param must be a \code{character}. Default:
#' \code{NULL}.
#' @param legend description
#' @param legend.position Value for the \code{legend.position} param from
#' ggplot2. Default: "right".
#' @param legend.box description
#' @param graph Produce the graph. \code{TRUE} or \code{FALSE}. Default:
#' \code{TRUE}.
#'
#'
#' @return
#' @export
#'
plot.projection <- function(projection,
                            color = NULL, shape = NULL, size = 3,
                            show_names = TRUE,
                            title = NULL,
                            legend = TRUE, legend.position = "right",
                            legend.box = "vertical",
                            graph = TRUE){

  # check
  checkmate::assert_list(projection, types = c("data.frame", "character"), len = 2)
  checkmate::assert_data_frame(projection$coord, min.rows = 2, min.cols = 3)
  checkmate::assert_character(projection$labs, len = 2)

  checkmate::assert_character(color, len = 1, null.ok = TRUE)
  checkmate::assert_choice(color, names(projection$coord), null.ok = TRUE)

  checkmate::assert_character(shape, len = 1, null.ok = TRUE)
  checkmate::assert_choice(shape, names(projection$coord), null.ok = TRUE)

  checkmate::assert_double(size, lower = 0.01, len = 1)

  checkmate::assert_flag(show_names)

  checkmate::assert_character(title, len = 1, null.ok = TRUE)

  checkmate::assert_flag(legend)
  checkmate::assert_character(legend.position, len = 1)
  checkmate::assert_choice(legend.position, choices= c("left","top", "right", "bottom"))
  checkmate::assert_character(legend.box, len = 1)
  checkmate::assert_choice(legend.box, choices= c("vertical", "horizontal"))

  checkmate::assert_flag(graph)

  # misc
  ## To avoid 6 shapes limitation
  if (!is.null(shape)) {
    shape_vec <- c(15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10,11,12,13,14)
    shape_vec <- shape_vec[1:length(unique(projection$coord[[shape]]))]
  }


  gg <- ggplot2::ggplot(projection$coord, ggplot2::aes(x = Dim1, y = Dim2)) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = projection$labs[1], y = projection$labs[2])

  ## Add color and shape
  if (!is.null(color) & !is.null(shape)) {
    gg <- gg +
      ggplot2::geom_point(ggplot2::aes(col = !!sym(color), shape = !!sym(shape)),
                          size = size) +
      ggplot2::scale_shape_manual(values = shape_vec)
  } else if (!is.null(color) & is.null(shape)) {
    gg <- gg +
      ggplot2::geom_point(ggplot2::aes(col = !!sym(color)))
  } else if (is.null(color) & !is.null(shape)) {
    gg <- gg +
      ggplot2::geom_point(ggplot2::aes(shape = !!sym(shape)), size = size) +
      ggplot2::scale_shape_manual(values = shape_vec)
  } else {
    gg <- gg +
      ggplot2::geom_point(size = size)
  }

  # title
  if (!is.null(title)) {
    gg <- gg + ggplot2::ggtitle(title)
  }

  # legend
  if (!is.null(color) | !is.null(shape)) {
    gg <- gg + ggplot2::theme(legend.position = legend.position,
                              legend.box = legend.box)
  }

  # show_names
  if(show_names){
    gg <- gg + ggrepel::geom_text_repel(ggplot2::aes(label = sample),
                                        color = "black", force = 10)
  }

  # graph
  if (graph) {
    print(gg)
  }

  return(invisible(gg))
}
