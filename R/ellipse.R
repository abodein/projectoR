#' Title
#'
#' @param x projectionObj
#'
#' @return a tibble
#' @export
#'
#' @examples
#' data(iris)
#' .data <- iris[1:4]
#' rownames(.data) <- paste0("sample_", 1:nrow(.data))
#' .metadata <- data.frame(sample =  paste0("sample_", 1:nrow(.data)), species = iris$Species)

#' Y = iris$Species %>% as.factor() %>% as.numeric()
#' res_pca <- projection(.data, .metadata = .metadata, method = "pca")
#'
#' res_ellipse <- get_ellipse(res_pca, group = "species")
#' res_ellipse <- get_ellipse(res_pca)
#'
#' res_ellipse %>% dplyr::select(c(sample, group, inside_distance)) %>% unnest(cols = c(sample, inside_distance))


get_ellipse <- function(projectionObj, group = NULL, ellipse.level = 0.95){

  # validate projection object

  # validate group
  checkmate::assert_choice(group, names(projectionObj$coord), null.ok = TRUE)

  # validate ellipse.level

  ellipse_df <- projectionObj$coord
  if(!is.null(group)){
    ellipse_df[, "group"] <- projectionObj$coord[, group]
  } else {
    ellipse_df[, "group"] <- "no_group"
  }
  ellipse_df_nested <- tidyr::nest(ellipse_df,
                            .by = group,
                            sample = sample,
                            dims = c(Dim1, Dim2),
                            metadata = !any_of(c("Dim1", "Dim2", "group", "sample")))

  # get variance, cdg
  ellipse_df_nested <- ellipse_df_nested %>%
    mutate(cdg = imap(dims, ~colMeans(.x))) %>%
    mutate(variance = imap(dims, ~var(.x))) %>%
    mutate(coord_ellipse = map2(variance, cdg,
                                ~ellipse::ellipse(x = .x, centre = .y,
                                         level = ellipse.level))) %>%
    mutate(coord_ellipse = imap(coord_ellipse, ~.x %>% as.data.frame %>% set_names(c("Dim1", "Dim2")))) %>%
#   mutate(ellipse_params = map2(cdg, coord_ellipse,~get_ellipse_params(.x, .y))) %>%
    mutate(inside_ellipse = map2(coord_ellipse, dims, ~inside_ellipse(.x, .y)))

  return(ellipse_df_nested)
}

euclidean <- function(a, b) sqrt(sum((a - b)^2))

get_ellipse_params <- function(ellipse_center, ellipse_coord){
  ellipse_coord_t <- ellipse_coord %>% t %>% as.data.frame()
  euclidean_dist <- map_dbl(ellipse_coord_t, function(b) {
    euclidean(as.vector(ellipse_center), as.vector(b))})
  ellipse_params <- tibble(r_max = max(euclidean_dist),
               r_min = min(euclidean_dist),
               h = ellipse_center[1],
               k = ellipse_center[2])

  return(ellipse_params)
}

inside_ellipse <- function(coord_ellipse, dims){
  inside <- sp::point.in.polygon(point.x = dims$Dim1,
                       point.y = dims$Dim2,
                       pol.x = coord_ellipse$Dim1,
                       pol.y = coord_ellipse$Dim2)
  return(tibble(inside = as.logical(inside)))
}

# points <- ellipse_df_nested$dims[[1]]
.inside_ellipse <- function(ellipse_params, points){

  distance_disc_ellipse <- function(x, y, rx, ry, h, k){
    theta = atan2(ry * (y - k), rx * (x - h))
    # sans rotation
    # ((x - h)^2)/(rx^2)) + (((y - k)^2)/(ry^2)
    d  = ((x - h) * cos(theta) + (y - k) * sin(theta))^2 / rx^2 + ((x - h) * sin(theta) - (y - k) * cos(theta))^2 / ry^2
    return(d)
  }

  points_xy <- points %>% t %>% as.data.frame()

  disc_dist <- map_dbl(points_xy, function(x) {
    distance_disc_ellipse(x = x[1], y = x[2],
                          rx = ellipse_params$r_max, ry = ellipse_params$r_min,
                          h = ellipse_params$h, k = ellipse_params$k)})
  res <- tibble(distance = disc_dist) %>% mutate(inside = distance <= 1)
  return(res)
}

# from ggplot2: different from ellipse::ellipse
calculate_ellipse <- function(data, vars, type, level, segments){
  dfn <- 2
  dfd <- nrow(data) - 1

  if (!type %in% c("t", "norm", "euclid")) {
    cli::cli_inform("Unrecognized ellipse type")
    ellipse <- matrix(NA_real_, ncol = 2)
  } else if (dfd < 3) {
    cli::cli_inform("Too few points to calculate an ellipse")
    ellipse <- matrix(NA_real_, ncol = 2)
  } else {
    if (type == "t") {
      v <- MASS::cov.trob(data[,vars])
    } else if (type == "norm") {
      v <- stats::cov.wt(data[,vars])
    } else if (type == "euclid") {
      v <- stats::cov.wt(data[,vars])
      v$cov <- diag(rep(min(diag(v$cov)), 2))
    }
    shape <- v$cov
    center <- v$center
    chol_decomp <- chol(shape)
    if (type == "euclid") {
      radius <- level/max(chol_decomp)
    } else {
      radius <- sqrt(dfn * stats::qf(level, dfn, dfd))
    }
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    ellipse <- t(center + radius * t(unit.circle %*% chol_decomp))
  }

  colnames(ellipse) <- vars
  mat_2_df(ellipse)
}
