#' @import ggplot2
#' @export
plot.pointgen <- function(x,
                          legend_title = "Labels",
                          boundary_color = "grey40",
                          boundary_fill= "grey95",
                          size=0.01,
                          ...) {
  
  boundary <- attr(x, "geometry")
  
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = boundary, fill = boundary_fill, color=boundary_color) +
    ggplot2::geom_point(data = x, ggplot2::aes(x = x, y = y, color = labels), alpha =0.8, size = size, ...) +
    ggplot2::labs(color = legend_title) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 1.5)))+
    ggplot2::theme_void()
}


#' @export
summary.pointgen <- function(x) {
  
  # Extract boundary geometry
  boundary <- attr(x, "geometry")
  
  # Number of events
  n_events <- nrow(x)
  
  # Labels (if present)
  if ("labels" %in% names(x)) {
    label_counts <- table(x$labels)
  } else {
    label_counts <- NULL
  }
  
  # Boundary info
  boundary_type <- class(boundary)[1]
  n_units <- nrow(boundary)
  
  # Print summary
  cat("Summary of 'pointgen' object\n")
  cat("Number of events: ", n_events, "\n", sep = "")
  if (!is.null(label_counts)) {
    cat("Event labels:\n")
    print(label_counts)
  }
  cat("Boundary type:   ", boundary_type, "\n", sep = "")
  cat("Boundary units:  ", n_units, "\n", sep = "")
  
  invisible(x)
}
