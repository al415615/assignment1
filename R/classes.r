  #| code-fold: true
  # constructor of a cycling_network instance
  new_cycling_network <- function(city, sf_lines) {
    
    # validate the length of the city argument (it is not empty)
    if (!is.character(city) || length(city) != 1 || nchar(city) == 0) {
      stop("`city` cannot be an empty character string, e.g. 'Münster, Germany'")
    }
    
    # validate that sf_lines is an sf object (argument type)
    if (!inherits(sf_lines, "sf")) {
      stop("`sf_lines` must be an sf object")
    }
    
    # object structure as a list with class attribute
    structure(
      list(
        city         = city,
        lines        = sf_lines,   # LINESTRING geometries of the cycling network
        download_date = Sys.Date()  # date when the data was downloaded
      ),
      class = "cycling_network"
    )
  }

  # print()
  #' @export
  print.cycling_network <- function(x, ...) {
    cat("cycling_network object\n")
    cat("  City         :", x$city, "\n")
    cat("  Download date:", format(x$download_date), "\n")
    cat("  Network lines:", nrow(x$lines), "segments\n")
    cat("  CRS          :", sf::st_crs(x$lines)$input, "\n")
    invisible(x)
  }

  # plot()
  #' @export
  plot.cycling_network <- function(x, ...) {
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = x$lines, ggplot2::aes(color = "cycling infrastructure"), linewidth = 0.4) +
      ggplot2::scale_color_manual(values = c("cycling infrastructure" = "steelblue"), name = NULL) +
      ggplot2::labs(
        title    = paste("Cycling network:", x$city),
        subtitle = paste("Downloaded on", format(x$download_date)),
        caption  = "Source: OpenStreetMap contributors"
      ) +
      ggplot2::theme_minimal()
  }




  #| code-fold: true
  # constructor of a cycling_classification instance
  new_cycling_classification <- function(network, classified_lines, summary_stats) {
    
    # validate that network is a cycling_network object
    if (!inherits(network, "cycling_network")) {
      stop("`network` must be a `cycling_network` object")
    }
    
    # validate that classified_lines is an sf object with the required column (argument type)
    if (!inherits(classified_lines, "sf")) {
      stop("`classified_lines` must be an sf object")
    }
    if (!"infra_type" %in% names(classified_lines)) {
      stop("`classified_lines` must contain a column named `infra_type`")
    }
    
    # object structure extending the original network
    structure(
      list(
        city           = network$city,
        lines          = network$lines,
        download_date  = network$download_date,
        classified     = classified_lines,  # sf with infra_type column added
        summary        = summary_stats      # data.frame with length per category
      ),
      class = c("cycling_classification", "cycling_network")
    )
  }

  # print()
  #' @export
  print.cycling_classification <- function(x, ...) {
    cat("cycling_classification object\n")
    cat("  City         :", x$city, "\n")
    cat("  Download date:", format(x$download_date), "\n")
    cat("  Segments     :", nrow(x$classified), "\n")
    cat("\nInfrastructure summary:\n")
    print(knitr::kable(x$summary, row.names = FALSE))
    invisible(x)
  }

  # plot())
  #' @export
  plot.cycling_classification <- function(x, ...) {
    
    # a color for each infrastructure type that it is present
    safety_colours <- c(
      "dedicated track" = "forestgreen",
      "footway track"   = "steelblue",
      "painted lane"    = "goldenrod",
      "shared lane"     = "orange",
      "shared road"     = "tomato",
      "unknown"         = "grey70"
    )

      
    ggplot2::ggplot() +
      ggspatial::annotation_map_tile(type = "cartolight", zoom = 13, quiet = TRUE) +
      ggplot2::geom_sf(
        data = x$classified,
        ggplot2::aes(color = infra_type),
        linewidth = 0.5
      ) +
      ggplot2::scale_color_manual(
        values = safety_colours,
        name   = "Infrastructure type"
      ) +
      ggplot2::labs(
        title    = paste("Cycling infrastructure:", x$city),
        subtitle = "Classified by safety level",
        caption  = "Source: OpenStreetMap contributors"
      ) +
      ggplot2::theme_minimal()
  }
