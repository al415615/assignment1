
  #| code-fold: true
  # download the cycling network of a city from OpenStreetMap

  # city = character string with the city name, e.g. "Münster, Germany"
  # crs = coordinate reference system to use. Default is 4326 (WGS84)
  #creturn = a cycling_network instance 

  # global variables for ggplot
  infra_type <- NULL
  total_length_km <- NULL

  #' Download cycling network from OpenStreetMap
  #'
  #' Downloads cycling infrastructure data for a given city using OpenStreetMap
  #' and returns a `cycling_network` object containing spatial LINESTRING data.
  #'
  #' @param city Character string. Name of the city (e.g. "Münster, Germany").
  #' @param crs Numeric. EPSG coordinate reference system (default: 4326).
  #' @param bbox_km Numeric. Radius in kilometers for the bounding box.
  #'
  #' @return A `cycling_network` object with spatial cycling infrastructure.
  #'
  #' @importFrom stats aggregate
  #' @export
  get_cycling_network <- function(city, crs = 4326, bbox_km = 5) {
    
    # input validation (same as in the constructor)
    if (!is.character(city) || length(city) != 1 || nchar(city) == 0) {
      stop("`city` cannot be an empty character string, e.g. 'Münster, Germany'")
    }
    if (!is.numeric(crs) || length(crs) != 1) {
      stop("`crs` must be a single numeric EPSG code, e.g. 4326")
    }
    if (!is.numeric(bbox_km) || length(bbox_km) != 1 || bbox_km <= 0) {
      stop("`bbox_km` must be a positive number indicating the radius in km, e.g. 5")
    }
  

    # cache: if we already have the file in the cache, no need to calculate again (Münster is executed twice)
    cache_file <- paste0(gsub("[^a-zA-Z0-9]", "_", city), "_", bbox_km, "km_cycling.rds")
    
    if (file.exists(cache_file)) {
      cat("Loading cached data for: ", city)
      return(readRDS(cache_file))
    }
    
    cat("Downloading cycling network for: ", city)

    # find the center of the city we are interested in
    centre <- tryCatch(
      osmdata::getbb(city, format_out = "matrix"),
      error = function(e) stop("Could not find city '", city, "' in OpenStreetMap.")
    )


    # establish the boundary
    lon_centre <- mean(centre["x", ])
    lat_centre <- mean(centre["y", ])

    delta_lat <- bbox_km / 111
    delta_lon <- bbox_km / (111 * cos(lat_centre * pi / 180))
      
    bbox <- c(
      left   = lon_centre - delta_lon,
      bottom = lat_centre - delta_lat,
      right  = lon_centre + delta_lon,
      top    = lat_centre + delta_lat
    )
    
    # create the bounding box query for the given city
    q <- osmdata::opq(bbox = bbox, timeout = 120) 
    
    # One query with multiple features (this is to avoid timeout when I make too many queries)
    raw <- osmdata::osmdata_sf(
      osmdata::add_osm_features(q, features = list(
        "highway"  = "cycleway",
        "cycleway" = "lane",
        "cycleway" = "track",
        "cycleway" = "shared_lane",
        "cycleway" = "opposite",
        "cycleway" = "opposite_lane",
        "cycleway" = "opposite_track",
        "bicycle"  = "designated",
        "bicycle"  = "yes"
      ))
    )$osm_lines

    
    if (is.null(raw) || nrow(raw) == 0) {
      stop("No cycling infrastructure found for '", city, "'.")
    }
    
    # Assign osm_tag 
    raw$osm_tag <- dplyr::case_when(
      !is.na(raw$highway)  & raw$highway  == "cycleway"        ~ "highway=cycleway",
      !is.na(raw$cycleway) & raw$cycleway == "track"           ~ "cycleway=track",
      !is.na(raw$cycleway) & raw$cycleway == "opposite_track"  ~ "cycleway=opposite_track",
      !is.na(raw$cycleway) & raw$cycleway == "lane"            ~ "cycleway=lane",
      !is.na(raw$cycleway) & raw$cycleway == "opposite_lane"   ~ "cycleway=opposite_lane",
      !is.na(raw$cycleway) & raw$cycleway == "opposite"        ~ "cycleway=opposite",
      !is.na(raw$cycleway) & raw$cycleway == "shared_lane"     ~ "cycleway=shared_lane",
      !is.na(raw$bicycle)  & raw$bicycle  == "designated"      ~ "bicycle=designated",
      !is.na(raw$bicycle)  & raw$bicycle  == "yes"             ~ "bicycle=yes",
      TRUE ~ "other"
    )
    
    # Filter just recognized tag
    all_lines <- raw[raw$osm_tag != "other", c("osm_tag", "geometry")]

    # check that we got some data back
    if (is.null(all_lines) || nrow(all_lines) == 0) {
      stop("No cycling infrastructure found for '", city, "'. ")
    }
    
    # keep only geometrically valid LINESTRINGS
    all_lines <- all_lines[sf::st_is_valid(all_lines), ]
    
    # transform to the CRS specified
    all_lines <- sf::st_transform(all_lines, crs = crs)
    
    # save in the cache so it can be accessed
    result <- new_cycling_network(city = city, sf_lines = all_lines)
    saveRDS(result, cache_file)
    cat("Downloaded ", nrow(all_lines), " cycling segments.")   

    # return the cycling_network instance
    result
  }





  #| code-fold: true
  # classify cycling parts by infrastructure type

  # network = cycling_network instance from get_cycling_network()
  # return = cycling_classification instance

  #' Classify cycling infrastructure types
  #'
  #' Takes a `cycling_network` object and classifies each road segment into
  #' infrastructure safety categories, also computing summary statistics.
  #'
  #' @param network A `cycling_network` object created by `get_cycling_network()`.
  #'
  #' @return A `cycling_classification` object containing:
  #' \itemize{
  #'   \item classified spatial lines with `infra_type`
  #'   \item summary table of total length per infrastructure type
  #' }
  #'
  #' @importFrom stats aggregate
  #' @export
  classify_bike_infrastructure <- function(network) {
    
    # input validation (same as in the contructor)
    if (!inherits(network, "cycling_network")) {
      stop("`network` must be a `cycling_network` object",
          "Use get_cycling_network() first.")
    }
    
    lines <- network$lines
    
    # classify each segment based on OSM tags
    lines$infra_type <- dplyr::case_when(

      # LEVEL 1: lane completely separated from traffic
      lines$osm_tag == "highway=cycleway"       ~ "dedicated track",
      lines$osm_tag == "cycleway=track"         ~ "dedicated track",
      lines$osm_tag == "cycleway=opposite_track" ~ "dedicated track",
      # LEVEL 2: bike lane next to pedestrian lane (Münster Radwege)
      lines$osm_tag == "bicycle=designated"     ~ "footway track",
      # LEVEL 3: bike lane painted on road
      lines$osm_tag == "cycleway=lane"          ~ "painted lane",
      lines$osm_tag == "cycleway=opposite_lane" ~ "painted lane",
      lines$osm_tag == "cycleway=opposite"      ~ "painted lane",
      # LEVEL 4: bikes and cars no separation
      lines$osm_tag == "cycleway=shared_lane"   ~ "shared lane",
      # LEVEL 5: bike allowed in normal road
      lines$osm_tag == "bicycle=yes"            ~ "shared road",
      TRUE ~ "unknown"
    )
    
    # summary statistics: total length (SUM operation) in km per infrastructure type
    # st_length() returns length in m for projected CRS
    lines$length_m <- as.numeric(sf::st_length(lines))
    
    summary_stats <- stats::aggregate(
      length_m ~ infra_type,
      data = lines,
      FUN  = function(x) round(sum(x) / 1000, 2)  # convert m to km
    )
    names(summary_stats) <- c("infra_type", "total_length_km")
    
    # sort by total length descending (predominant infrastructure type first)
    summary_stats <- summary_stats[order(-summary_stats$total_length_km), ]
    
    # return the cycling_classification instance
    new_cycling_classification(
      network          = network,
      classified_lines = lines,
      summary_stats    = summary_stats
    )
  }


  #| code-fold: true
  # plot a safety map of cycling infrastructure

  # classification = cycling_classification instance
  # show_stats = If TRUE, prints summary statistics. Default TRUE
  # return = ggplot object that can be printed

  #' Plot cycling infrastructure safety map
  #'
  #' Creates a map of cycling infrastructure classified by safety level.
  #' Optionally includes a bar chart of total infrastructure length by type.
  #'
  #' @param classification A `cycling_classification` object.
  #' @param show_stats Logical. If TRUE, also shows summary statistics plot.
  #'
  #' @return A `ggplot` object (map or combined map + bar chart).
  #'
  #' @import ggplot2
  #' @import ggspatial
  #' @import patchwork
  #' @importFrom stats reorder
  #' @export
  plot_cycling_safety_map <- function(classification, show_stats = TRUE) {
    
    # input validation (same as in the contructors)
    if (!inherits(classification, "cycling_classification")) {
      stop("`classification` must be a `cycling_classification` object. ",
          "Use classify_bike_infrastructure() first.")
    }
    if (!is.logical(show_stats) || length(show_stats) != 1) {
      stop("`show_stats` must be TRUE or FALSE")
    }
    
    # optionally print summary statistics to console
    if (show_stats) {
      cat("\nInfrastructure summary for", classification$city, ":\n")
      print(knitr::kable(classification$summary, row.names = FALSE))
      cat("\n")
    }
    
    # a color for each infrastructure type
    safety_colours <- c(
      "dedicated track" = "forestgreen",
      "footway track"   = "steelblue",
      "painted lane"    = "goldenrod",
      "shared lane"     = "orange",
      "shared road"     = "tomato",
      "unknown"         = "grey70"
    )
    
    # build the map
    # reuse the plot() from the class 
    map_plot <- plot.cycling_classification(classification)
    
    if (!show_stats) return(map_plot)
      
      # bar chart of total km per infrastructure type
      bar_plot <- ggplot2::ggplot(classification$summary,
                        ggplot2::aes(x = stats::reorder(infra_type, total_length_km),
                            y = total_length_km,
                            fill = infra_type)) +
        ggplot2::geom_col(show.legend = FALSE) +
        ggplot2::scale_fill_manual(values = safety_colours) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = "By type (km)",
          x     = NULL,
          y     = "Total length (km)"
        ) +
        ggplot2::theme_minimal()
      
      # combine map and bar chart side by side using patchwork
      map_plot + bar_plot + patchwork::plot_layout(widths = c(2, 1))
    }