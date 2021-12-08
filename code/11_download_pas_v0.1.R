library(wdpar)

x = wdpa_fetch("global")
crs = "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
snap_tolerance = 5000
simplify_tolerance = 5000
geometry_precision = 50
erase_overlaps = TRUE
verbose = interactive()




  # check arguments are valid
  assertthat::assert_that(inherits(x, "sf"),
                          nrow(x) > 0,
                          all(assertthat::has_name(x, c("ISO3", "STATUS",
                                                        "DESIG_ENG", "REP_AREA",
                                                        "MARINE"))),
                          assertthat::is.string(crs) ||
                            assertthat::is.count(crs),
                          assertthat::is.number(snap_tolerance),
                          isTRUE(snap_tolerance >= 0),
                          assertthat::is.number(simplify_tolerance),
                          isTRUE(simplify_tolerance >= 0),
                          assertthat::is.count(geometry_precision),
                          assertthat::is.flag(erase_overlaps),
                          assertthat::is.flag(verbose))
  # check that x is in wgs1984
  assertthat::assert_that(sf::st_crs(x) == sf::st_crs(4326),
                          msg = "argument to x is not longitude/latitude (i.e. EPSG:4326)")
  # clean data
  ## remove areas that are not currently in action
  if (verbose) message("removing areas that are not implemented: ",
                       cli::symbol$continue, "\r", appendLF = FALSE)
  x <- x[x$STATUS %in% c("Designated", "Inscribed", "Established"), ]
  if (verbose) {
    utils::flush.console()
    message("removing areas that are not implemented: ", cli::symbol$tick)
  }
  ## remove UNESCO sites
  if (verbose) message("removing UNESCO reserves: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- x[x$DESIG_ENG != "UNESCO-MAB Biosphere Reserve", ]
  if (verbose) {
    utils::flush.console()
    message("removing UNESCO reserves: ", cli::symbol$tick)
  }
  ## assign column indicating geometry type
  is_point <- vapply(sf::st_geometry(x), inherits, logical(1),
                     c("POINT", "MULTIPOINT"))
  x$GEOMETRY_TYPE <- "POLYGON"
  x$GEOMETRY_TYPE[is_point] <- "POINT"
  ## remove protected areas represented as points that do not have
  ## a reported area
  if (verbose) message("removing points with no reported area: ",
                       cli::symbol$continue, "\r", appendLF = FALSE)
  x <- x[!(x$GEOMETRY_TYPE == "POINT" & !is.finite(x$REP_AREA)), ]
  if (verbose) {
    utils::flush.console()
    message("removing points with no reported area: ", cli::symbol$tick)
  }
  ## repair geometry
  if (verbose) message("repairing geometry: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- sf::st_set_precision(x, geometry_precision)
  x <- lwgeom::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), ]
  x <- extract_polygons_and_points(x)
  if (verbose) {
    utils::flush.console()
    message("repairing geometry: ", cli::symbol$tick)
  }
  
  ## wrap dateline issues
  if (verbose) message("wrapping dateline: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- sf::st_set_precision(x, geometry_precision)
  x <- suppressWarnings(sf::st_wrap_dateline(x,
                                             options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")))
  x <- x[!sf::st_is_empty(x), ]
  x <- extract_polygons_and_points(x)
  x <- sf::st_set_precision(x, geometry_precision)
  if (verbose) {
    utils::flush.console()
    message("wrapping dateline: ", cli::symbol$tick)
  }
  ## repair geometry
  if (verbose) message("repairing geometry: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- sf::st_set_precision(x, geometry_precision)
  x <- lwgeom::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), ]
  x <- extract_polygons_and_points(x)
  x <- sf::st_set_precision(x, geometry_precision)
  if (verbose) {
    utils::flush.console()
    message("repairing geometry: ", cli::symbol$tick)
  }
  ## reproject data
  if (verbose) message("projecting areas: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- sf::st_set_precision(x, geometry_precision)
  x <- sf::st_transform(x, crs)
  x <- sf::st_set_precision(x, geometry_precision)
  if (verbose) {
    utils::flush.console()
    message("projecting areas: ", cli::symbol$tick)
  }
  ## repair geometry again
  if (verbose) message("repairing geometry: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- lwgeom::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), ]
  x <- extract_polygons_and_points(x)
  x <- sf::st_set_precision(x, geometry_precision)
  if (verbose) {
    utils::flush.console()
    message("repairing geometry: ", cli::symbol$tick)
  }
  ## buffer polygons by zero to fix any remaining issues
  x_polygons_pos <- which(x$GEOMETRY_TYPE == "POLYGON")
  if (length(x_polygons_pos) > 0) {
    if (verbose) message("buffering by zero: ", cli::symbol$continue, "\r",
                         appendLF = FALSE)
    x_polygons_data <- x[x_polygons_pos, ]
    x_polygons_data <- sf::st_set_precision(x_polygons_data, geometry_precision)
    x_polygons_data <- sf::st_buffer(x_polygons_data, 0)
    x <- rbind(x[which(x$GEOMETRY_TYPE == "POINT"), ], x_polygons_data)
    x <- sf::st_set_precision(x, geometry_precision)
    if (verbose) {
      utils::flush.console()
      message("buffering by zero: ", cli::symbol$tick)
    }
  }
  ## buffer areas represented as points
  x_points_pos <- which(x$GEOMETRY_TYPE == "POINT")
  if (length(x_points_pos) > 0) {
    if (verbose) message("buffering points: ", cli::symbol$continue, "\r",
                         appendLF = FALSE)
    x_points_data <- x[x_points_pos, ]
    x_points_data <- sf::st_buffer(x_points_data,
                                   sqrt((x_points_data$REP_AREA * 1e6) / pi))
    x <- rbind(x[which(x$GEOMETRY_TYPE == "POLYGON"), ], x_points_data)
    x <- sf::st_set_precision(x, geometry_precision)
    if (verbose) {
      utils::flush.console()
      message("buffering points: ", cli::symbol$tick)
    }
  }
  ## simplify geometries
  if (simplify_tolerance > 0) {
    if (verbose) message("simplifying geometry: ", cli::symbol$continue,
                         "\r", appendLF = FALSE)
    x <- sf::st_set_precision(x, geometry_precision)
    x <- sf::st_simplify(x, TRUE, simplify_tolerance)
    x <- sf::st_set_precision(x, geometry_precision)
    x <- x[!sf::st_is_empty(x), ]
    x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
    x <- sf::st_set_precision(x, geometry_precision)
    if (verbose) {
      utils::flush.console()
      message("simplifying geometry: ", cli::symbol$tick)
    }
  }
  ## repair geometry again
  if (verbose) message("repairing geometry: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- sf::st_set_precision(x, geometry_precision)
  x <- lwgeom::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), ]
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  x <- sf::st_set_precision(x, geometry_precision)
  if (verbose) {
    utils::flush.console()
    message("repairing geometry: ", cli::symbol$tick)
  }
  ## snap geometry to grid
  if (snap_tolerance > 0) {
    if (verbose) message("snapping geometry to grid: ", cli::symbol$continue,
                         "\r", appendLF = FALSE)
    x <- sf::st_set_precision(x, geometry_precision)
    x <- lwgeom::st_snap_to_grid(x, snap_tolerance)
    x <- sf::st_set_precision(x, geometry_precision)
    if (verbose) {
      utils::flush.console()
      message("snapping geometry to grid: ", cli::symbol$tick)
    }
  }
  ## repair geometry again
  if (verbose) message("repairing geometry: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- sf::st_set_precision(x, geometry_precision)
  x <- lwgeom::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), ]
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  x <- sf::st_set_precision(x, geometry_precision)
  if (verbose) {
    utils::flush.console()
    message("repairing geometry: ", cli::symbol$tick)
  }
  ## format columns
  if (verbose) message("formatting attribute data: ", cli::symbol$continue,
                       "\r", appendLF = FALSE)
  x$MARINE[x$MARINE == "0"] <- "terrestrial"
  x$MARINE[x$MARINE == "1"] <- "partial"
  x$MARINE[x$MARINE == "2"] <- "marine"
  x$STATUS_YR[x$STATUS_YR == 0] <- NA_real_
  x$NO_TK_AREA[x$NO_TAKE %in% c("Not Reported", "Not Applicable")] <- NA_real_
  if (verbose) {
    utils::flush.console()
    message("formatting attribute data: ", cli::symbol$tick)
  }
  ## remove overlaps data
  if (erase_overlaps && isTRUE(nrow(x) > 1)) {
    if (verbose) message("erasing overlaps: ", cli::symbol$continue)
    x$IUCN_CAT <- factor(as.character(x$IUCN_CAT),
                         levels = c("Ia", "Ib", "II", "III", "IV", "V", "VI",
                                    "Not Reported", "Not Applicable",
                                    "Not Assigned"))
    x <- sf::st_set_precision(x, geometry_precision)
    x = ST_makeV
    x <- st_erase_overlaps(x[order(x$IUCN_CAT, x$STATUS_YR), ], verbose)
    x$IUCN_CAT <- as.character(x$IUCN_CAT)
    x <- x[!sf::st_is_empty(x), ]
    x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
    x <- sf::st_set_precision(x, geometry_precision)
    if (verbose) message("erasing overlaps: ", cli::symbol$tick)
  }
  ## remove slivers
  if (verbose) message("removing slivers: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  x <- x[as.numeric(sf::st_area(x)) > 0.1, ]
  if (verbose) {
    utils::flush.console()
    message("removing slivers: ", cli::symbol$tick)
  }
  ## calculate area in square kilometers
  if (verbose) message("calulating area: ", cli::symbol$continue, "\r",
                       appendLF = FALSE)
  areas <- as.numeric(sf::st_area(x)) * 1e-6
  x$AREA_KM2 <- as.numeric(areas)
  if (verbose) {
    utils::flush.console()
    message("calculating area: ", cli::symbol$tick)
  }
  ## move geometry to last column
  if ((!"geometry" %in% names(x))) {
    geom_col <- attr(x, "sf_column")
    attr(x, "sf_column") <- "geometry"
    names(x)[names(x) == geom_col] <- "geometry"
  }
  x <- x[, c(setdiff(names(x), "geometry"), "geometry")]
  
  
  saveRDS(x, "Documents/ModellingTrends/Data/ProtectedAreas/PA_simple.rds")
  