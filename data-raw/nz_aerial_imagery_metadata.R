# Regional shapefiles ----
# Source: https://datafinder.stats.govt.nz/layer/111182-regional-council-2023-generalised/
reg_shp <- read_web_sf(url = "https://datafinder.stats.govt.nz/",
                       layer_id = 111182)
reg_shp <- reg_shp |>
  dplyr::filter(C2023_V != "Area Outside Region")

reg_shp_84 <- reg_shp |> sf::st_transform(4326)

library(leaflet)
leaflet() |>
  addWMSTiles(baseUrl = "https://basemaps.linz.govt.nz/v1/tiles/aerial/WebMercatorQuad/{z}/{x}/{y}.webp?api=c01hbexn54aeskebvrbthr8e2vf", layers = "Basemap", group = "basemap", options = list(maxZoom = 18)) |>
  addPolygons(data = reg_shp_84, group = "regions", color = "black", weight = 1, fillOpacity = 0, popup = ~C2023_V)

# LINZ API data
CSW <- ows4R::CSWClient$new(url = "https://data.linz.govt.nz/services/csw/",
                            serviceVersion = "2.0.2")

records <- CSW$getRecords()
# CSW$describeRecord(records[[1]])
length(records)
records[[1]]

# Flatten records to dataframe with title and references
records2 <- records |>
  purrr::map_dfr(\(x) {
    ref <- ifelse(is.null(x$references), NA, x$references)
    data.frame(title = x$title, references = ref)
  })

features <- lapply(1:length(records), \(i) {
  x <- records[[i]]
  print(i)

  chk <- grepl("aerial photos", x$title, ignore.case = TRUE) &
    !grepl("index tiles|tile index", x$title, ignore.case = TRUE)
  if (!chk) return()

  bbox <- x$BoundingBox
  layer_id <- gsub("https://data.linz.govt.nz/layer/", "", x$references)
  # print(x$references)
  if (length(layer_id) > 0) {
    layer_id <- as.numeric(strsplit(layer_id, "-")[[1]][1])
    if (is.na(layer_id)) {
      layer_id <- gsub("https://data.linz.govt.nz/set/", "", x$references)
      layer_id <- as.numeric(strsplit(layer_id, "-")[[1]][1])
    }
  } else {
    layer_id <- NA
  }

  if (x$title == "Tasman 0.1m Urban Aerial Photos (2017)") {
    layer_id <- 104167
  }
  if (x$title == "Tasman 0.075m Urban Aerial Photos (2017)") {
    layer_id <- 104166
  }


  abs <- x$abstract

  # Extract the "0.075m" value using a regular expression
  pattern <- "\\b\\d+\\.\\d+m\\b" # Metres
  extracted_value <- regmatches(abs, regexpr(pattern, abs))

  if (length(extracted_value) == 0) {
    pattern <- "\\b\\d+cm\\b" # cm
    extracted_value <- regmatches(abs, regexpr(pattern, abs))
    if (length(extracted_value) == 0) {
      return()
    }
  }


  # Use regular expression to split numeric and units
  result <- strsplit(extracted_value, "(?<=\\d)(?=[A-Za-z])", perl = TRUE)

  # Extract numeric and units
  numeric_value <- as.numeric(result[[1]][1])
  units <- result[[1]][2]


  if (!is.null(bbox)) {
    # strsplit(bbox, "(?<=-| )", perl = T)[[1]]
    # unlist(strsplit(bbox, " (?=-|\\d)", perl = TRUE))
    # unlist(strsplit(bbox, "(?<=\\d)\\s+(?=-)", perl = TRUE))
    matches <- gregexpr("(-?\\d+\\.\\d+) (-?\\d+\\.\\d+)", bbox)
    coords <- regmatches(bbox, matches)[[1]]

    nums <- as.numeric(unlist(strsplit(coords, " ")))
    if (length(nums) != 4) return()

    bb <- pgirmess::bbox2sf(n = nums[3],
                            s = nums[1],
                            w = nums[2],
                            e = nums[4]) |>
      sf::st_as_sf()
  } else {
    return()
  }

  if (is.na(layer_id)) print(i)

  aer <- bb |>
    dplyr::rename(geometry = x) |>
    dplyr::mutate(
      title = x$title,
      RGB = grepl("RGB", abs), RGBi = grepl("RGBi", abs), res = numeric_value,
      units = units,
      abstract = abs, layer_id = layer_id
    )

  cent <- aer |>
    sf::st_transform(2193) |>
    sf::st_centroid()

  region <- sf::st_intersection(cent, reg_shp) |>
    dplyr::pull(C2023_V)
  if (length(region) == 0) {
    region <- reg_shp[sf::st_nearest_feature(cent, reg_shp), ] |>
      dplyr::pull(C2023_V)
  }
  aer$region <- region

  # Extract the years in parentheses using a regular expression
  pattern <- "\\d{4}(?:-\\d{4})?"
  years <- regmatches(aer$title, gregexpr(pattern, aer$title))[[1]]
  # Split the years using the hyphen and convert to numeric
  year_range <- unlist(strsplit(years, "-"))
  years_numeric <- as.numeric(year_range)

  # Find and print the largest year
  aer$year <- max(years_numeric)
  aer
})

# feat_sf <- dplyr::bind_rows(features)
aerial_sf <- dplyr::bind_rows(features) |>
  dplyr::arrange(region, RGBi, res, dplyr::desc(year))
aerial_sf

aerial_sf$title[grep("Otago", aerial_sf$title)]


aerial_sf2 <- lapply(1:nrow(aerial_sf), \(i) {

  # Your input string
  input_string <- aerial_sf$abstract[i]

  # Extract the number "105089" from the URL
  layer_id <- regmatches(input_string, regexpr("\\d+$", input_string))

  # Print the extracted number
  print(i)

  # Extract the layer IDs from the URLs
  layer_ids <- regmatches(input_string, gregexpr("(?<=/layer/)\\d+", input_string, perl=TRUE))

  # Flatten the list and convert to numeric
  layer_id <- as.numeric(unlist(layer_ids)) |>
    dplyr::last()
  print(layer_id)

  f <- read_web_sf(url = "https://data.linz.govt.nz", layer_id = layer_id)

  if (is.null(f)) {
    return()
  }

  poly <- sf::st_union(f) |>
    # sf::st_cast("POLYGON") |>
    sf::st_as_sf() |>
    dplyr::rename(geometry = x) |>
    dplyr::mutate(
      title = aerial_sf$title[i],
      res = aerial_sf$res[i],
      units = aerial_sf$units[i],
      abstract = aerial_sf$abstract[i],
      layer_id = aerial_sf$layer_id[i],
      region = aerial_sf$region[i],
      year = aerial_sf$year[i]
    )

  return(poly)

  poly <- poly |>
    st_transform(4326)

  library(leaflet)
  leaflet() |>
    addWMSTiles(baseUrl = "https://basemaps.linz.govt.nz/v1/tiles/aerial/WebMercatorQuad/{z}/{x}/{y}.webp?api=c01hbexn54aeskebvrbthr8e2vf", layers = "Basemap", group = "basemap", options = list(maxZoom = 18)) |>
    addPolygons(data = poly, group = "aerial", color = "red", weight = 1, opacity = 1, fillOpacity = 0.5)


})


nz_aerial_imagery_metadata <- dplyr::bind_rows(aerial_sf2) |>
  dplyr::select(layer_id, title, abstract, region, res, units, year, geometry)
usethis::use_data(nz_aerial_imagery_metadata, overwrite = TRUE)


nz_aerial_imagery_metadata_84 <- nz_aerial_imagery_metadata |>
sf::st_transform(4326)

library(leaflet)
leaflet() |>
  addWMSTiles(baseUrl = "https://basemaps.linz.govt.nz/v1/tiles/aerial/WebMercatorQuad/{z}/{x}/{y}.webp?api=c01hbexn54aeskebvrbthr8e2vf", layers = "Basemap", group = "basemap", options = list(maxZoom = 18)) |>
  addPolygons(data = nz_aerial_imagery_metadata_84, fillOpacity = 0.3, label = ~title)

