#' Robust geocoding function
#'
#' geocode_robust geocodes locations via the Google Maps API. It then
#' selects the nonmissing observations for the final and returned latlons.
#' It continues this process until all latlons are found.
#'
#' @importFrom ggmap geocode
#'
#' @param data the data.frame containing the location column to be geocoded.
#' The default is "address"
#' @param location the name of the location column to be geocoded (character
#' vector of length 1)
#' @param n the number of times the location column should be geocoded
#' The default is 10. (numeric vector of length 1)
#' @param return_full_dataframe if TRUE, returns the original dataframe plus
#' the lat and lon columns. If FALSE, returns the lat and lon columns.
#'
#' @return a data.frame identical to that of the input data frame except with
#' lat and lon columns for the location column or just the lat and lon columns.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#' addr = c("The White House", "11 Wall St, New York, NY"),
#' stringsAsFactors = FALSE
#' )
#' data1 <- geocode_robust(data, location = "addr")
#' }
#'
#' @note this function will throw an error if `data` is not a data.frame
#' @note this function will throw an error if `location` is not a non-null
#' column of `data`
#' @note this function will throw an error if `n' is not coercible to a
#' non-null integer vector of length 1
#'
#' @export
geocode_robust <- function(data, location = "address", n = 10, return_full_dataframe = FALSE) {

  ##### Ensure inputs are compatible

  ### Ensure `data` is a data frame
  if(!"data.frame" %in% class(data)) {
    stop("`data` must be a data.frame")
  }

  ### Ensure `location` is a non-null column of `data`
  if(is.null(data[[location]])) {
    stop("`location` must be a non-null column of `data`")
  }

  ### Ensure `n` is coercible to be an integer vector of length 1
  n_use <- round(n, 0)
  if(is.null(n_use) | length(n_use) > 1) {
    stop("`n` must be coercible to be an integer vector of length 1")
  } else if (!identical(n, n_use)){
    message(paste0("Converting n to an integer. Locations will be gocoded ",
                   n_use, " times."))
  }

  ### Keep only unique locations, but save a copy of the full dataset for later
  if(length(unique(data[[location]])) < nrow(data)) {
    message("Duplicates found in location column. Geocoding unique
            locations, but returning all observations")
    data0 <- data
    data0$order <- as.numeric(row.names(data0))
    data <- data0[which(!duplicated(data0[[location]])),]

  } else {
    data0 <- data
    data0$order <- as.numeric(row.names(data0))
  }

  ##### Geocode

  ### Create blank lat/lon columns
  data$lon <- NA
  data$lat <- NA

  ### Loop through the number of geocoding iterations
  for(i in seq(1, n_use)) {

    ### Keep only observations missing latlons
    data_still_missing <- data[which(is.na(data$lat) & is.na(data$lon)),]

    ### Keep only the location variable
    data_still_missing <- data_still_missing[,location, drop = FALSE]

    ### Geocode the addresses
    data_still_missing[[paste0("latlons", i)]] <- suppressMessages(suppressWarnings(ggmap::geocode(data_still_missing[[location]])))

    ### Rename
    data_still_missing$lon <- data_still_missing[[paste0("latlons", i)]][["lon"]]
    data_still_missing$lat <- data_still_missing[[paste0("latlons", i)]][["lat"]]

    ### Keep only the location and latlons variables
    data_still_missing <- data_still_missing[,c(location, "lon", "lat")]

    ### Combine newly geocoded data with pre-existing data
    data <- merge(data, data_still_missing, by = location, all = TRUE)


    ### Updated NA latlons with non-NA latlons if they're present
    data$lon = ifelse(
      is.na(data$lon.x),
      as.numeric(data$lon.y),
      data$lon.x
    )
    data$lat = ifelse(
      is.na(data$lat.x),
      as.numeric(data$lat.y),
      data$lat.x
    )

    ### Drop the merging latlon variables
    data <- data[,-which(names(data) %in% c("lon.x", "lon.y", "lat.x", "lat.y"))]


    ### Stop the loop if all locations are geocoded
    if(all(!is.na(data$lat)) & all(!is.na(data$lon))) {
      break
    }
  }

  ### Warn if looping cycle did not geocode all locations
  if(any(is.na(data$lat))) {
    warning("NA columns produced. Try using a higher value of `n`")
  }

  ### If data had duplicate locations, merge 1:m back onto original dataset so
  ### that no observations are missing, also retaining original order
    data <- data[,c(location, "lat", "lon")]
    data <- merge(data0, data, by = location, all = TRUE, sort = FALSE)
    data <- data[order(data$order),]
    data <- data[,-which(names(data) == "order")]

  ### Return new dataset
  if(return_full_dataframe == FALSE) {
    data[,c("lon", "lat")]
  } else {
    data
  }
}
