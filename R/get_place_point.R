#' get_place_point
#' 
#' Queries OpenStreetMap to find the geographic point for a specified place
#'
#' @param place_name string containing name of the desired place
#' @param place_name_detail optional additional identifying string. Usually used to specify country.
#' @param place_value required string designating the place value, e.g. city, country, town. See: https://wiki.openstreetmap.org/wiki/Key:place
#' @param closest_match if there are multiple results, computes Levenshtein distance and filters to just the closest match. 
#' @param filter_extra_cols default TRUE. OpenStreetMap nodes contain a variety of additional data such as names in different languages. `filter_extra_cols` filters these to just osm_id, name, english name, and place value. 
#' @param ignore_multiple_output_warnings ignores warnings for multiple results if the `place_name` and `place_extra_detail` arguments return more than one valid point. 
#'
#' @returns "sf" "data.frame"
#' 
#' @export
#'
#' @import sf
#' @import osmdata
#' 
#' @examples
#' ## Not run:
#' 
#' # Multiple results, Sydney, Australia & Sydney, Canada
#' get_place_point("Sydney", place_value = "city")
#' 
#' # Since these are in different countries we can narrow down using the `place_extra_detail` argument: 
#' get_place_point("Sydney", "Australia", place_value = "city")
#' 
#' 
#' # Multiple results, Taipei, Taiwan & New Taipei, Taiwan
#' get_place_point("Taipei", place_value = "city")
#' 
#' # Since these are in the same country we should use `closest_match = TRUE` to match with only 'Taipei'
#' get_place_point("Taipei", place_value = "city", closest_match = TRUE)
#' # If 'New Taipei' is desired then just directly search for it: 
#' get_place_point("New Taipei", place_value = "city")
#' 
#' ## End(Not run)

get_place_point <- function(place_name, place_name_detail = NULL, place_value, closest_match = FALSE, filter_extra_cols = TRUE, ignore_multiple_output_warnings = FALSE) {

  # Various catches for invalid inputs
  if (missing(place_name)){
    stop('argument "place_name" is missing, with no default\n', "Please provide a place name to search for.")
  }
  
  if (missing(place_value)){
    stop('argument "place_value" is missing, with no default\n', 'Please provide a place type / value for the desired place_name.\n E.g. for `place_name = "Australia"` you would probably want `place_value = "country"`.\nSee https://wiki.openstreetmap.org/wiki/Map_features#Place for valid options.')
  }
  
  # Allow optional additional detail argument
  # Intention is for arguments such as place_name = "Sydney" followed by place_name_detail = "Australia" to exclude Sydney, Canada. 
  if (is.null(place_name_detail)) {
    prompt <- place_name
  } else {
    prompt <- paste(place_name, place_name_detail)
  }
  
  # Pull from osm
  # Unfortunately this gives us the administrative boundaries, we want a point
  osm_bb <- osmdata::getbb(prompt, format_out = "sf_polygon", featuretype = place_value)
  
  # Use the bbox of the above call to look for the polygon for each administrative boundary
  osm_place_query <- osmdata::opq(bbox = osm_bb, timeout = 120) |>
    # Only pull the place type we want
    osmdata::add_osm_feature(key = "place", value = place_value)
  
  # Overpass query 
  # Calls boundary but also the defined 'point'. 
  osm_place_result <- osmdata::osmdata_sf(osm_place_query)
  
  # Ignore boundary polygon points
  osm_place <- subset(osm_place_result$osm_points, place == place_value)
  
  if (isTRUE(closest_match)){
    
    # Calculate Levenshtein distances from original query for each entry
    osm_place <- osm_place |> 
      # Separate entries
      split(osm_place$osm_id) |>
      # Perform match on each entry
      lapply(
        # Matching function
        # Uses both English and native language to match
        # Support for searching in other languages not implemented
        FUN = function(df, match = prompt) {
          df$fuzzy_match_og <- adist(df$name, match)[, 1]
          df$fuzzy_match_en <- adist(df$`name:en`, match)[, 1]
          df$fuzzy_match <- pmin(df$fuzzy_match_og, df$fuzzy_match_en, na.rm = TRUE)
          return(df)
        }
      ) |>
      # Puts the split dataset back together again
      do.call(rbind, args = _)
    
    # Filter on closest match
    # lowest value
    fuzzy_min <- osm_place$fuzzy_match |> min()
    # filter
    osm_place <- subset(osm_place, fuzzy_match == fuzzy_min)
  }
  
  if (isTRUE(filter_extra_cols)) {
    osm_place <- osm_place[, c("osm_id", "name", "name:en", "place")]
  }
  
  # Give warning if multiple outputs provided
  if (nrow(osm_place) > 1){
    warning(paste0("More than one ", place_value," found for '", place_name,"'."), immediate. = !ignore_multiple_output_warnings)
  }
  
  # Give advice on best ways to resolve multiple outputs depending on arguments used.
  
  if (nrow(osm_place) > 1 & is.null(place_name_detail)){
    message(paste0('Please be more specific by using the `place_name_detail` argument if multiple outputs are not desired.\n E.g. using `place_name = "Sydney", place_name_detail = "Australia", place_value = "city"` instead of just `place_name = Sydney` to not capture "Sydney, Canada".'))
  }
  
  if (nrow(osm_place) > 1 & isTRUE(closest_match) & is.null(place_name_detail)) {
    message(paste0('Setting `closest_match = T` without first specifying `place_name_detail` is not recommended'))
  }
  
  if (nrow(osm_place) > 1 & !isTRUE(closest_match) & !is.null(place_name_detail)) {
    message(paste0('Refining the `place_name_detail` argument or using `closest_match = T` is recommended if multiple outputs are not desired. Otherwise manual filtering is likely needed.'))
  }
  
  return(osm_place)
}
