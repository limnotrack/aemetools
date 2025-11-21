#' Encode a parameter name with its group and index.
#' @param group The group name.
#' @param name The parameter name.
#' @param index The index of the parameter.
#' @return A string in the format "group/name\[index]".
#' @noRd
encode_param <- function(group, name, index) {
  paste0(group, "/", name, "[", index, "]")
}

#' Decode a full parameter name to extract the base name.
#' @param name_full The full parameter name in the format "group/name\[index]".
#' @return The base parameter name without group and index.
#' @noRd
decode_param <- function(name_full) {
  sub("^[^/]*/(.*)\\[.*\\]$", "\\1", name_full)
}

#' Decode a full parameter name into group, name, and index.
#' @param name_full The full parameter name in the format
#' "group/.../name\[index]".
#' @return A data.frame with columns: group, name, index.
#' @noRd
decode_param_full <- function(name_full) {
  
  # Extract group = everything before the last "/" occurrence
  group <- sub("/.*$", "", name_full)
  group <- ifelse(group == "NA", NA_character_, group)
  
  # Extract name = the part after the last "/"
  name  <- sub("^[^/]*/(.*)\\[.*\\]$", "\\1", name_full)
  
  # Extract index = inside [...]
  raw_index <- sub(".*\\[(.*)\\]$", "\\1", name_full)
  
  # Convert "NA" to real NA, keep numbers
  index <- suppressWarnings(as.integer(raw_index))
  index[raw_index == "NA"] <- NA_integer_
  
  data.frame(
    group = group,
    name = name,
    index = index,
    stringsAsFactors = FALSE
  )
}
