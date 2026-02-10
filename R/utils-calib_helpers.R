#' Encode a parameter name with its group and index.
#' @param group The group name.
#' @param name The parameter name.
#' @param index The index of the parameter.
#' @return A string in the format "group/name\[index]".
#' @export
encode_param <- function(group, name, index) {
  paste0(group, "/", name, "[", index, "]")
}

#' Display parameter information in a human-readable format.
#' @param group The group name (can be NA).
#' @param name The parameter name.
#' @param index The index of the parameter (can be NA).
display_param_name <- function(group, name, index) {
  grp <- ifelse(is.na(group), "", paste0(group, "/"))
  idx <- ifelse(is.na(index), "", paste0("[", index, "]"))
  paste0(grp, name, idx)
}

#' Decode a full parameter name to extract the base name.
#' @param name_full The full parameter name in the format "group/name\[index]".
#' @return The base parameter name without group and index.
#' @export
decode_param <- function(name_full) {
  sub("^[^/]*/(.*)\\[.*\\]$", "\\1", name_full)
}

#' Decode a full parameter name into group, name, and index.
#' @param name_full The full parameter name in the format
#' "group/.../name\[index]".
#' @return A data.frame with columns: group, name, index.
#' @export
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
