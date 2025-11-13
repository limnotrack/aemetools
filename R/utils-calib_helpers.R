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
