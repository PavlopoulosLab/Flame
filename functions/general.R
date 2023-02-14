mapRange <- function(numericList, newMin, newMax) {
  oldRange <- max(numericList) - min(numericList)
  if (oldRange != 0)
    numericList <- (((numericList - min(numericList)) * (newMax - newMin)) / oldRange) + newMin
  else
    numericList <- rep(
      (EDGE_WIDTH_MIN - EDGE_WIDTH_MIN) / 2,
      length(numericList)
    )
  return(numericList)
}

isPOSTResponseValid <- function(request) {
  isValid <- T
  if (request$status_code != 200) {
    isValid <- F
    renderWarning("Invalid response from the called API.
                  Please try again in a while.")
  } else if (identical(request$content, raw(0)))
    isValid <- F
  return(isValid)
}
