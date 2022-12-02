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
