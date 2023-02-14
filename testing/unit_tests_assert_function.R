assertEquals <- function(item1, item2) {
  tryCatch({
    if (isTRUE(all.equal(item1, item2))){
      testsPassed <<- testsPassed + 1
    } else {
      failTest()
    }
  }, error = function(e) {
    cat(sprintf("Error while testing: %s\n\n", e))
  })
}

failTest <- function() {
  testsFailed <<- testsFailed + 1
  callerFunctionName <- as.character(sys.call(-6))
  cat(sprintf("Test function %s failed.\n\n", callerFunctionName))
}

assertTrue <- function(condition) {
  tryCatch({
    if (condition){
      testsPassed <<- testsPassed + 1
    } else {
      failTest()
    }
  }, error = function(e) {
    cat(sprintf("Error while testing: %s\n\n", e))
  })
}

assertFalse <- function(condition) {
  tryCatch({
    if (!condition){
      testsPassed <<- testsPassed + 1
    } else {
      failTest()
    }
  }, error = function(e) {
    cat(sprintf("Error while testing: %s\n\n", e))
  })
}
