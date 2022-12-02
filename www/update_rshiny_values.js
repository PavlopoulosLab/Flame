//global JS variables
var newNames = []; //global array to contain user-defined file names

// Updates global file names at R, based on javascript global selected renamed files
// @return: true
function updateFileNames(){
  //console.log(newNames);
  Shiny.setInputValue("js_fileNames", newNames);
  newNames = [];
  return true;
}
