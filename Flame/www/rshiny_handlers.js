// This function creates an alert message for the user
// @param messsage: Error message to be printed
// @return true
function shinyAlert(message){
  alert(message);
  return true;
}

// This function updates a global variable (file_names) in R
// @param message: array of selected file names
// @return true
function shinyRenameFiles(message){
  var i;
  if (typeof(message) == "object"){
    for (i=0; i<message.length; i++){
      newNames[i] = prompt("Rename file: ", message[i]);
    }
  } else newNames = prompt("Rename file: ", message);
  updateFileNames(newNames);
  return true;
}

function disableSourcesTabs(m){
  var i,
      navbar_li_children = document.getElementById("sources_panel").children;
  for (i = 0; i < navbar_li_children.length; i++){ //leaving conductance and help out
    navbar_li_children[i].style.pointerEvents = "none";
    navbar_li_children[i].style.opacity = 0.5;
  }
  return true;
}

// @param i: the position of the tab to be enabled
function enableSourceTab(i){
  navbar_li_children = document.getElementById("sources_panel").children[i];
  navbar_li_children.style.pointerEvents = "all";
  navbar_li_children.style.opacity = 1;
  return true;
}

function disableSourcesTabsaGoTool(m){
  var i,
      navbar_li_children = document.getElementById("sources_panel_aGoTool").children;
  for (i = 0; i < navbar_li_children.length; i++){ //leaving conductance and help out
    navbar_li_children[i].style.pointerEvents = "none";
    navbar_li_children[i].style.opacity = 0.5;
  }
  return true;
}

// @param i: the position of the tab to be enabled
function enableSourceTabaGoTool(i){
  navbar_li_children = document.getElementById("sources_panel_aGoTool").children[i];
  navbar_li_children.style.pointerEvents = "all";
  navbar_li_children.style.opacity = 1;
  return true;
}

//@param m[0]: position of loader and relevant div in available global loaders array
//@param m[1]: percentage to fill loader
function startLoader(m){
  var element = document.getElementById(array_el[m[0]]),
      loader = document.getElementsByClassName("ldBar")[0];
      //loader = document.getElementById(array_load[m[0]]);
  element.style.opacity = 0.5;
  loader.style.display = "inline-block";
  var bar = loader.ldBar;
  bar.set(m[1]);
  return bar;
}

//@param m: position of loader and relevant div in available global loaders array
function finishLoader(m){
  var element = document.getElementById(array_el[m]),
      loader = document.getElementsByClassName("ldBar")[0];
      //loader = document.getElementById(array_load[m]);
  var bar = loader.ldBar;
  bar.set(0);
  element.style.opacity = 1;
  loader.style.display = "none";
  return bar;
}

/*function startLoader_old(m){ //deprectaed
  var array_el = ["upsetjs", "contents", "sources_panel", "manhattan", "gconvert_table", "gorth_table", "heatmap", "scatterPlot", "barplot", "network",                    "heatmap2", "network2","sources_panel_aGoTool", "literatureTable", "aGoScatterPlot", "aGoBarplot", "aGoHeatmapPlot", "aGoHeatmapPlot2", "aGoNetwork", "aGoNetwork2",
  "literatureScatterPlot", "literatureBarplot", "literatureHeatmapPlot", "literatureHeatmapPlot2", "literatureNetwork", "literatureNetwork2"],
      
      array_load = ["upset_loader", "data_view_loader", "gprofiler_loader","manhattan_loader", "gconvert_table_loader", "gorth_table_loader","heatmap_loader","scatterPlot_loader", "barplot_loader", "network_loader", "heatmap2_loader", "network2_loader", "aGootool_loader", "literature_loader", "scatterPlotaGo_loader",
      "barplotaGo_loader", "heatmapaGo_loader", "heatmapaGo2_loader", "networkaGo_loader", "networkaGo2_loader", "scatterPlotLiterature_loader","barplotLiterature_loader",
      "heatmapLiterature_loader", "heatmapLiterature2_loader", "networkliterature_loader","networkliterature2_loader"],
      element = document.getElementById(array_el[m]),
      loader = document.getElementById(array_load[m]);
  element.style.opacity = 0.5;
  loader.style.display = "inline-block";
  return true;
}

function finishLoader_old(m){ //deprectaed
  var array_el = ["upsetjs", "contents", "sources_panel","manhattan","gconvert_table","gorth_table", "heatmap","scatterPlot", "barplot",       "network","heatmap2","network2","sources_panel_aGoTool", "literatureTable", "aGoScatterPlot", "aGoBarplot", "aGoHeatmapPlot", "aGoHeatmapPlot2", "aGoNetwork", "aGoNetwork2",
   "literatureScatterPlot", "literatureBarplot", "literatureHeatmapPlot", "literatureHeatmapPlot2", "literatureNetwork", "literatureNetwork2"],
      
      array_load = ["upset_loader", "data_view_loader", "gprofiler_loader","manhattan_loader", "gconvert_table_loader","gorth_table_loader", "heatmap_loader","scatterPlot_loader", "barplot_loader", "network_loader", "heatmap2_loader", "network2_loader","aGootool_loader", "literature_loader", "scatterPlotaGo_loader",
      "barplotaGo_loader", "heatmapaGo_loader", "heatmapaGo2_loader", "networkaGo_loader", "networkaGo2_loader", "scatterPlotLiterature_loader","barplotLiterature_loader",
      "heatmapLiterature_loader", "heatmapLiterature2_loader", "networkliterature_loader","networkliterature2_loader"],
      element = document.getElementById(array_el[m]),
      loader = document.getElementById(array_load[m]);
  element.style.opacity = 1;
  loader.style.display = "none";
  return true;
}*/

function disableAllButtons(m){
  var i,
      buttons = document.getElementsByClassName("btn");
  for (i=0; i<buttons.length; i++){
    buttons[i].style.display = "none";
  }
  return true;
}

function enableAllButtons(m){
  var i,
      buttons = document.getElementsByClassName("btn");
  for (i=0; i<buttons.length; i++){
    buttons[i].style.display = "inline-block";
  }
  return true;
}

Shiny.addCustomMessageHandler("handler_alert", shinyAlert);
Shiny.addCustomMessageHandler("handler_rename", shinyRenameFiles);
Shiny.addCustomMessageHandler("handler_disableSourcesTabs", disableSourcesTabs);
Shiny.addCustomMessageHandler("handler_enableSourceTab", enableSourceTab);
Shiny.addCustomMessageHandler("handler_disableSourcesTabsaGoTool", disableSourcesTabsaGoTool);
Shiny.addCustomMessageHandler("handler_enableSourceTabaGoTool", enableSourceTabaGoTool);
Shiny.addCustomMessageHandler("handler_startLoader", startLoader);
Shiny.addCustomMessageHandler("handler_finishLoader", finishLoader);
Shiny.addCustomMessageHandler("handler_disableAllButtons", disableAllButtons);
Shiny.addCustomMessageHandler("handler_enableAllButtons", enableAllButtons);
