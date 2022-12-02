//GLOBAL STATIC VARIABLES
var array_el = ["upsetjs", "contents", "sources_panel_gprofiler", "manhattan", "gconvert_table", "gorth_table", "heatmap1", 
                "scatterPlot", "barplot", "network1", "heatmap2", "network2","sources_panel_aGoTool", "literatureTable",
                "aGoScatterPlot", "aGoBarplot", "aGoHeatmapPlot", "aGoHeatmapPlot2", "aGoNetwork", "aGoNetwork2",
                "literatureScatterPlot", "literatureBarplot", "literatureHeatmapPlot", "literatureHeatmapPlot2", "literatureNetwork",                                                           "literatureNetwork2", "network3","aGoNetwork3","literatureNetwork3", "string_viewer"],
    array_load = ["upset_loader", "data_view_loader", "gprofiler_loader","manhattan_loader", "gconvert_table_loader", "gorth_table_loader",                                     "heatmap_loader","scatterPlot_loader", "barplot_loader", "network1_loader", "heatmap2_loader", "network2_loader",
                  "aGootool_loader", "literature_loader", "scatterPlotaGo_loader", "barplotaGo_loader", "heatmapaGo_loader",
                  "heatmapaGo2_loader", "networkaGo_loader", "networkaGo2_loader", "scatterPlotLiterature_loader","barplotLiterature_loader",
                  "heatmapLiterature_loader", "heatmapLiterature2_loader", "networkliterature_loader","networkliterature2_loader", "network3_loader"];

function hideLoadingBars(){
  var ldBars = document.getElementsByClassName("ldBar");
  for (var i=0; i<ldBars.length; i++){
    ldBars[i].style.display = "none";
  }
  
  return(true);
}

document.addEventListener('DOMContentLoaded', function () {
  //done = hideLoadingBars();
  
  return(true);
});
