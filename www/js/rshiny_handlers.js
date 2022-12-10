function shinyRenameFiles(selectedFileNames){
  var i;
  if (typeof(selectedFileNames) == "object"){
    for (i=0; i<selectedFileNames.length; i++){
      newNames[i] = prompt("Rename file: ", selectedFileNames[i]);
    }
  } else newNames = prompt("Rename file: ", selectedFileNames);
  updateFileNames(newNames);
  return true;
}

const hideSourceTabs = (prefix) => {
  let i;
  const navbar_li_children = document.getElementById(prefix.concat("_sources_panel")).children;
  for (i = 0; i < navbar_li_children.length; i++){
    navbar_li_children[i].style.display = "none";
  }
  return true;
};

const showSourceTab = (args) => {
  const { prefix, tabPosition } = args,
   navbar_li_children = document.getElementById(prefix.concat("_sources_panel")).children[tabPosition];
  navbar_li_children.style.display = "inline-block";
  return true;
};

const browseUrl = url => {
  window.open(url, "_blank");
};

Shiny.addCustomMessageHandler("handler_rename", shinyRenameFiles);
Shiny.addCustomMessageHandler("handler_hideSourceTabs", hideSourceTabs);
Shiny.addCustomMessageHandler("handler_showSourceTab", showSourceTab);
Shiny.addCustomMessageHandler("handler_browseUrl", browseUrl);
