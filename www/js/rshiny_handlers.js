let LISTNAME_NCHAR_LIMIT = 0;

const setListLimit = (limit) => {
  LISTNAME_NCHAR_LIMIT = limit;
  return true;
};

const shinyRenameLists = (selectedListNames) => {
  let i;
  if (typeof(selectedListNames) == "object") {
    for (i = 0; i < selectedListNames.length; i++) {
      newListNames[i] = parseNewListName(selectedListNames[i]);
    }
  } else
    newListNames = parseNewListName(selectedListNames);
  updateRenamedListNames(newListNames);
  return true;
};

const parseNewListName = (listName) => {
  return(
    prompt(
      "Rename list ".concat(listName).concat(" (").concat(
        LISTNAME_NCHAR_LIMIT).concat(" characters max):"),
      listName
    )
  )
};

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

Shiny.addCustomMessageHandler("handler_setListLimit", setListLimit);
Shiny.addCustomMessageHandler("handler_renameLists", shinyRenameLists);
Shiny.addCustomMessageHandler("handler_hideSourceTabs", hideSourceTabs);
Shiny.addCustomMessageHandler("handler_showSourceTab", showSourceTab);
Shiny.addCustomMessageHandler("handler_browseUrl", browseUrl);
