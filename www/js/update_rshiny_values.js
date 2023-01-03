let newListNames = [];

const updateRenamedListNames = () => {
  Shiny.setInputValue("js_listNames", newListNames);
  newListNames = [];
  return true;
};
