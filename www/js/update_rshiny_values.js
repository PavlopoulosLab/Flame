let newListNames = [];

const updateRenamedListNames = () => {
  Shiny.setInputValue("js_listNames", newListNames);
  newListNames = [];
  return true;
};



/*Select / Deselect all table checkboxes for the textmining input table of extracted genes*/
function textmining_selectAll(chk) {
    var  tx = document.getElementsByName('text_mining_result_table[]');
    for(var i=0; i < tx.length; i +=1) {
      if(chk == true) {
        tx[i].checked = true;
      }
      else {
        tx[i].checked = false;
      }
    }
}

function textmining_getAllSelected() {
     var  tx = document.getElementsByName('text_mining_result_table[]');
        var values = [];
    for(var i=0; i < tx.length; i +=1) {
      if (tx[i].checked == true) {
        values+= tx[i].value + ',';
      }
    }
    Shiny.setInputValue('textmining_selected', values, {priority: "event"});
    return true;
}

//