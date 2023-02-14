/*JavaScript code for the show_popup functionality in
the tagged documents part of DARLING.

Adapted from the original EXTRACT javascript code (https://extract.jensenlab.org/scripts/extract.js)



The original EXTRACT's license follows: 
 * (c) Hellenic Center for Marine Research, 2015
 * 
 * Licensed under the The BSD 2-Clause License; you may not
 * use this file except in compliance with the License.
 * You may obtain a copy of the license at
 * 
 * http://opensource.org/licenses/BSD-2-Clause
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.



*/

/*Global variables*/
var cpr_tagger_url = "https://tagger.jensenlab.org/";
var extract_popup_mouseover_waiting_period = 1000;  
var extract_popup_mouseover_timer = 1000; // timer to handle this delay
var debug = false;
var entities = "-1+-2+-3+-21+-22+-23+-25+-26+-27+-36";
var species = 9606;
var auto_detect ="&auto_detect=1";

 
 


/*
Helper functions
*/

function updateSpecies(taxid) {
  species = taxid;
}

function startReflectPopupTimer(){
    var tag_matched_text = arguments[1];
    extract_popup_mouseover_timer = setTimeout( function (){
            extract_show_popup ( tag_matched_text );
        },  extract_popup_mouseover_waiting_period );
}

function stopReflectPopupTimer(){
    clearTimeout( extract_popup_mouseover_timer  );
}

function showReflectPopup() {
    var tag_matched_text = arguments[1];
    extract_show_popup ( tag_matched_text ); 
}

function extract_clean_up() {
    if (document.getElementById("extract_js_script") ) { document.getElementById("extract_js_script").remove() };
}

function extract_openInNewTab( url ) {
   var win = window.open(url, '_blank');
   win.focus();
}


/*
The main show_popup function
*/
function extract_show_popup( user_selected_text ) {

    var selected_text = arguments[0];
    var extract_body = document.getElementsByTagName('body')[0];
    var entity_types = entities + "+" + species + auto_detect;
    //get the source page url
    var source_page_uri = encodeURIComponent ( window.location.href );
    
    
    if ( debug ) {   console.log( "Selected text for curation: " + selected_text ); }
    if ( debug ) {   console.log( "Source page is: " + decodeURIComponent ( source_page_uri )); }
    if ( debug ) {   console.log( "Entity types are: " + entity_types ); }
    
    
    // house keeping: close any previous instances of the popup
    if ( document.getElementById("extract_popup") != null) {
       document.getElementById("extract_popup").remove();
    }
    

    // create popup elements
    ////////////////////////
    
    // popup outermost container
    var extract_popup_div = document.createElement('div');
    extract_popup_div.setAttribute ( "id","extract_popup");
    extract_popup_div.setAttribute ( "class","extract_popup");
    
    //popup header container
    var extract_popup_header_div = document.createElement('div');
    extract_popup_header_div.setAttribute ( "id", "extract_popup_header");
    extract_popup_header_div.setAttribute ( "class", "extract_popup_header");
    
    // Open popup in a new page
    var pop_out_anchor = document.createElement('a');
    pop_out_anchor.setAttribute ( "id", "pop_out_anchor");
    var new_tab_url = cpr_tagger_url + "Extract?document=" + encodeURIComponent( selected_text ) + "&entity_types=" + entity_types + "&uri=" + source_page_uri ;
    pop_out_anchor.setAttribute ( "onclick", "extract_openInNewTab( '"+new_tab_url+"');" );
    pop_out_anchor.setAttribute ( "title", "Open Popup in New Tab");
    pop_out_anchor.innerHTML = "&#8679;"; //unicode for the UP pointing arrow like the shift button
    
    
    // Close popup
    var close_anchor = document.createElement('a');
    close_anchor.setAttribute ( "id", "close_anchor");
    close_anchor.setAttribute ( "onclick", "var extract_popup_elem = document.getElementById('extract_popup'); extract_popup_elem.parentNode.removeChild(extract_popup_elem);" );
    close_anchor.setAttribute ( "title", "Close Popup");
    close_anchor.innerHTML =  "x"; 
    
    
    
    //popup header title
    var extract_header_title_div = document.createElement('div');
    extract_header_title_div.setAttribute ( "id","extract_header_title");
    extract_header_title_div.innerHTML = "DETAILS";
    
    //popup iframe
    var extract_iframe         = document.createElement('iframe');
    extract_iframe.setAttribute ( "id", "extract_iframe");
    extract_iframe.setAttribute ( "class", "extract_iframe");
    var iframe_url = cpr_tagger_url + "ExtractPopup?document=" + encodeURIComponent( selected_text ) + "&entity_types=" + entity_types + "&uri=" + source_page_uri ;
    extract_iframe.setAttribute ( "src", iframe_url);
    
    
    // Build the popup element hierarchy
    ///////////////////////////////////
    
    //popup header coomponents in popup header
    extract_popup_header_div.appendChild( extract_header_title_div );
    extract_popup_header_div.appendChild( pop_out_anchor );
    extract_popup_header_div.appendChild( close_anchor );
    
    // popup header and iframe in popup
    extract_popup_div.appendChild( extract_popup_header_div );
    extract_popup_div.appendChild( extract_iframe );
    
    // popup in the page (enable draggability too)
    extract_body.appendChild ( extract_popup_div );
    //add the ESC key pressed event hanlder to close the popup
    document.addEventListener ("keyup", function ( event ) { extract_handle_key_up ( event ) }, true);
    
    
    
    //House keeping: clean up javascripts
    extract_clean_up();
}//end of extract_show_popup




