shinyjs.pageRed = function(){$('#bindingSitesManagerPageContent > div:nth-child(1) > div > h3').css('background', '#FF00FF');}
//----------------------------------------------------------------------------------------------------
shinyjs.installReturnKeyHandlers = function(){
  document.getElementById('textInput_exploreAnotherTF').onkeypress = function(e){
    if (!e) e = window.event;
    //console.log("key  entered in textInput_exploreAnotherTF");
    var keyCode = e.keyCode || e.which;
    if (keyCode == '13'){
      console.log(" keycode 13 found in textInput_exploreAnotherTF");
      Shiny.onInputChange('textInput_exploreAnotherTF_widget_returnKey', Math.random());
      return true;
      }
    return true;
  }
} // installReturnKeyHandlers
//----------------------------------------------------------------------------------------------------
shinyjs.clear_textInput_exploreAnotherTF = function(){

    console.log("shinyjs.sclear_textInput_exploreAnotherTF");

   $("#textInput_exploreAnotherTF").val("");

} // setBindingSitesManagerPageTitle
//----------------------------------------------------------------------------------------------------
shinyjs.setBindingSitesManagerPageTitle = function(newTranscriptionFactor){

   console.log("shinyjs.setBindingSitesManagerPageTitle: " + newTranscriptionFactor);

   $("#bindingSitesManager_currentTF").text("TF: " + newTranscriptionFactor);

} // setBindingSitesManagerPageTitle
//----------------------------------------------------------------------------------------------------

//$(document).ready(function() {
//console.log("--- defining onkeypress function for textInput_exploreAnotherTF")
//
//  document.getElementById('textInput_exploreAnotherTF').onkeypress = function(e){
//    if (!e) e = window.event;
//    console.log("key  entered in textInput_exploreAnotherTF");
//    var keyCode = e.keyCode || e.which;
//    if (keyCode == '13'){
//      console.log(" keycode 13 found in textInput_exploreAnotherTF");
//      Shiny.onInputChange('textInput_exploreAnotherTF_widget_returnKey', Math.random());
//      return true;
//      }
//    return true;
//  }
//});

