//----------------------------------------------------------------------------------------------------
shinyjs.installTrenaVizReturnKeyHandlers = function(){
  document.getElementById('textInput_offListGene').onkeypress = function(e){
    if (!e) e = window.event;
    console.log("key  entered in textInput_offListGene");
    var keyCode = e.keyCode || e.which;
    if (keyCode == '13'){
      console.log(" keycode 13 found in textInput_offListGene");
      Shiny.onInputChange('textInput_offListGene_widget_returnKey', Math.random());
      return true;
      }
    return true;
  }
} // installTrenaVizReturnKeyHandlers
//----------------------------------------------------------------------------------------------------
