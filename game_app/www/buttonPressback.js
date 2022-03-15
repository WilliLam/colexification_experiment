var start;
var wordChoice = null;

document.addEventListener('mousedown',function(e){
    
    try{
      console.log(e.target.parentNode.parentNode.classList.contains("radio"));
    if(e.target && e.target.parentNode.parentNode.classList.contains("radio")){
      console.log("ok")
      e.target.addEventListener("mousedown", function(){
          start = new Date();
      });
    }
    }
    catch(TypeError) {
      
    }
})

document.addEventListener('mouseup', function(e) {
          try {
          if(e.target && e.target.parentNode.parentNode.classList.contains("radio")) {
                        end = new Date();
          delta = end - start;
          console.log(e.target.textContent);
          
          var wordLen = e.target.textContent.replace(/\s+/g, '').length;
          console.log(wordLen)
          if (delta >= wordLen*100) {
              wordChoice = e.target.textContent.replace(/\s+/g, '');
              Shiny.setInputValue("wordchoice", wordChoice);
              alert("transmitted");
          }
          else {
            alert("press longer");
          }
          }}
          catch(TypeError) {
            
          }
          })



  
  (function(window, document){
    'use strict';
    var delta;
    var start;
    var end;
    
    var button = document.getElementsByClassName("radio");
    console.log(button.innerContent)
    for (var i = 0; i < button.length; i++) {
      var wordLen = button[i].textContent.replace(/\s+/g, '').length;
      console.log(button[i].textContent.replace(/\s+/g, '').length);
      button[i].addEventListener("mousedown", function(){
          start = new Date();
      });

      button[i].addEventListener("mouseup", function() {
          end = new Date();
          delta = end - start;
          console.log(delta);
          console.log(wordLen)
          if (delta >= wordLen*100) {
              alert("transmitted");
          }
          else {
            alert("press longer");
          }
      });
    }
    
})(window, document);
