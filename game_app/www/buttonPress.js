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
              Shiny.setInputValue("wordChoice", wordChoice);
              alert("transmitted");
          }
          else {
            alert("press longer");
          }
          }}
          catch(TypeError) {
            
          }
          })


