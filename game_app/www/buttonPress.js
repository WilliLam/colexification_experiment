var start = null;
var wordChoice = null;
var mouseUp = false;
var progress = 0;
var wordLen = null;
document.addEventListener('mousedown',function(e){
  console.log("mouse down");
  start = new Date();
  try{
    if(e.target && ((e.target.tagName = "LABEL" && e.target.parentNode.parentNode.childNodes.length == 15) || (e.target.tagName="SPAN" && e.target.parentNode.parentNode.parentNode.childNodes.length == 15)))  {
      indicatorDiv = document.getElementById('indicator');
      indicatorDiv.style.backgroundColor = "yellow";  
      console.log(progress);
      if (progress == 0) {
        
          progress = 1;
          //var elem = document.getElementById("myBar");
          var width = 1;
          wordLen = e.target.textContent.replace(/\s+/g, '').length;
          wordChoice = e.target.textContent.replace(/\s+/g, '');
          var id = setInterval(frame, timeToHold(wordLen));
          function frame() {
            if (indicatorDiv.style.backgroundColor == "red") {
              clearInterval(id);
              progress = 0;
            }
            if (width >= 100) {
              if (indicatorDiv.style.backgroundColor == "yellow") {
                indicatorDiv.style.backgroundColor = "green";    
              }
              
              clearInterval(id);
              progress = 0;
            } else {
              width++;
              indicatorDiv.style.width = width + "%";
            }
          }
        }
      if (indicatorDiv.style.backgroundColor == "red") {
                indicatorDiv.style.backgroundColor = "black";  
      }
      
          //start = new Date();
          //wordChoice = e.target.textContent.replace(/\s+/g, '');
          //var wordLen = e.target.textContent.replace(/\s+/g, '').length;
          
          

    }
    }
    catch(TypeError) {
      console.log(TypeError)
    }
})

function timeToHold(wordLen) {
  return wordLen**1.5
}
document.addEventListener('mouseup', function(e) {
          try {
            console.debug("parent nodes")
            console.debug(e.target && e.target.parentNode.childNodes);
            console.debug(e.target && e.target.parentNode.parentNode.childNodes);

          if(start != null ) {
            
            console.log("success mouseUp")
          end = new Date();
          /*console.log(e.target.textContent);*/
          console.log(start)
          console.log(end)
          //var wordLen = e.target.textContent.replace(/\s+/g, '').length;
          delta = (end.getTime() - start.getTime());
          console.info("delta", delta);
          console.log(wordChoice)
          console.log(timeToHold(wordLen)*100)
          if (delta >= timeToHold(wordLen)*100 ) {
              console.log("changed");
              
              // priority event - for observing event, even if value is not changed.
              Shiny.setInputValue("signalChoice", wordChoice, {priority: "event"});
              //alert("transmitted");
          }
          else {
            indicatorDiv = document.getElementById('indicator');
            console.log("too quick/changed selection")
            indicatorDiv.style.backgroundColor = "red";
          }
          }}
          catch(TypeError) {
            console.debug("oh dear")
          }
          finally {
            try {
            progress = 0;
            indicatorDiv = document.getElementById('indicator');
            indicatorDiv.style.width = "0%";
            if (indicatorDiv.style.backgroundColor == "green") {
              
              indicatorDiv.style.backgroundColor = "black";
            } else{
              indicatorDiv.style.backgroundColor = "red";
            }
            }
            catch(TypeError) {
              
            }
          }
          })


