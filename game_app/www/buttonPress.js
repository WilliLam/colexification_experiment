var start = null;
var wordChoice = null;
var mouseUp = false;
var progress = 0;

var isBaselineStr;
var isBaseline;

window.onload = init;
function init(){ 
  
  console.log(isBaseline);
};

function timeToHold(wordLen) {
  //5x for greater effect
  if (wordLen < 5) {
    return 10;
  } else {
    return 50;
  }
  //return wordLen**1.6
}




//console.log("loading");
document.addEventListener('mousedown',function(e){
  var isBaselineStr = document.getElementsByClassName("costLength")[0].id;
  var isBaseline = (isBaselineStr == "TRUE");
  //console.log(isBaselineStr);
  console.log("mouse down");
  start = new Date();
  try{
    if(e.target && ((e.target.tagName = "LABEL" && e.target.parentNode.parentNode.childNodes.length == 15) || (e.target.tagName="SPAN" && e.target.parentNode.parentNode.parentNode.childNodes.length == 15)))  {
      indicatorDiv = document.getElementById('indicator');
      indicatorDiv.style.width = "0%";
      indicatorDiv.style.backgroundColor = "yellow";  
      //console.log(progress);
      if (progress == 0) {
          progress = 1;
          //var elem = document.getElementById("myBar");
          var width = 1;
          wordLen = e.target.textContent.replace(/\s+/g, '').length;
          wordChoice = e.target.textContent.replace(/\s+/g, '');
          
          if (isBaseline) {
            var id = setInterval(frame, 1);  
          } else{
            var id = setInterval(frame, timeToHold(wordLen));  
          }
          
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
              if (isBaseline) {
                width = 100;
                indicatorDiv.style.width = width + "%";
              }
              width++;
              indicatorDiv.style.width = width + "%";
            }
          }
        }
      if (indicatorDiv.style.backgroundColor == "red") {
                indicatorDiv.style.backgroundColor = "black";  
      }
    }
    }
    catch(TypeError) {
      console.log(TypeError)
    }
})


document.addEventListener('mouseup', function(e) {
          var isBaselineStr = document.getElementsByClassName("costLength")[0].id;
          var isBaseline = (isBaselineStr == "TRUE");
          try {
            console.debug("parent nodes")
            console.debug(e.target && e.target.parentNode.childNodes);
            console.debug(e.target && e.target.parentNode.parentNode.childNodes);

          if(start != null ) {
        
            //console.log("success mouseUp")
          end = new Date();
          /*console.log(e.target.textContent);*/
          //console.log(start)
          //console.log(end)
          //var wordLen = e.target.textContent.replace(/\s+/g, '').length;
          delta = (end.getTime() - start.getTime());
          var mouseUpChoice = e.target.textContent.replace(/\s+/g, '');
          console.info("delta", delta);
          console.log(wordChoice)
          console.log(timeToHold(wordLen)*100)
          if (isBaseline) {
            if (mouseUpChoice == wordChoice ) {
              Shiny.setInputValue("signalChoice", wordChoice, {priority: "event"});
              progress = 0;
              start = null;
            }
            else {
              
            }
          } 
          if (delta >= timeToHold(wordLen)*100 && mouseUpChoice == wordChoice ) {
              console.log("changed");
              
              // priority event - for observing event, even if value is not changed.
              Shiny.setInputValue("signalChoice", wordChoice, {priority: "event"});
              progress = 0;
              start = null;
              //indicatorDiv = document.getElementById('indicator');
              //indicatorDiv.style.width = "0%";
              //if (indicatorDiv.style.backgroundColor == "green") {
              //  indicatorDiv.style.backgroundColor = "black";
              }
          }
          else {
            indicatorDiv = document.getElementById('indicator');
            indicatorDiv.style.backgroundColor = "red";
          }
          }
          catch(TypeError) {
            //console.log(TypeError)
            //console.debug("oh dear")
          }
          finally {
            try {
              progress = 0;
              start = null;
              indicatorDiv = document.getElementById('indicator');
              //indicatorDiv.style.width = "0%";
              if (indicatorDiv.style.backgroundColor == "green") {
                indicatorDiv.style.backgroundColor = "black";
              } else if (indicatorDiv.style.backgroundColor == "yellow"){
                indicatorDiv.style.backgroundColor = "red";
              }
            }
            catch(TypeError) {
              
            }
          }
          })



