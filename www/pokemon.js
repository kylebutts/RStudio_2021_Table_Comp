// Event-listener for pokemon image to make adaptiveBackground run 
// Initialize
$(document).ready(function(){
  $.adaptiveBackground.run({parent: "#pokemon_color"});
});

// ensures this works for some older browsers
MutationObserver = window.MutationObserver || window.WebKitMutationObserver || window.MozMutationObserver;

new MutationObserver(function(){
    $.adaptiveBackground.run({parent: "#pokemon_color"});
}).observe(
    document.querySelector('#pokemon_highlight img'), 
    {attributes: true, attributeFilter: ["src"]}
)


