// HMR setup
if (module.hot) {
  module.hot.accept(function () {
      console.log('Reloaded, running main again');
      document.querySelector("#app").innerHTML = "";
  });
}

fetch("sus.analysis.json")
    .then(resp => resp.text())
    .then(json => require("../output/Main/index.js").mainJSON("#app")(json)());
