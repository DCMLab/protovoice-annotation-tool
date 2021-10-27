// HMR setup
if (module.hot) {
  module.hot.accept(function () {
      console.log('Reloaded, running main again');
      document.querySelector("#sus").innerHTML = "";
      document.querySelector("#bach").innerHTML = "";
  });
}

const viewer = require("../output/Main/index.js");

fetch("sus.analysis.json")
    .then(resp => resp.text())
    .then(json => viewer.createViewer("#sus", json));

fetch("bach.analysis.json")
    .then(resp => resp.text())
    .then(json => viewer.createViewer("#bach", json, {xscale: -1.7, showSettings: true}));
