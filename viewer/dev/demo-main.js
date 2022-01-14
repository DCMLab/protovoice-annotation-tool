const viewer = require("../output/Main/index.js");

function selectFile() {
    const file = this.files[0];
    // clear target div
    document.getElement.ById("widget").innerHTML = "";
    // load file and create viewer widget
    file.text().then(json => viewer.createViewer("#widget", json));
}

document
    .getElementById("file-upload")
    .addEventListener("change", selectFile, false);
