const viewer = require("../../output/Main/index.js");

function selectFile() {
    const file = this.files[0];
    file.text().then(json => viewer.createViewer("#widget", json));
}

document
    .getElementById("file-upload")
    .addEventListener("change", selectFile, false);
