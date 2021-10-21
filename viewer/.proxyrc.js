// const serveStatic = require('serve-static');

// module.exports = function (app) {
//     // Use static middleware
//     app.use("/static", serveStatic('dev/static'));
// };

const express = require('express');
const path = require('path');

module.exports = function (app) {
    app.use(express.static(path.join(__dirname, 'dev/static')));
};
