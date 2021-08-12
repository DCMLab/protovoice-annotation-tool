"use strict";

exports.unsafeStringifyPretty = function(json) {
    return JSON.stringify(json, null, 2);
};
