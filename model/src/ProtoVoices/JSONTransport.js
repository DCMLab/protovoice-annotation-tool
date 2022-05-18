"use strict";

export function unsafeStringifyPretty(json) {
    return JSON.stringify(json, null, 2);
}
