"use strict";

import jsDownload from "downloadjs";
import { WASI } from "@runno/wasi";

export const copyToClipboard = (str) => () =>
  navigator.clipboard.writeText(str);

export function download_(data) {
  return function (fileName) {
    return function (mimeType) {
      return function () {
        // the function returns true on success,
        // so we explicitly cast null to false, just in case
        return Boolean(jsDownload(data, fileName, mimeType));
      };
    };
  };
}

export const examplePieceJSON = [
  {
    time: "0.4.0",
    notes: [
      { pitch: "E4", id: "slice0.note0" },
      { pitch: "C4", id: "slice0.note1", hold: "slice1.note1" },
    ],
  },
  {
    time: "1.1.0",
    notes: [
      { pitch: "D4", id: "slice1.note0", hold: "slice2.note0" },
      { pitch: "C4", id: "slice1.note1" },
    ],
  },
  {
    time: "1.2.0",
    notes: [
      { pitch: "D4", id: "slice2.note0" },
      { pitch: "B3", id: "slice2.note1" },
    ],
  },
  { time: "1.3.0", notes: [{ pitch: "C4", id: "slice3.note0" }] },
];

export const examplePieceJSONLong = [
  {
    time: "0.4.3/4",
    notes: [{ id: "slice0.note0", pitch: "D5" }],
  },
  {
    time: "1.1.0",
    notes: [
      { id: "slice1.note2", pitch: "D5" },
      { hold: "slice2.note24", id: "slice1.note24", pitch: "D3" },
    ],
  },
  {
    time: "1.1.1/4",
    notes: [
      { id: "slice2.note3", pitch: "A4" },
      { hold: "slice3.note24", id: "slice2.note24", pitch: "D3" },
    ],
  },
  {
    time: "1.1.1/2",
    notes: [
      { id: "slice3.note4", pitch: "B♭4" },
      { hold: "slice4.note24", id: "slice3.note24", pitch: "D3" },
    ],
  },
  {
    time: "1.1.3/4",
    notes: [
      { id: "slice4.note5", pitch: "G4" },
      { hold: "slice5.note24", id: "slice4.note24", pitch: "D3" },
    ],
  },
  {
    time: "1.2.0",
    notes: [
      { id: "slice5.note6", pitch: "A4" },
      { hold: "slice6.note24", id: "slice5.note24", pitch: "D3" },
    ],
  },
  {
    time: "1.2.1/4",
    notes: [
      { id: "slice6.note7", pitch: "F4" },
      { hold: "slice7.note24", id: "slice6.note24", pitch: "D3" },
    ],
  },
  {
    time: "1.2.1/2",
    notes: [
      { id: "slice7.note8", pitch: "E4" },
      { hold: "slice8.note24", id: "slice7.note24", pitch: "D3" },
    ],
  },
  {
    time: "1.2.3/4",
    notes: [
      { id: "slice8.note9", pitch: "D4" },
      { hold: "slice9.note24", id: "slice8.note24", pitch: "D3" },
    ],
  },
  {
    time: "1.3.0",
    notes: [
      { hold: "slice10.note23", id: "slice9.note23", pitch: "C♯4" },
      { hold: "slice10.note24", id: "slice9.note24", pitch: "D3" },
    ],
  },
  {
    time: "1.3.1/4",
    notes: [
      { id: "slice10.note11", pitch: "B♭4" },
      { hold: "slice11.note20", id: "slice10.note20", pitch: "B♭4" },
      { hold: "slice11.note23", id: "slice10.note23", pitch: "C♯4" },
      { hold: "slice11.note24", id: "slice10.note24", pitch: "D3" },
    ],
  },
  {
    time: "1.3.1/2",
    notes: [
      { id: "slice11.note12", pitch: "E5" },
      { hold: "slice12.note20", id: "slice11.note20", pitch: "B♭4" },
      { hold: "slice12.note23", id: "slice11.note23", pitch: "C♯4" },
      { hold: "slice12.note24", id: "slice11.note24", pitch: "D3" },
    ],
  },
  {
    time: "1.3.3/4",
    notes: [
      { hold: "slice13.note13", id: "slice12.note13", pitch: "G5" },
      { hold: "slice13.note23", id: "slice12.note23", pitch: "C♯4" },
      { hold: "slice13.note24", id: "slice12.note24", pitch: "D3" },
      { id: "slice12.note20", pitch: "B♭4" },
    ],
  },
  {
    time: "1.4.0",
    notes: [
      { hold: "slice14.note21", id: "slice13.note21", pitch: "A4" },
      { hold: "slice14.note23", id: "slice13.note23", pitch: "C♯4" },
      { hold: "slice14.note24", id: "slice13.note24", pitch: "D3" },
      { id: "slice13.note13", pitch: "G5" },
    ],
  },
  {
    time: "1.4.1/4",
    notes: [
      { id: "slice14.note15", pitch: "G5" },
      { hold: "slice15.note21", id: "slice14.note21", pitch: "A4" },
      { hold: "slice15.note23", id: "slice14.note23", pitch: "C♯4" },
      { hold: "slice15.note24", id: "slice14.note24", pitch: "D3" },
    ],
  },
  {
    time: "1.4.1/2",
    notes: [
      { id: "slice15.note16", pitch: "F5" },
      { hold: "slice16.note21", id: "slice15.note21", pitch: "A4" },
      { hold: "slice16.note23", id: "slice15.note23", pitch: "C♯4" },
      { hold: "slice16.note24", id: "slice15.note24", pitch: "D3" },
    ],
  },
  {
    time: "1.4.3/4",
    notes: [
      { id: "slice16.note17", pitch: "E5" },
      { hold: "slice17.note21", id: "slice16.note21", pitch: "A4" },
      { hold: "slice17.note24", id: "slice16.note24", pitch: "D3" },
      { id: "slice16.note23", pitch: "C♯4" },
    ],
  },
  {
    time: "2.1.0",
    notes: [
      { id: "slice17.note25", pitch: "G5" },
      { hold: "slice18.note34", id: "slice17.note34", pitch: "D4" },
      { hold: "slice18.note21", id: "slice17.note21", pitch: "A4" },
      { hold: "slice18.note24", id: "slice17.note24", pitch: "D3" },
    ],
  },
  {
    time: "2.1.1/4",
    notes: [
      { id: "slice18.note26", pitch: "F5" },
      { hold: "slice19.note21", id: "slice18.note21", pitch: "A4" },
      { id: "slice18.note34", pitch: "D4" },
      { id: "slice18.note24", pitch: "D3" },
    ],
  },
  {
    time: "2.1.1/2",
    notes: [
      { id: "slice19.note27", pitch: "E5" },
      { hold: "slice20.note39", id: "slice19.note39", pitch: "A3" },
      { hold: "slice20.note21", id: "slice19.note21", pitch: "A4" },
    ],
  },
  {
    time: "2.1.3/4",
    notes: [
      { id: "slice20.note28", pitch: "D5" },
      { id: "slice20.note39", pitch: "A3" },
      { id: "slice20.note21", pitch: "A4" },
    ],
  },
  {
    time: "2.2.0",
    notes: [
      { id: "slice21.note40", pitch: "D4" },
      { id: "slice21.note29", pitch: "F5" },
    ],
  },
];

export const musicxml2pv =
  (mkLeft) => (mkRight) => (unfold) => (musicxml) => async () => {
    var output = "";
    var args = ["musicxml2pv", "/input.musicxml"];
    if (unfold) {
      args.splice(1, 0, "-u");
    }
    console.log(args);
    const wasi = new WASI({
      args: args,
      stdout: (out) => (output += out),
      stderr: (err) => console.error("wasm err:", err),
      fs: {
        "/input.musicxml": {
          path: "/input.musicxml",
          timestamps: {
            access: new Date(),
            change: new Date(),
            modification: new Date(),
          },
          mode: "string",
          content: musicxml,
        },
      },
    });
    // console.log(wasi);

    const wasm = await WebAssembly.instantiateStreaming(
      fetch(new URL("musicxml2pv.wasm", import.meta.url)),
      wasi.getImportObject(),
    );
    // console.log(wasm);

    const result = await wasi.start(wasm, {});
    console.log(result);
    if (output == "") {
      return mkLeft("musicxml2pv returned empty string");
    } else if (result.exitCode != 0) {
      return mkLeft(
        "musicxml2pv returned exit code" + result.exitCode.toString(),
      );
    } else {
      return mkRight(output);
    }
  };

export function eventTargetIsBody(eventTarget) {
  try {
    return eventTarget.tagName.toLowerCase() == "body";
  } catch {
    return false;
  }
}

export function getElementHTML(id) {
  return () => document.getElementById(id).outerHTML;
}
