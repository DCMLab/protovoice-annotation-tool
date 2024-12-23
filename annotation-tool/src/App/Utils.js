"use strict";

import jsDownload from "downloadjs";
import { WASI } from "@runno/wasi";


const wasm_binary = fetch(new URL("musicxml2pv.wasm", import.meta.url));

export const copyToClipboard = str => () => navigator.clipboard.writeText(str);

export function download_ (data) {
    return function (fileName) {
        return function (mimeType) {
            return function () {
                // the function returns true on success,
                // so we explicitly cast null to false, just in case
                return Boolean(jsDownload(data, fileName, mimeType));
            };
        };
    };
};

export const examplePieceJSON = [
    {time: "0.4.3/4", notes: [{ pitch: "D5", hold: false }]},
    {time: "1.1.0", notes: [
        { pitch: "D5", hold: false },
        { pitch: "D3", hold: true },
    ]},
  {time: "1.1.1/4", notes: [
    { pitch: "A4", hold: false },
    { pitch: "D3", hold: false },
  ]},
];

export const examplePieceJSONLong = [
    {time: "0.4.3/4", notes: [{ pitch: "D5", hold: false }]},
    {time: "1.1.0", notes: [
        { pitch: "D5", hold: false },
        { pitch: "D3", hold: true },
    ]},
  {time: "1.1.1/4", notes: [
    { pitch: "A4", hold: false },
    { pitch: "D3", hold: true },
  ]},
  {time: "1.1.1/2", notes: [
    { pitch: "Bb4", hold: false },
    { pitch: "D3", hold: true },
  ]},
  {time: "1.1.3/4", notes: [
    { pitch: "G4", hold: false },
    { pitch: "D3", hold: true },
  ]},
  {time: "1.2.0", notes: [
    { pitch: "A4", hold: false },
    { pitch: "D3", hold: true },
  ]},
  {time: "1.2.1/4", notes: [
    { pitch: "F4", hold: false },
    { pitch: "D3", hold: true },
  ]},
  {time: "1.2.1/2", notes: [
    { pitch: "E4", hold: false },
    { pitch: "D3", hold: true },
  ]},
  {time: "1.2.3/4", notes: [
    { pitch: "D4", hold: false },
    { pitch: "D3", hold: true },
  ]},
  {time: "1.3.0", notes: [
    { pitch: "C#4", hold: true },
    { pitch: "D3", hold: true },
  ]},
  {time: "1.3.1/4", notes: [
    { pitch: "Bb4", hold: true },
    { pitch: "C#4", hold: true },
    { pitch: "D3", hold: true },
  ]},
  {time: "1.3.1/2", notes: [
    { pitch: "E5", hold: false },
    { pitch: "Bb4", hold: true },
    { pitch: "C#4", hold: true },
    { pitch: "D3", hold: true },
  ]},
  {time: "1.3.3/4", notes: [
    { pitch: "G5", hold: true },
    { pitch: "Bb4", hold: false },
    { pitch: "C#4", hold: true },
    { pitch: "D3", hold: true },
  ]},
  {time: "1.4.0", notes: [
    { pitch: "G5", hold: false },
    { pitch: "A4", hold: true },
    { pitch: "C#4", hold: true },
    { pitch: "D3", hold: true },
  ]},
  {time: "1.4.1/4", notes: [
    { pitch: "G5", hold: false },
    { pitch: "A4", hold: true },
    { pitch: "C#4", hold: true },
    { pitch: "D3", hold: true },
  ]},
  {time: "1.4.1/2", notes: [
    { pitch: "F5", hold: false },
    { pitch: "A4", hold: true },
    { pitch: "C#4", hold: true },
    { pitch: "D3", hold: true },
  ]},
  {time: "1.4.3/4", notes: [
    { pitch: "E5", hold: false },
    { pitch: "A4", hold: true },
    { pitch: "C#4", hold: false },
    { pitch: "D3", hold: true },
  ]},
  {time: "2.1.0", notes: [
    { pitch: "G5", hold: false },
    { pitch: "A4", hold: true },
    { pitch: "D4", hold: true },
    { pitch: "D3", hold: true },
  ]},
  {time: "2.1.1/4", notes: [
    { pitch: "F5", hold: false },
    { pitch: "A4", hold: true },
    { pitch: "D4", hold: false },
    { pitch: "D3", hold: false },
  ]},
  {time: "2.1.1/2", notes: [
    { pitch: "E5", hold: false },
    { pitch: "A4", hold: true },
    { pitch: "A3", hold: true },
  ]},
  {time: "2.1.3/4", notes: [
    { pitch: "D5", hold: false },
    { pitch: "A4", hold: false },
    { pitch: "A3", hold: false },
  ]},
  {time: "2.2.0", notes: [
    { pitch: "F5", hold: false },
    { pitch: "D4", hold: false },
  ]},
];

export const musicxml2pv = (unfold) => (musicxml) => async () => {
  var output = null;
  var args = ["musicxml2pv", "/input.musicxml"];
  if (unfold) {
    args.splice(1, 0, "-u");
  }
  console.log(args);
  const wasi = new WASI({
    args: args,
    stdout: (out) => output = out,
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
      }
    }
  });
  // console.log(wasi);
  
  const wasm = await WebAssembly.instantiateStreaming(wasm_binary, wasi.getImportObject());
  // console.log(wasm);
  
  await wasi.start(wasm, {});
  return output;
};

