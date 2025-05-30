# Proto-Voice Annotation Tool

[![build](https://github.com/DCMLab/protovoice-annotation-tool/actions/workflows/pages.yaml/badge.svg)](https://github.com/DCMLab/protovoice-annotation-tool/actions/workflows/pages.yaml)


A simple frontend for proto-voice annotations written in Purescript.

## Quick Links

- [Annotation Tool](https://dcmlab.github.io/protovoice-annotation-tool/)
- [Viewer Demo](https://dcmlab.github.io/protovoice-annotation-tool/viewer/)
- [Example Analyses](https://dcmlab.github.io/protovoice-annotations/) ([github](https://github.com/DCMLab/protovoice-annotations))

## Building

To install the tool dependencies, run
```shell
$ npm install
```
in the main directory.
Alternatively, you can use your global installations of these tools.

For a development build, run
```shell
$ npm run build
```
then open `dev/index.html`.

For a production build, run
```shell
$ npm run build-prod
```
then open `prod/index.html`.

For live-reloading on changes to the code, run
```shell
$ npm run serve
```
This will automatically open the app in a browser,
and recompile and reload the page every time a source file is changed.

## Usage

### Annotation Tool

The [annotation tool](https://dcmlab.github.io/protovoice-annotation-tool/) is used to create and modify protovoice analyses.
The general procedure is as follows:

1. Load a piece (MusicXML or custom `.piece.json` format) or an existing analysis (`.analysis.json`).
2. Analyze the piece by "reducing" it:
   - Reduce a slices and transitions step by step.
   - Link notes to their parents to indicate their function.
3. (optional) Add style information to highlight particular elements of the analysis.
4. Download the analysis to your computer or generate an SVG graph.

### Viewer

The viewer can be used to display an analysis and walk through it step by step.
You can also select individual notes to see how they are linked to other notes.

To quickly view and explore an analysis, you can use the [viewer demo page](https://dcmlab.github.io/protovoice-annotation-tool/viewer/).
However, you can also use the viewer to create embedded interactive visualizations using Javascript.
Check the demo page to get an idea how this works.
Another example is the [repository of protovoice analyses](https://dcmlab.github.io/protovoice-annotations/) which automatically generates such a page for each analysis.
