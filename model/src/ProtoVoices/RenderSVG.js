"use strict";

import * as Vex from "vexflow";
const VF = Vex.Flow;

// TODO: replace this with Accidental.applyAccidentals
function addAcc(i, chord, n) {
  if (n > 0) {
    chord.addModifier(new VF.Accidental("#".repeat(n)), i);
  }
  if (n < 0) {
    chord.addModifier(new VF.Accidental("b".repeat(-n)), i);
  }
}

function noteToVex(n) {
  return n.name + "/" + n.oct;
}

function drawSlice(slice, grand, useIDs) {
  var div = document.createElement("div");
  var renderer = new VF.Renderer(div, VF.Renderer.Backends.SVG);
  //renderer.resize(500, 500);
  var ctx = renderer.getContext();

  var voices = [];
  var formatter = new VF.Formatter();

  function renderNotes(notes, clef) {
    var voice = new VF.Voice({ num_beats: 1, beat_value: 4 });
    if (notes.length > 0) {
      var chord = new VF.StaveNote({
        clef: clef,
        keys: notes.map(noteToVex),
        duration: "q",
      });
      if (useIDs) {
        for (var i = 0; i < notes.length; i++) {
          chord._noteHeads[i].attrs.id = notes[i].id;
        }
        chord.attrs.id = `slice${slice.id}-${clef}`;
      }
      chord.getStem().setVisibility(false);

      notes.forEach((n, i) => addAcc(i, chord, n.accs));
      voice.addTickables([chord]);
      formatter.joinVoices([voice]);
      voices.push({ v: voice, c: clef });
    }
  }

  if (grand) {
    var notesT = slice.notes.filter((n) => n.oct >= 4);
    renderNotes(notesT, "treble");

    var notesB = slice.notes.filter((n) => n.oct < 4);
    renderNotes(notesB, "bass");
  } else {
    renderNotes(slice.notes, "treble");
  }

  formatter.format(voices.map((v) => v.v));

  var width = formatter.getMinTotalWidth();
  if (grand) {
    var staveT = new VF.Stave(0, 0, width);
    var staveB = new VF.Stave(0, 70, width);
    voices.forEach((voice) =>
      voice.v.draw(ctx, voice.c == "treble" ? staveT : staveB),
    );
  } else {
    var staff = new VF.Stave(0, 0, width);
    voices.forEach((voice) => voice.v.draw(ctx, staff));
  }

  var elt = div.children[0];
  elt.setAttribute("transform", `translate(${-width / 2} 0)`);
  return elt;
}

function drawStaff(width, grand) {
  var div = document.createElement("div");
  var renderer = new VF.Renderer(div, VF.Renderer.Backends.SVG);
  var ctx = renderer.getContext();

  var staveT = new VF.Stave(0, 0, width).addClef("treble");
  staveT.setContext(ctx).draw();

  if (grand) {
    var staveB = new VF.Stave(0, 70, width).addClef("bass");
    staveB.setContext(ctx).draw();
  }

  var elt = div.children[0];
  elt.setAttribute("style", "overflow: visible;");
  return elt;
}

export const drawScore = (slices) => (totalWidth) => (scale) => (grandStaff) =>
  drawSystem(slices, totalWidth, scale, grandStaff);

function drawSystem(slices, totalWidth, scale, grandStaff, useIDs = true) {
  var container = document.createElementNS("http://www.w3.org/2000/svg", "g");

  // draw staff
  var staffG = document.createElementNS("http://www.w3.org/2000/svg", "g");
  staffG.setAttribute("transform", "scale(" + scale + " " + scale + ")");
  staffG.appendChild(drawStaff(totalWidth, grandStaff));
  container.appendChild(staffG);

  // draw slices
  slices.forEach((slice) => {
    if (slice.notes.length === 0) {
      return;
    }
    var sliceG = document.createElementNS("http://www.w3.org/2000/svg", "g");
    sliceG.setAttribute(
      "transform",
      "translate(" + slice.x + " 0) scale(" + scale + " " + scale + ")",
    );
    sliceG.appendChild(drawSlice(slice, grandStaff, useIDs));
    container.appendChild(sliceG);
  });
  return container;
}

export const insertScore = (el) => (score) => () => el.replaceChildren(score);

// drawing an entire derivation

const markerWidth = 5;

// gets the bounding box of a note relative to container
function getNoteBBox(noteid, container) {
  var note = container.querySelector("#vf-" + CSS.escape(noteid));
  var notebb = getBBox(note.children[0], false, container);
  var slicebb = getBBox(note.parentElement, false, container);
  return {
    x: slicebb.x,
    y: notebb.y,
    width: slicebb.width,
    height: notebb.height,
  };
}

function drawEdge(container, edge, selection, passing, looseness = 1) {
  const bbleft = getNoteBBox(edge.left.id, container);
  const bbright = getNoteBBox(edge.right.id, container);
  var x1 = bbleft.x + bbleft.width + markerWidth + 2;
  var y1 = bbleft.y + bbleft.height / 2;
  var x2 = bbright.x - markerWidth - 2;
  var y2 = bbright.y + bbright.height / 2;
  var dist = Math.sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2);
  var d = dist * looseness * 0.3915; // from TikZ source
  // var line = document.createElementNS("http://www.w3.org/2000/svg", "line");
  // line.setAttribute("x1", x1);
  // line.setAttribute("y1", y1);
  // line.setAttribute("x2", x2);
  // line.setAttribute("y2", y2);
  var line = document.createElementNS("http://www.w3.org/2000/svg", "path");
  line.setAttribute(
    "d",
    `M ${x1} ${y1} C ${x1 + d} ${y1}, ${x2 - d} ${y2}, ${x2} ${y2}`,
  );
  if (passing) {
    line.setAttribute("stroke-dasharray", "6,3");
  }
  if (
    selection &&
    (selection.note.id == edge.left.id || selection.note.id == edge.right.id)
  ) {
    line.setAttribute("class", "pv-edge pv-selected");
  } else {
    line.setAttribute("class", "pv-edge");
  }
  line.setAttribute("fill", "transparent");
  return line;
}

function drawHori(hori, container) {
  var bbparent = getBBox(
    container.querySelector("#vf-slice" + hori.parent + "-bass") ||
      container.querySelector("#vf-slice" + hori.parent + "-treble"),
    false,
    container,
  );
  var bbchild = getBBox(
    container.querySelector("#vf-slice" + hori.child + "-treble") ||
      container.querySelector("#vf-slice" + hori.child + "-bass"),
    false,
    container,
  );
  var line = document.createElementNS("http://www.w3.org/2000/svg", "line");
  line.setAttribute("x1", bbparent.x + bbparent.width / 2);
  line.setAttribute("y1", bbparent.y + bbparent.height + 5);
  line.setAttribute("x2", bbchild.x + bbparent.width / 2);
  line.setAttribute("y2", bbchild.y - 5);
  line.setAttribute("stroke", "lightgray");
  // line.setAttribute("stroke-dasharray", "10,5");
  line.setAttribute("stroke-width", "5");
  return line;
}

function drawMarker(noteid, expl, container) {
  const bbox = getNoteBBox(noteid, container);
  const y = bbox.y + bbox.height / 2;
  var marker = document.createElementNS("http://www.w3.org/2000/svg", "g");
  container.appendChild(marker);

  // select markers based on ornament type (TODO)
  var left = "none";
  var right = "none";
  switch (expl.typ) {
    case "Root":
      left = "root";
      right = "root";
      break;
    case "Hori":
      // TODO
      break;
    case "RightRepeat":
      left = "repeat";
      break;
    case "RightNeighbor":
      left = "neighbor-left";
      break;
    case "LeftRepeat":
      right = "repeat";
      break;
    case "LeftNeighbor":
      right = "neighbor-right";
      break;
    case "FullNeighbor":
      left = "neighbor-left";
      right = "neighbor-right";
      break;
    case "FullRepeat":
      left = "repeat";
      right = "repeat";
      break;
    case "LeftRepeatOfRight":
      left = "neighbor-left";
      right = "repeat";
      break;
    case "RightRepeatOfLeft":
      left = "repeat";
      right = "neighbor-right";
      break;
    case "PassingMid":
      left = "pass";
      right = "pass";
      break;
    case "PassingLeft":
      left = "pass";
      break;
    case "PassingRight":
      right = "pass";
      break;
  }

  marker.innerHTML = `
    <use href="#marker-${left}" x="${bbox.x - markerWidth - 1}" y="${y}"/>
    <use href="#marker-${right}" x="${bbox.x + bbox.width + 1}" y="${y}"/>
  `;
}

export const drawGraph = (graph) => (totalWidth) => (scale) => (grandStaff) => {
  // initialize container
  var graphContainer = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "g",
  );
  // add marker defs
  graphContainer.innerHTML = markers;
  var fg_sep = document.createElementNS("http://www.w3.org/2000/svg", "g");
  graphContainer.appendChild(fg_sep);

  // draw levels with slices
  // if top-level is only ⋊-⋉: start at level 1
  var minLevel = graph.slices.filter((s) => s.depth == 0).length == 2 ? 1 : 0;
  var levelOffset = grandStaff ? 150 : 80;
  for (var level = minLevel; level <= graph.maxd; level++) {
    var levelSlices = graph.slices.filter((s) => s.depth == level);
    var levelG = drawSystem(levelSlices, totalWidth, scale, grandStaff);
    levelG.setAttribute(
      "transform",
      `translate(0 ${(level - minLevel) * levelOffset})`,
    );
    graphContainer.appendChild(levelG);
  }

  // mark seleted notes
  if (graph.selection) {
    graphContainer
      .querySelector("#vf-" + CSS.escape(graph.selection.note.id))
      .setAttribute("class", "pv-note pv-selected");
    graph.selection.parents.forEach((parent) => {
      graphContainer
        .querySelector("#vf-" + CSS.escape(parent.id))
        .setAttribute("class", "pv-note pv-parent");
    });
  }

  // add callbacks
  if (graph.select) {
    graph.slices.forEach((slice) => {
      slice.notes.forEach((note) => {
        var elt = graphContainer.querySelector("#vf-" + CSS.escape(note.id));
        elt.setAttribute("class", elt.getAttribute("class") + " pv-selectable");
        // this note already selected?
        if (graph.selection && graph.selection.note.id == note.id) {
          // yes -> deselect
          elt.addEventListener("click", graph.select(null));
        } else {
          // no -> select
          elt.addEventListener("click", graph.select(note.sel));
        }
      });
    });
  }

  // draw score
  var score = drawSystem(graph.surface, totalWidth, scale, grandStaff, false);
  score.setAttribute(
    "transform",
    `translate(0 ${(graph.maxd - minLevel + 1) * levelOffset})`,
  );
  graphContainer.appendChild(score);

  // draw connections

  // make sure that the container is attached to the DOM (required for computing bboxes)
  var fakesvg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  fakesvg.appendChild(graphContainer);
  document.body.appendChild(fakesvg);

  // render horis
  graph.horis.forEach((hori) => {
    graphContainer.insertBefore(drawHori(hori, graphContainer), fg_sep);
  });

  // render edges
  graph.transitions.forEach((transition) => {
    transition.regular.forEach((edge) => {
      graphContainer.insertBefore(
        drawEdge(graphContainer, edge, graph.selection, false),
        fg_sep,
      );
    });
    transition.passing.forEach((edge) => {
      graphContainer.insertBefore(
        drawEdge(graphContainer, edge, graph.selection, true),
        fg_sep,
      );
    });
  });

  // render markers
  graph.slices.forEach((slice) => {
    slice.notes.forEach((note) => {
      // graphContainer.appendChild(
      // TODO: select correct note type
      drawMarker(note.id, note.expl, graphContainer);
      // );
    });
  });

  // remove container from DOM
  fakesvg.remove();
  graphContainer.remove();

  return graphContainer;
};

// svg for markers
const markerStroke = "black";

const markers = `<defs>
  <symbol id="marker-repeat" class="pv-op-marker" width="${markerWidth}" height="3" viewport="0 -2 5 3" overflow="visible">
    <line x1="0" y1="1.5" x2="${markerWidth}" y2="1.5"/>
    <line x1="0" y1="-1.5" x2="${markerWidth}" y2="-1.5"/>
  </symbol>
  <symbol id="marker-neighbor-left" class="pv-op-marker" width="${markerWidth}" height="3" viewport="0 -2 5 3" overflow="visible">
    <line x1="0" y1="1.5" x2="${markerWidth}" y2="-1.5"/>
  </symbol>
  <symbol id="marker-neighbor-right" class="pv-op-marker" width="${markerWidth}" height="3" viewport="0 -2 5 3" overflow="visible">
    <line x1="0" y1="-1.5" x2="${markerWidth}" y2="1.5"/>
  </symbol>
  <symbol id="marker-pass" class="pv-op-marker" width="${markerWidth}" height="3" viewport="0 -2 5 3" overflow="visible">
    <line x1="0" y1="-1.5" x2="${markerWidth}" y2="0"/>
    <line x1="0" y1="1.5" x2="${markerWidth}" y2="0"/>
  </symbol>
</defs>
<style>
.pv-op-marker line {
    stroke: black;
    stroke-width: 1.5;
}
.pv-edge {
    stroke: black;
}
.pv-edge.pv-selected {
    stroke: rgb(30, 144, 255);
}
.pv-note.pv-selected {
    fill: rgb(30, 144, 255);
}
.pv-note.pv-parent {
    fill: rgb(135, 206, 250)
}
.pv-selectable {
    cursor: pointer;
}
</style>
<rect width="100%" height="100%" fill="white"/>`;

/**
 * From https://gsap.com/community/forums/topic/13681-svg-gotchas/page/2/#comment-72060
 *
 * @param {SVGElement} element - Element to get the bounding box for
 * @param {boolean} [withoutTransforms=false] - If true, transforms will not be calculated
 * @param {SVGElement} [toElement] - Element to calculate bounding box relative to
 * @returns {SVGRect} Coordinates and dimensions of the real bounding box
 */
function getBBox(element, withoutTransforms, toElement) {
  var svg = element.ownerSVGElement;

  if (!svg) {
    return { x: 0, y: 0, cx: 0, cy: 0, width: 0, height: 0 };
  }

  var r = element.getBBox();

  if (withoutTransforms) {
    return {
      x: r.x,
      y: r.y,
      width: r.width,
      height: r.height,
      cx: r.x + r.width / 2,
      cy: r.y + r.height / 2,
    };
  }

  var p = svg.createSVGPoint();

  var matrix = (toElement || svg)
    .getScreenCTM()
    .inverse()
    .multiply(element.getScreenCTM());

  p.x = r.x;
  p.y = r.y;
  var a = p.matrixTransform(matrix);

  p.x = r.x + r.width;
  p.y = r.y;
  var b = p.matrixTransform(matrix);

  p.x = r.x + r.width;
  p.y = r.y + r.height;
  var c = p.matrixTransform(matrix);

  p.x = r.x;
  p.y = r.y + r.height;
  var d = p.matrixTransform(matrix);

  var minX = Math.min(a.x, b.x, c.x, d.x);
  var maxX = Math.max(a.x, b.x, c.x, d.x);
  var minY = Math.min(a.y, b.y, c.y, d.y);
  var maxY = Math.max(a.y, b.y, c.y, d.y);

  var width = maxX - minX;
  var height = maxY - minY;

  return {
    x: minX,
    y: minY,
    width: width,
    height: height,
    cx: minX + width / 2,
    cy: minY + height / 2,
  };
}
