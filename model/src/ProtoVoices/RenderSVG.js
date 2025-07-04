"use strict";

import * as Vex from "vexflow";
const VF = Vex.Flow;

// Drawing basic score elements
// ============================

function drawSlice(slice, staffType, scale, useIDs, styles) {
  let div = document.createElementNS("http://www.w3.org/2000/svg", "g");
  let renderer = new VF.Renderer(div, VF.Renderer.Backends.SVG);
  //renderer.resize(500, 500);
  let ctx = renderer.getContext();

  let voices = [];
  let formatter = new VF.Formatter();

  // renders the notes on one staff
  function renderNotes(notes, clef) {
    let voice = new VF.Voice({ num_beats: 1, beat_value: 4 });
    if (notes.length > 0) {
      // generate notes using Vexflow
      let chord = new VF.StaveNote({
        clef: clef,
        keys: notes.map(noteToVex),
        duration: "q",
      });
      
      // add IDs
      if (useIDs) {
        for (let i = 0; i < notes.length; i++) {
          chord._noteHeads[i].attrs.id = notes[i].id;
        }
        chord.attrs.id = `slice${slice.id}-${clef}`;
      }

      // add style attributes
      if (styles) {
        for (let i = 0; i< notes.length; i++) {
          let style = styles.noteStyles[notes[i].id];
          if (style) {
            chord._noteHeads[i].attrs.class = style.classes;
          }
        }
      }
      chord.getStem().setVisibility(false);

      notes.forEach((n, i) => addAcc(i, chord, n.accs));
      voice.addTickables([chord]);
      formatter.joinVoices([voice]);
      voices.push({ v: voice, c: clef });
    }
  }

  if (staffType == "grand") {
    let notesT = slice.notes.filter((n) => n.oct >= 4);
    renderNotes(notesT, "treble");

    let notesB = slice.notes.filter((n) => n.oct < 4);
    renderNotes(notesB, "bass");
  } else {
    renderNotes(slice.notes, staffType);
  }

  formatter.format(voices.map((v) => v.v));

  let width = formatter.getMinTotalWidth();
  if (staffType == "grand") {
    let staveT = new VF.Stave(0, 0, width);
    let staveB = new VF.Stave(0, 70, width);
    voices.forEach((voice) =>
      voice.v.draw(ctx, voice.c == "treble" ? staveT : staveB),
    );
  } else {
    let staff = new VF.Stave(0, 0, width);
    voices.forEach((voice) => voice.v.draw(ctx, staff));
  }

  // Vexflow draws notes with 17px x-offset, so to place the slice at the correct position,
  // its x coordinate must be corrected for this offset plus half of the width of the slice.
  let xcorrection = (width / 2 + 17 * scale).toFixed(3);
  div.setAttribute(
    "transform",
    `translate(${-xcorrection} 0)`,
  );

  return div;
}

function drawStaff(width, staffType) {
  let div = document.createElement("div");
  let renderer = new VF.Renderer(div, VF.Renderer.Backends.SVG);
  let ctx = renderer.getContext();

  if (staffType == "grand") {
    let staveT = new VF.Stave(0, 0, width).addClef("treble");
    staveT.setContext(ctx).draw();
    let staveB = new VF.Stave(0, 70, width).addClef("bass");
    staveB.setContext(ctx).draw();
  } else {
    let stave = new VF.Stave(0, 0, width).addClef(staffType);
    stave.setContext(ctx).draw();
  }

  let elt = div.children[0];
  elt.setAttribute("style", "overflow: visible;");
  return elt;
}

function drawSystem(slices, totalWidth, scale, staffType, useIDs, styles = null) {
  let container = document.createElementNS("http://www.w3.org/2000/svg", "g");

  // draw staff
  let staff = document.createElementNS("http://www.w3.org/2000/svg", "g");
  staff.setAttribute("transform", "scale(" + scale + " " + scale + ")");
  staff.appendChild(drawStaff(totalWidth / scale, staffType));
  // container.appendChild(staffG);

  // draw slices
  slices.forEach((slice) => {
    if (slice.notes.length === 0) {
      return;
    }
    let sliceG = document.createElementNS("http://www.w3.org/2000/svg", "g");
    sliceG.setAttribute(
      "transform",
      "translate(" + slice.x + " 0) scale(" + scale + " " + scale + ")",
    );
    sliceG.setAttribute("id", "slice-" + slice.id);
    sliceG.appendChild(drawSlice(slice, staffType, scale, useIDs, styles));
    
    // apply slice styles
    if (styles !== null) {
      let sliceStyle = styles.sliceStyles[slice.id];
      if (sliceStyle) {
        // console.log(sliceStyle);
        // console.log(sliceStyle.classes);
        // console.log(sliceStyle.label);
        // add classes:
        sliceG.setAttribute("class", "pv-slice " + sliceStyle.classes);
        // add label
        let label = document.createElementNS("http://www.w3.org/2000/svg", "text");
        label.setAttribute("y", 20);
        label.textContent = sliceStyle.label;
        label.setAttribute("text-anchor", "middle");
        label.setAttribute("dominant-baseline", "middle");
        
        sliceG.appendChild(label);
      }
    }
    container.appendChild(sliceG);
  });
  return { notes: container, staff };
}

// Drawing a score
// ===============

export const drawScore =
  (slices) => (staffType) => (totalWidth) => (scale) => {
    let system = drawSystem(slices, totalWidth, scale, staffType, false);
    let container = document.createElementNS("http://www.w3.org/2000/svg", "g");
    container.appendChild(system.staff);
    container.appendChild(system.notes);
    return container;
  };

export const insertScore = (el) => (score) => () => el.replaceChildren(score);

// Drawing Derivations
// ===================

function drawEdge(container, edge, selection, passing, looseness = 1) {
  const bbleft = getNoteBBox(edge.left.id, container);
  const bbright = getNoteBBox(edge.right.id, container);
  let x1 = bbleft.x + bbleft.width + markerWidth + 2;
  let y1 = bbleft.y + bbleft.height / 2;
  let x2 = bbright.x - markerWidth - 2;
  let y2 = bbright.y + bbright.height / 2;
  let dist = Math.sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2);
  let d = dist * looseness * 0.3915; // from TikZ source
  // let line = document.createElementNS("http://www.w3.org/2000/svg", "line");
  // line.setAttribute("x1", x1);
  // line.setAttribute("y1", y1);
  // line.setAttribute("x2", x2);
  // line.setAttribute("y2", y2);
  let line = document.createElementNS("http://www.w3.org/2000/svg", "path");
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
  line.setAttribute("fill", "none");
  return line;
}

function drawHori(hori, container) {
  let bbparent = getBBox(
    container.querySelector("#vf-slice" + hori.parent + "-bass") ||
      container.querySelector("#vf-slice" + hori.parent + "-treble"),
    false,
    container,
  );
  let bbchild = getBBox(
    container.querySelector("#vf-slice" + hori.child + "-treble") ||
      container.querySelector("#vf-slice" + hori.child + "-bass"),
    false,
    container,
  );
  let line = document.createElementNS("http://www.w3.org/2000/svg", "line");
  line.setAttribute("x1", bbparent.x + bbparent.width / 2);
  line.setAttribute("y1", bbparent.y + bbparent.height + 5);
  line.setAttribute("x2", bbchild.x + bbparent.width / 2);
  line.setAttribute("y2", bbchild.y - 5);
  line.setAttribute("stroke", "lightgray");
  // line.setAttribute("stroke-dasharray", "10,5");
  line.setAttribute("stroke-width", "5");
  return line;
}

function drawMarker(noteid, expl, container, notemap) {
  const bbox = getNoteBBox(noteid, container);
  const y = bbox.y + bbox.height / 2;
  let marker = document.createElementNS("http://www.w3.org/2000/svg", "g");
  marker.setAttribute("id", "marker-" + noteid);
  marker.setAttribute("class", "pv-op-marker");
  container.appendChild(marker);
  // const noteG = container.querySelector("#vf-" + CSS.escape(noteid));
  // noteG.appendChild(marker);

  // select markers based on ornament type (TODO)
  let left = "none";
  let right = "none";
  switch (expl.typ) {
    case "Root":
      left = "root-left";
      right = "root-right";
      break;
    case "Hori":
      if (expl.parent) {
        if (notemap[expl.parent] > notemap[noteid]) {
          right = "spread-right";
        } else {
          left = "spread-left";
        }
      }
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

function drawTimeLabel(time, y) {
  let label = document.createElementNS("http://www.w3.org/2000/svg", "text");
  label.setAttribute("x", time.x);
  label.setAttribute("y", y);
  label.setAttribute("text-anchor", "middle");
  label.setAttribute("dominant-baseline", "middle");
  label.innerHTML = time.label;
  return label;
}

export const drawGraph = (graph) => (totalWidth) => (scale) => {
  // The graph is drawn in several steps:
  // - create a container element, add marker defs and CSS
  // - add a separator element to be able to sort element into foreground and background
  // - draw the staves and notes for each level
  // - add styles and optional callbacks to the notes
  // - draw the score at the bottom of the graph
  // - add horis, edges, and transitions
  // - add markers
  // - highlight selected notes
  // The last few steps compute bounding boxes so they require the previous
  // elements to be attached to the DOM, which is done temporarily.
  
  // initialize container
  let graphContainer = document.createElementNS(
    "http://www.w3.org/2000/svg",
    "g",
  );
  // add marker defs
  graphContainer.innerHTML = markers;

  // add CSS
  let styleElt = document.createElementNS("http://www.w3.org/2000/svg", "style");
  let combinedStyles = defaultStyles + "\n" + graph.styles.css + "\n" + uiStyles;
  styleElt.innerHTML = combinedStyles;
  graphContainer.appendChild(styleElt);

  // add a foreground separator.
  // this later allows us to place obects in the foreground or background.
  let fg_sep = document.createElementNS("http://www.w3.org/2000/svg", "g");
  graphContainer.appendChild(fg_sep);

  // draw levels with slices
  // if top-level is only ⋊-⋉: start at level 1
  let minLevel = graph.slices.filter((s) => s.depth == 0).length == 2 ? 1 : 0;
  let levelOffset = graph.styles.staffType == "grand" ? 150 : 80;
  for (let level = minLevel; level <= graph.maxd; level++) {
    let levelSlices = graph.slices.filter((s) => s.depth == level);
    let levelG = drawSystem(levelSlices, totalWidth, scale, graph.styles.staffType, true, graph.styles);
    let transform = `translate(0 ${(level - minLevel) * levelOffset})`;
    levelG.notes.setAttribute("transform", transform);
    levelG.staff.setAttribute(
      "transform",
      transform + " " + levelG.staff.getAttribute("transform"),
    );
    graphContainer.appendChild(levelG.notes);
    graphContainer.insertBefore(levelG.staff, fg_sep);
  }

  // add note callbacks and styles
  graph.slices.forEach((slice) => {
    slice.notes.forEach((note) => {
      let elt = graphContainer.querySelector("#vf-" + CSS.escape(note.id));
      let classes = elt.getAttribute("class") + " pv-note";

      if (graph.styles) {
        let noteStyle = graph.styles.noteStyles[note.id];
        if (noteStyle) {
          classes += " " + noteStyle.classes;
        }
      }

      if (graph.select) {
        classes += " pv-selectable";
        // this note already selected?
        if (graph.selection && graph.selection.note.id == note.id) {
          // yes -> deselect
          elt.addEventListener("click", graph.select(null));
        } else {
          // no -> select
          elt.addEventListener("click", graph.select(note.sel));
        }
      }

      elt.setAttribute("class", classes);
    });
  });

  // draw score
  let score = drawScore(graph.surfaceSlices)(graph.styles.staffType)(totalWidth)(scale);
  score.setAttribute(
    "transform",
    `translate(0 ${(graph.maxd - minLevel + 1) * levelOffset})`,
  );
  graphContainer.appendChild(score);
  let yoff = (graph.maxd - minLevel + 2) * levelOffset + 20;
  graph.times.forEach((time) => {
    graphContainer.appendChild(drawTimeLabel(time, yoff));
  });

  // draw connections

  // make sure that the container is attached to the DOM (required for computing bboxes)
  let fakesvg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  fakesvg.appendChild(graphContainer);
  document.body.appendChild(fakesvg);

  // render horis
  graph.horis.forEach((hori) => {
    graphContainer.insertBefore(drawHori(hori, graphContainer), fg_sep);
  });

  // render transitions and edges
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

  // collect all notes (needed for markers)
  let notemap = {};
  graph.slices.forEach((slice) => {
    slice.notes.forEach((note) => {
      notemap[note.id] = slice.x;
    });
  });

  // render markers
  graph.slices.forEach((slice) => {
    slice.notes.forEach((note) => {
      drawMarker(note.id, note.expl, graphContainer, notemap);
    });
  });

  // mark seleted notes
  if (graph.selection) {
    let note = graphContainer.querySelector(
      "#vf-" + CSS.escape(graph.selection.note.id),
    );
    note.setAttribute("class", note.getAttribute("class") + " pv-selected");
    graphContainer
      .querySelector("#marker-" + CSS.escape(graph.selection.note.id))
      .setAttribute("class", "pv-op-marker pv-selected");
    graph.selection.parents.forEach((parent) => {
      let parentNode = graphContainer.querySelector(
        "#vf-" + CSS.escape(parent.id),
      );
      parentNode.setAttribute(
        "class",
        parentNode.getAttribute("class") + " pv-parent",
      );
    });
  }

  // remove container from DOM
  fakesvg.remove();
  graphContainer.remove();

  return graphContainer;
};

// Constant definitions (markers, CSS, ...)
// ========================================

// size of a note-function marker
const markerWidth = 5;

// const markerStroke = "black";

// symbols for markers
const markers = `<defs>
  <symbol id="marker-repeat" width="${markerWidth}" height="3" viewport="0 -2 5 3" overflow="visible">
    <line x1="0" y1="1.5" x2="${markerWidth}" y2="1.5"/>
    <line x1="0" y1="-1.5" x2="${markerWidth}" y2="-1.5"/>
  </symbol>
  <symbol id="marker-neighbor-left" width="${markerWidth}" height="3" viewport="0 -2 5 3" overflow="visible">
    <line x1="0" y1="1.5" x2="${markerWidth}" y2="-1.5"/>
  </symbol>
  <symbol id="marker-neighbor-right" width="${markerWidth}" height="3" viewport="0 -2 5 3" overflow="visible">
    <line x1="0" y1="-1.5" x2="${markerWidth}" y2="1.5"/>
  </symbol>
  <symbol id="marker-pass" width="${markerWidth}" height="3" viewport="0 -2 5 3" overflow="visible">
    <line x1="0" y1="-1.5" x2="${markerWidth}" y2="0"/>
    <line x1="0" y1="1.5" x2="${markerWidth}" y2="0"/>
  </symbol>
  <symbol id="marker-spread-left" width="${markerWidth}" height="3" viewport="0 -2 5 3" overflow="visible">
    <polyline points="1,-1.5 1,1.5 ${markerWidth},1.5"/>
  </symbol>
  <symbol id="marker-spread-right" width="${markerWidth}" height="3" viewport="0 -2 5 3" overflow="visible">
    <polyline points="0,1.5 ${markerWidth - 1},1.5 ${markerWidth - 1},-1.5"/>
  </symbol>
  <symbol id="marker-root-left" width="${markerWidth}" height="3" viewport="0 -2 5 3" overflow="visible">
    <polygon points="1,0 ${markerWidth},-1.5 ${markerWidth},1.5 0,0"/>
  </symbol>
  <symbol id="marker-root-right" width="${markerWidth}" height="3" viewport="0 -2 5 3" overflow="visible">
    <polygon points="0,1.5 0,-1.5 ${markerWidth - 1},0 0,1.5"/>
  </symbol>
</defs>
<!--rect width="100%" height="100%" fill="white"/-->`;

// default CSS styles for graph elements
const defaultStyles = `
  text {
    font-size: 16px;
  }
  .pv-op-marker {
    stroke: black;
    stroke-width: 1.5;
    fill: none;
  }
  .pv-edge {
    stroke: black;
  }
`;

const uiStyles = `
  .pv-edge.pv-selected, .pv-op-marker.pv-selected {
    stroke: rgb(30, 144, 255);
  }
  .vf-notehead.pv-selected {
    fill: rgb(30, 144, 255);
  }
  .vf-notehead.pv-parent {
    fill: rgb(135, 206, 250);
  }
  .pv-selectable {
    cursor: pointer;
  }
`;

// Helper Functions
// ================

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

// gets the bounding box of a note relative to container
function getNoteBBox(noteid, container) {
  let note = container.querySelector("#vf-" + CSS.escape(noteid));
  let notebb = getBBox(note.children[0], false, container);
  let slicebb = getBBox(note.parentElement, false, container);
  return {
    x: slicebb.x,
    y: notebb.y,
    width: slicebb.width,
    height: notebb.height,
  };
}

/**
 * From https://gsap.com/community/forums/topic/13681-svg-gotchas/page/2/#comment-72060
 *
 * @param {SVGElement} element - Element to get the bounding box for
 * @param {boolean} [withoutTransforms=false] - If true, transforms will not be calculated
 * @param {SVGElement} [toElement] - Element to calculate bounding box relative to
 * @returns {SVGRect} Coordinates and dimensions of the real bounding box
 */
function getBBox(element, withoutTransforms, toElement) {
  let svg = element.ownerSVGElement;

  if (!svg) {
    return { x: 0, y: 0, cx: 0, cy: 0, width: 0, height: 0 };
  }

  let r = element.getBBox();

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

  let p = svg.createSVGPoint();

  let matrix = (toElement || svg)
    .getScreenCTM()
    .inverse()
    .multiply(element.getScreenCTM());

  p.x = r.x;
  p.y = r.y;
  let a = p.matrixTransform(matrix);

  p.x = r.x + r.width;
  p.y = r.y;
  let b = p.matrixTransform(matrix);

  p.x = r.x + r.width;
  p.y = r.y + r.height;
  let c = p.matrixTransform(matrix);

  p.x = r.x;
  p.y = r.y + r.height;
  let d = p.matrixTransform(matrix);

  let minX = Math.min(a.x, b.x, c.x, d.x);
  let maxX = Math.max(a.x, b.x, c.x, d.x);
  let minY = Math.min(a.y, b.y, c.y, d.y);
  let maxY = Math.max(a.y, b.y, c.y, d.y);

  let width = maxX - minX;
  let height = maxY - minY;

  return {
    x: minX,
    y: minY,
    width: width,
    height: height,
    cx: minX + width / 2,
    cy: minY + height / 2,
  };
}
