"use strict";

const Vex = require("vexflow");
const VF = Vex.default.Flow;

function addAcc(i,chord,n) {
    if (n > 0) {
        chord.addAccidental(i, new VF.Accidental("#".repeat(n)));
    }
    if (n < 0) {
        chord.addAccidental(i, new VF.Accidental("b".repeat(-n)));
    }
}

function noteToVex(n) {
    return n.name + "/" + n.oct;
}

function drawNotes(notes) {
    var div = document.createElement("div");
    var renderer = new VF.Renderer(div, VF.Renderer.Backends.SVG);
    //renderer.resize(500, 500);
    var ctx = renderer.getContext();

    var notesT = notes.filter(n => n.oct >= 4);
    var chordT = new VF.StaveNote({clef: "treble", keys: notesT.map(noteToVex), duration: "q" });
    chordT.setStemStyle({strokeStyle:"#00000000"});
    notesT.forEach((n,i) => addAcc(i, chordT, n.accs));    
    var voiceT = new VF.Voice({num_beats: 1,  beat_value: 4});
    voiceT.addTickables([chordT]);

    var notesB = notes.filter(n => n.oct < 4);
    var chordB = new VF.StaveNote({clef: "bass", keys: notesB.map(noteToVex), duration: "q" });
    chordB.setStemStyle({strokeStyle:"#00000000"});
    notesB.forEach((n,i) => addAcc(i, chordB, n.accs));
    var voiceB = new VF.Voice({num_beats: 1,  beat_value: 4});
    voiceB.addTickables([chordB]);

    var formatter = new VF.Formatter();
    formatter.joinVoices([voiceT]);
    formatter.joinVoices([voiceB]);
    formatter.format([voiceT,voiceB]);
    
    var width = formatter.getMinTotalWidth()+50;
    var staveT = new VF.Stave(0, 0, width);
    var staveB = new VF.Stave(0, 70, width);
    
    //staveT.setContext(ctx).draw();
    //staveB.setContext(ctx).draw();

    if (notesT.length > 0) {
        voiceT.draw(ctx, staveT);
    }
    if (notesB.length > 0) {
        voiceB.draw(ctx, staveB);
    }
    return div.children[0];
};

function drawStaff(width) {
    var div = document.createElement("div");
    var renderer = new VF.Renderer(div, VF.Renderer.Backends.SVG);
    renderer.resize(width, 160);
    var ctx = renderer.getContext();

    var staveT = new VF.Stave(0, 0, width).addClef("treble");
    var staveB = new VF.Stave(0, 70, width).addClef("bass");

    staveT.setContext(ctx).draw();
    staveB.setContext(ctx).draw();
    return div.children[0];
};

exports.drawScore = slices => totalWidth => scale => {
    var container = document.createElementNS("http://www.w3.org/2000/svg", "g");

    // draw staff
    var staffG = document.createElementNS("http://www.w3.org/2000/svg", "g");
    staffG.setAttribute("transform", "scale(" + scale + "," + scale + ")");
    staffG.appendChild(drawStaff(totalWidth));
    container.appendChild(staffG);

    // draw slices
    slices.forEach(slice => {
        if (slice.notes.length === 0) {
            return;
        }
        var sliceElt = document.createElementNS("http://www.w3.org/2000/svg", "svg");
        sliceElt.setAttribute("x", slice.x);
        sliceElt.setAttribute("y", 0);
        var sliceG = document.createElementNS("http://www.w3.org/2000/svg", "g");
        sliceG.setAttribute("transform", "scale(" + scale + "," + scale + ")");
        sliceG.appendChild(drawNotes(slice.notes));
        sliceElt.appendChild(sliceG);
        container.appendChild(sliceElt);
    });
    return container;
};

exports.insertScore = el => (score => (() => el.replaceChildren(score)));
