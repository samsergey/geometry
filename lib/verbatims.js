var styles = '.graphics {
    stroke-width : 1.75px;
    stroke-linejoin : round;
    stroke-linecap  : round;    
    font-family : sans-serif;
    font-style : italic;
    font-size : 16px;
}

.plot .area {
    fill : url(#h1);
}

.plot .line {
    stroke : black;
}

.points {
    stroke : white;
    stroke-width : 1.25;
    fill : black;
}

.grayGrad .line {
    stroke : black;
}

.grayGrad .area {
    fill : black;
    fill-opacity : 0.125;
}

.graySolid .line {
    stroke : black;
}

.graySolid .area {
    fill : #888;
}

.A .area {
    fill : url(#h1);
}

.A .line {
    stroke : black;
}

.B .area {
    fill : url(#h2);
}

.B .line {
    stroke : black;
    stroke-dasharray: 5,5;
}

.C .area {
    fill : #888;
}

.C .line {
    stroke : black;
}

.marker {
    fill : black;
    stroke: none;
    font-family : sans-serif;
    font-size : 120%;

}

.needles {
    stroke : black;
    stroke-width : 1;
}

.refference {
    stroke:black;
}

.axis {
    stroke-width : 1.25;
    font-family : sans-serif;
    font-style : italic;
    font-size : 80%;
}

.axis .tick {
    stroke-width : 1.25;
    font-family : sans-serif;
    font-style : italic;
    font-size : 120%;
}

.axis .label {
    fill : black;
}

.label {
    fill : black;
    font-size: 80%;
    font-family: "Arial";
    font-style : italic;
}

.diffListPlot .areaAbove {
    fill : #F80
}

.diffListPlot .areaBelow {
    fill : #08F
}

.diffListPlot1 .above line {
    stroke : pink;
    stroke-width:2;
}

.diffListPlot1 .below line {
    stroke : skyblue;
    stroke-width:2;
}


.Lorentz .upper .area {
    fill : white;
}

.Lorentz .upper .line {
    stroke : black;
    stroke-width : 2.5px;
}

.Lorentz .lower .area {
    fill : url(#h1);
}

.Lorentz .lower .line {
    stroke : black;
}

.gridLine {
    stroke : black;
    stroke-width: 1px;
    stroke-dasharray: 5,6;
}

.gridLineLabel {
    stroke : none;
    fill:#444;
}


.histogram rect {
    stroke : black;
    stroke-width: 1px;
    fill : url(#h2);
}

.histogram {
    stroke : black;
    stroke-width: 1px;
    fill: url(#h1);
}

.histogram .stairs {
    stroke : black;
}


.marks {
    fill:red;
    stroke:white;
}

.clip {
    fill:white;
    stroke:none;
}

.matrixPlot .negatives {
    fill : navy;
}

.matrixPlot .positives {
    fill : indianred;
}

.hatch {
    stroke : black;
    stroke-width : 2px;
}'

var patterns = '<pattern id="h1" width="5px" height="5px" viewBox="0,0,20,20" patternUnits="userSpaceOnUse"><line class="hatch" x1="1" y1="20" x2="20" y2="1" /><line  class="hatch" x1="0" y1="1" x2="1" y2="0" /></pattern><pattern id="h2" width="5px" height="5px" viewBox="0,0,20,20" patternUnits="userSpaceOnUse"><line  class="hatch" x1="0" y1="20" x2="20" y2="0" /><line  class="hatch" x1="0" y1="0" x2="20" y2="20" /></pattern>'
