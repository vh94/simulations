// delete previous plot group
svg.selectAll("g").remove("plotGroup")

// set up constants used throughout script
const margin = {top: 0.05 * width, 
                right: 0.125 * width, 
                bottom: 0.05 * width, 
                left: 0.075 * width}
const plotWidth = width - margin.left - margin.right
const plotHeight = height - margin.top - margin.bottom

const lineWidth = 0.002 * plotWidth
const mediumText = 0.01 * plotWidth
const bigText = 0.01 * plotWidth

// set width and height of svg element (plot + margin)
svg.attr("width", plotWidth + margin.left + margin.right)
   .attr("height", plotHeight + margin.top + margin.bottom)
   


// create plot group and move it
let plotGroup = svg.append("g")
                   .attr("transform",
                         "translate(" + margin.left + "," + margin.top + ")")
                         
// x-axis values to time range in data
// x-axis goes from 0 to width of plot
let xAxis = d3.scaleLinear()
    .domain(d3.extent(data, d => { return d.time; }))
    .range([ 0, plotWidth ]);
    
// y-axis values to cumulative caught range
// y-axis goes from height of plot to 0
let yAxis = d3.scaleLinear()
    .domain(d3.extent(data, d => { return d.value; }))
    .range([ plotHeight, 0]);
    
// add x-axis to plot
// move x axis to bottom of plot (height)
// format tick values as date (no comma in e.g. 2,001)
// set stroke width and font size

plotGroup.append("g")
   .attr("transform", "translate(0," + plotHeight + ")")
   .call(d3.axisBottom(xAxis).tickFormat(d3.format("d")))
   .attr("stroke-width", lineWidth)
   .attr("font-size", mediumText);

// add y-axis to plot
// set stroke width and font size
plotGroup.append("g")
    .call(d3.axisLeft(yAxis))
    .attr("stroke-width", lineWidth)
    .attr("font-size", mediumText);

// turns data into nested structure for multiple line chart
// d3.nest() no longer available in D3 v6 and above hence version set to 5
let nestedData = d3.nest()
    .key(d => { return d.character;})
    .entries(data);
    
  
function drawLabels() {
  // create legend labels i.e. name names
plotGroup.append("g")
  .selectAll("text")
  .data(nestedData)
  .enter()
  .append("text")
  // add class so name_labels can be removed in drawLines()
  .attr("class", "name_labels")
  .style("font-weight", "bold")
  .style("font-size", mediumText)
  // set location for labels (at the end)
  .attr("x", (d,i) => xAxis(d.values[d.values.length-1].time) + mediumText/2)
  .attr("y", (d, i) => yAxis(d.values[d.values.length-1].value) + mediumText/3)
  .attr("fill", d => {return d.values[0].color})
  .text(d => {return d.values[0].character})
  .attr("opacity", 0)
  .transition()
  .duration(500)
  .attr("opacity", 1)
}

function tweenDash() {
  let l = this.getTotalLength(),
      i = d3.interpolateString("0," + l, l + "," + l);
  return function(t) { return i(t) };
}

function lineTransition(path) {
  path.transition()
      .duration(4500)
      .attrTween("stroke-dasharray", tweenDash)
      .on("end", () => { 
        drawLabels();
      });
}

function drawLines() {
  // remove previously drawn lines when re-drawing
  plotGroup.selectAll(".drawn_lines").remove()
  // remove labels e.g. "Daphne" when re-drawing
  plotGroup.selectAll(".name_labels").remove()
  
let path = plotGroup.selectAll(".drawn_lines")
    .data(nestedData)
    .enter()
    .append("path")
    // set up class so only this path element can be removed
    .attr("class", "drawn_lines")
    .attr("fill", "none")
    // color of lines from hex codes in data
    .attr("stroke", d => {return d.values[0].color}) 
    .attr("stroke-width", lineWidth*1.2)
     // draw line according to data
    .attr("d", d => {
      return d3.line()
        .x(d => { return xAxis(d.time);})
        .y(d => { return yAxis(d.value);})
        (d.values)
    })
    .call(lineTransition)
}

drawLines()
