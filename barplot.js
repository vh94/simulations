// Delete previous plot group
svg.selectAll("g").remove("plotGroup")

// Set up constants used throughout script
const margin = {top: 0.05 * width, 
                right: 0.125 * width, 
                bottom: 0.05 * width, 
                left: 0.075 * width}
const plotWidth = width - margin.left - margin.right
const plotHeight = height - margin.top - margin.bottom

const barWidth = 0.02 * plotWidth
const mediumText = 0.01 * plotWidth
const bigText = 0.01 * plotWidth

// Set width and height of svg element (plot + margin)
svg.attr("width", plotWidth + margin.left + margin.right)
   .attr("height", plotHeight + margin.top + margin.bottom)
   
// Create plot group and move it
let plotGroup = svg.append("g")
                   .attr("class", "plotGroup")
                   .attr("transform",
                         "translate(" + margin.left + "," + margin.top + ")")

// X-axis values to categories in data
// X-axis goes from 0 to width of plot
let xAxis = d3.scaleBand()
    .domain(data.map(d => d.character))
    .range([0, plotWidth])
    .paddingInner(0.1)
    .paddingOuter(0.1)

// Y-axis values to cumulative caught range
// Y-axis goes from height of plot to 0
let yAxis = d3.scaleLinear()
    .domain([0, d3.max(data, d => d.value + d.time)]) // Adjust domain to accommodate the time-dependent value
    .range([plotHeight, 0])

// Add X-axis to plot
// Move X-axis to bottom of plot (height)
// Set stroke width and font size
plotGroup.append("g")
   .attr("transform", "translate(0," + plotHeight + ")")
   .call(d3.axisBottom(xAxis))
   .attr("stroke-width", barWidth)
   .attr("font-size", mediumText)

// Add Y-axis to plot
// Set stroke width and font size
plotGroup.append("g")
    .call(d3.axisLeft(yAxis))
    .attr("stroke-width", barWidth)
    .attr("font-size", mediumText)

// Function to draw bars
function drawBars() {
  // Remove previously drawn bars when re-drawing
  plotGroup.selectAll(".drawn_bars").remove()

  // Add bars
  let bars = plotGroup.selectAll(".drawn_bars")
                      .data(data)
                      .enter()
                      .append("rect")
                      .attr("class", "drawn_bars")
                      .attr("x", d => x(d.character))
                      .attr("width", x.bandwidth())
                      .attr("fill", d => d.color)
                      .attr("y", d => yAxis(d.value)) // Start with y position based on the initial value
                      .attr("height", 0) // Set initial height to 0

                      // Transition bars' heights to their respective values at each time step
                      .transition()
                      .duration(1000)
                      .attr("y", d => yAxis(d.value + d.time)) // Update y position based on the time-dependent value
                      .attr("height", d => plotHeight - yAxis(d.value + d.time)) // Update height based on the time-dependent value
}

drawBars()
