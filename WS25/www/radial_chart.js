// radial_chart.js
(function() {
  if (typeof d3 === 'undefined') {
    throw new Error('d3 is required');
  }

  function renderRadial(data, svg, options) {
    const radius = Math.min(options.width, options.height) / 2 - 40;
    
    // Clear previous content
    svg.selectAll("*").remove();

    const g = svg.append("g")
      .attr("transform", `translate(${options.width / 2},${options.height / 2})`);

    const color = d3.scaleOrdinal()
      .domain(data.map(d => d.county))
      .range(d3.schemeCategory10);

    const pie = d3.pie()
      .value(d => d.n)
      .sort((a, b) => b.n - a.n);

    const arc = d3.arc()
      .innerRadius(radius * 0.4)
      .outerRadius(radius);

    const arcs = g.selectAll(".arc")
      .data(pie(data))
      .enter().append("g")
      .attr("class", "arc")
      .style("cursor", "pointer");

    arcs.append("path")
      .attr("d", arc)
      .attr("fill", d => color(d.data.county))
      .attr("stroke", "white")
      .attr("stroke-width", 2)
      .on("click", function(event, d) {
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue("county_click", d.data.county);
        }
      })
      .on("mouseover", function(event, d) {
        d3.select(this)
          .transition()
          .duration(200)
          .attr("transform", "scale(1.05)");
      })
      .on("mouseout", function(event, d) {
        d3.select(this)
          .transition()
          .duration(200)
          .attr("transform", "scale(1)");
      });

    // Add labels
    const labelArc = d3.arc()
      .innerRadius(radius * 0.8)
      .outerRadius(radius * 0.8);

    arcs.append("text")
      .attr("transform", d => `translate(${labelArc.centroid(d)})`)
      .attr("text-anchor", "middle")
      .text(d => `${d.data.county}: ${d.data.n}`)
      .style("font-size", "11px")
      .style("font-weight", "bold")
      .style("fill", "white");

    // Add title
    svg.append("text")
      .attr("x", options.width / 2)
      .attr("y", 20)
      .attr("text-anchor", "middle")
      .style("font-size", "16px")
      .style("font-weight", "bold")
      .style("fill", "#2c3e50")
      .text("Patient Distribution by County");
  }

  if (typeof module !== 'undefined' && module.exports) {
    module.exports = renderRadial;
  } else {
    this.radialChart = renderRadial;
  }
})();