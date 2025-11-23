// sunburst_chart.js
(function() {
  if (typeof d3 === 'undefined') {
    throw new Error('d3 is required');
  }

  function renderSunburst(data, svg, options) {
    const radius = Math.min(options.width, options.height) / 2;
    
    // Clear previous content
    svg.selectAll("*").remove();

    const g = svg.append("g")
      .attr("transform", `translate(${options.width / 2},${options.height / 2})`);

    const color = d3.scaleOrdinal()
      .domain(data.map(d => d.diagnosis))
      .range(d3.quantize(d3.interpolateRainbow, data.length + 1));

    const pie = d3.pie()
      .value(d => d.n)
      .sort((a, b) => b.n - a.n);

    const arc = d3.arc()
      .innerRadius(0)
      .outerRadius(radius);

    const arcs = g.selectAll(".arc")
      .data(pie(data))
      .enter().append("g")
      .attr("class", "arc")
      .style("cursor", "pointer");

    arcs.append("path")
      .attr("d", arc)
      .attr("fill", d => color(d.data.diagnosis))
      .attr("stroke", "white")
      .attr("stroke-width", 2)
      .on("click", function(event, d) {
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue("diagnosis_click", d.data.diagnosis);
        }
      })
      .on("mouseover", function(event, d) {
        d3.select(this)
          .transition()
          .duration(200)
          .attr("transform", "scale(1.02)");
      })
      .on("mouseout", function(event, d) {
        d3.select(this)
          .transition()
          .duration(200)
          .attr("transform", "scale(1)");
      });

    // Add labels for larger segments
    arcs.filter(d => (d.endAngle - d.startAngle) > 0.2)
      .append("text")
      .attr("transform", d => `translate(${arc.centroid(d)})`)
      .attr("text-anchor", "middle")
      .text(d => {
        const label = d.data.diagnosis;
        return label.length > 12 ? label.substring(0, 12) + "..." : label;
      })
      .style("font-size", "10px")
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
      .text("Diagnosis Distribution");
  }

  if (typeof module !== 'undefined' && module.exports) {
    module.exports = renderSunburst;
  } else {
    this.sunburstChart = renderSunburst;
  }
})();