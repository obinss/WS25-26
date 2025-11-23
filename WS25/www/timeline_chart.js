// timeline_chart.js
(function() {
  if (typeof d3 === 'undefined') {
    throw new Error('d3 is required');
  }

  // Main rendering function
  function renderTimeline(data, svg, options) {
    const margin = { top: 20, right: 30, bottom: 40, left: 40 };
    const width = options.width - margin.left - margin.right;
    const height = options.height - margin.top - margin.bottom;

    // Clear previous content
    svg.selectAll("*").remove();

    const x = d3.scaleTime()
      .domain(d3.extent(data, d => new Date(d.month)))
      .range([0, width]);

    const y = d3.scaleLinear()
      .domain([0, d3.max(data, d => d.n)]).nice()
      .range([height, 0]);

    const g = svg.append("g")
      .attr("transform", `translate(${margin.left},${margin.top})`);

    // Add area
    const area = d3.area()
      .x(d => x(new Date(d.month)))
      .y0(height)
      .y1(d => y(d.n))
      .curve(d3.curveMonotoneX);

    g.append("path")
      .datum(data)
      .attr("fill", "#3498db")
      .attr("fill-opacity", 0.3)
      .attr("d", area);

    // Add line
    g.append("path")
      .datum(data)
      .attr("fill", "none")
      .attr("stroke", "#3498db")
      .attr("stroke-width", 3)
      .attr("d", line);

    // Add line generator
    const line = d3.line()
      .x(d => x(new Date(d.month)))
      .y(d => y(d.n))
      .curve(d3.curveMonotoneX);

    // Add circles
    g.selectAll("circle")
      .data(data)
      .enter().append("circle")
      .attr("cx", d => x(new Date(d.month)))
      .attr("cy", d => y(d.n))
      .attr("r", 6)
      .attr("fill", "#2980b9")
      .attr("stroke", "white")
      .attr("stroke-width", 2)
      .style("cursor", "pointer")
      .on("click", function(event, d) {
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue("month_click", d.month);
        }
      })
      .on("mouseover", function(event, d) {
        d3.select(this)
          .transition()
          .duration(200)
          .attr("r", 8)
          .attr("fill", "#e74c3c");
      })
      .on("mouseout", function(event, d) {
        d3.select(this)
          .transition()
          .duration(200)
          .attr("r", 6)
          .attr("fill", "#2980b9");
      });

    // Add axes
    g.append("g")
      .attr("transform", `translate(0,${height})`)
      .call(d3.axisBottom(x).ticks(6).tickFormat(d3.timeFormat("%b %Y")));

    g.append("g")
      .call(d3.axisLeft(y));

    // Add labels
    g.append("text")
      .attr("x", width / 2)
      .attr("y", height + 35)
      .attr("text-anchor", "middle")
      .style("font-size", "12px")
      .style("fill", "#7f8c8d")
      .text("Month");

    g.append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", -margin.left + 15)
      .attr("x", -height / 2)
      .attr("text-anchor", "middle")
      .style("font-size", "12px")
      .style("fill", "#7f8c8d")
      .text("Number of Procedures");
  }

  // Export for r2d3
  if (typeof module !== 'undefined' && module.exports) {
    module.exports = renderTimeline;
  } else {
    this.timelineChart = renderTimeline;
  }
})();