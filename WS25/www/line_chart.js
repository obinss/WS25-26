// line_chart.js - For outcomes timeline
(function() {
  if (typeof d3 === 'undefined') {
    throw new Error('d3 is required');
  }

  function renderLineChart(data, svg, options) {
    const margin = { top: 50, right: 80, bottom: 50, left: 60 };
    const width = options.width - margin.left - margin.right;
    const height = options.height - margin.top - margin.bottom;

    // Clear previous content
    svg.selectAll("*").remove();

    const g = svg.append("g")
      .attr("transform", `translate(${margin.left},${margin.top})`);

    // Extract metrics (excluding timepoint)
    const metrics = Object.keys(data[0]).filter(key => key !== 'timepoint');
    
    // Create scales
    const x = d3.scalePoint()
      .domain(data.map(d => d.timepoint))
      .range([0, width])
      .padding(0.5);

    const y = d3.scaleLinear()
      .domain([0, 100])
      .nice()
      .range([height, 0]);

    const color = d3.scaleOrdinal()
      .domain(metrics)
      .range(["#e74c3c", "#3498db", "#2ecc71", "#f39c12", "#9b59b6"]);

    // Create line generator
    const line = d3.line()
      .x(d => x(d.timepoint))
      .y(d => y(d.value))
      .curve(d3.curveMonotoneX);

    // Draw grid lines
    g.append("g")
      .attr("class", "grid")
      .call(d3.axisLeft(y)
        .tickSize(-width)
        .tickFormat("")
      )
      .selectAll("line")
      .attr("stroke", "#ecf0f1")
      .attr("stroke-width", 1);

    // Draw lines for each metric
    metrics.forEach(metric => {
      const metricData = data.map(d => ({
        timepoint: d.timepoint,
        value: d[metric],
        metric: metric
      }));

      const path = g.append("path")
        .datum(metricData)
        .attr("fill", "none")
        .attr("stroke", color(metric))
        .attr("stroke-width", 3)
        .attr("d", line)
        .style("opacity", 0)
        .transition()
        .duration(1000)
        .style("opacity", 1);

      // Animate drawing the line
      const totalLength = path.node().getTotalLength();
      
      path
        .attr("stroke-dasharray", totalLength + " " + totalLength)
        .attr("stroke-dashoffset", totalLength)
        .transition()
        .duration(1000)
        .ease(d3.easeLinear)
        .attr("stroke-dashoffset", 0);

      // Add data points
      g.selectAll(`.dot-${metric}`)
        .data(metricData)
        .enter().append("circle")
        .attr("class", `dot dot-${metric}`)
        .attr("cx", d => x(d.timepoint))
        .attr("cy", d => y(d.value))
        .attr("r", 5)
        .attr("fill", color(metric))
        .attr("stroke", "white")
        .attr("stroke-width", 2)
        .style("cursor", "pointer")
        .on("click", function(event, d) {
          if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
            Shiny.setInputValue("outcome_click", {
              timepoint: d.timepoint,
              metric: d.metric,
              value: d.value
            });
          }
        })
        .on("mouseover", function(event, d) {
          d3.select(this)
            .transition()
            .duration(200)
            .attr("r", 8);
          
          tooltip.style("opacity", 1)
            .html(`
              <strong>${d.metric}</strong><br/>
              Time: ${d.timepoint}<br/>
              Score: ${d.value}
            `)
            .style("left", (event.pageX + 10) + "px")
            .style("top", (event.pageY - 28) + "px");
        })
        .on("mouseout", function(event, d) {
          d3.select(this)
            .transition()
            .duration(200)
            .attr("r", 5);
          
          tooltip.style("opacity", 0);
        })
        .style("opacity", 0)
        .transition()
        .duration(800)
        .delay((d, i) => i * 200 + 500)
        .style("opacity", 1);
    });

    // Add axes
    g.append("g")
      .attr("transform", `translate(0,${height})`)
      .call(d3.axisBottom(x))
      .selectAll("text")
      .style("font-size", "11px");

    g.append("g")
      .call(d3.axisLeft(y))
      .style("font-size", "11px");

    // Add axis labels
    g.append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", -margin.left + 15)
      .attr("x", -height / 2)
      .attr("text-anchor", "middle")
      .style("font-size", "12px")
      .style("fill", "#7f8c8d")
      .text("Score (0-100)");

    g.append("text")
      .attr("x", width / 2)
      .attr("y", height + margin.bottom - 10)
      .attr("text-anchor", "middle")
      .style("font-size", "12px")
      .style("fill", "#7f8c8d")
      .text("Time Point");

    // Add legend
    const legend = g.append("g")
      .attr("transform", `translate(${width - 100}, 0)`);

    const legendItems = legend.selectAll(".legend-item")
      .data(metrics)
      .enter().append("g")
      .attr("class", "legend-item")
      .attr("transform", (d, i) => `translate(0, ${i * 20})`)
      .style("cursor", "pointer")
      .on("click", function(event, d) {
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue("metric_click", d);
        }
      });

    legendItems.append("rect")
      .attr("width", 12)
      .attr("height", 12)
      .attr("fill", color)
      .attr("rx", 2)
      .attr("ry", 2);

    legendItems.append("text")
      .attr("x", 20)
      .attr("y", 10)
      .text(d => d.replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase()))
      .style("font-size", "11px")
      .style("fill", "#2c3e50");

    // Add title
    g.append("text")
      .attr("x", width / 2)
      .attr("y", -20)
      .attr("text-anchor", "middle")
      .style("font-size", "16px")
      .style("font-weight", "bold")
      .style("fill", "#2c3e50")
      .text("Patient Outcomes Over Time");

    // Add subtitle
    g.append("text")
      .attr("x", width / 2)
      .attr("y", -5)
      .attr("text-anchor", "middle")
      .style("font-size", "12px")
      .style("fill", "#7f8c8d")
      .text("Click points for detailed information");

    // Create tooltip
    const tooltip = d3.select("body").append("div")
      .attr("class", "d3-tooltip")
      .style("position", "absolute")
      .style("background", "rgba(0, 0, 0, 0.85)")
      .style("color", "white")
      .style("padding", "10px")
      .style("border-radius", "6px")
      .style("font-size", "12px")
      .style("pointer-events", "none")
      .style("opacity", 0)
      .style("z-index", "1000");
  }

  if (typeof module !== 'undefined' && module.exports) {
    module.exports = renderLineChart;
  } else {
    this.lineChart = renderLineChart;
  }
})();