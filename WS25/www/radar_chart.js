// radar_chart.js - For supplier performance radar chart
(function() {
  if (typeof d3 === 'undefined') {
    throw new Error('d3 is required');
  }

  function renderRadarChart(data, svg, options) {
    const margin = { top: 60, right: 80, bottom: 60, left: 80 };
    const width = options.width - margin.left - margin.right;
    const height = options.height - margin.top - margin.bottom;
    const radius = Math.min(width, height) / 2;

    // Clear previous content
    svg.selectAll("*").remove();

    const g = svg.append("g")
      .attr("transform", `translate(${margin.left + width / 2},${margin.top + height / 2})`);

    // Extract dimensions (metrics)
    const dimensions = Object.keys(data[0]).filter(key => key !== 'supplier');
    
    // Scales
    const angleScale = d3.scaleBand()
      .domain(dimensions)
      .range([0, 2 * Math.PI]);

    const radialScale = d3.scaleLinear()
      .domain([0, 100])
      .range([0, radius]);

    const color = d3.scaleOrdinal()
      .domain(data.map(d => d.supplier))
      .range(d3.schemeCategory10);

    // Draw circular grid lines
    const levels = 5;
    const levelStep = 100 / levels;
    
    for (let i = 1; i <= levels; i++) {
      const level = levelStep * i;
      
      // Grid circles
      g.append("circle")
        .attr("r", radialScale(level))
        .attr("fill", "none")
        .attr("stroke", "#ecf0f1")
        .attr("stroke-width", 1);
      
      // Level labels
      g.append("text")
        .attr("y", -radialScale(level) - 5)
        .attr("text-anchor", "middle")
        .style("font-size", "10px")
        .style("fill", "#7f8c8d")
        .text(level);
    }

    // Draw axis lines
    const axis = g.selectAll(".axis")
      .data(dimensions)
      .enter().append("g")
      .attr("class", "axis");

    axis.append("line")
      .attr("x1", 0)
      .attr("y1", 0)
      .attr("x2", (d, i) => radialScale(100) * Math.cos(angleScale(d) - Math.PI / 2))
      .attr("y2", (d, i) => radialScale(100) * Math.sin(angleScale(d) - Math.PI / 2))
      .attr("stroke", "#bdc3c7")
      .attr("stroke-width", 1);

    // Axis labels
    axis.append("text")
      .attr("text-anchor", "middle")
      .attr("dy", "0.35em")
      .attr("x", (d, i) => (radialScale(100) + 20) * Math.cos(angleScale(d) - Math.PI / 2))
      .attr("y", (d, i) => (radialScale(100) + 20) * Math.sin(angleScale(d) - Math.PI / 2))
      .text(d => d)
      .style("font-size", "11px")
      .style("font-weight", "bold")
      .style("fill", "#2c3e50")
      .call(wrap, 60);

    // Line generator for radar shapes
    const line = d3.lineRadial()
      .angle((d, i) => angleScale(dimensions[i]))
      .radius(d => radialScale(d.value))
      .curve(d3.curveLinearClosed);

    // Draw radar shapes for each supplier
    const supplierGroup = g.selectAll(".supplier")
      .data(data)
      .enter().append("g")
      .attr("class", "supplier")
      .style("cursor", "pointer")
      .on("click", function(event, d) {
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue("supplier_click", d.supplier);
        }
      })
      .on("mouseover", function(event, d) {
        d3.select(this).select("path")
          .transition()
          .duration(200)
          .attr("fill-opacity", 0.8)
          .attr("stroke-width", 3);
        
        d3.select(this).selectAll(".data-point")
          .transition()
          .duration(200)
          .attr("r", 6);
        
        tooltip.style("opacity", 1)
          .html(`
            <strong>${d.supplier}</strong><br/>
            ${dimensions.map(dim => `${dim}: ${d[dim]}`).join('<br/>')}
          `)
          .style("left", (event.pageX + 15) + "px")
          .style("top", (event.pageY - 28) + "px");
      })
      .on("mouseout", function(event, d) {
        d3.select(this).select("path")
          .transition()
          .duration(200)
          .attr("fill-opacity", 0.3)
          .attr("stroke-width", 2);
        
        d3.select(this).selectAll(".data-point")
          .transition()
          .duration(200)
          .attr("r", 4);
        
        tooltip.style("opacity", 0);
      });

    // Add filled areas
    supplierGroup.append("path")
      .attr("d", d => line(dimensions.map(dim => ({ value: d[dim] }))))
      .attr("fill", d => color(d.supplier))
      .attr("fill-opacity", 0.3)
      .attr("stroke", d => color(d.supplier))
      .attr("stroke-width", 2)
      .style("opacity", 0)
      .transition()
      .duration(1000)
      .style("opacity", 1);

    // Add data points
    supplierGroup.selectAll(".data-point")
      .data(d => dimensions.map(dim => ({ dimension: dim, value: d[dim] })))
      .enter().append("circle")
      .attr("class", "data-point")
      .attr("r", 4)
      .attr("cx", (d, i) => radialScale(d.value) * Math.cos(angleScale(dimensions[i]) - Math.PI / 2))
      .attr("cy", (d, i) => radialScale(d.value) * Math.sin(angleScale(dimensions[i]) - Math.PI / 2))
      .attr("fill", d => color(d.supplier))
      .attr("stroke", "white")
      .attr("stroke-width", 1)
      .style("opacity", 0)
      .transition()
      .duration(1000)
      .delay(500)
      .style("opacity", 1);

    // Add legend
    const legend = svg.append("g")
      .attr("transform", `translate(${margin.left},${margin.top - 40})`);

    const legendItems = legend.selectAll(".legend-item")
      .data(data)
      .enter().append("g")
      .attr("class", "legend-item")
      .attr("transform", (d, i) => `translate(${i * 120}, 0)`)
      .style("cursor", "pointer")
      .on("click", function(event, d) {
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue("supplier_click", d.supplier);
        }
      });

    legendItems.append("rect")
      .attr("width", 12)
      .attr("height", 12)
      .attr("fill", d => color(d.supplier))
      .attr("rx", 2)
      .attr("ry", 2);

    legendItems.append("text")
      .attr("x", 20)
      .attr("y", 10)
      .text(d => d.supplier)
      .style("font-size", "11px")
      .style("fill", "#2c3e50");

    // Add title
    svg.append("text")
      .attr("x", width / 2 + margin.left)
      .attr("y", 30)
      .attr("text-anchor", "middle")
      .style("font-size", "16px")
      .style("font-weight", "bold")
      .style("fill", "#2c3e50")
      .text("Supplier Performance Radar Chart");

    // Create tooltip
    const tooltip = d3.select("body").append("div")
      .attr("class", "d3-tooltip")
      .style("position", "absolute")
      .style("background", "rgba(0, 0, 0, 0.9)")
      .style("color", "white")
      .style("padding", "12px")
      .style("border-radius", "6px")
      .style("font-size", "12px")
      .style("pointer-events", "none")
      .style("opacity", 0)
      .style("z-index", "1000");

    // Helper function to wrap text
    function wrap(text, width) {
      text.each(function() {
        const text = d3.select(this);
        const words = text.text().split(/\s+/).reverse();
        let word;
        let line = [];
        let lineNumber = 0;
        const lineHeight = 1.1;
        const y = text.attr("y");
        const dy = parseFloat(text.attr("dy"));
        let tspan = text.text(null).append("tspan").attr("x", 0).attr("y", y).attr("dy", dy + "em");
        
        while (word = words.pop()) {
          line.push(word);
          tspan.text(line.join(" "));
          if (tspan.node().getComputedTextLength() > width) {
            line.pop();
            tspan.text(line.join(" "));
            line = [word];
            tspan = text.append("tspan").attr("x", 0).attr("y", y).attr("dy", ++lineNumber * lineHeight + dy + "em").text(word);
          }
        }
      });
    }
  }

  if (typeof module !== 'undefined' && module.exports) {
    module.exports = renderRadarChart;
  } else {
    this.radarChart = renderRadarChart;
  }
})();