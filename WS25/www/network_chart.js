// network_chart.js - For implant manufacturers network
(function() {
  if (typeof d3 === 'undefined') {
    throw new Error('d3 is required');
  }

  function renderNetworkChart(data, svg, options) {
    const width = options.width;
    const height = options.height;

    // Clear previous content
    svg.selectAll("*").remove();

    // Create force simulation
    const simulation = d3.forceSimulation(data)
      .force("charge", d3.forceManyBody().strength(-1000))
      .force("center", d3.forceCenter(width / 2, height / 2))
      .force("collision", d3.forceCollide().radius(d => Math.sqrt(d.n) * 8 + 30));

    // Create nodes based on data
    const nodes = data.map((d, i) => ({
      id: d.cup_manufucturer,
      group: i,
      value: d.n,
      radius: Math.sqrt(d.n) * 8 + 20
    }));

    // Create links between nodes (simplified circular layout)
    const links = [];
    for (let i = 0; i < nodes.length - 1; i++) {
      links.push({
        source: nodes[i].id,
        target: nodes[i + 1].id,
        value: 1
      });
    }

    // Update simulation with nodes and links
    simulation.nodes(nodes);
    simulation.force("link", d3.forceLink(links).id(d => d.id).distance(100));

    // Create color scale
    const color = d3.scaleOrdinal(d3.schemeCategory10);

    // Draw links
    const link = svg.append("g")
      .selectAll("line")
      .data(links)
      .enter().append("line")
      .attr("stroke", "#bdc3c7")
      .attr("stroke-width", 2)
      .attr("stroke-opacity", 0.6);

    // Draw nodes
    const node = svg.append("g")
      .selectAll("circle")
      .data(nodes)
      .enter().append("circle")
      .attr("r", d => d.radius)
      .attr("fill", d => color(d.id))
      .attr("stroke", "#fff")
      .attr("stroke-width", 2)
      .style("cursor", "pointer")
      .call(d3.drag()
        .on("start", dragstarted)
        .on("drag", dragged)
        .on("end", dragended))
      .on("click", function(event, d) {
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue("manufacturer_click", d.id);
        }
      })
      .on("mouseover", function(event, d) {
        d3.select(this)
          .transition()
          .duration(200)
          .attr("stroke-width", 4)
          .attr("stroke", "#2c3e50");
        
        tooltip.style("opacity", 1)
          .html(`
            <strong>${d.id}</strong><br/>
            Implants: ${d.value}<br/>
            Market Share: ${((d.value / d3.sum(nodes, n => n.value)) * 100).toFixed(1)}%
          `)
          .style("left", (event.pageX + 15) + "px")
          .style("top", (event.pageY - 28) + "px");
      })
      .on("mouseout", function(event, d) {
        d3.select(this)
          .transition()
          .duration(200)
          .attr("stroke-width", 2)
          .attr("stroke", "#fff");
        
        tooltip.style("opacity", 0);
      });

    // Add manufacturer labels
    const label = svg.append("g")
      .selectAll("text")
      .data(nodes)
      .enter().append("text")
      .text(d => d.id)
      .attr("text-anchor", "middle")
      .attr("dy", ".35em")
      .style("font-size", d => Math.max(10, Math.min(14, d.radius / 3)))
      .style("font-weight", "bold")
      .style("fill", "white")
      .style("pointer-events", "none");

    // Update positions on simulation tick
    simulation.on("tick", () => {
      link
        .attr("x1", d => d.source.x)
        .attr("y1", d => d.source.y)
        .attr("x2", d => d.target.x)
        .attr("y2", d => d.target.y);

      node
        .attr("cx", d => d.x)
        .attr("cy", d => d.y);

      label
        .attr("x", d => d.x)
        .attr("y", d => d.y);
    });

    // Drag functions
    function dragstarted(event, d) {
      if (!event.active) simulation.alphaTarget(0.3).restart();
      d.fx = d.x;
      d.fy = d.y;
    }

    function dragged(event, d) {
      d.fx = event.x;
      d.fy = event.y;
    }

    function dragended(event, d) {
      if (!event.active) simulation.alphaTarget(0);
      d.fx = null;
      d.fy = null;
    }

    // Add title
    svg.append("text")
      .attr("x", width / 2)
      .attr("y", 30)
      .attr("text-anchor", "middle")
      .style("font-size", "16px")
      .style("font-weight", "bold")
      .style("fill", "#2c3e50")
      .text("Implant Manufacturers Network");

    // Add subtitle
    svg.append("text")
      .attr("x", width / 2)
      .attr("y", 50)
      .attr("text-anchor", "middle")
      .style("font-size", "12px")
      .style("fill", "#7f8c8d")
      .text("Drag nodes to explore • Click to filter");

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
    module.exports = renderNetworkChart;
  } else {
    this.networkChart = renderNetworkChart;
  }
})();