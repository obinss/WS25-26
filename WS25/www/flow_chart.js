
(function() {
  if (typeof d3 === "undefined") {
    throw new Error("d3 is required");
  }

  function renderFlowChart(data, svg, options) {
    const margin = { top: 20, right: 30, bottom: 40, left: 40 };
    const width = options.width - margin.left - margin.right;
    const height = options.height - margin.top - margin.bottom;

    svg.selectAll("*").remove();

    const g = svg.append("g")
      .attr("transform", `translate(${margin.left},${margin.top})`);

    const x = d3.scaleBand()
      .domain(data.map(d => d.stage))
      .range([0, width])
      .padding(0.3);

    const y = d3.scaleLinear()
      .domain([0, d3.max(data, d => d.count)])
      .nice()
      .range([height, 0]);

    const color = d3.scaleOrdinal()
      .domain(data.map(d => d.stage))
      .range(["#3498db", "#2ecc71", "#f39c12", "#9b59b6"]);

    // Add bars
    g.selectAll("rect")
      .data(data)
      .enter().append("rect")
      .attr("x", d => x(d.stage))
      .attr("y", d => y(d.count))
      .attr("width", x.bandwidth())
      .attr("height", d => height - y(d.count))
      .attr("fill", d => color(d.stage))
      .attr("rx", 5)
      .attr("ry", 5);

    // Add value labels on bars
    g.selectAll("text")
      .data(data)
      .enter().append("text")
      .attr("x", d => x(d.stage) + x.bandwidth() / 2)
      .attr("y", d => y(d.count) - 10)
      .attr("text-anchor", "middle")
      .text(d => d.count)
      .style("font-size", "14px")
      .style("font-weight", "bold")
      .style("fill", "#2c3e50");

    // Add stage labels
    g.append("g")
      .attr("transform", `translate(0,${height})`)
      .call(d3.axisBottom(x))
      .selectAll("text")
      .style("text-anchor", "middle")
      .style("font-size", "12px");

    g.append("g")
      .call(d3.axisLeft(y));

    // Add title
    g.append("text")
      .attr("x", width / 2)
      .attr("y", -5)
      .attr("text-anchor", "middle")
      .style("font-size", "16px")
      .style("font-weight", "bold")
      .style("fill", "#2c3e50")
      .text("Patient Flow Through Registry Stages");
  }

  if (typeof module !== "undefined" && module.exports) {
    module.exports = renderFlowChart;
  } else {
    this.flowChart = renderFlowChart;
  }
})();

