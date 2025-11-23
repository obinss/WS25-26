
(function() {
  if (typeof d3 === "undefined") {
    throw new Error("d3 is required");
  }

  function renderBarChart(data, svg, options) {
    const margin = { top: 20, right: 30, bottom: 40, left: 40 };
    const width = options.width - margin.left - margin.right;
    const height = options.height - margin.top - margin.bottom;

    svg.selectAll("*").remove();

    const g = svg.append("g")
      .attr("transform", `translate(${margin.left},${margin.top})`);

    const x = d3.scaleBand()
      .domain(data.map(d => d.joint_type))
      .range([0, width])
      .padding(0.1);

    const y = d3.scaleLinear()
      .domain([0, d3.max(data, d => d.n)])
      .nice()
      .range([height, 0]);

    const color = d3.scaleOrdinal()
      .domain(["Hip Primary", "Hip Revision", "Knee Primary", "Knee Revision"])
      .range(["#3498db", "#2980b9", "#e74c3c", "#c0392b"]);

    g.selectAll("rect")
      .data(data)
      .enter().append("rect")
      .attr("x", d => x(d.joint_type))
      .attr("y", d => y(d.n))
      .attr("width", x.bandwidth())
      .attr("height", d => height - y(d.n))
      .attr("fill", d => color(d.joint_type))
      .style("cursor", "pointer")
      .on("click", function(event, d) {
        if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
          Shiny.setInputValue("joint_click", d.joint_type);
        }
      });

    g.append("g")
      .attr("transform", `translate(0,${height})`)
      .call(d3.axisBottom(x));

    g.append("g")
      .call(d3.axisLeft(y));

    // Add value labels
    g.selectAll("text")
      .data(data)
      .enter().append("text")
      .attr("x", d => x(d.joint_type) + x.bandwidth() / 2)
      .attr("y", d => y(d.n) - 5)
      .attr("text-anchor", "middle")
      .text(d => d.n)
      .style("font-size", "12px")
      .style("font-weight", "bold")
      .style("fill", "#2c3e50");
  }

  if (typeof module !== "undefined" && module.exports) {
    module.exports = renderBarChart;
  } else {
    this.barChart = renderBarChart;
  }
})();

