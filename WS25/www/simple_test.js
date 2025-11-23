// simple_test.js
(function() {
  if (typeof d3 === 'undefined') {
    throw new Error('d3 is required');
  }

  function renderSimpleTest(data, svg, options) {
    // Clear previous content
    svg.selectAll("*").remove();
    
    const margin = { top: 20, right: 30, bottom: 40, left: 40 };
    const width = options.width - margin.left - margin.right;
    const height = options.height - margin.top - margin.bottom;

    const g = svg.append("g")
      .attr("transform", `translate(${margin.left},${margin.top})`);

    // If no data, show message
    if (!data || data.length === 0) {
      g.append("text")
        .attr("x", width / 2)
        .attr("y", height / 2)
        .attr("text-anchor", "middle")
        .text("No data available with current filters")
        .style("fill", "#7f8c8d")
        .style("font-size", "14px");
      return;
    }

    // Simple bar chart
    const x = d3.scaleBand()
      .domain(data.map(d => d.county || d.diagnosis || "Category"))
      .range([0, width])
      .padding(0.1);

    const y = d3.scaleLinear()
      .domain([0, d3.max(data, d => d.n)])
      .nice()
      .range([height, 0]);

    g.selectAll("rect")
      .data(data)
      .enter().append("rect")
      .attr("x", d => x(d.county || d.diagnosis || "Category"))
      .attr("y", d => y(d.n))
      .attr("width", x.bandwidth())
      .attr("height", d => height - y(d.n))
      .attr("fill", "#3498db");

    g.append("g")
      .attr("transform", `translate(0,${height})`)
      .call(d3.axisBottom(x));

    g.append("g")
      .call(d3.axisLeft(y));
  }

  if (typeof module !== 'undefined' && module.exports) {
    module.exports = renderSimpleTest;
  } else {
    this.simpleTest = renderSimpleTest;
  }
})();