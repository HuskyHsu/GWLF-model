function plot(init) {

    const {data} = init;

    const margin = { top: 20, right: 30, bottom: 80, left: 50 },
        width = document.querySelector('.plot').clientWidth - margin.left - margin.right - 40,
        height = document.querySelector('.table').clientHeight - margin.top - margin.bottom - 40;

    const x = d3.scaleTime().range([0, width]);
    const y = d3.scaleLinear().range([height, 0]);

    const valueline = d3.line()
        .x( d => x(d.date) )
        .y( d => y(d.Q + d.G) );

    const svg = d3.select("body svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    // Scale the range of the data
    x.domain(d3.extent(data, d => d.date ));
    y.domain([0, d3.max(data, d => d.Q + d.G )]);

    svg.append("path")
        .data([data])
        .attr("class", "line")
        .attr("d", valueline);

    // Add the X Axis
    svg.append("g")
        .attr("class", "axis")
        .attr("transform", "translate(0," + height + ")")
        .call(d3.axisBottom(x)
            .tickFormat(d3.timeFormat("%Y-%m")))
        .selectAll("text")
        .style("text-anchor", "end")
        .attr("dx", "-.8em")
        .attr("dy", "-0.5em")
        .attr("transform", "rotate(-90)");

    // Add the Y Axis
    svg.append("g")
        .attr("class", "axis")
        .call(d3.axisLeft(y));

    // text label for the x axis
    svg.append("text")
        .attr("transform", "translate(" + (width / 2) + " ," + (height + margin.top + 50) + ")")
        .style("text-anchor", "middle")
        .text("日期");

    // text label for the y axis
    svg.append("text")
        .attr("transform", "rotate(-90)")
        .attr("y", 0 - margin.left)
        .attr("x", 0 - margin.top  - 20)
        .attr("dy", "1em")
        .style("text-anchor", "middle")
        .text("逕流深度(cm)");  

}