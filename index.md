---
title: 'Developing Data Products'
subtitle    : 'Dynamic Sankey Diagram'
author      : "Chris Walsh"
job         : "July 15, 2017"
framework   : revealjs      # {io2012, html5slides, shower, dzslides, ...}
revealjs    :
  theme: night
  transition: convex
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

<style>
.reveal p {
    font-size: .75em;
}

.reveal small {
    width: 500px;
}

.reveal .slides {
    text-align: left;
}

.reveal .roll {
    vertical-align: text-bottom;
}

code {
    color: red;
}

.reveal pre code { 
     height: 250px;
}
</style>



## The Assignment

This assignment consisted of two parts:

1. An **R Shiny** application, complete with input, reactive output, and associated documentation

2. A **Reproducible Pitch Presentation** in R Studio Presenter, complete with embedded R code


<br>
<br>
 
### The Links

1. The Shiny application: [link here](https://cwalsh.shinyapps.io/developing_data_products_dynamic_sankey_diagram/)

2. The Pitch Presentation: [link here](https://cwalsh.shinyapps.io/developing_data_products_dynamic_sankey_diagram/)


---


## The Use Case

This application was designed to provide insight into the flow of observations through a complex, multi-step workflow. The inital tab provides a Sankey Diagram to help **visualize flow through the system**. The diagram is reactive, and as cases are filtered on their underlying characteristics (e.g. priority level, expected value) the Sankey will update as well. The second tab includes a reactive bar chart that provides **insight into case outcomes** (e.g. cost and revenue). 

This application is a proof of concept and all underyling data was simulated.


---

## The Sankey

<img src="figure/Sankey-1.png" title="plot of chunk Sankey" alt="plot of chunk Sankey" style="display: block; margin: auto;" />

The **Sankey Diagram** allows users to visualize the flow of cases between steps in the workflow based on the reactive filters applied. In the example above we can see how cases start from a common point but eventually diverge towards *Mid2* and *Mid3*.

---

## The Bar Chart

<img src="figure/Bar-1.png" title="plot of chunk Bar" alt="plot of chunk Bar" style="display: block; margin: auto;" />

The **Bar Chart** allows users to understand the costs and revenues associated with the cases selected by the reactive filters. In the example above we can see the cases that end at *Node 2* (Mid2) resulted in the greatest cost to work. This is likely driven by their volume, which can be observed on the previous Sankey Diagram.
