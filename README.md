# Purpose
Simple app to explore the contents exposed by the public API of Statistics Sweden (www.scb.se/en), a government agency responsible for producing official statistics regarding Sweden.
# How to Use
## Installation
Copy `index.html` and `scbclient.js` to a directory of your choice. Then direct your favourite browser at `index.html`.
## Instructions
Select first language and then topics until a page with possible filter values is shown. Select at least one value per filter and then press *Submit*. A table with the results is shown.

If the results only contain numerical data and only one type of value, e.g. average or maximum, it can be plotted in a graph.

At any time you can close the current page and go back to the previous one by pressing the *x* in the top right corner.
# N.B.
The server exposing the API doesn't handle preflight calls well so you'll need to help the browser along a bit by, for instance, installing an extension that handles CORS.
# Planned Improvements
* Cache results
* Make plot interactive: Give details on hover
* Improve query selection: *Select all* for time dimension
## Recently done
* Plot graphs of numeric data
