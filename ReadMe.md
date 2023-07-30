
## Analysis of street access and development in sub-Saharan Africa


### Block complexity aggregation and visualization:

* [complexity-analysis.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/complexity-analysis.R) aggregates the block level data and generates several visualizations used in the analysis. When running on your own computer please be sure to change directory paths hardcoded at the beginning of the file.
* [aggregation_func.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/aggregation_func.R) is a function to facilitate aggregation from the the block level to higher geographic scales.
* [millionneighborhoods.africa/download](https://www.millionneighborhoods.africa/download) is the online source for the block-level database used in this analysis.

### Demographic and Health Survey (DHS) statistical analysis:

* [dhs-analysis.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/dhs-analysis.R) downloads [DHS data](https://dhsprogram.com/) via an API connection, joins to the block level database (referenced above), and performs a statistical analysis that runs correlations, PCA, and regressions on the relationships between block complexity and social development indicators corresponding to human well-being and household characteristics. When running on your own computer please be sure to change directory paths hardcoded at the beginning of the file and add your own DHS login credentials (this requires undergoing a dataset access approval process).

### Block complexity graph visualizations:

* [graph-viz.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/graph-viz.R) visualizes block complexity in the format of a network graph.  When running on your own computer please be sure to change directory paths hardcoded at the beginning of the file.
* [graph_funcs.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/graph_funcs.R) contains functions to generate a complexity graph.
* [layer_lusaka.geojson](https://github.com/mansueto-institute/kblock-analysis/blob/main/data/layer_lusaka.geojson) contains data for a community area in Lusaka, Zambia. 

### Workflow diagram:
```mermaid
graph LR
G[DHS API] --> C
A[africa_data.parquet] --> B[complexity-analysis.R]
E[aggregation_func.R] --> B
A --> C[dhs-analysis.R]
B --> Z[Summary stats and<br>visualizations]
C --> X[Statistical analysis and<br>visualizations]
Q[kblock.git] --> A
Q[kblock.git] --> J

F[graph_funcs.R] --> D[graph-viz.R]
J[layer_lusaka.geojson] --> D
D --> Y[Complexity graph<br>visualization]

style A fill:#eba8d3
style G fill:#eba8d3
style J fill:#eba8d3

style Z fill: #f7f5eb
style X fill: #f7f5eb
style Y fill: #f7f5eb

style F fill: #ADD8E6
style E fill: #ADD8E6

style Q fill: #b1d8b7
```

## Contact 
Nicholas Marchio, data scientist at the Mansueto Institute

For related technical work see the following repos:
* [kblock](https://github.com/mansueto-institute/kblock): Python codebase for generating the block complexity and population data available at [millionneighborhoods.africa](millionneighborhoods.africa).
* [geopull](https://github.com/mansueto-institute/geopull): Python package for extracting OSM data and generating street block delineations.
* [cloudtile](https://github.com/mansueto-institute/cloudtile): CLI for converting (Geo)Parquet files to PMTiles on AWS.
* [prclz](https://github.com/mansueto-institute/prclz): Python codebase from previous iteration of block complexity research (see kblock for more recent version).
