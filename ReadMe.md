
## Analysis of street access and development in sub-Saharan Africa


### Block complexity aggregation and visualization:

* [complexity-analysis.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/complexity-analysis.R) aggregates the block level data and generates several visualizations used in the analysis. When running on your own computer please be sure to change directory paths hardcoded at the beginning of the file.
* [aggregation_func.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/aggregation_func.R) is a function to facilitate aggregation from the the block level to higher geographic scales.
* To replicate the analysis be sure to download the data referenced at this [line](https://github.com/mansueto-institute/kblock-analysis/blob/main/complexity-analysis.R#L33) and add to your local directory. [millionneighborhoods.africa](https://www.millionneighborhoods.africa/download) is the online source for the block-level database.

### Demographic and Health Survey (DHS) statistical analysis:

* [dhs-analysis.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/dhs-analysis.R) downloads [DHS data](https://dhsprogram.com/) via an API connection, joins to the block level database, and performs a statistical analysis that runs correlations, PCA, and regressions on the relationships between block complexity and social development indicators corresponding to human well-being and household characteristics.
* When running on your own computer please be sure to confirm the directory paths are correct and add your own [DHS login credentials](https://github.com/mansueto-institute/kblock-analysis/blob/main/dhs-analysis.R#L39) to the script (this requires undergoing a dataset access approval process). Also be sure to download the data referenced at these [lines](https://github.com/mansueto-institute/kblock-analysis/blob/main/dhs-analysis.R#L25) and add to your local directory.

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

## Details on R environment for replication

* R version 4.1.2 (2021-11-01)
* Platform: aarch64-apple-darwin20 (64-bit)
* Running under: macOS 13.4.1

| Package | Version | | Package | Version | | Package | Version |
|---|---|- |---|---|- |---|---|
| tidyverse | 1.3.1 | | broom | 1.0.3 | | readxl | 1.3.1 | 
| sf | 1.0-8 | | betareg | 3.1-4 | | osmdata | 0.1.9 |
| units | 0.8-0 | | ggpmisc | 0.5.2 | | Hmisc | 4.8-0 | 
| viridis | 0.6.2 | | kableExtra | 1.3.4 | | scatterpie | 0.1.7 |
| patchwork | 1.1.1 | | xtable | 1.8-4 | | tidymodels | 0.2.0 | 
| scales | 1.2.1 | | ggplot2 | 3.4.1 | | ggrepel | 0.9.3 |
| rdhs | 0.7.5 | | tidygeocoder | 1.0.5 | | readr | 2.1.1 | 
| rmapshaper | 0.4.6 | | writexl | 1.4.0 | | DescTools | 0.99.48 |
| arrow | 7.0.0 | | ggsn | 0.5.0 | | ggpubr | 0.6.0 | 
| sfarrow | 0.4.1 | | geoarrow | 0.0.0.9000 | 


## Contact 
Nicholas Marchio, data scientist at the Mansueto Institute. For any technical inquiries please feel free to create a Git issue and tag `nmarchio`. 

For related technical work see the following repos:
* [kblock](https://github.com/mansueto-institute/kblock): Python codebase for generating the underlying block complexity and population data available at [millionneighborhoods.africa](millionneighborhoods.africa).
* [geopull](https://github.com/mansueto-institute/geopull): Python package for extracting OSM data and generating street block delineations.
* [cloudtile](https://github.com/mansueto-institute/cloudtile): CLI for converting (Geo)Parquet files to PMTiles on AWS.
* [prclz](https://github.com/mansueto-institute/prclz): Python codebase from previous iteration of block complexity research (see kblock for more recent version).
