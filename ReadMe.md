
## Analysis of street access and development in sub-Saharan Africa


### Block complexity aggregation and visualization:

* [complexity-analysis.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/complexity-analysis.R) aggregates the block level data and generates several visualizations used in the analysis. The script uses [aggregation_func.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/aggregation_func.R) to facilitate aggregation from the the block level to higher geographic scales.
* [complexity-analysis.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/complexity-analysis.R) will automatically download several GB of block level data and add it to folders within the top level of the repo. The database is available at the website [millionneighborhoods.africa](https://www.millionneighborhoods.africa/download) with a data dictionary available [here](https://docs.google.com/spreadsheets/d/1S35EhQcmmHtzuPUkiVllzQOVtpdjkdsHpqvMTJtagi8/). The script also uses the following files in this repo: [data
/buildings_lusaka.geojson](https://github.com/mansueto-institute/kblock-analysis/blob/main/data/buildings_lusaka.geojson), [data/streets_lusaka.geojson](https://github.com/mansueto-institute/kblock-analysis/blob/main/data/streets_lusaka.geojson). Data sources and methods for generating the block level population and informality data are available in the [mansueto-institute/kblock](https://github.com/mansueto-institute/kblock) repo.

### Demographic and Health Survey (DHS) statistical analysis:

* [dhs-analysis.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/dhs-analysis.R) downloads [DHS data](https://dhsprogram.com/) via an API connection, spatial joins DHS administrative regions to the block level database, aggregates blocks statistics to the DHS regions, and then performs a statistical analysis that runs correlations, PCA, and regressions on the relationships between block statistics and social development indicators corresponding to human well-being and household characteristics.
* [dhs-analysis.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/dhs-analysis.R) must be run after the step in the [complexity-analysis.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/complexity-analysis.R) that downloads the block level database. When running on your own computer please be sure to register and add your own [DHS login credentials](https://github.com/mansueto-institute/kblock-analysis/blob/main/dhs-analysis.R#L66-L67) to the script (this requires undergoing a dataset access approval process). The pre-processing steps that involve API calls to the DHS website and spatial joins may take an hour of processing time. This script also uses data downloaded from [data/un-habitat](https://github.com/mansueto-institute/kblock-analysis/tree/main/data/un-habitat). To account for street densities this script uses [streets_dhs_regions.csv](https://github.com/mansueto-institute/kblock-analysis/blob/main/data/streets_dhs_regions.csv).

### Block complexity graph visualizations:

* [graph-viz.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/graph-viz.R) visualizes block complexity in the format of a network graph. This script uses the [graph_funcs.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/graph_funcs.R) to generate a complexity graph. These functions were specifically designed for the R programming language to replicate the basic functionality of the kblock repo, which performs the actual method used in the scientific article and associated datasets.
* Below are functions applied in the [graph-viz.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/graph-viz.R) script which demonstrate how to compute block complexity graphs and visualize parcel level diagrams. Again these methods were designed for heuristic purposes and not used in production at scale.
  ```
  # Function to generate graph using building parcels (graph_polys) and on-network streets that connect interior buildings to exterior roads (graph_exclude)
  graph_lines <- generate_graph(graph_polys = building_parcels_polygons, graph_exclude = street_linestrings)
  # Function to convert graph to polygon representation
  graph_polys <- convert_to_poly(lines = graph_lines)
  # Function to compute block complexity at the parcel level such that each row to the output represents each subsequent interior layer of buildings. 
  graph_duals <- extract_duals(graph_sf = graph_polys)
  ```
* The script uses [data/layer_lusaka.geojson](https://github.com/mansueto-institute/kblock-analysis/blob/main/data/layer_lusaka.geojson), which contains data for a community area in Lusaka, Zambia. 

### Validation analysis:
* To ground-truth block complexity statistics use [sdi-analysis.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/sdi-analysis.R) which compares block geometries to SDI settlements. Note, the data requires special access permissions. Contact authors for more information.
* To validate the completeness of OSM street data use [street-analysis.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/street-analysis.R) which utilizes three data files: [streets_metrics.parquet](https://github.com/mansueto-institute/kblock-analysis/blob/main/data/streets_metrics.parquet), [streets_summary.csv](https://github.com/mansueto-institute/kblock-analysis/blob/main/data/streets_summary.csv), and [streets_validation.csv](https://github.com/mansueto-institute/kblock-analysis/blob/main/data/streets_validation.csv).
* To compare block complexity performance to alternative predictors of wealth, see these [statistical comparisons](https://gist.github.com/nmarchio/a03ebe075489f57eb157799578a62646) with [Blumenstock et al. (2022)](https://www.pnas.org/doi/10.1073/pnas.2113658119) and [Yeh et al. (2020)](https://www.nature.com/articles/s41467-020-16185-w#Fig3)
* To test the fit of the following distributions to different geographies use [distribution-analysis.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/distribution-analysis.R). For building footprint area distribution analysis use [bldg-area-analysis.R](https://github.com/mansueto-institute/kblock-analysis/blob/main/bldg-area-analysis.R).


### Workflow diagram:
```mermaid
graph LR
G[DHS API] --> C
A[africa_data.parquet] --> B[Step 1: complexity-analysis.R]
M[africa_geodata.parquet] --> B
M[africa_geodata.parquet] --> C

Q[Step 0: kblock.git] --> M

E[aggregation_func.R] --> B
A --> C[Step 2: dhs-analysis.R]
B --> Z[Summary stats and<br>visualizations]
C --> X[Statistical analysis and<br>visualizations]
Q[Step 0: kblock.git] --> A

F[graph_funcs.R] --> D[Step 3: graph-viz.R]
J[layer_lusaka.geojson] --> D

K[buildings_lusaka.geojson] --> B
L[streets_lusaka.geojson] --> B

D --> Y[Complexity graph<br>visualization]

style A fill:#eba8d3
style G fill:#eba8d3
style J fill:#eba8d3
style K fill:#eba8d3
style L fill:#eba8d3

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
Nicholas Marchio, data scientist at the Mansueto Institute. For any technical inquiries please feel free to create a Git issue and tag `nmarchio`. DOI-minted version of this repo is available at [https://zenodo.org/records/15702173](https://zenodo.org/records/15702173).

For related technical work see the following repos:
* [kblock](https://github.com/mansueto-institute/kblock): Python codebase for generating the underlying block complexity and population data available at [millionneighborhoods.africa](millionneighborhoods.africa). Data is also available at [https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DQY54U](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DQY54U).
* [geopull](https://github.com/mansueto-institute/geopull): Python package for extracting OSM data and generating street block delineations.
* [cloudtile](https://github.com/mansueto-institute/cloudtile): CLI for converting (Geo)Parquet files to PMTiles on AWS.
* [prclz](https://github.com/mansueto-institute/prclz): Python codebase from previous iteration of block complexity research (see kblock for more recent version).

#### Restricted data dependencies for SDI and DHS (contact author for more info)
* [Google drive](https://drive.google.com/drive/folders/1MwfLHhuV2XkaJDFCK7LBondEJZvJgeDj) with restricted data for full reproducibility for analysis.
* [UChicago Box](https://uchicago.box.com/s/42oouket4lu6xdt2t9u8gfyz8bifrq6x) with restricted data for full reproducibility for analysis.
* [Google drive](https://drive.google.com/drive/folders/1hVd4B2Dkl9lN8ZI6KV86BFPUnD7PlQDV?usp=drive_link) with data for minimal reproducible example of block complexity metrics.
