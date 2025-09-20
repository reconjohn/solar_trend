# Analyzing siting trends of utility-scale solar 


---

## Project Workflow

The analysis is structured as a sequential pipeline. The scripts are intended to be run in the following order to reproduce the analysis, from initial data processing to final figure generation.

1.  **Setup & Data Preparation**:
    * Processes interconnection queue data to identify matched substations (`solar_que_sub.R`).
    * Identifies the nearest substations for existing solar projects (`solar_sub.R`).
    * Rasterizes the solar project location data (`solar_inter.R`).

2.  **Predictor and Model Matrix Generation**:
    * Samples pseudo-absence locations for the regression models (`absence.R`).
    * Combines all predictor layers (e.g., climate risk, land use, policy incentives) into a single raster stack (`stack.R`).
    * Performs zonal statistics to extract predictor values for all presence and pseudo-absence locations (`zonal.R`).
    * Prepares dataset for modeling (`modeling_solar.R`).

3.  **Modeling & Analysis**:
    * Loads all necessary libraries and custom functions (`function.R`).
    * Run models, and generates the foundational datasets required for the analysis (`prep.R`).

4.  **Visualization & Output**:
    * Generates and saves the main figures for the publication (`code.R`).
    * Generates and saves all supplementary figures (`SI.R`).

---

## Script Descriptions

### Core Scripts
* **`function.R`**: The foundational setup script. It loads all required R packages, defines custom functions used throughout the analysis, and sets global environment options.
* **`prep.R`**: Orchestrates the initial data generation pipeline, creating a complete and clean dataset for the modeling phase.
* **`modeling_solar.R`**: This script takes the processed data to prepare regression models.

### Data Processing Scripts
* **`solar_que_sub.R`**: Extracts substation locations that are explicitly matched with projects in the interconnection queue data.
* **`solar_sub.R`**: Complements the above by identifying the nearest substation to existing solar projects as a proxy for grid accessibility.
* **`solar_inter.R`**: Rasterizes the solar project data, transforming vector points/polygons to a raster format for alignment with predictor layers.
* **`absence.R`**: Performs pseudo-absence sampling to create a contrastive baseline for the model.
* **`stack.R`**: Creates a unified raster "stack" by combining multiple predictor variable files into a single object.
* **`zonal.R`**: Extracts the predictor variable values for each presence and pseudo-absence point.

### Figure Generation Scripts
* **`code.R`**: Contains all the plotting code to create the final figures for the main body of the manuscript.
* **`SI.R`**: Dedicated to producing all visual outputs for the Supplementary Information (SI) section.

---

## How to Run the Analysis

1.  **Dependencies**: The complete list of required R packages is located at the top of the **`function.R`** script.

2.  **Running the Analysis**:
    * Begin by running the **`prep.R`** script to generate the necessary datasets.
    * Follow the sequence outlined in the **Project Workflow** section.
    * After running the models, execute **`code.R`** and **`SI.R`** to generate the final outputs.

---

## Repository Structure


.
├── syntax/
│   ├── function.R
│   ├── prep.R
│   ├── solar_que_sub.R
│   ├── solar_sub.R
│   ├── solar_inter.R
│   ├── absence.R
│   ├── stack.R
│   ├── zonal.R
│   ├── modeling_solar.R
│   ├── code.R
│   └── SI.R
│
├── data/
│   └── ... (Input data files)
│
└── figures/
└── ... (Output figures)