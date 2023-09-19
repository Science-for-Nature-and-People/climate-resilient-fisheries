

# Overview 

This repository contains the data and scripts used for generating the four main figures featured in our manuscript [Diverse pathways for climate resilience in marine fishery  systems](https://doi.org/10.1111/faf.12790) published in _Fish and Fisheries_ (doi:10.1111/faf.12790). This publication is a product of the SNAPP Working Group on Climate Resilient Fisheries. 

Our associated resilience planning tool is available at: https://ClimateResilientFisheries.net/

## About the SNAPP Working Group on Climate Resilient Fisheries

The [SNAPP Working Group on Climate Resilient Fisheries](https://snappartnership.net/teams/climate-resilient-fisheries/) was led by Kathy Mills (GMRI), Kristin Kleisner (EDF), and Patrick Sullivan (Cornell) and included fisheries experts and practitioners from nearly every continent. The working group's goals were to address the following questions:

1. What key features make fisheries inherently resilient to the effects of climate change?
2. What approaches and tools confer resilience for fishery systems affected by climate change?
3. How can practitioners diagnose system resilience and identify ways to support it to enhance sustainability, economic benefits, and human well-being and equity?

## Repository Breakdown

### 1. clean_data
- This folder contains the cleaned dataset and metadata crucial for our analysis.
   - **case_study_attribute_score_data** - Dataset Description:
     - **case_study**: Represents one of 18 fishery system case studies across diverse geographies, target species, fishery scales, and management contexts.
     - **attribute**: Describes one of the 38 specific attributes assessed for resilience against climate stressors.
     - **score**: Indicates the strength of each attribute within a system, recorded on a 4-option scale: very low (1), low (2), moderate (3), high (4).
     - **dimension**: Categorizes the broader dimension or category to which an attribute belongs.

*Note: Derived metrics are annotated in the code.*

### 2. figures
- This folder contains graphical representations derived from our data, which includes the visualizations for the main manuscript's four figures.

### 3. scripts
- This folder contains R scripts used for analysis and visualization, generating the manuscript's four main figures.

*Note*: If interested in supplemental analyses and figures, please contact us. We can provide additional code or data as needed. Check our associated publication, *Mason et al. 2023*, [here](https://doi.org/10.1111/faf.12630).
