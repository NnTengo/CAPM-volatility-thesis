# CAPM Volatility Thesis

This repository contains materials for my bachelor thesis:

**Testing the Explanatory Power of CAPM under High- and Low-Volatility Market Conditions**

## Project Overview

This thesis examines whether the explanatory power of the Capital Asset Pricing Model (CAPM) differs between low-volatility and high-volatility market periods.

Instead of estimating only one full-sample CAPM regression, the analysis compares CAPM results across different market environments. The main goal is to see whether the market factor explains industry portfolio returns differently during calmer and more turbulent periods.

## Research Question

**To what extent does the explanatory power of the CAPM differ between low-volatility and high-volatility market periods?**

## Data

The empirical analysis uses monthly data from the Kenneth French Data Library.

The main datasets are:

- Fama-French 10 Industry Portfolios
- Fama-French Research Data Factors

The sample period covers monthly observations from **July 1963 to December 2024**.

## Methodology

The analysis follows these main steps:

1. Calculate excess returns for each industry portfolio.
2. Use the market excess return as the CAPM market factor.
3. Construct volatility regimes using the 12-month rolling standard deviation of market excess returns.
4. Split the sample into low-volatility and high-volatility periods using the median rolling volatility.
5. Estimate separate OLS CAPM regressions for each industry portfolio.
6. Compare adjusted R-squared, alpha, and beta across regimes.

## Main Tools

- R / RStudio
- OLS regression
- Rolling volatility analysis
- Fama-French data
- Excel / data tables

## Current Status

This repository currently contains the thesis draft. Additional files such as R scripts, cleaned datasets, regression outputs, and figures may be added later.

## Author

**Tengizi Naneishvili**  
Bachelor’s in Management Technologies  
Kutaisi International University
