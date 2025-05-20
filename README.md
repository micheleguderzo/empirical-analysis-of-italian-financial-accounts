# Empirical analysis of Italian financial accounts – Advanced Econometrics Project

![Language](https://img.shields.io/badge/code-R-blue?logo=r&logoColor=white)
![License](https://img.shields.io/badge/license-MIT-green?logo=open-source-initiative)
![Status](https://img.shields.io/badge/status-finished-success?style=flat&logo=github)

This repository contains the final project for the **Advanced Econometrics** course, carried out during the academic year 2021/2022 at the Faculty of Economic Sciences, University of Warsaw, as part of an Erasmus exchange program.

## 📌 Project Overview

The goal of this project is to analyze the **Italian financial accounts from 1961 to 2011** in order to estimate the **Gross Domestic Product (GDP) for the year 2011**. The study investigates how different macroeconomic variables — such as value added, public spending, investment, import, and export — have contributed to shaping the Italian economy over the last five decades, with a focus on modeling techniques suited for time series data.

The project was a collaborative effort, developed in partnership with a teammate as part of an academic course.

## 🧠 Key Topics

- Time series transformation and differencing  
- Stationarity testing (ADF, PP, KPSS)  
- ARDL (Autoregressive Distributed Lag) modeling  
- ARIMA model construction and comparison  
- Model diagnostics (RESET, Breusch-Pagan, Breusch-Godfrey, Jarque-Bera tests)  
- Model selection using AIC and BIC criteria  

## 🗂️ Repository Structure

├── README.md     # This file  
├── LICENSE       # Project license (MIT)  
├── .gitignore    # Git ignore file for excluded files/folders  
├── code/         # R script for time series analysis, ARDL and ARIMA modeling  
├── data/         # Dataset from Bank of Italy (cleaned Excel file used in the analysis)  
├── plots/        # Graphs and diagnostic plots generated during the analysis  
└── report/       # Final report (PDF written in LaTeX)

## 📊 Dataset

The dataset was collected from the [Bank of Italy’s official historical publication](https://www.bancaditalia.it/pubblicazioni/quaderni-storia/2011-0018/index.html?com.dotmarketing.htmlpage.language=1), edited for the 150th anniversary of the Unification of Italy. The original Excel file contains macroeconomic indicators in current prices from 1861 to 2011.

After data cleaning and dimensional reduction, the final dataset used in the analysis includes:

- **51 observations** (years from 1961 to 2011)  
- **12 variables**, including GDP (market prices), value added, net indirect taxes, imports, exports, consumption, investments, and total uses

## 🛠 Tools & Libraries

- R  
- RStudio  
- LaTeX  
- R packages used:  
  `stargazer`, `lmtest`, `sandwich`, `tseries`, `car`, `haven`, `MASS`, `plm`, `zoo`, `aod`, `htmltools`, `mfx`, `logistf`, `DescTools`, `maxLik`, `pscl`, `ucminf`, `ordinal`, `reshape`, `generalhoslem`, `oglmx`, `brant`, `nnet`, `Formula`, `miscTools`, `mlogit`, `survival`, `AER`, `installr`, `coin`, `vcd`, `vcdExtra`, `censReg`, `truncreg`, `sampleSelection`, `mvtnorm`, `xts`, `fBasics`, `urca`, `fUnitRoots`, `dynlm`, `devtools`, `akima`, `ggplot2`, `viridisLite`, `readxl`, `ARDL`, `dynamac`, `forecast`, `TSstudio`, `rstatix`, `dplyr`, `tidyverse`

## 📄 Final Report

You can read the full report with methodology, regression models, and conclusions in [`report/final_report.pdf`](report/final_report.pdf).

## 👤 Authors

**Michele Guderzo**  
**Valentina Ceccarelli**

## 📝 License

This project is licensed under the MIT License – see the [LICENSE](LICENSE) file for details.

---

*This project was developed for educational purposes only.*
