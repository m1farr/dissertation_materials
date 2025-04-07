# dissertation_materials: Overview

*Please note this is very much a **work in progress**.*

This repository contains all of the data wrangling (and eventually, modelling) 
for my economics dissertation on legal origin theory.

At the moment, the repo only contains information relating to the econometric 
portion of the dissertation, but it may expand to include 
data for my case study.

## To Do
- Fix name changing procedure so that it adheres to WDI naming conventions
rather than the La Porta et al. (2008) names
- Create output code such that updated variables are first added to table
within the project, and then the table as a whole saved 


## Code

`99_functions.R ` defines various functions that I wrote to streamline 
the coding process.

`00_dependencies.R` loads all of the additional packages that I utilized 
in this project. 

`01_load_files.R` loads all of the datasets contained in the data folder 
and additionally cleans them in preparation for pulling the variables.

`02_process_data.R` pulls the updated versions of all variables from the 
appropriate dataset.

`03_produce_output.R` creates the updated versions of all tables from 
the original paper by combining the variables in the appropriate tables. 
Any additional plots or tables will also be found here.

## Data

### Legal Origins
The econometric portion of the dissertation is based on the dataset from the 
seminal paper **"The Economic Consequences of Legal Origins"** by La Porta et 
al. (2008). Their original dataset is included here, and I will be updating 
variables with additional datasets and work. 

Their methodology relies on variables defined or constructed in the authors' 
previous works. Full citations of those papers is forthcoming.

The datasets used to update their variables so far include:
- Bayer, M., Croissant, A., Izadi, R., and Scheeder, N. (2023) Multidimensional
Measures of Militarization (m3) – A Global Dataset. Armed Forces and Society.
Available at: https://m3-militarization.com/. (Accessed: 1 April 2025)
- Haerpfer, C., Inglehart, R., Moreno, A., Welzel, C., Kizilova, K.,
Diez-Medrano J., M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.) 2022. World Values Survey Trend File (1981- 2022) Cross-National Data-Set.
Madrid, Spain & Vienna, Austria: JD Systems Institute & WVSA Secretariat. Data
File Version 4.0.0, doi:10.14281/18241.27
- Heritage Foundation (2025) Index of Economic Freedom. Available at:
https://www.heritage.org/index/ (Accessed: 1 April 2025).
- Hofstede, G. (2015) Dimension data matrix (Version 2015 12 08). Available
at: https://geerthofstede.com/research-and-vsm/dimension-data-matrix/. (Accessed:
1 April 2025). 
- Inter-American Development Bank (2021) Database of Political Institutions 2020. Available at: https://publications.iadb.org/en/database-political-institutions-2020-dpi2020 (Accessed: 1 April 2025). 
- International Labour Organization (2025) ILOSTAT - Industrial Relations Data. Available at: https://ilostat.ilo.org/topics/industrial-relations/ (Accessed: 1 April 2025).
- Panizza, U. (2024) ‘Bank ownership around the world’, Journal of Banking and Finance. Available at: https://www.upanizza.com/general-4.
- World Bank (2020) Doing Business 2020. Available at: https://databank.worldbank.org/source/doing-business (Accessed: 1 April 2025).
- World Bank (2025) World Development Indicators. Available at: https://databank.worldbank.org/source/world-development-indicators (Accessed: 1 April 2025).
- World Bank (2025) Worldwide Governance Indicators. Available at: https://info.worldbank.org/governance/wgi/ (Accessed: 1 April 2025).
