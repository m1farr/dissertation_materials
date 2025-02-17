# dissertation_materials: Overview

*Please note this is very much a **work in progress**.*

This repository contains all of the data wrangling (and eventually, modelling) 
for my economics dissertation on legal origin theory and pre-colonial 
institutions.

At the moment, the repo only contains information relating to the econometric 
portion of the dissertation, but it may expand to include 
data for my case studies.

## To Do
- Fix "Serbia and Montenegro": original paper had 2 as 1 but data now tracks 
separate countries
- Clean numeric transformations of private credit and market cap
- Create source page loading either all or all main datasets
- Change output: updated variables should first be added to table here, then 
table as a whole saved to output

## Data

### Legal Origins
The econometric portion of the dissertation is based on the dataset from the 
seminal paper **"The Economic Consequences of Legal Origins"** by La Porta et 
al. (2008). Their original dataset is included here, and I will be updating 
variables with additional datasets and work. 

The datasets used to update their variables so far include:
- World Development Indicators (World Bank)
- World Governance Indicators (World Bank)
- Hofstede's Dimensions
- World Values Survey
- Industrial Relations Data (International Labour Organization)

### Pre-colonial Institutions and Institutional Strength
Additionally, I am adding variables for the centralization of pre-colonial 
societies (*stateness*) and institutional strength (via the instrumental 
variable *settler mortality*).

The *stateness* variable is taken from the 2011 paper **"An Institutional Theory
of Direct and Indirect Rule"** by Gerring et al. I will be using their data on 
states within the former British Empire, and using their methodology to 
construct the variable for states outside of that dataset. They construct the
*stateness* variable by finding the average of "Judicial Hierarchy of Local 
Community" scores for all communities within a state, where the state is defined
by Encyclopedia Britannica.

The data used to update this variables is from:
- The Ethnographic Atlas (Murdock et al., 1999)

The *settler mortality* variable is taken from the 2001 paper 
**"The Colonial Origins of Comparative Development"** by Acemoglu et al. 
