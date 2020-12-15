## Final Project 
### Qestion: 
Does breastfeeding prevent early childhood overweight?
### Data:
[NHANES 2017-2018](https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2017)
#### Variable Used

| Sub-Dataset   | Variables Name   |Variable Description     |
| ------------- |:-------------:|:-------------:|
| [DEMO_J](https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.htm#SDMVPSU)   | RIAGENDR  | Gender
| DEMO_J  | RIDAGEYR  | Age in years at screening |
| DEMO_J  | RIDRETH3 | Race/Hispanic origin w/ NH Asian |
| DEMO_J  | WTMEC2YR | Full sample 2 year MEC exam weight |
| DEMO_J  | SDMVPSU  | Masked variance pseudo-PSU  |
| DEMO_J | SDMVSTRA |  Masked variance pseudo-stratum  |
| [BMX_J](https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.htm) | BMXBMI | Body Mass Index (kg/m**2) |
| [DBQ_J](https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DBQ_J.htm) | DBQ010 | Ever breastfed or fed breastmilk |
| [ECQ_J](https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/ECQ_J.htm#ECQ020)  | ECD070A | Weight at birth, pounds |
| ECQ_J | ECQ020   | Mother smoked when pregnant |

### Files:
- analysis.R
- Fianl_report.Rmd
- Fianl_report.html
