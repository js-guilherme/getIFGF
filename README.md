# getIFGF

R package to access and manipulate municipal and state fiscal management data from the **FIRJAN Fiscal Management Index (IFGF)**, provided by [FIRJAN – Federação das Indústrias do Estado do Rio de Janeiro](https://www.firjan.com.br/ifgf/analises-e-rankings/).


## **Installation**

```r
install.packages("remotes")
remotes::install_github("js-guilherme/getIFGF")
```
## **Using**

### **get_IFGF()**

Downloads, cleans, and filters data from the most recent **FIRJAN IFGF edition** directly from the official FIRJAN website.

The function automatically:

-   Scrapes the latest IFGF `.xlsx` file from FIRJAN;
    
-   Downloads and reads all indicator sheets;
    
-   Transforms the data into tidy format;
    
-   Adds regional information (N, NE, CO, SE, S);
    
-   Caches the cleaned dataset in a temporary directory for faster future access.
    

----------

### **Arguments**

| Argument | Description | Default |
| --- | ----------------- | --------------------------------- |
| `year` | Year of reference | All available years |
| `city` | Municipality code | All municipalities |
| `uf` | State abbreviation (e.g., `"RJ"`, `"SP"`) | All states |
| `region` | Region abbreviation (`"N"`, `"NE"`, `"CO"`, `"SE"`, `"S"`) | All regions |
| `indicator` | Indicator code (`"GP"`, `"AT"`, `"IN"`, `"LI"`, `"IG"`) | All indicators |
| `ranking` | `TRUE` to include ranking columns (state/national) | `FALSE` |

### **Indicators**

| Code | Indicator | Description |
| ---- | ----------------- | --------------------------------- |
| `GP` | Gasto com Pessoal | Personnel expenditure management |
| `AT` | Autonomia | Revenue autonomy |
| `IN` | Investimentos | Investment capacity |
| `LI` | Liquidez | Liquidity and debt sustainability |
| `IG` | IFGF Global | Overall IFGF composite index |

### **Examples**

```r
# Load all available data
library(ifgfR)

# Fiscal data for the state of Rio de Janeiro (RJ) in 2022
ifgf_rj <- get_IFGF(year = 2022, uf = "RJ")

# Fiscal indicator "Gasto com Pessoal" (GP) for the city of São Paulo in 2022
ifgf_gp <- get_IFGF(indicator = "GP") ifgf_rj <- get_IFGF(year = 2022, city = 3550308, indicator = "GP")

# IFGF data including rankings (state and national)
ifgf_rank <- get_IFGF(ranking = TRUE)
```
### **Returned Data Structure**

| Column | Description |
| ------------------ | ----------------------------------------------- |
| `Ano` | Reference year |
| `Código` | Municipality IBGE code |
| `Região` | Geographic region (`N`, `NE`, `CO`, `SE`, `S`) |
| `UF` | State abbreviation |
| `Município` | Municipality name |
| `Indicador` | IFGF indicator name |
| `Valor` | Index value (numeric) |
| `Ranking Estadual` | Position within the state (if `ranking = TRUE`) |
| `Ranking Geral` | National ranking (if `ranking = TRUE`) |

## **Resources**

-   [FIRJAN IFGF Official Page](https://www.firjan.com.br/ifgf/analises-e-rankings/)
-   [FIRJAN IFGF Methodology (pt-br)](https://www.firjan.com.br/ifgf/metodologia/)

## **License**

This project is released under the **MIT License**.
