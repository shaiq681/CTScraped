# CTScraper
An R package for Lab-5    

## _Build Status_

[![Build Status](https://travis-ci.org/shaiq681/CTScraped.svg?branch=main)](https://travis-ci.org/shaiq681/CTScraped)


## Installation

```r
devtools::install_github("shaiq681/CTScraped")
```


## Examples

### Count Trials

```r
nsclc_cancer <- count_clinical_trials(query = "nsclc AND NOT small cell lung cancer")
```

### Download Trials

```r
covid19_italy <- download_clinical_trials(search_query = "covid19 AND italy")
```
