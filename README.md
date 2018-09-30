# UShoard

The goal of UShoard is to show Data of Sales Volume of US Treasury Bills and Bonds

## Example

This is a basic example which shows  how to aquire US Treasure Bill and Bonds time indexed by 'issue date'

``` r
#' ## issue dates
#' treasuryTRISS10YEARNOTE <- getSymbols("TRISS10YEARNOTE", src = "treasury", auto.assign = FALSE)
#' head(treasuryTRISS10YEARNOTE)
#'
```
This is a basic example which shows  how to aquire US Treasure Bill and Bonds time indexed by 'maturity date'

``` r
#' ## maturity dates
#' treasuryTRMTR10YEARNOTE <- getSymbols("TRMTR10YEARNOTE", src = "treasury", auto.assign = FALSE)
#' head(treasuryTRMTR10YEARNOTE)
#' tail(treasuryTRMTR10YEARNOTE)
```
