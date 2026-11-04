# Combine multiplier tables

Combines output, income, labour, and SUB multiplier tables into a single
tibble.

## Usage

``` r
combine_multipliers(OM, IM, LM, SUBM)
```

## Arguments

- OM:

  Output from \[calc_output_multiplier()\].

- IM:

  Output from \[calc_income_multiplier()\].

- LM:

  Output from \[calc_labour_multiplier()\].

- SUBM:

  Output from \[calc_sub_multiplier()\].

## Value

A tibble with columns: SECTOR, CATEGORY, Out.multiplier, Inc.multiplier,
Lab.multiplier, SUB.multiplier.
