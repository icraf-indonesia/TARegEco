# Create linkage table

Creates a tibble containing direct, total, and index linkage values for
each sector.

## Usage

``` r
create_linkages_table(sector, linkage_result)
```

## Arguments

- sector:

  A data frame or tibble with at least two columns: sector name and
  category.

- linkage_result:

  A list returned by \[compute_linkage()\].

## Value

A tibble with columns: ID, SECTOR, CATEGORY, DFL, TFL, ITFL, DBL, TBL,
ITBL, \|TFL-DFL\|, \|TBL-DBL\|.
