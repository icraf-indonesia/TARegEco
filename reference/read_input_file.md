# Read input file (Excel or CSV) into a tibble or matrix

Reads an input file from Excel (\`.xlsx\`, \`.xls\`) or CSV (\`.csv\`,
\`.txt\`), with optional conversion to a numeric matrix.

## Usage

``` r
read_input_file(
  path,
  sheet = NULL,
  skip = 0,
  header = FALSE,
  as_matrix = FALSE
)
```

## Arguments

- path:

  Character. Path to the input file.

- sheet:

  Character or \`NULL\`. Sheet name for Excel files. If \`NULL\`, the
  first sheet is used.

- skip:

  Integer. Number of rows to skip before reading data.

- header:

  Logical. If \`TRUE\`, the first row after \`skip\` is used as column
  names.

- as_matrix:

  Logical. If \`TRUE\`, the data is returned as a matrix.

## Value

A \`tibble\` if \`as_matrix = FALSE\`, or a \`matrix\` if \`TRUE\`.

## Details

The function automatically detects the file extension. For Excel files,
the \`readxl\` package is used. For CSV/TXT files, \`utils::read.csv\`
is used.

## Examples

``` r
if (FALSE) { # \dontrun{
sector <- read_input_file("data/io.xlsx", sheet = "sector", header = TRUE)
ic <- read_input_file("data/io.xlsx", sheet = "intermediate_consumption",
                       as_matrix = TRUE)
} # }
```
