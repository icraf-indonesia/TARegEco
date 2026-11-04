# Load scenario inputs with fallback to baseline

Load scenario inputs with fallback to baseline

## Usage

``` r
load_scenario_inputs(sc_input, P_default)
```

## Arguments

- sc_input:

  Named list berisi path file skenario: \`luc_projection\`,
  \`land_distribution\`, \`growth_rate\`, \`lpc\`, \`io_changes\`,
  \`intensity\`.

- P_default:

  Matriks proporsi lahan baseline.

- growth_rate:

  Data growth rate baseline.

## Value

List berisi:

- luc_projection:

  Path ke file proyeksi lahan

- area_matrix_scenario:

  Matriks tutupan lahan skenario (opsional)

- P:

  Matriks proporsi lahan skenario

- lpc_scenario:

  Tibble LPC skenario atau NULL

- io_changes:

  Tibble perubahan IO atau NULL
