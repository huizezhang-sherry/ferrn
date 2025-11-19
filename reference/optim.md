# Function to calculate smoothness and squintability

Function to calculate smoothness and squintability

## Usage

``` r
sample_bases(
  idx,
  data = sine1000,
  n_basis = 300,
  parallel = FALSE,
  best = matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1), nrow = 6),
  min_proj_dist = NA,
  step_size = NA,
  seed = 123
)

# S3 method for class 'basis_df'
print(x, width = NULL, ...)

# S3 method for class 'basis_df'
tbl_sum(x)

calc_smoothness(
  basis_df,
  start_params = c(0.001, 0.5, 2, 2),
  other_gp_params = NULL,
  verbose = FALSE
)

# S3 method for class 'smoothness_res'
print(x, width = NULL, ...)

# S3 method for class 'smoothness_res'
tbl_sum(x)

calc_squintability(
  basis_df,
  method = c("ks", "nls"),
  scale = TRUE,
  bin_width = 0.005,
  other_params = NULL
)

# S3 method for class 'squintability_res'
print(x, width = NULL, ...)

# S3 method for class 'squintability_res'
tbl_sum(x)

fit_ks(basis_df, idx, other_params = NULL)

fit_nls(basis_df, other_params = NULL)
```

## Arguments

- idx:

  character, the name of projection pursuit index function, e.g. "holes"

- data:

  a matrix or data frame, the high dimensional data to be projected

- n_basis:

  numeric, the number of random bases to generate

- parallel:

  logic, whether to use parallel computing for calculating the index.
  Recommend for the stringy index.

- best:

  a matrix, the theoretical/ empirical best projection matrix to
  calculate the projection distance from the simulated random bases.

- min_proj_dist:

  only for squintability, the threshold for projection distance for the
  random basis to be considered in sampling

- step_size:

  numeric, step size for interpolating from each random basis to the
  best basis, recommend 0.005

- seed:

  numeric, seed for sampling random bases

- x:

  objects with specialised printing methods

- width:

  only used when `max.levels` is NULL, see above.

- ...:

  further arguments passed to or from other methods.

- basis_df:

  the basis data frame returned from `sample_bases`

- start_params:

  list, the starting parameters for the Gaussian process for smoothness

- other_gp_params:

  list, additional parameters to be passed to \[GpGp::fit_model()\] for
  calculating smoothness

- verbose:

  logical, whether to print optimisation progression when fitting the
  Gaussian process

- method:

  either "ks" (kernel smoothing) or "nls" (non-linear least square) for
  calculating squintability.

- scale:

  logic, whether to scale the index value to 0-1 in squintability

- bin_width:

  numeric, the bin width to average the index value before fitting the
  kernel, recommend to set as the same as \`step\` parameter

- other_params:

  list additional parameters for fitting kernel smoothing or non-linear
  least square, see \[stats::ksmooth()\] and \[stats::nls()\] for
  details

## Examples

``` r
if (FALSE) { # \dontrun{
library(GpGp)
library(fields)
library(tourr)
basis_smoothness <- sample_bases(idx = "holes")
calc_smoothness(basis_smoothness)
basis_squint <- sample_bases(idx = "holes", n_basis = 100, step_size = 0.01, min_proj_dist = 1.5)
calc_squintability(basis_squint, method = "ks", bin_width = 0.01)
} # }
```
