# Data-based starting values for `stan_glmer()`: methodology and output format

This document describes how `stan_glmer_inits()` computes starting values for the Bayesian hierarchical models used by `perf_mod()`, and exactly how the results map onto the internal parameters of `rstanarm::stan_glmer()`. It is intended as source material for package documentation (e.g., a vignette or an `@includeRmd` fragment) and as a maintenance reference, since the mapping depends on rstanarm internals.

Everything here was verified against **rstanarm 2.32.2 and rstan 2.32.7** by round-tripping values through `rstan::get_inits()` on fitted models and by reading the generative Stan program via `rstan::get_stancode(rstanarm:::stanmodels$continuous)`. The package's formula parsing is verified against reference layouts generated with `lme4::lFormula()` (lme4 2.0.6); see `inst/generate_lme4_layouts.R`.

## 1. Motivation

`perf_mod()` compares models by fitting a hierarchical Gaussian model to matched resampling statistics. With `hetero_var = TRUE` the formula is

```r
statistic ~ model + (model + 0 | id)
```

so each model gets its own random-effect variance and the full model-by-model covariance across resamples is estimated. These fits can be slow: rstan initializes every unconstrained parameter uniformly on $(-2, 2)$, and a chain that starts in a poor region can spend most of the warmup adapting. Because the data are balanced and replicated, nearly every quantity in the model has an obvious sample-statistic analogue. The idea is to start each chain at (a jittered version of) those sample statistics.

Two facts about rstan make this practical:

* `init` may be a **function of `chain_id`** that returns a named list of parameter values; rstan calls it once per chain.
* The list may be **partial**: parameters that are not named keep rstan's random initialization. Only the parameters we can estimate well need to be supplied.

Both behaviors pass through `stan_glmer()`'s `...` untouched, which is also why `perf_mod(..., init = )` works without any changes to `perf_mod()`.

## 2. The model and its Stan parameterization

For a Gaussian model with an identity link, `stan_glmer()` fits (in lme4-style notation)

$$
y_i = \alpha + x_i^\top \beta + z_i^\top b + \varepsilon_i,
\qquad \varepsilon_i \sim N(0, \sigma^2),
\qquad b \sim N(0, \Sigma)
$$

but the Stan program does not sample $\alpha$, $\beta$, $b$, $\sigma$, or $\Sigma$ directly. It samples standardized primitives and reconstructs the natural parameters in the transformed-parameters block. Starting values must therefore be given **on the primitive scale**. The primitives (for the default priors) are:

| Primitive       | Length                    | Natural parameter it produces |
|-----------------|---------------------------|-------------------------------|
| `gamma`         | 1                         | intercept *given centered predictors* |
| `z_beta`        | number of non-intercept fixed-effect columns | $\beta_j = z_{\beta j} \cdot s_j$ where $s_j$ is the adjusted prior scale |
| `aux_unscaled`  | 1                         | $\sigma = \texttt{aux\_unscaled} \cdot \text{sd}(y)$ |
| `tau`           | one per random-effect term | overall covariance scale of a term (relative to $\sigma$) |
| `zeta`          | $p$ per term with $p > 1$ | variance proportions (a Dirichlet-style simplex after normalization) |
| `rho`           | $p - 1$ per term with $p > 1$ | correlation "onion" parameters, each in $(0, 1)$ |
| `z_T`           | $(p-1)(p-2)$ per term with $p > 1$ | off-diagonal directions of the correlation Cholesky factor |
| `z_b`           | $\sum_t p_t (l_t + 1)$    | standardized random effects: $b = T\, z_b$ per level |

where $p_t$ is the number of coefficients in random-effect term $t$ (e.g., the number of models for `(model + 0 | id)`) and $l_t$ is the number of observed levels of its grouping factor.

Three defaults matter because they define the scale of the primitives (rstanarm's "autoscaling"):

* coefficients: `normal(0, 2.5, autoscale = TRUE)`, giving the adjusted scale $s_j = 2.5 \cdot \text{sd}(y) / \text{sd}(x_j)$;
* residual scale: `exponential(1, autoscale = TRUE)`, giving rate $1/\text{sd}(y)$, so $\sigma = \texttt{aux\_unscaled} \cdot \text{sd}(y)$;
* covariance: `decov(regularization = 1, concentration = 1, shape = 1, scale = 1)`.

If the user overrides these priors, the starting values are still finite and valid, but they no longer land exactly on the sample statistics.

### 2.1 The `decov()` covariance decomposition

For a term with $p > 1$ coefficients, the Stan function `make_theta_L()` builds the lower-triangular Cholesky factor $T$ of the term's covariance matrix from the primitives. With `scale = 1` (the `decov()` default) and $\pi = \zeta / \sum_j \zeta_j$:

$$
\operatorname{trace}(\Sigma) = p \,(\tau \, \sigma)^2, \qquad
\text{sd}_j = \sqrt{\pi_j \cdot \operatorname{trace}(\Sigma)}
$$

Note that **the residual standard deviation $\sigma$ multiplies the covariance scale** (unlike lme4's parameterization, the dispersion is baked into $T$). Then, row by row:

* Row 1: $T_{11} = \text{sd}_1$.
* Row 2: with $u = 2\rho_1 - 1$, $T_{21} = \text{sd}_2\, u$ and $T_{22} = \text{sd}_2 \sqrt{1 - u^2}$.
* Rows $r + 1 \ge 3$ (the "onion" rows): a segment of $z_T$ of length $r$ provides the direction of the off-diagonal entries,

  $$
  T_{r+1, c} = z_{T,c} \cdot
  \sqrt{\frac{\rho_r}{\lVert z_{T,\text{seg}} \rVert^2}} \cdot \text{sd}_{r}
  \qquad
  T_{r+1, r+1} = \sqrt{1 - \rho_r} \cdot \text{sd}_{r+1}
  $$

Two quirks of the Stan program (as of rstanarm 2.32.2) are load-bearing:

1. **The previous row's standard deviation scales the off-diagonals.** In the Stan source, the off-diagonal scale factor for row $r+1$ is computed *before* `std_dev` is updated, so it uses $\text{sd}_r$, not $\text{sd}_{r+1}$. The R replica `theta_L_forward()` reproduces this verbatim. Do not "fix" it: the `z_b` solve in step 3.5 relies on knowing the exact $T$ that Stan will build.
2. **`z_T` is declared longer than it is used.** The declared length per term is $(p-1)(p-2)$ (from `for (j in 3:p) len_z_T += p - 1`), but `make_theta_L()` only consumes the first $2 + 3 + \dots + (p-1)$ elements. For $p = 3$ the two lengths agree (2); for $p = 4$ the model declares 6 and consumes 5. Supplying a vector of the consumed length fails at the parameter-initialization stage (`mismatch in dimension declared and found ... dims declared=(6); dims found=(5)`), so the returned `z_T` is padded to the declared length with inert values (0.01).

## 3. Methodology: from sample statistics to primitives

All estimation is ordinary least squares; no Stan machinery is involved until the values are handed to the sampler.

### 3.1 Layout (matching lme4's formula conventions without depending on lme4)

`stan_glmer()` builds its random-effect structures with lme4, so the layout of the starting values must follow lme4's conventions exactly. Rather than importing lme4, the package uses a small purpose-built parser (`split_bar_terms()`, `expand_nesting()`, `eval_group_factor()`, and `glmer_init_layout()`) that reproduces the relevant behavior:

* **Bar extraction**: the right-hand side is split into fixed-effect terms and random-effect "bar" terms such as `(model + 0 | id)`.
* **Nesting expansion**: `(1 | id2/id)` expands to an effect for the outer factor (`id2`) plus one for the inner factor crossed with it, labeled `id:id2` as in lme4. The crossed factor is built with R's `:` operator on factors and unused levels are dropped with `factor()`, which is the same computation lme4 performs.
* **Term order**: terms are sorted by *decreasing number of levels*, matching lme4's `reTrms` ordering (this is why, for `(1 | id2/id)`, the `id:id2` block precedes the `id2` block).
* **Coefficient and level order**: each term's design values come from `model.matrix()` on the bar's left-hand side, which is also how lme4 constructs them, so the coefficient order matches the Stan model.

The parser is verified against reference layouts that were generated with `lme4::lFormula()` and saved as a base-R fixture (`tests/testthat/fixtures/lme4-layouts.rds`): term names and order, coefficient names, factor levels and codes, per-observation design values, and the fixed-effect model matrix, for all supported structures. The script that generates the fixture is shipped in `inst/generate_lme4_layouts.R`; running it from the package source directory (with lme4 installed) recreates or extends the fixture, e.g., to spot-check a new lme4 version. This keeps lme4 out of the package dependencies entirely — it is only needed by a developer regenerating the fixture.

### 3.2 Fixed effects (`gamma`, `z_beta`)

From `lm.fit()` on the fixed-effect model matrix $X$:

* `gamma` $= \bar{y}$. Stan centers the predictors internally, so its intercept parameter is the expected outcome at the predictor means; for a balanced one-way design that is the grand mean.
* `z_beta`$_j = \hat\beta_j / s_j$ with $s_j = 2.5\, \text{sd}(y) / \text{sd}(x_j)$, undoing the prior autoscaling. Aliased or zero-variance columns get 0.

### 3.3 Residual scale (`aux_unscaled`)

$\hat\sigma$ is the residual standard deviation of an **additive** fixed-effects fit that includes the fixed effects plus a main effect for every grouping factor (e.g., `statistic ~ model + id`), floored at $0.01 \cdot \text{sd}(y)$. Then `aux_unscaled` $= \hat\sigma / \text{sd}(y)$.

With one observation per model/resample cell — the typical `perf_mod()` situation — the residual scale is confounded with the random effects, so this is a deliberate heuristic: it captures "what is left after additive row and column effects," which is where the posterior for $\sigma$ tends to concentrate in these models.

### 3.4 Random-effect estimates and their covariance

For each term, per-level least squares on the fixed-effect residuals gives a level-by-coefficient matrix $\hat B$ ($l \times p$). For `(model + 0 | id)` with one value per cell this reduces to the cell deviations from each model's mean; for intercept terms it reduces to level means. Terms are processed from the **fewest levels to the most**, subtracting each term's estimates from the working residuals, so that with nested factors the outer factor (e.g., the repeat) absorbs the variation that is shared by its inner levels.

The sample covariance $S = \operatorname{cov}(\hat B)$ is then inverted onto the `decov()` primitives:

$$
\tau = \frac{\sqrt{\operatorname{trace}(S)/p}}{\hat\sigma}, \qquad
\zeta_j = p \cdot \frac{S_{jj}}{\operatorname{trace}(S)}
$$

and, with $L$ the lower Cholesky factor of $R = \operatorname{cov2cor}(S)$ (so each row of $L$ has unit norm):

$$
\rho_1 = \frac{L_{21} + 1}{2}, \qquad
\rho_r = 1 - L_{r+1,\,r+1}^2 \;\; (r \ge 2), \qquad
z_{T,\text{seg}(r)} = L_{r+1,\,1:r}
$$

The $z_T$ segments only enter through their direction (the Stan code normalizes them), so setting them to the rows of $L$ is exact up to the previous-row-sd quirk noted above. $\rho$ is clamped to $(0.005, 0.995)$ to keep chains off the constraint boundaries — resampling effects are often correlated above 0.99 across models, which would otherwise start a chain at the edge of the parameter space. If `chol()` fails (a numerically singular correlation matrix), $R$ is shrunk toward the identity with an escalating ridge. For $p = 1$ terms the whole block collapses to $\tau = \text{sd}(\hat b) / \hat\sigma$.

### 3.5 Standardized random effects (`z_b`)

Stan computes $b = T z_b$ level by level. Rather than assuming the inversion in 3.4 is exact, the helper **replays** `make_theta_L()` in R (`theta_L_forward()`, quirks included) to obtain the exact $T$ implied by the chosen primitives, then solves

$$
z_b^{(\text{level } l)} = T^{-1} \hat B_{l\cdot}^\top
$$

As a result the transformed random effects start *exactly* at the empirical estimates (verified to ~1e-16) even where the covariance inversion is only approximate. This decoupling is the key design point: small imperfections in `tau`/`zeta`/`rho`/`z_T` do not propagate into the starting values of $b$.

### 3.6 Per-chain jitter

Identical starting values across chains would undermine convergence diagnostics (R-hat cannot detect multimodality if all chains start together). Each call `fn(chain_id)` therefore perturbs the base values with `set.seed(seed + chain_id)`:

* location-type parameters (`gamma`, `z_beta`, `z_T`, `z_b`): additive Gaussian noise with sd `jitter` (`gamma` uses `jitter * sd(y)` since it is on the outcome scale);
* scale-type parameters (`tau`, `zeta`, `aux_unscaled`): multiplicative log-normal noise, `x * exp(rnorm(., 0, jitter))`, preserving positivity;
* `rho`: additive noise, then re-clamped to $(0.005, 0.995)$.

The user's RNG state (`.Random.seed`) is saved and restored around each call, so drawing starting values does not perturb the caller's random number stream. `jitter = 0` returns the base values for every chain.

## 4. Output format: what rstanarm receives

`stan_glmer_inits()` returns a **classed closure** `function(chain_id)`. This is one of the formats `rstan::sampling()` accepts for `init`, and it adapts automatically to any number of chains. The closure's environment holds only the base list and a few scalars (never the data), so it is cheap to serialize to parallel workers when `cores > 1`. The unjittered values are attached as `attr(x, "inits")` for inspection.

Each call returns a named list such as (3 models, 10 resamples, heterogeneous variances):

```r
str(inits(1))
#> List of 8
#>  $ gamma       : num [1(1d)] 0.795     # ~ mean(y)
#>  $ z_beta      : num [1:2(1d)] ...     # (p - 1) treatment contrasts
#>  $ aux_unscaled: num 0.169             # sigma_hat / sd(y)
#>  $ tau         : num [1(1d)] 1.2
#>  $ zeta        : num [1:3(1d)] ...
#>  $ rho         : num [1:2(1d)] ...
#>  $ z_T         : num [1:2(1d)] ...
#>  $ z_b         : num [1:33(1d)] ...
```

Formatting rules required by rstan:

* **Length-one vector parameters must be 1-d arrays** (`as.array()`), e.g. `gamma` and a single `tau`; a plain scalar fails dimension checking. Genuinely scalar parameters (`aux_unscaled`) stay plain numerics.
* **Partial lists are allowed**: with `blocks = c("fixed", "aux")` the list simply omits the covariance primitives and rstan randomizes them.
* **`z_b` layout** is: terms concatenated in lme4's `reTrms` order (most levels first); within a term, level-major (all $p$ coefficients for level 1, then level 2, ...); levels in the grouping factor's level order; and one **fictitious `_NEW_` level appended per grouping factor** (rstanarm adds it for posterior predictions on unseen groups), initialized near zero.

Worked lengths for the supported structures:

| Formula (3 models, 10 folds; 2 repeats for the nested row) | `tau` | `zeta` | `rho` | `z_T` | `z_b` |
|------------------------------------------|-------|--------|-------|-------|-------|
| `statistic ~ model + (model + 0 \| id)`  | 1     | 3      | 2     | 2     | 3 (10 + 1) = 33 |
| `statistic ~ model + (1 \| id)`          | 1     | —      | —     | —     | 10 + 1 = 11 |
| `statistic ~ model + (1 \| id2/id)`      | 2     | —      | —     | —     | (20 + 1) + (2 + 1) = 24 |
| 4 models: `(model + 0 \| id)`            | 1     | 4      | 3     | 6 (5 used + 1 pad) | 4 (10 + 1) = 44 |

## 5. Usage

Directly with `stan_glmer()`:

```r
inits <- stan_glmer_inits(
  statistic ~ model + (model + 0 | id),
  data = roc_data,
  seed = 1
)

fit <- rstanarm::stan_glmer(
  statistic ~ model + (model + 0 | id),
  data = roc_data,
  init = inits,
  seed = 2
)
```

Through `perf_mod()`, the `initialize` argument (default `FALSE`) computes and applies the starting values automatically. It requires the default Gaussian/identity model, cannot be combined with an `init` value in `...`, and reuses the sampler `seed` (when one is passed) so that the whole fit is reproducible:

```r
perf_mod(resamples, hetero_var = TRUE, initialize = TRUE, seed = 2)
```

To control `blocks` or `jitter`, call `stan_glmer_inits()` manually instead: recreate the long data format that `perf_mod()` builds internally (a `statistic` column with the `transform$func` applied, a `model` column, and the resampling id column(s)), compute the starting values from it, and pass `init`:

```r
roc_data <- resamples |>
  dplyr::select(id, dplyr::contains("ROC")) |>
  tidyr::pivot_longer(c(-id), names_to = "model", values_to = "statistic")
  # + the same transformation given to perf_mod(), if any

inits <- stan_glmer_inits(statistic ~ model + (model + 0 | id), roc_data)

perf_mod(resamples, hetero_var = TRUE, init = inits, seed = 2)
```

**The formula and data must be identical between the two calls** (including any outcome transformation). The dimensions would often still match after, say, dropping a resample or releveling a factor, so a mismatch can be silent: the chains would start at values aligned to the wrong levels. This is also why the helper aborts on any missing value rather than dropping rows.

## 6. When it helps (and when it does not)

Benchmarks during development (4 chains, defaults, single core):

* On a hard, noisy data set the informed values cut total time by roughly a quarter to two-thirds — mostly by eliminating the occasional chain that starts in a terrible region and crawls through warmup (one default-init run took 333 s vs ~100–125 s informed).
* On an easy data set they *added* ~30% wall time (a few seconds) but consistently reduced divergent transitions (e.g., 6 → 1 on the `precise_example` ROC model).
* Divergences caused by posterior geometry (the funnel between $\sigma$ and the random-effect scales) are **not** fixed by starting values; raise `adapt_delta` for those.

Practical guidance: reach for `stan_glmer_inits()` when a `hetero_var = TRUE` fit is slow or unstable; skip it for models that already fit in seconds.

## 7. Assumptions, limitations, and maintenance notes

* Gaussian family with identity link, no case weights, `QR = FALSE`, and default priors. Non-default priors mis-scale (but do not invalidate) the values.
* The fixed part must contain an intercept; the no-intercept parameterization has no `gamma` and is not supported.
* `statistic ~ model + (model + 0 | id2/id)` (heterogeneous + nested) is accepted by the helper but **cannot be fit by `stan_glmer()`** for balanced resamples: the expanded interaction term has exactly as many random effects as observations, and lme4's `checkNobsVsRanef` aborts.
* The mapping targets rstanarm *internals* (primitive names, `decov()` layout, the `make_theta_L()` quirks). These are stable in practice but not API. The test suite round-trips every structure through `rstan::get_inits()` and compares the formula parsing against saved `lme4::lFormula()` reference layouts (`tests/testthat/test_initialize.R`; regenerate the fixture with `inst/generate_lme4_layouts.R`), so an rstanarm update that changes the conventions will fail loudly there. To re-derive the mapping, the two tools used originally are: `rstan::get_inits(fit$stanfit)` (names/shapes/accepted values) and `rstan::get_stancode(rstanarm:::stanmodels$continuous)` (the generative code, including `make_theta_L()`).
