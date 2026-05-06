# Time-series methods used in this lab

This document maps the lab's time-series models to the methods commonly
used at the Deutsche Bundesbank and the wider Eurosystem (BMPE — Broad
Macroeconomic Projection Exercise) and points to the literature behind
each one.

## Univariate baselines

| Model | R wrapper | Use when... |
|---|---|---|
| Naive | `forecast::naive` | benchmark for noisy non-seasonal series |
| Seasonal Naive | `forecast::snaive` | clearly seasonal data with stable seasonality |
| Random Walk + Drift | `forecast::rwf(drift = TRUE)` | series with a stable linear trend |
| Mean | `forecast::meanf` | stationary mean-reverting series |
| Theta | `forecast::thetaf` | strong M-competition baseline |
| ARIMA / SARIMA | `forecast::auto.arima` | univariate workhorse |
| ETS | `forecast::ets` | exponential-smoothing state-space |
| TBATS | `forecast::tbats` | multi-seasonal data |
| STL + ARIMA | `forecast::stlm(method = "arima")` | monthly/quarterly with stable seasonality |

The Theta method (Assimakopoulos & Nikolopoulos 2000) is competitive
with much more complex alternatives in the M3/M4 competitions and is
useful as a stress-test benchmark. STL+ARIMA is often more robust than
plain SARIMA when the seasonal pattern is regular and the trend is
non-monotonic.

## Multivariate / structural

| Model | R package | Bundesbank-relevant references |
|---|---|---|
| VAR | `vars::VAR` | Sims (1980); Lütkepohl (2005) |
| VECM | `urca::ca.jo` + `vars::vec2var` | Johansen (1995) |
| BVAR (Minnesota / Litterman) | `BVAR::bvar` | Bańbura, Giannone & Reichlin (2010); Carriero, Clark & Marcellino (2015) |
| Dynamic Factor Model (DFM) | `dfms::DFM` | Stock & Watson (2002); Doz, Giannone & Reichlin (2011) |

VAR is the standard small-system tool. When the system is larger or the
researcher wants tighter forecast intervals, **BVAR** with a Minnesota /
Litterman prior is the workhorse. The lambda hyperparameter controls
shrinkage toward a random walk; smaller values pull more strongly. See
**Bundesbank Discussion Paper 24/2017** (Berg & Henzel) for an applied
BVAR study and **Bańbura, Giannone & Reichlin (2010)** for the
methodological foundations of large BVARs.

For nowcasting, the **Dynamic Factor Model** is the central-bank
benchmark. The Eurosystem's Bridge models and many Bundesbank monthly
indicators rely on small DFMs that summarise dozens of soft and hard
indicators in a handful of latent factors.

## Mixed-frequency / nowcasting

`midas` (Mixed-Data Sampling regression) lets you forecast a low-
frequency target (quarterly GDP) directly from lags of a high-frequency
predictor (monthly industrial production, weekly sentiment surveys).
The lab uses `midasr::midas_r` with Almon polynomial weighting; the
Beta polynomial (`nealmon`) is also exposed. References: Ghysels,
Santa-Clara & Valkanov (2004); Foroni & Marcellino (2014).

## How to choose

- For a **single, short series**: start with seasonal-naive and ARIMA.
  If a tuned XGBoost on lag features barely beats them, the simpler
  model usually wins.
- For a **small system of macro variables**: VAR, then BVAR if the
  posterior intervals are too wide.
- For a **panel of many indicators**: DFM.
- For **target / predictor frequency mismatch**: MIDAS.

## Bottom line

The lab does not pick a model for you. It provides every standard
Bundesbank-style baseline so the researcher can compare them honestly
on the same horizon, hold-out, and metric. Use the Predictor analysis
block to inspect what each fitted model is leaning on; remember the
caveats in `docs/causal_inference.md` before reading the importance as
a causal effect.
