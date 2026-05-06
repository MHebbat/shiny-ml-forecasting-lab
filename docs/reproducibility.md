# Reproducibility (Manifest + Project Workspace + Reproduce)

The Lab pairs three artifacts so that any run can be re-derived from a
single bundle:

1. **Reproducibility manifest** — a JSON snapshot of the dataset
   fingerprint, recipe, survey design, model spec, validation strategy,
   metrics (with bootstrap CI), software stack, and git commit. Built
   by `R/manifest.R::make_manifest()`.
2. **Project bundle** — `~/.shinyml/projects/<name>/`, written by
   `R/io_project.R::project_save()`. Contains:
   - `<name>.shinyml.json` — the manifest.
   - `<name>.shinyml.rds` — the full state payload (datasets, recipe,
     survey design, last model, last params, prep log, manifest).
3. **Reproduce flow** — invoked from tab `10 · Runs & Projects → Projects`.

## Manifest

Every save and brief-report export embeds the manifest. Inspect it
mid-session via *Show Manifest* in Model Lab or Editorial Studio.

Top-level keys:

```
app           {name, version (= git rev), generated_at}
dataset       {name, n_rows, n_cols, sha256, source}
recipe        {steps (list), log (list)}
survey_design {method, weight, strata, psu, repweights_pattern,
               repdesign_type, implicates, variance{method,
               replicates, brr_rho}}
model         {id, label, params, available_backend}
validation    {strategy, params, seed}
metrics       {train, test, bootstrap_ci_95}
software      {R, platform, os, packages}
python        {python, version}      # only when reticulate is active
git_commit
```

The dataset SHA-256 is computed by `digest::digest(df, algo='sha256')`
on the in-memory data.frame. It uniquely fingerprints the dataset
without shipping rows.

## Project bundle layout

```
~/.shinyml/projects/
  <name>/
    <name>.shinyml.json    # manifest, pretty-printed
    <name>.shinyml.rds     # full state payload
  <name>_v2/               # second save under the same base name
  <name>_v3/
  …
```

`project_save()` versions the directory automatically: a second save
under the same `<name>` writes to `<name>_v2/`, and so on, so previous
bundles are never overwritten.

## Reproduce flow

In tab `10 · Runs & Projects → Projects`, select a row and click
**Reproduce**. The sequence:

1. Recompute the dataset SHA-256 from the bundled `raw_data` and
   compare it to the SHA stored in the manifest.
2. Replay the recipe using `apply_recipe(raw_data, steps, target,
   time_col)` and compare the prep log line-for-line against the
   stored log.
3. Surface the result in a flash notice: *Reproduced: dataset SHA +
   recipe log match.* Otherwise it explains which check failed.

The configurable **tolerance epsilon** (default `1e-6`) governs metric
comparisons in flows that re-train; the current Reproduce path verifies
recipe determinism only and leaves training to the user (some models
are heavy).

## Diff

Select exactly two project rows and click **Diff selected pair**. A
small JSON-style diff renderer compares: name, saved_at, dataset
metadata (SHA, n_rows, n_cols), model id, primary metric, recipe step
count, survey method, and variance method. Identical fields are
suppressed.

## What this guarantees

- Two saves of the same workspace produce identical manifests
  (modulo timestamps and git commit hash).
- Loading a bundle in a fresh R session restores the recipe, survey
  design, last params, and last metric exactly.
- A SHA mismatch on Reproduce flags the bundle as having drifted from
  the dataset that was originally fingerprinted.

## What it doesn't (yet)

- Reproduce does not re-train heavy models from scratch. Re-fit by
  hitting **Train** after Open.
- Some R packages publish reproducibility-affecting flags (e.g.
  `xgboost` thread count) that aren't yet captured in `validation$params`.
