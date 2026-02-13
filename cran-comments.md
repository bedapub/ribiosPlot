## Test environments

* Local: Linux Mint 21.2 (x86_64), R 4.5.2
* GitHub Actions:
  - Windows (R release)
  - macOS (R release, R devel)
  - Ubuntu (R release, R oldrel)

## R CMD check results

0 errors | 0 warnings | 2 notes

### Notes explanation

1. **New submission**

2. **Suggests or Enhances not in mainstream repositories: ribiosArg, Vennerable**
   - ribiosArg is available from https://bedapub.r-universe.dev
     (listed in Additional_repositories)
   - Vennerable is maintained on GitHub (not on CRAN)

## Notes for CRAN reviewers

This package provides plotting utilities for computational biology in drug
discovery, developed at F. Hoffmann-La Roche AG.

Key functionality includes:
- Heatmap visualization (biosHeatmap)
- Color palette management
- PCA visualization
- Various plotting utilities

The package depends on ribiosUtils (on CRAN).

The optional Vennerable integration is for users who install Vennerable
separately from GitHub. The main functionality of the package does not
depend on Vennerable.
