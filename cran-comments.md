## Test environments

* Local: Linux Mint 21.2 (x86_64), R 4.5.2
* GitHub Actions:
  - Windows (R release)
  - macOS (R release, R devel)
  - Ubuntu (R release, R oldrel)

## R CMD check results

0 errors | 0 warnings | 3 notes

### Notes explanation

1. **Suggests or Enhances not in mainstream repositories: ribiosArg, Vennerable**
   - ribiosArg will be submitted to CRAN after ribiosUtils is accepted
   - Vennerable is not on CRAN (maintained on GitHub)

2. **Unavailable namespace imported from by a ':::' call: 'Vennerable'**
   The package enhances Vennerable (not on CRAN) with a modified Venn diagram
   plotting function. This functionality is optional and protected by
   `if(require("Vennerable"))`. Users who don't have Vennerable installed
   will simply not have access to this single function.

3. **Possibly unsafe calls: unlockBinding, assignInNamespace**
   These calls are used solely within the optional `plotVenn()` function to
   patch a layout issue in the Vennerable package. The function is only
   called when Vennerable is available and the user explicitly requests
   Venn diagram plotting.

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
