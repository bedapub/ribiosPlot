Please always explain all acronyms in the description text. -> PCA
For more details:
<https://contributor.r-project.org/cran-cookbook/description_issues.html#explaining-acronyms>

If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'https:' and angle brackets for
auto-linking. (If you want to add a title as well please put it in
quotes: "Title")
For more details:
<https://contributor.r-project.org/cran-cookbook/description_issues.html#references>

Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
For more details:
<https://contributor.r-project.org/cran-cookbook/docs_issues.html#missing-value-tags-in-.rd-files>

-> Missing Rd-tags:
      getLims.Rd: \value
      rowVars.Rd: \value

Please always make sure to reset to user's options(), working directory
or par() after you changed it in examples and vignettes and demos.
e.g.:
oldpar <- par(mfrow = c(1,2))
...
par(oldpar)

-> man/colorpanel.Rd, man/plotPCA.prcomp.Rd

For more details:
<https://contributor.r-project.org/cran-cookbook/code_issues.html#change-of-options-graphical-parameters-and-working-directory>
