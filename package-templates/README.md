Australian 2011 Census Data for {{level_desc}} [{{level}}]
----------------------------------------------------------

* This is a data-only R package containing data for the 2011 Australian Census at the level of {{level}} i.e. {{level_desc}}.
* Install:
```{r}
devtools::install_github("coolbutuseless/AuCensus2011.{{level}}")
```
* The data for this particular package was sourced from [ABS](http://www.abs.gov.au) and transformed and packed up using the [AuCensus2011](http://www.github.com/coolbutuseless/AuCensus2011) package.
* `AuCensus2011` contains more meta-information along with an example splitting of variables into more useful components, rather than just the raw ABS column name.
* Look to the `AuCensus2011` vignettes to see how to use these packages together.

