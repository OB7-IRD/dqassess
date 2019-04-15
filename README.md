# Package Data Quality ASSESSment

This package was developed initially under the cover of the European project MARE/2016/22 « Strengthening regional cooperation in the area of fisheries data collection » Annex III « Biological data collection for fisheries on highly migratory species » (acronym: RECOLAPE). He answered the aims of the two first task of the Work Package 5: propose procedures for data quality assessment, at national and regional levels, in the area of large pelagic fisheries data collection.

# Package installation

You can run the following code in the R console:

```R
# Devtools is a necessary package
# If it is not installed, run the following line
install.packages("devtools")
# Load the package from the Git
devtools::install_github("https://github.com/OB7-IRD/dqassess.git",  build_opts = c("--no-resave-data", "--no-manual"))
# Load the library
library(dqassess)
# You can access the package documentation with the following line
?dqassess
# If you want the documentation of a specific package function use the same syntax, for example for the function build_template_format_db
?build_template_format_db
```
