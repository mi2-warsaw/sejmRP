Scraping data from sejm.gov.pl
==============================

<h5> Description: </h5>
**sejmRP** package enables scraping data from Polish Diet's webpage [sejm.gov.pl](http://www.sejm.gov.pl/)
about votings and deputies in actual term of office. All data is storaged in database.

<h5> Usage: </h5>
Please see [instruction](https://github.com/mi2-warsaw/sejmRP/blob/master/sejmRP/vignettes/INSTRUCTION.pdf) for information how to use sejmRP package.

<h5> Installation of the sejmRP package: </h5>
To get started, install the latest version of **sejmRP** from GitHub:
```{Ruby}
if (!require(devtools)) {
    install.packages("devtools")
    require(devtools)
}
install_github("mi2-warsaw/sejmRP/sejmRP")
```
Make sure you have [rtools](http://cran.r-project.org/bin/windows/Rtools/) installed on your computer.
