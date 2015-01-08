# Create .md, .html, and .pdf files
library(knitr)
library(markdown)

setwd('/Users/Dan/Documents/Research/Stream_Climate_Change/temperatureProject/')

# MADEP Report
# knit("01-functions.Rmd")
markdownToHTML('reports/MADEP/2014-07-14_brief_summary_report.md', 'reports/MADEP/2014-07-14_brief_summary_report.html', options=c("use_xhml"))
system("pandoc -s reports/MADEP/2014-07-14_brief_summary_report.html -o reports/MADEP/2014-07-14_brief_summary_report.pdf -H formatting/margins.sty")

