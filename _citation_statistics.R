#| fig-cap: "Number of citations per year according to Google Scholar."
#| fig-height: 3
#| warning: false
#| echo: false
library(scholar)
profileMNavascues <- get_profile("I9Rd7ocAAAAJ")
publicationsMNavascues <- get_publications("I9Rd7ocAAAAJ")
citation_historyMNavascues <- get_citation_history("I9Rd7ocAAAAJ")
par(mar=c(5,5,0,0)+0.2)
barplot(citation_historyMNavascues$cites,names.arg=citation_historyMNavascues$year,col="orange",border = NA,xlab="",ylab="",las=2)
box()