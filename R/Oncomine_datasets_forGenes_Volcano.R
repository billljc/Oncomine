#' @title to plot volcano for Genes datasets
#'
#' @param filenames filenames can be omitted, a list of string
#'
#' @return a plot by plotly
#' @export
#'
#' @examples Oncomine_datasets_forGenes_Volcano()
#' @examples Oncomine_datasets_forGenes_Volcano("1.txt")
#' @examples Oncomine_datasets_forGenes_Volcano(c("1.txt","2.txt"))
Oncomine_datasets_forGenes_Volcano<-function(filenames){
  if (missing(filenames)){
    datatemp=Oncomine_datasets_forGenes()
  } else if (is.data.frame(filenames)){
    datatemp=filenames
  } else {
    datatemp=Oncomine_datasets_forGenes(filenames)
  }
  `-log10(p.value)`=round(-log10(as.numeric(as.character(datatemp$p.value))),2)
  `Fold change`=as.numeric(as.character(datatemp$`Fold Change`))
  datatemp2=cbind(datatemp,`-log10(p.value)`,`Fold change`)
  library(plotly)
  plot_ly(data = datatemp2,
          x=~`Fold change`,
          y=~`-log10(p.value)`,
          type="scatter",
          mode="markers",
          text = paste("Comparison:", datatemp2$Comparison),
          name = ~Study
  ) %>% layout(
    title="Volcano for Genes",
    xaxis = list(range = c(-max(datatemp2$`Fold change`), max(datatemp2$`Fold change`)))
  )
}
