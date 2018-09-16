#' @title To draw a scatter plot for Genes wiht goal Gene using correlation and
#' @description To draw a scatter plot for Genes wiht goal Gene using correlation as x and -log10(p.value) as y.
#' @param data_file_name This param can be file name end with .txt .doc .docx, or can be omitted
#'
#' @return a scatter plt using plotly
#' @export
#'
#' @examples Oncomine_heatmap_Genes_Coexpression_Scatter("1.txt")
Oncomine_heatmap_Genes_Coexpression_Scatter <- function(data_file_name){
  library(plotly)
  if (missing(data_file_name)){
    datatemp=Oncomine_heatmap_Genes_Coexpression()
  } else if (is.data.frame(data_file_name)){
    datatemp=data_file_name
  } else {
    datatemp=Oncomine_heatmap_Genes_Coexpression(data_file_name)
  }
  datauniqueGene1=unique(datatemp[,c("Correlation","p_value","Gene")])
  p.value=datauniqueGene1$p_value
  datauniqueGene=cbind(datauniqueGene1,-log10(p.value))
  titleheatmap=paste("Correlation Scatter plot for Gene: ",datauniqueGene[1,"Gene"])
  plot_ly(data = datauniqueGene,
          x=~`Correlation`,
          y=~`-log10(p.value)`,
          type="scatter",
          mode="markers",
          color = ~ `Correlation`,
          text = paste("Gene: ", datauniqueGene$Gene)
  ) %>% layout(title=titleheatmap)
}
