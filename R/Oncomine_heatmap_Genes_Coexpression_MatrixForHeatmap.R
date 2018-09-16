#' To prepare a matrix for coexpression data in order to plot heatmap
#' @description you can use function Oncomine_heatmap_Genes_Coexpression() fisrt to get a data frame, and ent to get matrix.
#' @param filenames can be filenames end with .txt .doc .docx or a dataframe or omitted
#'
#' @return a matrix
#' @export
#'
#' @examples Oncomine_heatmap_Genes_Coexpression_MatrixForHeatmap()
#' @examples Oncomine_heatmap_Genes_Coexpression_MatrixForHeatmap("1.txt")
#' @examples Oncomine_heatmap_Genes_Coexpression_MatrixForHeatmap(c("1.txt","2.txt"))
#' @examples Oncomine_heatmap_Genes_Coexpression_MatrixForHeatmap(dataframe)
Oncomine_heatmap_Genes_Coexpression_MatrixForHeatmap <- function(filenames){
  if (missing(filenames)){
    heatmap_prepare=Oncomine_heatmap_Genes_Coexpression()
  }else if (is.data.frame(filenames)){
    heatmap_prepare=filenames
  }else {
    heatmap_prepare=Oncomine_heatmap_Genes_Coexpression(filenames)
  }
  library(tidyr)
  for (k in 1:nrow(heatmap_prepare)){ # k is column number per row
    if (heatmap_prepare[k,"Gene"]!=heatmap_prepare[k+1,"Gene"]){
      break #just break to get k goal genes in the first file
    }
  }
  gene_number1=seq(from=1,to=nrow(heatmap_prepare),k)
  gene_number=rep(gene_number1,each=k)
  heatmap_prepare_new=cbind(gene_number,
                            heatmap_prepare[,c("Sample Name",
                                               "Gene Expression")])
  heatmap_matrix=spread(heatmap_prepare_new,
                        `Sample Name`, `Gene Expression`,
                        fill = NA)[,-1]
  heatmap_matrix=as.matrix(heatmap_matrix)
  row.names(heatmap_matrix)=heatmap_prepare[gene_number1,"Gene"]
  message("Row names are Gene. Column names are Sample Name.")
  return(heatmap_matrix)
}