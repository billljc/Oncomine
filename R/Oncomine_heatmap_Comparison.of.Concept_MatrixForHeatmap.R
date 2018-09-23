#' To prepare a matrix for Comparison of Concept with primary concept in order to plot heatmap
#' @description you can use function Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease_MatrixForHeatmap() fisrt to get a data frame, and ent to get matrix.
#'
#' @param filenames can be filenames end with .txt .doc .docx or a dataframe or omitted
#' @param rowscale a logical param to scale each row if rowscale=TRUE
#'
#' @return a matrix
#' @export
#'
#' @examples Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease_MatrixForHeatmap()
#' @examples Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease_MatrixForHeatmap("1.txt")
#' @examples Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease_MatrixForHeatmap(c("1.txt","2.txt"))
#' @examples Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease_MatrixForHeatmap(dataframe)
Oncomine_heatmap_Comparison.of.Concept_MatrixForHeatmap <- function(filenames,rowscale=TRUE){
  if (missing(filenames)){
    filenames=list.files() #get all the data picture
    heatmap_prepare=Oncomine_heatmap_Comparison.of.Concept(filenames)
  }else if (is.data.frame(filenames)){
    heatmap_prepare=filenames
  }else if(is.character(filenames)){
    heatmap_prepare=Oncomine_heatmap_Comparison.of.Concept(filenames)
  }
  Gene.mini=c()
  for (j in 1:length(unique(heatmap_prepare$Gene))) {
    Gene.mini=c(Gene.mini,sum((heatmap_prepare$Gene %in% (unique(heatmap_prepare$Gene)[j]))==TRUE))
  }
  for (mini.i in 1:length(Gene.mini)) {
    Gene.mini.sort=sort(Gene.mini)
    if ( ( nrow(heatmap_prepare) %% (Gene.mini.sort[mini.i]) )==0){
      colnumber=Gene.mini.sort[mini.i]
      break
    }else{
      next
    }
  }
  gene_number1=seq(from=1,to=nrow(heatmap_prepare),colnumber)
  gene_number=rep(gene_number1,each=colnumber)#due to replicate gene names so we give one gen a number
  heatmap_prepare_new=cbind(gene_number,
                            heatmap_prepare[,c("Sample Name",
                                               "Expression")])
  library(tidyr)
  heatmap_matrix=spread(data = heatmap_prepare_new,
                        key = `Sample Name`, value = `Expression`,
                        fill = NA)[,-1]
  heatmap_matrix=heatmap_matrix[,heatmap_prepare[1:colnumber,"Sample Name"]]
  heatmap_matrix=as.matrix(heatmap_matrix)
  row.names(heatmap_matrix)=heatmap_prepare[gene_number1,"Gene"]
  heatmap_matrix=unique(heatmap_matrix)
  message("Row names are Gene. Column names are Sample Name.")
  if (rowscale==TRUE){
      heatmap_matrix_ts=data.frame()
      for (ts in 1:nrow(heatmap_matrix)){
        heatmap_matrix_ts=rbind(heatmap_matrix_ts,t(scale(heatmap_matrix[ts,])))
      }
      heatmap_matrix_ts=as.matrix(heatmap_matrix_ts)
      rownames(heatmap_matrix_ts)= rownames(heatmap_matrix)
      message("Data is scaled by each row respectively.")
      return(heatmap_matrix_ts)
  }else {
    heatmap_matrix=as.matrix(heatmap_matrix)
    return(heatmap_matrix)
  }
}
