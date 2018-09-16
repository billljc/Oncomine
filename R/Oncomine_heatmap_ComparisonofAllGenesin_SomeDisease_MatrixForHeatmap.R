#' To prepare a matrix for coexpression data in order to plot heatmap
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
Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease_MatrixForHeatmap <- function(filenames,rowscale=TRUE){
  if (missing(filenames)){
    files=list.files() #get all the data picture
    txtcheck=files[grepl(pattern = ".txt",x = files)==FALSE]
    docxcheck=txtcheck[grepl(pattern = ".docx",x = txtcheck)==FALSE]
    doccheck=docxcheck[grepl(pattern = ".doc",x = docxcheck)==FALSE]
      if (length(doccheck)==0){
        heatmap_prepare=Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease()
      } else {
        doccheck1=paste(doccheck,collapse="\n")
        warning("The following files shoul be removed","\n",doccheck1)
        stop("Please make sure ONLY data file end with .txt .doc .docx including in the working directory. Or give object to this function")
      }
  }else if (is.data.frame(filenames)){
    heatmap_prepare=filenames
  }else {
    heatmap_prepare=Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease(filenames)
  }
  library(tidyr)
  for (k in 1:nrow(heatmap_prepare)){ # k is column number per row
    if (heatmap_prepare[k,"Gene"]!=heatmap_prepare[k+1,"Gene"]){
      break #just break to get k goal genes in the first file
    }
  }
  gene_number1=seq(from=1,to=nrow(heatmap_prepare),k)
  gene_number=rep(gene_number1,each=k)#due to replicate gene names so we give one gen a number
  heatmap_prepare_new=cbind(gene_number,
                            heatmap_prepare[,c("Sample Name",
                                               "Gene Expression")])
  heatmap_matrix=spread(data = heatmap_prepare_new,
                        key = `Sample Name`, value = `Gene Expression`,
                        fill = NA)[,-1]
  heatmap_matrix=heatmap_matrix[,heatmap_prepare[1:k,"Sample Name"]]
  heatmap_matrix=as.matrix(heatmap_matrix)
  row.names(heatmap_matrix)=heatmap_prepare[gene_number1,"Gene"]
  heatmap_matrix=unique(heatmap_matrix)
  message("Row names are Gene. Column names are Sample Name.")
  if (rowscale==TRUE){
      heatmap_matrix_ts=data.frame()
      for (ts in 1:nrow(heatmap_matrix)){
        heatmap_matrix_ts=rbind(heatmap_matrix_ts,t(scale(heatmap_matrix[ts,])))
      }
      rownames(heatmap_matrix_ts)= rownames(heatmap_matrix)
      message("Data is scaled by each row respectively.")
      heatmap_matrix_ts=as.matrix(heatmap_matrix_ts)
      return(heatmap_matrix_ts)
  }else {
    heatmap_matrix=as.matrix(heatmap_matrix)
    return(heatmap_matrix)
  }
}
