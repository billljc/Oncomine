#' To get heat map matrix for meta analysis
#'
#' @param filename can be .txt .doc .docx or omitted or dataframe acquired by function Oncomine_meta
#' @param rowscale a logistic object to scale each row
#'
#' @return an matrix
#' @export
#'
#' @examples Oncomine_meta_MatrixForHeatmap("txt.txt")
Oncomine_meta_MatrixForHeatmap<- function(filename,rowscale=TRUE){
  if (missing(filename)){
    files=list.files()
    meta_matrix=Oncomine_meta()
  }else if(is.character(filename)){
    meta_matrix=Oncomine_meta(filename)
  }else if(is.data.frame(filename)){
    meta_matrix=filename
  }
  for (Genei in 1:nrow(meta_matrix)){
    if (meta_matrix[,1][Genei] != meta_matrix[,1][Genei+1]){break}
  }
  Legend=rep(1:Genei,nrow(meta_matrix)%/%Genei)
  heatmap_prepare_new=cbind(meta_matrix,Legend)
  library(tidyr)
  heatmap_matrix=spread(data = heatmap_prepare_new[,c("Gene","Gene Rank","Legend")],
                        key = Legend,value = `Gene Rank`,fill = NA)
  Genename=meta_matrix[,1][seq(1,nrow(meta_matrix),Genei)]
  heatmap_matrix=heatmap_matrix[Genename,]
  heatmap_matrix=unique(heatmap_matrix)
  rownames(heatmap_matrix)=heatmap_matrix[,"Gene"]
  heatmap_matrix=heatmap_matrix[,-1]
  if (rowscale==TRUE){
    heatmap_matrix_ts=data.frame()
    for (ts in 1:nrow(heatmap_matrix)){
      heatmap_matrix_ts=rbind(heatmap_matrix_ts,t(scale(as.numeric(heatmap_matrix[ts,]))))
    }
    rownames(heatmap_matrix_ts)= rownames(heatmap_matrix)
    message("Data is scaled by each row respectively.")
    heatmap_matrix_ts=as.matrix(heatmap_matrix_ts)
    message("Row names are Gene. Column names are Legends.")
    return(heatmap_matrix_ts)
  }else {
    heatmap_matrix=as.matrix(heatmap_matrix)
    message("Row names are Gene. Column names are Legends.")
    return(heatmap_matrix)
  }
}
