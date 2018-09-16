#' @title to clear heat map data of Comparison of All Genes in Some Disease in treatment and control
#'
#' @param filenames filenames can be omitted, a list of string
#'
#' @return a dataframe
#' @export
#'
#' @examples Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease()
#' @examples Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease("1.txt")
#' @examples Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease(c("1.txt","2.txt"))
Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease<-function(filenames){
  if(missing(filenames)){
    cbind4=Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease_manypages_filename_empty()
  }else{
    cbind4=Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease_manypages_filename_not_empty(filenames)
  }
  #because p value and fd is not caculated accuracy, so i decide to drop them
  log2fdcn=grep(c("log2_fd"),colnames(cbind4))
  if (length(log2fdcn)!=0){cbind5=cbind4[,-log2fdcn]}else{cbind5=cbind4}
  log10pvcn=grep("log10_pvalue",colnames(cbind5))
  if (length(log10pvcn)!=0){cbind6=cbind5[,-log10pvcn]}else{cbind6=cbind5}
  if ("file" %in% colnames(cbind6)){
    filen=grep("file",colnames(cbind6))
    cbind7=cbind6[!duplicated(cbind6[,-filen]), ]
    return(cbind7)
  }else{
    return(cbind6)
  }

}
