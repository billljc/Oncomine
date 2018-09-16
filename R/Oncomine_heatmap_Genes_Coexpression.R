#' To clear Oncomine heatmap Genes Coexpression data including one page and many pages
#'
#' @param filenames can be end with .txt .doc .docx or omitted
#'
#' @return a dataframe
#' @export
#'
#' @examples data1=Oncomine_heatmap_Genes_Coexpression("3.txt")
#' @examples data2=Oncomine_heatmap_Genes_Coexpression(c("1.txt","2.txt","3.txt"))
#' @examples data3=Oncomine_heatmap_Genes_Coexpression()
Oncomine_heatmap_Genes_Coexpression<-function(filenames){
  if(missing(filenames)){
    file_number_check=list.files() #get all the data picture
        if (length(file_number_check)==1){
        cbind4=Oncomine_heatmap_GenesCoexpression_one_page_filenames_empty()
        }
        if (length(file_number_check)>=2){
          cbind4=Oncomine_heatmap_GenesCoexpression_manypages_filenames_empty()
        }
  }else{
        if (length(filenames)==1){
          cbind4=Oncomine_heatmap_GenesCoexpression_one_page_filenames_not_empty(filenames)
        }
        if (length(filenames)>=2){
          cbind4=Oncomine_heatmap_GenesCoexpression_manypages_filenames_not_empty(filenames)
        }
  }
  return(cbind4)
}
