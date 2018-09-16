#' To clear Oncomine datasets for Genes
#'
#' @param filenames filenames can be one or more or missing
#' @param Simple if Simple=TRUE,then to get just data type and study name. If Simple=FALSE, then to get whole information
#'
#' @return a dataframe
#' @export
#'
#' @examples Oncomine_datasets_forGenes()
Oncomine_datasets_forGenes<-function(filenames,Simple=TRUE){
  if(missing(filenames)){
    file_number_check=list.files() #get all the data picture
    if (length(file_number_check)==1){
      cbind4=Oncomine_datasets_one_page_filenames_empty(Simple=Simple)
    }
    if (length(file_number_check)>=2){
      cbind4=Oncomine_datasets_manypages_filenames_empty(Simple=Simple)
    }
  }else{
    if (length(filenames)==1){
      cbind4=Oncomine_datasets_one_page_filenames_not_empty(filenames,Simple=Simple)
    }
    if (length(filenames)>=2){
      cbind4=Oncomine_datasets_manypages_filenames_not_empty(filenames,Simple=Simple)
    }
  }
  return(cbind4)
}
