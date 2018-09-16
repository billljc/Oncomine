#' To download data heatmap ComparssionOfAllGenes for meta analysis
#' To download data heatmap ComparssionOfAllGenes for meta analysis from oncomine
#' @param filename string name
#'
#' @return A dataframe
#' @export
#'
#' @examples Oncomine_meta("txt.txt")
Oncomine_meta<-function(filename){
  if (missing(filename)){
    files=list.files() #get all the data picture
    if (length(files)==1){
      metadata=Oncomine_meta_onefile_empty()
    }else if(length(files)>=2){
      metadata=Oncomine_meta_manyfile_empty()
    }else if(length(files)==0){
      stop("No files found in this directory")
    }
  }else if(is.character(filename)){
    if (length(filename)==1){
      metadata=Oncomine_meta_onefile_notempty(filename)
    }else if(length(filename)>=2){
      metadata=Oncomine_meta_manyfiles_notempty(filename)
    }
  }
  return(metadata)
}
