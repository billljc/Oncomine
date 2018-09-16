Oncomine_datasets_manypages_filenames_empty<-function(Simple=TRUE){
  files=list.files() #get all the data picture
  txtcheck=files[grepl(pattern = ".txt",x = files)==FALSE]
  docxcheck=txtcheck[grepl(pattern = ".docx",x = txtcheck)==FALSE]
  doccheck=docxcheck[grepl(pattern = ".doc",x = docxcheck)==FALSE]
  if (length(doccheck)==0){
    datasetsss=Oncomine_datasets_manypages_filenames_not_empty(files,Simple=Simple)
    return(datasetsss)
  }else{
    message(" Files should be ended wiht .txt .docx .doc","\n","The following files don't meet that.","\n")
    cat(doccheck)
  }
}
