Oncomine_datasets_manypages_filenames_not_empty<-function(filenames,Simple=TRUE){
  files=filenames #get all the data picture
  txtcheck=files[grepl(pattern = ".txt",x = files)==FALSE]
  docxcheck=txtcheck[grepl(pattern = ".docx",x = txtcheck)==FALSE]
  doccheck=docxcheck[grepl(pattern = ".doc",x = docxcheck)==FALSE]
  if (length(doccheck)==0){
    datasettemp=data.frame()
        for (i in 1:length(files)){
            filename=files[i]
            datasettemp_nof=Oncomine_datasets_one_page_filenames_not_empty(filename,Simple=Simple)
            datasettemp_i=cbind(filename,datasettemp_nof)
            datasettemp=rbind(datasettemp,datasettemp_i)
          }
    rownames(datasettemp)=1:nrow(datasettemp) #datasettemp is near last data
    return(datasettemp)
  }else{
    message(" Files should be ended wiht .txt .docx .doc","\n","The following files don't meet that.","\n")
    cat(doccheck)
  }
}
