Oncomine_heatmap_GenesCoexpression_manypages_filenames_empty<-function(){
  files=list.files() #get all the data picture
  txtcheck=files[grepl(pattern = ".txt",x = files)==FALSE]
  docxcheck=txtcheck[grepl(pattern = ".docx",x = txtcheck)==FALSE]
  doccheck=docxcheck[grepl(pattern = ".doc",x = docxcheck)==FALSE]
  if (length(doccheck)==0){
    coexpressiontemp=data.frame()
    for (i in 1:length(files)){
      coexpressiontemp_nof=Oncomine_heatmap_GenesCoexpression_one_page_filenames_not_empty(files[i])
      #i==1
      if (i==1){
        filename=files[i]
        coexpressiontemp_i=cbind(filename,coexpressiontemp_nof)
        #combine
        coexpressiontemp=rbind(coexpressiontemp,coexpressiontemp_i)
      }
      #if i>=2,delet the first row
      if (i>=2){
            for (j in 1:nrow(coexpressiontemp_nof)){
                if (coexpressiontemp_nof[j,3]!=coexpressiontemp_nof[j+1,3]){
                  coexpressiontemp_cut=coexpressiontemp_nof[-(1:j),] #delet duplicat goal gene in the other files
                  filename=files[i]
                  coexpressiontemp_i=cbind(filename,coexpressiontemp_cut)
                  #combine
                  coexpressiontemp=rbind(coexpressiontemp,coexpressiontemp_i)
                  break
                }
            }
      }
    }
    rownames(coexpressiontemp)=1:nrow(coexpressiontemp) #coexpressiontemp is near last data
    return(coexpressiontemp)
  }else{
    message(" Files should be ended wiht .txt .docx .doc","\n","The following files don't meet that.","\n")
    cat(doccheck)
  }
}
