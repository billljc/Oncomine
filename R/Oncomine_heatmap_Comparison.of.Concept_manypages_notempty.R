Oncomine_heatmap_Comparison.of.Concept_manypages_notempty<-function(filename=c("2.txt","1.txt")){
  txtcheck=filename[grepl(pattern = ".txt",x = filename)==FALSE]
  docxcheck=txtcheck[grepl(pattern = ".docx",x = txtcheck)==FALSE]
  doccheck=docxcheck[grepl(pattern = ".doc",x = docxcheck)==FALSE] 
  if (length(docxcheck)==0){
    manypages.temp=data.frame()
    for (filename.i in 1:length(filename)){
      manypages.i=Oncomine_heatmap_Comparison.of.Concept_onepage_notempty(filename[filename.i])
      manypages.i2=cbind(file=filename[filename.i],manypages.i)
      manypages.temp=rbind(manypages.temp,manypages.i2)
    }
    return(manypages.temp)
  }else{
    stop(" Files should be ended wiht .txt .docx .doc","\n","The following files don't meet that condition.","\n")
    cat(doccheck)
  }
}