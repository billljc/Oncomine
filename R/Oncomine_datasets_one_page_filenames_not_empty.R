Oncomine_datasets_one_page_filenames_not_empty<- function(filenames,Simple=TRUE){
  library("textreadr")
  library("stringr")

  #read the long string
  if (grepl(".txt",filenames)) { oncomine=suppressWarnings(paste(readLines(filenames), collapse=" "))
  }else if(grepl(".doc",filenames) | grepl(".docx",filenames)) {oncomine=read_docx(filenames)}
  oncomine_not=gsub(pattern = "\t",replacement = "",x = oncomine)
  step0=strsplit(x=oncomine_not,split = "<b>")
  step1=step0[[1]][-1]
  steplength=length(step1)
  Datasets=data.frame()

  if (Simple==TRUE){
    for (i in 1:steplength) {
      Datatype=ifelse(grepl(pattern = 'Type">DNA',x = step1[[i]]),"DNA","mRNA")
      study=str_extract(string = step1[[i]], pattern = '.*</b>')
      study2=sub(pattern = "</b>",replacement = "",x = study)
      study.no=str_extract(string = step1[[i]],pattern = 'pDatasetInfo">\\([0-9]*')
      study.no2=sub(pattern = 'pDatasetInfo">\\(',replacement = "",x = study.no)
      Dataset=cbind(Datatype=Datatype,Study=study2)
      Datasets=rbind(Datasets,Dataset)
    }
  }else{
    for (i in 1:steplength) {
      Datatype=ifelse(grepl(pattern = 'Type">DNA',x = step1[[i]]),"DNA","mRNA")
      study=str_extract(string = step1[[i]], pattern = '.*</b>')
      study2=sub(pattern = "</b>",replacement = "",x = study)
      study.no=str_extract(string = step1[[i]],pattern = 'pDatasetInfo">\\([0-9]*')
      study.no2=sub(pattern = 'pDatasetInfo">\\(',replacement = "",x = study.no)
      comparison.group=strsplit(x=step1[[i]],split = '<div>')[[1]][-1]
      comparison.item2=sub(pattern = '</div>  <div class=.*',replacement = "",x = comparison.group)
      comparison.p.value=str_extract(string = comparison.group,pattern = 'p = [0-9]*.[0-9E-]*')
      comparison.p.value2=sub(pattern = 'p = ',replacement = "",x = comparison.p.value)
      comparison.p.fd=str_extract(string = comparison.group,pattern = 'fold change = [-0-9]*.[0-9E-]*')
      comparison.p.fd2=sub(pattern = 'fold change = ',replacement = "",x = comparison.p.fd)
      comparison.rank=str_extract(string = comparison.group,pattern = 'pAnalysisRank">[0-9]*')
      comparison.rank2=sub(pattern = 'pAnalysisRank">',replacement = "",x = comparison.rank)
      Dataset=cbind(Datatype=Datatype,Study=study2,Comparison=comparison.item2,p.value=comparison.p.value2,`Fold Change`=comparison.p.fd2,`Gene Rank`=comparison.rank2)
      judge=tryCatch({Datasets=rbind(Datasets,Dataset)},error=function(e){"ERROR"})
      if (is.character(judge)){
        message("please click the ",i,"th drop down arrow in Oncomine web, then copy data")
        next
      }
    }
  }
  return(Datasets)
}
