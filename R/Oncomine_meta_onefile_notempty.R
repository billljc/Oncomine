Oncomine_meta_onefile_notempty <- function(filename){
  library("textreadr")
  library("stringr")
  #read the long string
  if (grepl(".txt",filename)) { oncomine=suppressWarnings(paste(readLines(filename), collapse=" "))
  }else if(grepl(".doc",filename) | grepl(".docx",filename)) {oncomine=read_docx(filename)}
    #get whole data
  whole=str_extract(string =oncomine,pattern = '<area.*\">')#delet hat and tail
  heatmapdata=strsplit(x = whole,split = '<area')[[1]][-1]#split to unit, after split ,the first list is space so we delet it
  ####get true true true  true  data
  #get left content as column title
  leftcontenttrue=str_extract_all(string = heatmapdata,pattern = 'leftcontent="[\\w\\s\\:\\|-]*')#extract left content
  leftcontenttrue1=gsub(pattern = 'leftcontent="',replacement = "",x = leftcontenttrue)#delet leftcontent=
  #judge wether each unit has both lenged value, aim to get data in heatmap in middle of the pic
  leftcontenttrue2=gsub(pattern = ":",replacement = "",x = leftcontenttrue1)#delet :
  #GET GEN
  judge= grepl('\\|\\|' ,leftcontenttrue2)
  NOGene=leftcontenttrue2[judge==TRUE]
  Gene=gsub(pattern = " Rank[\\|a-zA-Z-]*",replacement = "",x = NOGene)
  leftcontenttrue3=strsplit(x = NOGene,split = "\\|{2}")#split by ||
  leftcontenttrue4=as.data.frame(leftcontenttrue3[1],col.names ='leftcontenttrue3')#creat header for rightcontent4true
  #get right data######################################################################
  rightcontenttrue=str_extract_all(string = heatmapdata,
                                   pattern = 'rightcontent="-?[\\d.E-a-za-z\\sA-Z]*(\\|{2}[\\#\\w\\s\'./();,-~&\\*\\@\\!\\`$+{}\\%\\^\\[\\]\\?_]*)*')
  #in pattern,we give character as soon as possible,
  #if oncominebar can not work and give error, remember to add new character to []
  #as follows:
  #Error in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  :
  #                      arguments imply differing number of rows: 3, 4
  
  rightcontenttrue1=gsub(pattern = 'rightcontent="',replacement = "",x = rightcontenttrue[judge==TRUE])
  rightcontenttrue2=gsub(pattern = '"',replacement = "",x = rightcontenttrue1)
  rightcontenttrue3=strsplit(x = rightcontenttrue2,split = "\\|{2}")
  #testtttttttttttt,aline
  for (i in 1:length(rightcontenttrue3)){
    b=length(rightcontenttrue3[[i]])
    if (b>nrow(leftcontenttrue4)){
      t1=rightcontenttrue3[[i]][nrow(leftcontenttrue4):b]
      t2=rightcontenttrue3[[i]][1:(nrow(leftcontenttrue4)-1)]
      rightcontenttrue3[[i]]=""
      rightcontenttrue3[[i]]=c(t2,toString(t1))
    }else if( b < nrow(leftcontenttrue4)){
      rightcontenttrue3[[i]][b:nrow(leftcontenttrue4)]='...'
    }
  }
  finaldatatrue1=as.data.frame(rightcontenttrue3,col.names = 1:length(rightcontenttrue3),row.names = leftcontenttrue4[,1])#use leftcontenttrue4 as row name
  #transform
  finaldatatrue2=as.data.frame(t(finaldatatrue1))
  #return data
  rownames(finaldatatrue2)=1:nrow(finaldatatrue2)
  n=grep(pattern = "Rank",x = names(finaldatatrue2))
  colnames(finaldatatrue2)[n]="Gene Rank"
  finaldatatrue2[,2]=as.numeric(as.character(finaldatatrue2[,2]))
  finaldatawhole=cbind(Gene,finaldatatrue2)
  finaldatawhole[finaldatawhole=="-"]<-NA
  finaldatawhole[,-1]<-sapply(finaldatawhole[,-1],as.numeric)
  return(finaldatawhole)
}