#' To dowanlaod box data
#' To dowanlaod Oncomine box data
#' @param filename string file name
#' @param write_csv judge wheter to output box data
#' @return A dataframe
#' @export
#'
#' @examples Oncomine_box("txt.txt")
Oncomine_box <- function(filename,write_csv=FALSE){
  library("textreadr");library("stringr")
  if (missing(filename)){
    file_number_check=list.files()
    if (
      (length(file_number_check)>=2) |
      (length(file_number_check)==0)
    ){
      stop("Please make sure ONLY ONE file for bar plot data in your working directory")
    }else if (length(file_number_check)==1){
      filename=list.files()
    #read the long string
    if (grepl(".txt",filename)) { oncomine=suppressWarnings(paste(readLines(filename), collapse=" "))
    }else if(grepl(".doc",filename) | grepl(".docx",filename)) {oncomine=read_docx(filename)}

    #get whole data
    whole=str_extract(string =oncomine,pattern = '<area.*\">')#delet hat and tail
    ####get true true true  true  data
    #get left content as column title
    leftcontent=str_extract_all(string = whole,pattern = 'leftcontent="[\\w\\s\\:\\|-]*')#extract left content

    leftcontent1=gsub(pattern = 'leftcontent="',replacement = "",x = leftcontent[[1]])#delet leftcontent=

    leftcontent2=gsub(pattern = ":",replacement = "",x = leftcontent1)#delet :
    leftcontent3=strsplit(x = leftcontent2,split = "\\|{2}")#split by ||
    leftcontent4=as.data.frame(leftcontent3[1],col.names ='leftcontent3')#creat header for rightcontent4true

    #get right data######################################################################
    rightcontenttrue=str_extract_all(string = whole,
                                     pattern = 'rightcontent="-?[\\d.E-]*(\\|{2}[\\#\\w\\s\'./();,-~&\\*\\@\\!\\`$+{}\\%\\^\\[\\]\\?_]*)*')
    #in pattern,we give character as soon as possible,
    #if oncominebar can not work and give error, remember to add new character to []
    #as follows:
    #Error in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  :
    #                      arguments imply differing number of rows: 3, 4

    rightcontenttrue1=gsub(pattern = 'rightcontent="',replacement = "",x = rightcontenttrue[[1]])
    rightcontenttrue2=gsub(pattern = '"',replacement = "",x = rightcontenttrue1)
    rightcontenttrue3=strsplit(x = rightcontenttrue2,split = "\\|{2}")

    finaldatatrue1=as.data.frame(rightcontenttrue3,col.names = 1:length(rightcontenttrue3),row.names = leftcontent4[,1])#use leftcontent4 as row name
    #transform
    finaldatatrue2=as.data.frame(t(finaldatatrue1))
    #return data
    rownames(finaldatatrue2)=1:nrow(finaldatatrue2)
    rownames(finaldatatrue2)=finaldatatrue2[,"Legend Value"]
    finaldatatrue3=finaldatatrue2[,-grep("Legend Value",colnames(finaldatatrue2))]
    finaldatatrue4=data.frame(t(finaldatatrue3))
    for (i in 1:ncol(finaldatatrue4)){
      finaldatatrue4[,i]<-as.numeric(as.character(finaldatatrue4[,i]))
    }
    finaldatatrue4<-data.frame(finaldatatrue4)
    if (write_csv==TRUE){write.csv(finaldatatrue4,"boxdata.csv")}
    return(finaldatatrue4)
}
  }else if (is.character(filename)){
    #read the long string
    if (grepl(".txt",filename)) { oncomine=suppressWarnings(paste(readLines(filename), collapse=" "))
    }else if(grepl(".doc",filename) | grepl(".docx",filename)) {oncomine=read_docx(filename)}

    #get whole data
    whole=str_extract(string =oncomine,pattern = '<area.*\">')#delet hat and tail
    ####get true true true  true  data
    #get left content as column title
    leftcontent=str_extract_all(string = whole,pattern = 'leftcontent="[\\w\\s\\:\\|-]*')#extract left content

    leftcontent1=gsub(pattern = 'leftcontent="',replacement = "",x = leftcontent[[1]])#delet leftcontent=

    leftcontent2=gsub(pattern = ":",replacement = "",x = leftcontent1)#delet :
    leftcontent3=strsplit(x = leftcontent2,split = "\\|{2}")#split by ||
    leftcontent4=as.data.frame(leftcontent3[1],col.names ='leftcontent3')#creat header for rightcontent4true

    #get right data######################################################################
    rightcontenttrue=str_extract_all(string = whole,
                                     pattern = 'rightcontent="-?[\\d.E-]*(\\|{2}[\\w\\s\'./();,-~&\\*\\@\\!\\`$+{}\\%\\^\\[\\]\\?_]*)*')
    #in pattern,we give character as soon as possible,
    #if oncominebar can not work and give error, remember to add new character to []
    #as follows:
    #Error in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  :
    #                      arguments imply differing number of rows: 3, 4

    rightcontenttrue1=gsub(pattern = 'rightcontent="',replacement = "",x = rightcontenttrue[[1]])
    rightcontenttrue2=gsub(pattern = '"',replacement = "",x = rightcontenttrue1)
    rightcontenttrue3=strsplit(x = rightcontenttrue2,split = "\\|{2}")

    finaldatatrue1=as.data.frame(rightcontenttrue3,col.names = 1:length(rightcontenttrue3),row.names = leftcontent4[,1])#use leftcontent4 as row name
    #transform
    finaldatatrue2=as.data.frame(t(finaldatatrue1))
    #return data
    rownames(finaldatatrue2)=1:nrow(finaldatatrue2)
    rownames(finaldatatrue2)=finaldatatrue2[,"Legend Value"]
    finaldatatrue3=finaldatatrue2[,-grep("Legend Value",colnames(finaldatatrue2))]
    finaldatatrue4=data.frame(t(finaldatatrue3))
    for (i in 1:ncol(finaldatatrue4)){
      finaldatatrue4[,i]<-as.numeric(as.character(finaldatatrue4[,i]))
    }
    finaldatatrue4<-data.frame(finaldatatrue4)
    if (write_csv==TRUE){write.csv(finaldatatrue4,"boxdata.csv")}
    return(finaldatatrue4)
  }else{
    stop("Please make sure your filename is character")
  }
}
