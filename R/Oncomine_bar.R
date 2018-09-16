#' To download bar data
#' To download bar data frome Oncomine
#' @param filename string name
#' @param label_zero label zero data
#'
#' @return A dataframe
#' @export
#'
#' @examples Oncomine_bar("txt.txt")
Oncomine_bar <- function(filename,label_zero=TRUE){
  library("textreadr");library("stringr")
  if (missing(filename)){
    files=list.files() #get all the data picture
    txtcheck=files[grepl(pattern = ".txt",x = files)==FALSE]
    docxcheck=txtcheck[grepl(pattern = ".docx",x = txtcheck)==FALSE]
    doccheck=docxcheck[grepl(pattern = ".doc",x = docxcheck)==FALSE]
    if (length(doccheck)!=0 | length(files)>=2){
      doccheck1=paste(doccheck,collapse="\n")
      warning("The following files shoul be removed","\n",doccheck1)
      stop("Please make sure ONLY data file end with .txt .doc .docx including in the working directory. Or give object to this function")
    }
    if (length(doccheck)==0 & length(files)==1){
      filename=files
      if (grepl(".txt",filename)) { oncomine=suppressWarnings(paste(readLines(filename), collapse=" "))
      }else if(grepl(".doc",filename) | grepl(".docx",filename)) {oncomine=read_docx(filename)}

      whole=str_extract(string =oncomine,pattern = '<area.*\">')
      finaldata=strsplit(x = whole,split = '<area')[[1]][-1]
      judge= grepl('Legend Value' ,finaldata)

      if (FALSE %in% judge) {
        origindatatrue=finaldata[judge==TRUE]
        origindatafalse=finaldata[judge==FALSE]
      } else {
        origindatatrue=finaldata[judge==TRUE]
      }

      if ( (FALSE %in% judge) & (label_zero=TRUE) ){
        leftcontentfalse=str_extract_all(string = origindatafalse,pattern = 'leftcontent="[\\w\\s\\:\\|]*')
        leftcontentfalse1=gsub(pattern = 'leftcontent="',replacement = "",x = leftcontentfalse)
        leftcontentfalse2=gsub(pattern = ":",replacement = "",x = leftcontentfalse1)
        leftcontentfalse3=strsplit(x = leftcontentfalse2,split = "\\|{2}")
        leftcontentfalse4=as.data.frame(leftcontentfalse3[1],col.names ='leftcontentfalse3')

        rightcontentfalse=str_extract_all(string = origindatafalse,
                                          pattern = 'rightcontent="-?[\\d.E-]*(\\|{2}[\\#\\w\\s\'./();,-~&\\*\\@\\!\\`$+{}\\%\\^\\[\\]\\?_]*)*')
        rightcontentfalse1=gsub(pattern = 'rightcontent="',replacement = "",x = rightcontentfalse)
        rightcontentfalse2=gsub(pattern = '"',replacement = "",x = rightcontentfalse1)
        rightcontentfalse3=strsplit(x = rightcontentfalse2,split = "\\|{2}")
        for (i in 1:length(rightcontentfalse3)){
          b=length(rightcontentfalse3[[i]])
          if (b>nrow(leftcontentfalse4)){
            t1=rightcontentfalse3[[i]][nrow(leftcontentfalse4):b]
            t2=rightcontentfalse3[[i]][1:(nrow(leftcontentfalse4)-1)]
            rightcontentfalse3[[i]]=""
            rightcontentfalse3[[i]]=c(t2,toString(t1))
          }else if( b < nrow(leftcontentfalse4)){
            rightcontentfalse3[[i]][b:nrow(leftcontentfalse4)]='...'
          }
        }
        finaldatafalse1=as.data.frame(rightcontentfalse3,col.names = 1:length(rightcontentfalse3),row.names = leftcontentfalse4[,1])
        finaldatafalse2=as.data.frame(t(finaldatafalse1))
      }

      leftcontenttrue=str_extract_all(string = origindatatrue,pattern = 'leftcontent="[\\w\\s\\:\\|]*')
      leftcontenttrue1=gsub(pattern = 'leftcontent="',replacement = "",x = leftcontenttrue)
      leftcontenttrue2=gsub(pattern = ":",replacement = "",x = leftcontenttrue1)
      leftcontenttrue3=strsplit(x = leftcontenttrue2,split = "\\|{2}")
      leftcontenttrue4=as.data.frame(leftcontenttrue3[1],col.names ='leftcontenttrue3')

      rightcontenttrue=str_extract_all(string = origindatatrue,
                                       pattern = 'rightcontent="-?[\\d.E-]*(\\|{2}[\\#\\w\\s\'./();,-~&\\*\\@\\!\\`$+{}\\%\\^\\[\\]\\?_]*)*')

      rightcontenttrue1=gsub(pattern = 'rightcontent="',replacement = "",x = rightcontenttrue)
      rightcontenttrue2=gsub(pattern = '"',replacement = "",x = rightcontenttrue1)
      rightcontenttrue3=strsplit(x = rightcontenttrue2,split = "\\|{2}")

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
      finaldatatrue1=as.data.frame(rightcontenttrue3,col.names = 1:length(rightcontenttrue3),row.names = leftcontenttrue4[,1])

      finaldatatrue2=as.data.frame(t(finaldatatrue1))
      if ( (FALSE %in% judge) & (label_zero==TRUE) ){
        finaldatatrue2[setdiff(names(finaldatafalse2), names(finaldatatrue2))] <- 'Cancer'
        finaldatafalse2[setdiff(names(finaldatatrue2), names(finaldatafalse2))] <- 'No value'
        finaldatacombine=rbind(finaldatafalse2,finaldatatrue2)
        rownames(finaldatacombine)=1:nrow(finaldatacombine)
        if ("Copy Number value" %in% names(finaldatacombine)){
          finaldatacombine[,"Copy Number value"]=as.numeric(as.character(finaldatacombine[,"Copy Number value"]))}
        if ("Expression value" %in% names(finaldatacombine)){
          finaldatacombine[,"Expression value"]=as.numeric(as.character(finaldatacombine[,"Expression value"]))}
        return(finaldatacombine)
      }else{
        rownames(finaldatatrue2)=1:nrow(finaldatatrue2)
        finaldatatrue2[,"Expression value"]=as.numeric(as.character(finaldatatrue2[,"Expression value"]))
        return(finaldatatrue2)
      }
    }#else if (length(file_number_check)==1)

  }else if (is.character(filename)){
    if (grepl(".txt",filename)) { oncomine=suppressWarnings(paste(readLines(filename), collapse=" "))
    }else if(grepl(".doc",filename) | grepl(".docx",filename)) {oncomine=read_docx(filename)}
    whole=str_extract(string =oncomine,pattern = '<area.*\">')
    finaldata=strsplit(x = whole,split = '<area')[[1]][-1]
    judge= grepl('Legend Value' ,finaldata)

    if (FALSE %in% judge) {
      origindatatrue=finaldata[judge==TRUE]
      origindatafalse=finaldata[judge==FALSE]
    } else {
      origindatatrue=finaldata[judge==TRUE]
    }

    if ( (FALSE %in% judge) & (label_zero=TRUE) ){
      leftcontentfalse=str_extract_all(string = origindatafalse,pattern = 'leftcontent="[\\w\\s\\:\\|]*')
      leftcontentfalse1=gsub(pattern = 'leftcontent="',replacement = "",x = leftcontentfalse)
      leftcontentfalse2=gsub(pattern = ":",replacement = "",x = leftcontentfalse1)
      leftcontentfalse3=strsplit(x = leftcontentfalse2,split = "\\|{2}")
      leftcontentfalse4=as.data.frame(leftcontentfalse3[1],col.names ='leftcontentfalse3')

      rightcontentfalse=str_extract_all(string = origindatafalse,
                                        pattern = 'rightcontent="-?[\\d.E-]*(\\|{2}[\\#\\w\\s\'./();,-~&\\*\\@\\!\\`$+{}\\%\\^\\[\\]\\?_]*)*')
      rightcontentfalse1=gsub(pattern = 'rightcontent="',replacement = "",x = rightcontentfalse)
      rightcontentfalse2=gsub(pattern = '"',replacement = "",x = rightcontentfalse1)
      rightcontentfalse3=strsplit(x = rightcontentfalse2,split = "\\|{2}")
      for (i in 1:length(rightcontentfalse3)){
        b=length(rightcontentfalse3[[i]])
        if (b>nrow(leftcontentfalse4)){
          t1=rightcontentfalse3[[i]][nrow(leftcontentfalse4):b]
          t2=rightcontentfalse3[[i]][1:(nrow(leftcontentfalse4)-1)]
          rightcontentfalse3[[i]]=""
          rightcontentfalse3[[i]]=c(t2,toString(t1))
        }else if( b < nrow(leftcontentfalse4)){
          rightcontentfalse3[[i]][b:nrow(leftcontentfalse4)]='...'
        }
      }
      finaldatafalse1=as.data.frame(rightcontentfalse3,col.names = 1:length(rightcontentfalse3),row.names = leftcontentfalse4[,1])
      finaldatafalse2=as.data.frame(t(finaldatafalse1))
    }

    leftcontenttrue=str_extract_all(string = origindatatrue,pattern = 'leftcontent="[\\w\\s\\:\\|]*')
    leftcontenttrue1=gsub(pattern = 'leftcontent="',replacement = "",x = leftcontenttrue)
    leftcontenttrue2=gsub(pattern = ":",replacement = "",x = leftcontenttrue1)
    leftcontenttrue3=strsplit(x = leftcontenttrue2,split = "\\|{2}")
    leftcontenttrue4=as.data.frame(leftcontenttrue3[1],col.names ='leftcontenttrue3')

    rightcontenttrue=str_extract_all(string = origindatatrue,
                                     pattern = 'rightcontent="-?[\\d.E-]*(\\|{2}[\\#\\w\\s\'./();,-~&\\*\\@\\!\\`$+{}\\%\\^\\[\\]\\?_]*)*')

    rightcontenttrue1=gsub(pattern = 'rightcontent="',replacement = "",x = rightcontenttrue)
    rightcontenttrue2=gsub(pattern = '"',replacement = "",x = rightcontenttrue1)
    rightcontenttrue3=strsplit(x = rightcontenttrue2,split = "\\|{2}")

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
    finaldatatrue1=as.data.frame(rightcontenttrue3,col.names = 1:length(rightcontenttrue3),row.names = leftcontenttrue4[,1])

    finaldatatrue2=as.data.frame(t(finaldatatrue1))
    if ( (FALSE %in% judge) & (label_zero==TRUE) ){
      finaldatatrue2[setdiff(names(finaldatafalse2), names(finaldatatrue2))] <- 'Cancer'
      finaldatafalse2[setdiff(names(finaldatatrue2), names(finaldatafalse2))] <- 'No value'
      finaldatacombine=rbind(finaldatafalse2,finaldatatrue2)
      rownames(finaldatacombine)=1:nrow(finaldatacombine)
      if ("Copy Number value" %in% names(finaldatacombine)){
        finaldatacombine[,"Copy Number value"]=as.numeric(as.character(finaldatacombine[,"Copy Number value"]))}
      if ("Expression value" %in% names(finaldatacombine)){
        finaldatacombine[,"Expression value"]=as.numeric(as.character(finaldatacombine[,"Expression value"]))}
      return(finaldatacombine)
    }else{
      rownames(finaldatatrue2)=1:nrow(finaldatatrue2)
      finaldatatrue2[,"Expression value"]=as.numeric(as.character(finaldatatrue2[,"Expression value"]))
      return(finaldatatrue2)
    }
  }
}
