#' To get studys that research the same Genes
#' @description You can use this function to get the studys that reasearch the sames Genes. This funtion can not be used in Oncomine web for free. We design 2 styles. First is Venn plot, which can be ploted for no more than 7 Genes. Venn data is the other style, Which is suitable for Genes whiout limitation. We also recomend you to view this data in EXCEL.
#' @param filesname The names of files which contain the data of each Gene. We recomend the file name is identical with Gene.
#' @param Plot Plot is a logical object. TRUE to plot Venn and as defaulted. FALSE refuse to do.
#' @param data data is a logical object. TRUE, as defaulted, to Get Venn data which includes a binary combination and number of study and study name. FALSE refuse to do.
#' @param cexsn to adjust the size of number in venn
#' @param cexil to adjust the size of label beside venn
#'
#' @return a dataframe and a venn plot
#' @export
#' @author Jing Zhang
#' @examples Oncomine_datasets_forGenes_Venn(c( "ASXL1.txt", "KRAS.txt",  "SF3B1.txt", "TP53.txt" ))
#' 
Oncomine_datasets_forGenes_Venn<-function(filesname,Plot=TRUE,data=TRUE,cexsn=1,cexil=1){
  library(venn)  
  if(missing(filesname)){
    filesname=list.files() #get all the data picture
  }
  if (length(filesname)==0){
    stop("You only have 0 file to plot venn")
  }
  if (length(filesname)==1){
    stop("You only have one file, which is not enough to plot venn")
  }
  txtcheck=filesname[grepl(pattern = ".txt",x = filesname)==FALSE]
  docxcheck=txtcheck[grepl(pattern = ".docx",x = txtcheck)==FALSE]
  doccheck=docxcheck[grepl(pattern = ".doc",x = docxcheck)==FALSE]
  if (length(doccheck)==0){
    snames=c()
    for (j in 1:length(filesname)){
      if (grepl(".txt",filesname[j])){
        snames.i=sub(pattern = ".txt",replacement = "",x = filesname[j])
        snames=c(snames,snames.i)
      }
      if (grepl(".doc",filesname[j])){
        snames.i=sub(pattern = ".doc",replacement = "",x = filesname[j])
        snames=c(snames,snames.i)
      }
      if (grepl(".docx",filesname[j])){
        snames.i=sub(pattern = ".docx",replacement = "",x = filesname[j])
        snames=c(snames,snames.i)
      }
    }
    #judge to plot
    if (Plot==TRUE){
      if (length(filesname)==2){
        venndata1=Oncomine_datasets_forGenes(filesname[1],Simple = TRUE)
        venndata2=Oncomine_datasets_forGenes(filesname[2],Simple = TRUE)
        venn(x=list(venndata1$Study,venndata2$Study),
             snames = snames,cexsn = cexsn,cexil = cexil)
      }
      if (length(filesname)==3){
        venndata1=Oncomine_datasets_forGenes(filesname[1],Simple = TRUE)
        venndata2=Oncomine_datasets_forGenes(filesname[2],Simple = TRUE)
        venndata3=Oncomine_datasets_forGenes(filesname[3],Simple = TRUE)
        venn(x=list(venndata1$Study,venndata2$Study,venndata3$Study),
             snames = snames,cexsn = cexsn,cexil = cexil)
      }
      if (length(filesname)==4){
        venndata1=Oncomine_datasets_forGenes(filesname[1],Simple = TRUE)
        venndata2=Oncomine_datasets_forGenes(filesname[2],Simple = TRUE)
        venndata3=Oncomine_datasets_forGenes(filesname[3],Simple = TRUE)
        venndata4=Oncomine_datasets_forGenes(filesname[4],Simple = TRUE)
        venn(x=list(venndata1$Study,venndata2$Study,venndata3$Study,venndata4$Study),
             snames = snames,cexsn = cexsn,cexil = cexil)
      }
      if (length(filesname)==5){
        venndata1=Oncomine_datasets_forGenes(filesname[1],Simple = TRUE)
        venndata2=Oncomine_datasets_forGenes(filesname[2],Simple = TRUE)
        venndata3=Oncomine_datasets_forGenes(filesname[3],Simple = TRUE)
        venndata4=Oncomine_datasets_forGenes(filesname[4],Simple = TRUE)
        venndata5=Oncomine_datasets_forGenes(filesname[5],Simple = TRUE)
        venn(x=list(venndata1$Study,venndata2$Study,venndata3$Study,venndata4$Study,venndata5$Study),
             snames = snames,cexsn = cexsn,cexil = cexil)
      }
      if (length(filesname)==6){
        venndata1=Oncomine_datasets_forGenes(filesname[1],Simple = TRUE)
        venndata2=Oncomine_datasets_forGenes(filesname[2],Simple = TRUE)
        venndata3=Oncomine_datasets_forGenes(filesname[3],Simple = TRUE)
        venndata4=Oncomine_datasets_forGenes(filesname[4],Simple = TRUE)
        venndata5=Oncomine_datasets_forGenes(filesname[5],Simple = TRUE)
        venndata6=Oncomine_datasets_forGenes(filesname[6],Simple = TRUE)
        venn(x=list(venndata1$Study,venndata2$Study,venndata3$Study,venndata4$Study,venndata5$Study,venndata6$Study),
             snames = snames,cexsn = cexsn,cexil = cexil)
      }
      if (length(filesname)==7){
        venndata1=Oncomine_datasets_forGenes(filesname[1],Simple = TRUE)
        venndata2=Oncomine_datasets_forGenes(filesname[2],Simple = TRUE)
        venndata3=Oncomine_datasets_forGenes(filesname[3],Simple = TRUE)
        venndata4=Oncomine_datasets_forGenes(filesname[4],Simple = TRUE)
        venndata5=Oncomine_datasets_forGenes(filesname[5],Simple = TRUE)
        venndata6=Oncomine_datasets_forGenes(filesname[6],Simple = TRUE)
        venndata7=Oncomine_datasets_forGenes(filesname[7],Simple = TRUE)
        venn(x=list(venndata1$Study,venndata2$Study,venndata3$Study,venndata4$Study,venndata5$Study,venndata6$Study,venndata7$Study),
             snames = snames,cexsn = cexsn,cexil = cexil)
      }
      if (length(filesname)>=8){
        message("We can only Plot venn for no more than 7 Genes. Your number of Genes is ",length(filesname))
      }
    }
    # to get data
    if (data==TRUE){
      #dataframe
      vennframe1=comb.binary(snames)
      vennframe=vennframe1[rowSums(vennframe1) !=0,]
      venn.c=c()
      venn.n=c()
      for (vennframe.i in 1:nrow(vennframe)) {
        if (sum(vennframe[vennframe.i,])==1){
          study.in=Oncomine_datasets_forGenes(filesname[grep(pattern = 1,x = vennframe[vennframe.i,])])$Study
          study.both=unique(as.character(study.in))
          study.out=unique(Oncomine_datasets_forGenes(filesname[-grep(pattern = 1,x = vennframe[vennframe.i,])])$Study)
          venn.c.i=paste(as.character(study.both[(study.both %in% study.out)==FALSE]),collapse = ";")
          venn.n.i=length(as.character(study.both[(study.both %in% study.out)==FALSE]))
          venn.c=c(venn.c,venn.c.i)
          venn.n=c(venn.n,venn.n.i)
        }else if(vennframe.i==nrow(vennframe)){
          study.in=Oncomine_datasets_forGenes(filesname[grep(pattern = 1,x = vennframe[vennframe.i,])])$Study
          study.both=as.character(names(table(study.in))[table(study.in)==sum(vennframe[vennframe.i,])])
          venn.c.i=paste(study.both,collapse = ";")
          venn.c=c(venn.c,venn.c.i)
          venn.n.i=length(study.both)
          venn.n=c(venn.n,venn.n.i)
        }else{
          study.in=Oncomine_datasets_forGenes(filesname[grep(pattern = 1,x = vennframe[vennframe.i,])])$Study
          venn.judge=ifelse(table(study.in)==sum(vennframe[vennframe.i,]),
                            1,0)
          study.both=unique(as.character(names(table(study.in))[venn.judge==1] ))
          study.out=unique(Oncomine_datasets_forGenes(filesname[-grep(pattern = 1,x = vennframe[vennframe.i,])])$Study)
          venn.c.i=paste(as.character(study.both[(study.both %in% study.out)==FALSE]),collapse = ";")
          venn.c=c(venn.c,venn.c.i)
          venn.n.i=length(as.character(study.both[(study.both %in% study.out)==FALSE]))
          venn.n=c(venn.n,venn.n.i)
        }
      }
      venn.data=cbind(vennframe,study.no=venn.n,study=venn.c)
      return(venn.data)
    } 
  }else{
    doccheck1=paste(doccheck,collapse="\n")
    warning("The following files shoul be removed","\n",doccheck1)
    stop("Please make sure ONLY data file end with .txt .doc .docx including in the working directory. Or give object to this function")
  }
}