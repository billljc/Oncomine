Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease_manypages_filename_empty<-function(){
  files=list.files() #get all the data picture
  txtcheck=files[grepl(pattern = ".txt",x = files)==FALSE]
  docxcheck=txtcheck[grepl(pattern = ".docx",x = txtcheck)==FALSE]
  doccheck=docxcheck[grepl(pattern = ".doc",x = docxcheck)==FALSE]
  if (length(doccheck)==0){
    cbind3=data.frame()
    cbind_legend13=data.frame()

    for (filenumber in 1:length(files)){
      finaldatawhole=Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease_onepage(files[filenumber])

      legendlengjudge=length(unique(finaldatawhole[,"Legend Value"]))
      if ((legendlengjudge==1) | (legendlengjudge >= 3)){
        if (length(files) >=2 ){
          file=files[filenumber]
          finaldatawhole=cbind(finaldatawhole,file)
        }
        cbind_legend13=rbind(cbind_legend13,finaldatawhole)
      }else if(legendlengjudge==2){
        for (k in 1:nrow(finaldatawhole)){ # k is column number per row
          if (finaldatawhole[k,1]!=finaldatawhole[k+1,1]){
            break #just break to get k goal genes in the first file
          }
        }

        for (p in 1:nrow(finaldatawhole)){
          if (finaldatawhole[p,3]!=finaldatawhole[p+1,3]){
            break #just break to get k goal genes in the first file
          }
        }
        q=k-p # q is the number of legend2
        #caculate p value and fold change
        foldloop=nrow(finaldatawhole) %/%  k
        log2_foldchange1=data.frame()
        log10_pvalue1=data.frame()
        for (fd in 1:(foldloop)){
          finaldatawholei=finaldatawhole[(k*(fd-1)+1):(k*fd),] #data per row
          if (length(unique(finaldatawholei[,1]))==1){
            fd_legend1=na.omit(finaldatawholei[1:p,2])
            fd_legend2=na.omit(finaldatawholei[(p+1):k,2])
            FoldChange=median(2^fd_legend2)/median(2^fd_legend1)
            Log2_foldchange=log(FoldChange,2) #get log2(foldchange)
            log2_foldchange1=rbind(log2_foldchange1,Log2_foldchange)#combine Log2_foldchange
            p_value=t.test(2^fd_legend1,2^fd_legend2)$p.value
            minus_Log10_Pvalue=-log(p_value,10) # get -log10(P)
            log10_pvalue1=rbind(log10_pvalue1,minus_Log10_Pvalue) #combine minus_Log10_Pvalue
          }else{
            warning("Row from ",(k*(cp-1)+1)," to ",(k*cp)," does not have the same name. P value and fold change will not be caculated")
            log2_foldchange1=rbind(log2_foldchange1,NA)#combine Log2_foldchange
            log10_pvalue1=rbind(log10_pvalue1,NA) #combine minus_Log10_Pvalue
            next
          }
        }
        gene_leftn=nrow(finaldatawhole) %%  k #the gene were left

        if (gene_leftn>p){
          gene_tail=tail(finaldatawhole, gene_leftn)
          fd_legend1=na.omit(gene_tail[1:p,2])
          fd_legend2=na.omit(gene_tail[(p+1):gene_leftn,2])
          FoldChange=median(2^fd_legend2)/median(2^fd_legend1)
          Log2_foldchange=log(FoldChange,2) #get log2(foldchange)
          log2_foldchange1=rbind(log2_foldchange1,Log2_foldchange)#combine Log2_foldchange
          p_value=t.test(fd_legend1,fd_legend2)$p.value
          minus_Log10_Pvalue=-log(p_value,10) # get -log10(P)
          log10_pvalue1=rbind(log10_pvalue1,minus_Log10_Pvalue) #combine minus_Log10_Pvalue
        }else if(gene_leftn==0){
          cat("")
        }else{
          warning("This Gene was not included: ",Genename[length(seq(1,nrow(finaldatawhole),k))])
        }
        rep1=rep(log2_foldchange1[,1],each=k)#repet log2_foldchange1
        rep2=rep(log10_pvalue1[,1],each=k)#repet log10_pvalue
        log2_fd=rep1[1:nrow(finaldatawhole)]#delet rendent rep1
        log10_pvalue=rep2[1:nrow(finaldatawhole)]#delet rendent rep2
        cbind1=cbind(finaldatawhole,log2_fd,log10_pvalue)
        if (length(files)>1){
          file=filenumber
          cbind2=cbind(file,cbind1)
          cbind3=rbind(cbind3,cbind2)
        }else{
          cbind3=cbind1
        }
        #cbind2 is the last data
      }
    }

  }else{
    stop(" Files should be ended wiht .txt .docx .doc","\n","The following files don't meet that condition.","\n")
    cat(doccheck)
  }
  #judge to output
  if (nrow(cbind3)!=0 &  nrow(cbind_legend13)!=0){
    cbind3[setdiff(names(cbind_legend13), names(cbind3))] <- 0
    cbind_legend13[setdiff(names(cbind3), names(cbind_legend13))] <- 0
    cbind3=rbind(cbind3,cbind_legend13)
    return(cbind3)
  } else if (nrow(cbind3)!=0 &  nrow(cbind_legend13)==0){
    return(cbind3)
  } else if (nrow(cbind3)==0 &  nrow(cbind_legend13)!=0){
    return(cbind_legend13)
  }
}
