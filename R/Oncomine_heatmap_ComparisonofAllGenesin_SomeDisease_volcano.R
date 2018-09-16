#' @title to plot volcano heat map data of Comparison of All Genes in Some Disease in treatment and control
#'
#' @param filenames filenames can be omitted, a list of string
#'
#' @return a plot by plotly
#' @export
#'
#' @examples Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease_volcano()
#' @examples Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease_volcano("1.txt")
#' @examples Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease_volcano(c("1.txt","2.txt"))
Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease_volcano<-function(filenames){
  if(missing(filenames)){
    cbind4=suppressWarnings(Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease_manypages_filename_empty())
  }else{
    cbind4=suppressWarnings(Oncomine_heatmap_ComparisonofAllGenesin_SomeDisease_manypages_filename_not_empty(filenames))
  }
  if (length(unique(cbind4$`Legend Value`)) ==1){
    stop("Volcano plot is suitable for 2 groups of normal and cancer. Your data groups is 1")
  }else if(length(unique(cbind4$`Legend Value`)) >=3){
    #get legend1 as control legend group
    for (legend1 in 1:nrow(cbind4)){
      if (cbind4$`Legend Value`[legend1] != cbind4$`Legend Value`[legend1+1]){
        break
      }
    }
    #get the gene number
    for (Genei in 1:nrow(cbind4)){
      if (cbind4$Gene[Genei] != cbind4$Gene[Genei+1]){
        break
      }
    }

    log2_foldchange1=data.frame()
    log10_pvalue1=data.frame()

    foldloop=nrow(cbind4) %/%  Genei
    for (fd in 1:foldloop){
      fd_legend1=na.omit(cbind4$`Gene Expression`[(Genei*fd-(Genei-1)):(Genei*fd-(Genei-1)+(legend1-1))]) #the control
      fd_legend2=na.omit(cbind4$`Gene Expression`[(Genei*fd-(Genei-1)+legend1):(Genei*fd)]) #the other cancer subtype
      FoldChange=median(2^fd_legend2)/median(2^fd_legend1)
      Log2_foldchange=log(FoldChange,2) #get log2(foldchange)
      log2_foldchange1=rbind(log2_foldchange1,Log2_foldchange)#combine Log2_foldchange
      p_value=t.test(2^fd_legend1,2^fd_legend2)$p.value
      minus_Log10_Pvalue=-log(p_value,10) # get -log10(P)
      log10_pvalue1=rbind(log10_pvalue1,minus_Log10_Pvalue) #combine minus_Log10_Pvalue
    }

    Genenamn=seq(1,nrow(cbind4),Genei)
    Genename=cbind4$Gene[Genenamn]
    cGene=cbind(Genename,log2_foldchange1,log10_pvalue1)
    colnames(cGene)=c("Genename","log2_fd","log10_pvalue")

    datauniqueGene=unique(cGene)
    library(plotly)
    message("log2(fd) and log10(p.value) can only be refered. They have big difference from the real data.")
    plot_ly(data = datauniqueGene,
            x=~`log2_fd`,
            y=~`log10_pvalue`,
            type="scatter",
            mode="markers",
            text = paste("Gene: ", datauniqueGene$Genename)
    ) %>% layout(
      title="Volcano for Disease",
      xaxis = list(range = c(-max(datauniqueGene$log2_fd), max(datauniqueGene$log2_fd)))
    )
  }else{
        datauniqueGene=unique(cbind4[,c("Gene","log2_fd","log10_pvalue")])
        library(plotly)
        titleheatmap=paste("Volcano for Disease")
        message("log2(fd) and log10(p.value) can only be refered. They have big difference from the real data.")
        plot_ly(data = datauniqueGene,
                x=~`log2_fd`,
                y=~`log10_pvalue`,
                type="scatter",
                mode="markers",
                text = paste("Gene: ", datauniqueGene$Gene)
        ) %>% layout(
          title=titleheatmap,
          xaxis = list(range = c(-max(datauniqueGene$log2_fd), max(datauniqueGene$log2_fd)))
        )
  }
}
