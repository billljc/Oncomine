Oncomine_heatmap_Comparison.of.Concept_onepage_notempty <- function(filename){
  txtcheck=filename[grepl(pattern = ".txt",x = filename)==FALSE]
  docxcheck=txtcheck[grepl(pattern = ".docx",x = txtcheck)==FALSE]
  doccheck=docxcheck[grepl(pattern = ".doc",x = docxcheck)==FALSE]
  if (length(doccheck)==0){
      library("textreadr")
      library("stringr")
    
      #read the long string
      if (grepl(".txt",filename)) { oncomine=suppressWarnings(paste(readLines(filename), collapse=" "))
      }else if(grepl(".doc",filename) | grepl(".docx",filename)) {oncomine=read_docx(filename)}
      #get whole data
      whole=str_extract(string =oncomine,pattern = '<area.*\">')#delet hat and tail
      finaldata=strsplit(x = whole,split = '<area')[[1]][-1]#split to unit, after split ,the first list is space so we delet it
      ###################################
      #get 4 parts data
      #part1 geneInfoRight
      geneInfoRight.judge=ifelse(grepl(pattern = "geneInfoRight",x = finaldata),1,0)
      geneInfoRight=finaldata[geneInfoRight.judge==1]
      #part2 geneInfoLeft
      geneInfoLeft.judge=ifelse(grepl(pattern = "geneInfoLeft",x = finaldata),1,0)
      geneInfoLeft=finaldata[geneInfoLeft.judge==1]
      #part3 reporterInfo
      reporterInfo.judge=ifelse(grepl(pattern = "reporterInfo",x = finaldata),1,0)
      reporterInfo=finaldata[reporterInfo.judge==1]
      #part4 heatmap data
      heatmapdata.judge=geneInfoRight.judge+geneInfoLeft.judge+reporterInfo.judge
      heatmapdata=finaldata[heatmapdata.judge == 0]
      #get 4 parts over
      ###################################
      ####################################################       heatmap begin
      #deal with heatmap
        #left
      heatmapdata.l1=sub(pattern = ".*om4:leftcontent=.",replacement = "",x = heatmapdata)
      heatmapleft1=sub(pattern = ".{2}om4:rightcontent=.*",replacement = "",x = heatmapdata.l1)
      heatmapleft2=gsub(pattern = ":",replacement = "",x = heatmapleft1)
      heatmapleft3=str_split(string = heatmapleft2,pattern = "\\|{2}")
        #right
      heatmapdata.r1=sub(pattern = ".*om4:rightcontent=.",replacement = "",x = heatmapdata)
      heatmapright1=sub(pattern = ".coords=.*",replacement = "",x = heatmapdata.r1)
      heatmapright2=gsub(pattern = "\"",replacement = "",x = heatmapright1)
      heatmapright3=str_split(string = heatmapright2,pattern = "\\|{2}")
      #get left content as column title
      
      heatmapdata.temp=data.frame()
      for (i in 1:length(heatmapright3)){
        heatmapdata.i=data.frame(t(heatmapright3[[i]]),row.names = i)
        colnames(heatmapdata.i)=c("Expression",heatmapleft3[[i]][-1])
        heatmapdata.i[setdiff(names(heatmapdata.temp), names(heatmapdata.i))] <- ''
        if (i >=2 ){heatmapdata.temp[setdiff(names(heatmapdata.i), names(heatmapdata.temp))] <- ''}
        heatmapdata.temp=rbind(heatmapdata.temp,heatmapdata.i)
        heatmapdata.temp$Expression=as.numeric(as.character(heatmapdata.temp$Expression))
      }
      ####################################################       heatmap over
      ####################################################       gene begin
      reportfirst=grep(pattern = 1,x = reporterInfo.judge)[1]
      genefirst=grep(pattern = 1,x = geneInfoLeft.judge)[1]
      genenumber.perrow=reportfirst-1-genefirst                  #genenumber.perrow
      geneInfoLeft1=sub(pattern = ".*om4:leftcontent=.",replacement = "",x = geneInfoLeft)
      geneInfoLeft2=sub(pattern = ".om4:right.*",replacement = "",x = geneInfoLeft1)
      geneInfoLeft3=sub(pattern = '"',replacement = "",x = geneInfoLeft2)
      geneInfoLeft4=sub(pattern = ":",replacement = "",x = geneInfoLeft3)
      Gene=rep(geneInfoLeft4,each=genenumber.perrow)             #all Gene
      geneInfoLeft.r1=sub(pattern = ".*om4:rightcontent=.",replacement = "",x = geneInfoLeft)
      geneInfoLeft.r2=sub(pattern = ".coords.*",replacement = "",x = geneInfoLeft.r1)
      geneInfoLeft.r3=sub(pattern = '"',replacement = "",x = geneInfoLeft.r2)
      GeneInfo=rep(geneInfoLeft.r3,each=genenumber.perrow)
      ####################################################       gene over
      ####################################################       reporter begin
      reporterInfo1=sub(pattern = ".*\\(",replacement = "",x = reporterInfo)
      reporterInfo2=sub(pattern = '\\).*',replacement = '',x = reporterInfo1)
      reporterInfo3=rep(reporterInfo2,each=genenumber.perrow)
      reporterLink=paste0('https://www.oncomine.org/resource/ui/component/reporterInformation.html?component=f:',reporterInfo3)
      ####################################################       reporter over
      heatmapdata.whole=cbind(Gene,heatmapdata.temp,GeneInfo,reporterLink)
      return(heatmapdata.whole)
  }else{
    stop(" Files should be ended wiht .txt .docx .doc","\n","The following files don't meet that condition.","\n")
    cat(doccheck)
  }
}
