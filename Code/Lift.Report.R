Lift.Report = function(data,link,prediction,actual=NULL,current = NULL,eexp = NULL,by.variable=NULL,bucket, balance = TRUE ){
  
  item_list = c(link, prediction,if(!is.null(actual))(actual),if(!is.null(current))(current), if(!is.null(eexp))(eexp),if(!is.null(by.variable))(by.variable))
  temp_data = data[, item_list]
  
  names(temp_data) = c("link","prediction",if(!is.null(actual))("actual"),if(!is.null(current))("current"), if(!is.null(eexp))("eexp"),if(!is.null(by.variable))(by.variable))
  if (is.null(eexp)){temp_data$eexp = 1}
  if (is.null(by.variable)){
    by.variable =  "by.variable"
    temp_data$by.variable = 1
  }
  
  temp_data = temp_data[order(temp_data$link),]
  
  adjustment = 1
  if (balance == TRUE) { 
    adjustment =sum(temp_data$prediction)/sum(temp_data$actual)
    temp_data$prediction = temp_data$prediction/adjustment
  }
  Report = NULL
  Lift = NULL
  
  for (level in sort(unique(temp_data[[by.variable]]))){
    level_data = temp_data[temp_data[[by.variable]] == level,]
    bucket_eexp = sum(level_data$eexp)/bucket
    
    level_data$index = ceiling(cumsum(level_data$eexp)/bucket_eexp)
    level_data$index[level_data$index>bucket]=bucket
    
    level_Report = aggregate(eval(bquote(cbind(prediction, .(if (!is.null(actual)) as.name("actual")), .(if (!is.null(current)) as.name("current")),eexp,link)~.(as.name(by.variable)) + index)),data = level_data, FUN =sum)
    level_Report$prediction = level_Report$prediction/level_Report$eexp
    if (!is.null(actual)) {level_Report$actual = level_Report$actual/level_Report$eexp}
    if (!is.null(current)) {level_Report$current = level_Report$current/level_Report$eexp}
    
    Report = rbind(Report, level_Report)
    Lift = rbind(Lift , level_Report$actual[bucket]/mean(level_Report$actual) )
  }
  
  list(Report, Lift, adjustment)
  
}

Double.Lift = function (data,prediction,actual,current,eexp,by.variable=NULL,bucket, bin =c(0,seq(0.7,1.3,0.05),999), balance = TRUE, plot = TRUE, header = "Relative Performance of Proposed over Current",name = "on 20% independent validation dataset"){
  item_list = c(prediction,if(!is.null(actual))(actual),if(!is.null(current))(current), if(!is.null(eexp))(eexp),if(!is.null(by.variable))(by.variable))
  temp_data = data[, item_list]
  
  names(temp_data) = c("prediction",if(!is.null(actual))("actual"),if(!is.null(current))("current"), if(!is.null(eexp))("eexp"),if(!is.null(by.variable))(by.variable))
  if (is.null(eexp)){temp_data$eexp = 1}
  if (is.null(by.variable)){
    by.variable =  "by.variable"
    temp_data$by.variable = 1
  }

  adjustment = 1
  if (balance == TRUE) { 
    adjustment =sum(temp_data$prediction)/sum(temp_data$current)
    temp_data$prediction = temp_data$prediction/adjustment
  }
  
  if (!is.null(actual)) {
    LR_adjustment =sum(temp_data$actual)/sum(temp_data$current)
    temp_data$actual = temp_data$actual/LR_adjustment
  }
  
  temp_data$link = temp_data$prediction/temp_data$current
  temp_data = temp_data[order(temp_data$link),]
  Report = NULL
  Lift = NULL
  
  for (level in unique(temp_data[[by.variable]])){
    level_data = temp_data[temp_data[[by.variable]] == level,]
    
    level_data$index = level_data$prediction/level_data$current
    level_data$index_band = cut(level_data$index, breaks=bin)
    
    if (!is.null(actual)) {
      level_Report = aggregate(eval(bquote(cbind(prediction, actual, current,eexp)~.(as.name(by.variable)) + index_band)),data = level_data, FUN =sum)
      level_Report$dislocation = level_Report$prediction/level_Report$current
      level_Report$LR = level_Report$actual/level_Report$current
    } else {
      level_Report = aggregate(eval(bquote(cbind(prediction, current,eexp)~.(as.name(by.variable)) + index_band)),data = level_data, FUN =sum)
      level_Report$dislocation = level_Report$prediction/level_Report$current
    }
    
    #par(xpd = F, mfrow=c(2,2),mar=c(5, 4, 4, 4) + 0.1)

    if(plot ==TRUE){    
          upper = 1.2* max(level_Report$eexp/sum(level_Report$eexp))
          barplot(level_Report$eexp/sum(level_Report$eexp), names.arg=level_Report$index_band, ylab="Exposure Count", xlab="",col.lab="black", 
                  cex.axis=0.8, ylim = c(0,upper),col="light blue",las=1)
          
          par(new=TRUE)
          
          if (!is.null(actual)) {
            lower = round(0.95*min(level_Report$dislocation,level_Report$LR),1)
            upper = round(1.05*max(level_Report$dislocation,level_Report$LR),1)
            plot(level_Report$LR, axes=F, ylab='', xlab='', type="l",ylim =c(lower,upper), lwd= 2,col="dark red", pch=21) 
            axis(4,las=1) 
            mtext("Multiple of average Loss Ratio",4, col="dark red", cex=0.80)
          } else {
            lower = round(0.95*min(level_Report$dislocation),1)
            upper = round(1.05*max(level_Report$dislocation),1) 
          }
          
          par(new=TRUE)
          plot(level_Report$dislocation, axes=F, ylab='', xlab='', type="l",ylim =c(lower,upper), col="dark blue", pch=21)
          if (is.null(actual)) {axis(4,las=1)}
          
          title(main=  header,cex.main=1.5,line = 1)
          title(main = name,cex.main=1,line=0)
          
          par(xpd=T,mar=par()$mar+c(-2,0,0,0))
          if (!is.null(actual)) {
            legend(0.5,-0.40, c("Loss Ratio Multiple","Prediction/Current"), cex=0.8, lty=c(1,1), col = c("dark red","dark blue"),lwd =c(2,1), horiz = T,xpd=T)
          } else {
            legend(0.5,-0.40, c("Prediction/Current"), cex=0.8, lty=1, col = "dark blue",lwd =1, horiz = T,xpd=T)
          }
          par(xpd=T,mar=c(5, 4, 4, 4) + 0.1)
  }
    
    Report = rbind(Report,level_Report)
  }
  
  Report
  
}


Plot.Lift = function (report.data,prediction, actual, current, name ="Lift Plot", by.variable = NULL ,r.sq = TRUE, lift = NULL, type="Loss Cost" ){

  if (is.null(by.variable)) {
    by.variable = "by.variable"
    report.data$by.variable = 1
  }

  plot.size.x = plot.size.y = ceiling(sqrt(length(unique(report.data[[by.variable]]))))
  
  if (plot.size.x * (plot.size.x-1) >= length(unique(report.data[[by.variable]]))){
      plot.size.x = plot.size.x-1
  }
  
  if ( length(unique(report.data[[by.variable]])) >1){
    
    par(xpd=F, mfrow=c(plot.size.x,plot.size.y),oma = c(0, 0, 3, 0))    
  }
  
  for (level in unique(report.data[[by.variable]])){
    
      report.data.temp = report.data[report.data[[by.variable]] == level,]

      plot( report.data.temp[[prediction]],report.data.temp[[actual]],xlab =paste("Predicted ",type,sep=""), ylab = paste("Actual ",type,sep=""))
     
      if (!is.null(current)) {
        lines(report.data.temp[[current]],report.data.temp[[actual]], lty =2, col = 3,lwd =2)
        legend("bottomright", c("observations","perfect prediction", "current prediction"), pch=c(1,NA,NA), lty=c(0,1,2), lwd =c(NA,1,2), col = c(1,1,3),inset = .02, xjust=0.5, yjust=0.5,cex = 0.8)
      } else {
        legend("bottomright", c("observations","perfect prediction"), pch=c(1,NA), lty=c(0,1),  col = c(1,1),inset = .02, xjust=0.5, yjust=0.5,cex = 0.8)
      }

      if (r.sq == TRUE) (r.sq = (cor(report.data.temp[[prediction]],report.data.temp[[actual]]))^2)
      if (is.null(lift)) (lift1 = report.data.temp[[actual]][nrow(report.data.temp)]/mean(report.data.temp[[actual]]))
      title(main = paste(ifelse (length(unique(report.data[[by.variable]])) >1, paste(name,level,sep=" "),name)," *Lift = ",round(lift1,2)," *R sq. = ",round(r.sq,2), sep=""))
      frame_width = max(report.data.temp[[prediction]],report.data.temp[[actual]])
      lines(c(0,frame_width),c(0,frame_width))
      
      r.sq = TRUE #reset R.sq
  }
  
  if ( length(unique(report.data[[by.variable]])) >1){  
    mtext(paste(Coverage,": Prediction Performance by ",by.variable,sep=""), outer = TRUE, cex = 1.5)   
  }
  
}


if (F){

  data =lossdata[Train_content,]
  link = "link_freq"
  prediction="prediction_freq1"
  actual="temp1"
  current = "eprmon"
  eexp = "eexp"
  by.variable=NULL
  bucket = 10
  balance = TRUE
  
  data =lossdata[Test_content,]
  link = "link"
  prediction="prediction"
  actual="projnetinc"
  current = "eprmon"
  eexp = "eexp"
  by.variable="AY"
  bucket = 20
  balance = FALSE
  
  report.data = Report.items.Testing.AY[[1]]
  report.data = Report.items.Training[[1]]
  prediction ="prediction"
  actual = "actual"
  current = "current"
  current = NULL
  name ="Lift Plot"
  by.variable = NULL 
  r.sq = TRUE
  lift = NULL  
  
}