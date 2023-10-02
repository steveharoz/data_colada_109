
#DataColada 109

#Chapter 1 - Clusterfake: Shu Mazar Gino Ariely & Bazerman (2012)

#Outline of this script
#1 Load the packages
#2 Load and process the data
#3 Reproduce results in the paper
#4 Redflagged by condition 
#5 Plot - travel expenses
#6 t-test for other dv reported in text

#This version: 2023 06 17

#---------------------------------------


  #1 Packages
    rm(list = ls())
    library('groundhog')
    pkgs <- c('beeswarm')
    groundhog.library(pkgs,  "2023-04-01") 
    
    #Note on the groundhog day used:
    #You may change the date to a more recent one 
    #so that things load faster, and switch back 
    #to 2023-04-01 if the script does not run 
    #as it should with the more recent package versions.


#--------------------------------------------------------------    
  #2 Load and process the data
    
    
    #2 Load data, with hand-coded out-of-sequences rows
        data_path <- '<ENTER YOUR PATH HERE>' #put the directory where yousaved the .csv file here
    
    
    #<<<<<<<<<< IMPORTANT CHANGE PATH ABOVE >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
      s1<-read.csv(file.path(data_path,"S1_flagged_2023_06_09.csv"),stringsAsFactors = FALSE,header=TRUE)
        
    #The flag column was hand coded into .csv; original spreadsheet from 2010 unedited on researchbox
    
    
    #3.2 rename and create needed variables 
        names(s1)
        names(s1)[4] <- "id"


    #3.4 Descriptive condition variable called cond2
        s1$cond2<-ifelse(s1$Cond==0,"No signature",NA)
        s1$cond2<-ifelse(s1$Cond==1,"Sign-Top",s1$cond2)
        s1$cond2<-ifelse(s1$Cond==2,"Sign-Bottom",s1$cond2)
        table(s1$cond2)
        
    #3.5 Data without control to facilitate t-tests and plots
        s1b <- s1[!s1$Cond==0,]
#--------------------------------------------------------------
        
  #4 Reproduce results in the paper
        
      #4.1 % cheating
          #Paper states 
            #Sign top:    37% 
            #Sign bottom  79% 
            #No sign :    64%
        
          t1 = table(s1$CheatedOnMatrixTax,s1$cond2)
          t1/colSums(t1)  
          
        #"The percentage of participants who cheated .... x2(2,n101)=12.58
          chisq.test(t1)
          
      #4.2 Anova cheating
          #"results also hold when analyzing the average magnitude ... F(2,98)=9.21
          #Means
            aggregate(s1$OverReport,list(s1$cond2),mean)
          #Anova
            summary(aov(s1$OverReport~s1$cond2))
          
      #4.3 Participants claimed fewer expenses
          #"Finally, claims of travel expenses... F(2,98)=5.63
          summary(aov(s1$SumDeduction~s1$cond2))
    
          
      #4.4 T-tests
        #Over-reporting, 1/0
          t2=table(s1b$CheatedOnMatrixTax,s1b$cond2)
          t2/colSums(t2)
          chisq.test(t2)

        #Over-reporting, means
          t.test(s1b$OverReport~s1b$cond2,var.equal=TRUE)
          t.test(s1b$SumDeduction~s1b$cond2,var.equal=TRUE)

  #-------------------------------------------------------       
          
    #5 Flag by condition
        table(s1$flag,s1$cond2)

        
#--------------------------------------------------------------       
#6 Plot travel expenses

    
  #margins
    par(mar=c(5.1, 4.1, 7.1, 2.1))

  #Drop control for plot
    s1b = s1[s1$Cond!=0,]
  
   #Format normal vs redflag dots        
      pch.flag <- ifelse(s1b$flag==0,16,13)
      col.flag <- ifelse(s1b$flag==0,"dodgerblue","red")
      cex.flag <- ifelse(s1b$flag==0,.75,2)
   
    par(xpd=TRUE)

  #Beeswarm  plot
    b=beeswarm(s1b$SumDeduction~s1b$cond2,pwcex=cex.flag, lwd=1,
           method='center',
           cex.axis=1.5,
           ylim=c(0,27),
           pwcol=col.flag, pwpch=pch.flag,xlab='',main='',ylab='',las=1,bty="n")
  
  #Legend
    legend(cex=1,"top",inset=-.15,col=c('dodgerblue','red'),
        pch=c(16,13), pt.cex=c(.75,2),
        legend=c("No red flag","Flagged (duplicate or out of sequence)"))
  
  #Header
    mtext(side=3,line=5.5,font=2,cex=1.5,"Flagged Observations Show Huge Effect")
    mtext(side=3,line=4,font=3,cex=1.2,"Travel Expenses in Study 1 - Shu  et al. (2012)")

  #y-axis
    mtext(side=2,line=2.75,font=2,cex=1.5,"Expenses Claimed ($)")
    
  #x-axis
    mtext(side=1,line=3,font=2,cex=1.5,"Condition")
  
    
  #ARROW with t-test
    #t-test reported in legend
      #Run test
        t=t.test(SumDeduction[flag==1]~cond2[flag==1] ,data=s1b,var.equal=TRUE)
      #Write up results
        t.text=paste0("t(6) = ",round(t$statistic,2),",  p < .0000001")
      #Put on legend
            text(1.6,19.5,  t.text  ,col='red',cex=1)
      
      #point at it with arrows
        arrows(x0=1.2,x1=1.8,y0=18.6,y1=1.12,col='red')
        arrows(x1=1.2,x0=1.8,y1=18.6,y0=1.12,col='red')
  
    
#---------------------------------------------------------------

#7 t-test for other dv reported in text
    
  t.test(OverReport~cond2,data=s1b ,var.equal=TRUE)
    