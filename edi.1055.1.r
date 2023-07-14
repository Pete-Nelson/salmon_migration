# Package ID: edi.1055.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Chipps Island trawl, Delta Juvenile Fish Monitoring Program, Genetic Determination of Population of Origin 2017-2021.
# Data set creator:  Elissa Buttermore - U.S. Bureau of Reclamation 
# Data set creator:  Joshua Israel - U.S. Bureau of Reclamation 
# Data set creator:  Kevin Reece - California Department of Water Resources 
# Data set creator:  Scott Blankenship - Cramer Fish Sciences - Genidaqs 
# Metadata Provider:  Scott Blankenship - Cramer Fish Sciences - Genidaqs 
# Contact:  Joshua Israel -  U.S. Bureau of Reclamation  - jaisrael@usbr.gov
# Contact:  Scott Blankenship -  Cramer Fish Sciences - Genidaqs  - scott.blankenship@fishsciences.net
# Contact:  Elissa Buttermore -  U.S. Bureau of Reclamation  - ebuttermore@usbr.gov
# Contact:  Kevin Reece -  California Department of Water Resources  - kevin.reece@water.ca.gov
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1055/1/4a3b853edcf849ea4cbeb2b826885f0a" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "ID",     
                    "SampleDate",     
                    "ForkLength",     
                    "Julian",     
                    "GeneticID",     
                    "PosProb",     
                    "Ots28",     
                    "LengthByDate",     
                    "FieldID"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$ID)!="factor") dt1$ID<- as.factor(dt1$ID)                                   
# attempting to convert dt1$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1SampleDate<-as.Date(dt1$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1SampleDate) == length(tmp1SampleDate[!is.na(tmp1SampleDate)])){dt1$SampleDate <- tmp1SampleDate } else {print("Date conversion failed for dt1$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1SampleDate) 
if (class(dt1$ForkLength)=="factor") dt1$ForkLength <-as.numeric(levels(dt1$ForkLength))[as.integer(dt1$ForkLength) ]               
if (class(dt1$ForkLength)=="character") dt1$ForkLength <-as.numeric(dt1$ForkLength)
if (class(dt1$Julian)=="factor") dt1$Julian <-as.numeric(levels(dt1$Julian))[as.integer(dt1$Julian) ]               
if (class(dt1$Julian)=="character") dt1$Julian <-as.numeric(dt1$Julian)
if (class(dt1$GeneticID)!="factor") dt1$GeneticID<- as.factor(dt1$GeneticID)
if (class(dt1$PosProb)=="factor") dt1$PosProb <-as.numeric(levels(dt1$PosProb))[as.integer(dt1$PosProb) ]               
if (class(dt1$PosProb)=="character") dt1$PosProb <-as.numeric(dt1$PosProb)
if (class(dt1$Ots28)!="factor") dt1$Ots28<- as.factor(dt1$Ots28)
if (class(dt1$LengthByDate)!="factor") dt1$LengthByDate<- as.factor(dt1$LengthByDate)
if (class(dt1$FieldID)!="factor") dt1$FieldID<- as.factor(dt1$FieldID)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(ID)
summary(SampleDate)
summary(ForkLength)
summary(Julian)
summary(GeneticID)
summary(PosProb)
summary(Ots28)
summary(LengthByDate)
summary(FieldID) 
                # Get more details on character variables
                 
summary(as.factor(dt1$ID)) 
summary(as.factor(dt1$GeneticID)) 
summary(as.factor(dt1$Ots28)) 
summary(as.factor(dt1$LengthByDate)) 
summary(as.factor(dt1$FieldID))
detach(dt1)               
        




