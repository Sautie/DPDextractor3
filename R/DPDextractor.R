
library("R6")
#' @title DPDfile class
#'
#' @description R6 Class representing a DPD file.The DPDfile class has 5 methods and 1 private attribute, the dataframe (dfm) with the DPD file content
#' @export
#' @details
#' The 5 methods of this class are header, select,field_search, getter/setter functions and constructor.
#' DPDfile is the supoerclass of all protect classes

DPDfile <- R6Class("DPDfile",
                   portable = TRUE,
                   #' @description  active binding for dpd tables
                   active=list(
                     #' @description  Getter/Setter method for DPDfile object
                     #' @export
                     #' @examples
                     #' obj<-DPDfile$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/vet.txt")
                     #' Table<-obj$DPDtable
                     #' obj$DPDtable<-Table
                     DPDtable= function(value) {
                       if (missing(value)) {
                         private$dfm
                       } else {
                         stopifnot(is.data.frame(value))
                         private$dfm <- value
                       }
                     }
                   ),
                   public= list(
                     #' @description
                     #' Create a new DPDfile class object
                     #' @param fname file and path names.
                     #' @param dpdlabel distinctive elements of DPD file names.
                     #' @return A new `DPDfile` object.
                     #' @export
                     #' @examples
                     #' obj<-DPDfile$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/vet.txt")
                     #'
                     initialize = function(fname="") {
                       #stopifnot(is.character(fname),((grepl(dpdlabel, fname, ignore.case ="True")&&(!grepl("_", fname, ignore.case ="True")) )||(grepl(paste0(dpdlabel, "_ap"), fname, ignore.case ="True"))||(grepl(paste0(dpdlabel, "_ia"), fname, ignore.case ="True"))|| (grepl(paste0(dpdlabel, "_dr"), fname, ignore.case ="True"))) )
                       #if (grepl(dpdlabel, fname, ignore.case ="True")&&(!grepl("_", fname, ignore.case ="True")) ) {

                       if (missing(fname)){
                         private$dfm <- data.frame()
                         private$header( c())
                         print("No file")
                       }
                       else   {

                         if (!file.exists(fname)) {stop("This file does not exist")}
                         else if(!grepl(".txt", fname, ignore.case ="True")) {
                           stop("No text file")}

                         private$dfm <- read.table(fname,sep=',', header=FALSE)
                         private$dfm[,1]<-as.character(private$dfm[,1])

                         if  (grepl("vet", fname, ignore.case ="True")) {
                           private$header(c("DRUG_CODE", "VET_SPECIES", "VET_SUB_SPECIES", "VET_SPECIES_F"))
                         }
                         else if (grepl("status", fname, ignore.case ="True")){
                           private$header(c("DRUG_CODE", "CURRENT_STATUS_FLAG", "STATUS", "HISTORY_DATE", "STATUS_F", "LOT_NUMBER", "EXPIRATION_DATE"))
                         }
                         else if (grepl("route", fname, ignore.case ="True")) {
                           private$header(c("DRUG_CODE", "ROUTE_OF_ADMINISTRATION_CODE", "ROUTE_OF_ADMINISTRATION", "ROUTE_OF_ADMINISTRATION_F"))
                         }
                         else if (grepl("ingred", fname, ignore.case ="True"))  {
                           private$header(c("DRUG_CODE", "ACTIVE_INGREDIENT_CODE", "INGREDIENT", "INGREDIENT_SUPPLIED_IND", "STRENGTH", "STRENGTH_UNIT", "STRENGTH_TYPE", "DOSAGE_VALUE", "BASE", "DOSAGE_UNIT", "NOTES", "INGREDIENT_F", "STRENGTH_UNIT_F", "STRENGTH_TYPE_F", "DOSAGE_UNIT_F"))
                         }
                         else if (grepl("comp", fname, ignore.case ="True")) {
                           private$header(c("DRUG_CODE", "MFR_CODE", "COMPANY_CODE", "COMPANY_NAME", "COMPANY_TYPE", "ADDRESS_MAILING_FLAG", "ADDRESS_BILLING_FLAG", "ADDRESS_NOTIFICATION_FLAG", "ADDRESS_OTHER", "SUITE_NUMBER", "STREET_NAME", "CITY_NAME", "PROVINCE", "COUNTRY", "POSTAL_CODE", "POST_OFFICE_BOX", "PROVINCE_F", "COUNTRY_F") )
                         }
                         else if (grepl("drug", fname, ignore.case ="True")){
                           private$header(c("DRUG_CODE", "PRODUCT_CATEGORIZATION", "CLASS", "DRUG_IDENTIFICATION_NUMBER", "BRAND_NAME", "DESCRIPTOR", "PEDIATRIC_FLAG","ACCESSION_NUMBER","NUMBER_OF_AIS", "LAST_UPDATE_DATE", "AI_GROUP_NO","CLASS_F", "BRAND_NAME_F", "DESCRIPTOR_F"))
                         }
                         else if (grepl("package", fname, ignore.case ="True")){
                           private$header( c("DRUG_CODE", "UPC", "PACKAGE_SIZE_UNIT", "PACKAGE_TYPE", "PACKAGE_SIZE", "PRODUCT_INFORMATION", "PACKAGE_SIZE_UNIT_F", "PACKAGE_TYPE_F"))
                         }
                         else if (grepl("form", fname, ignore.case ="True")){
                           private$header( c("DRUG_CODE", "PHARM_FORM_CODE", "PHARMACEUTICAL_FORM", "PHARMACEUTICAL_FORM_F"))
                         }
                         else if (grepl("pharm", fname, ignore.case ="True")){
                           private$header(c("DRUG_CODE", "PHARMACEUTICAL_STD"))
                         }
                         else if (grepl("ther", fname, ignore.case ="True")){
                           private$header( c("DRUG_CODE", "TC_ATC_NUMBER", "TC_ATC", "TC_AHFS_NUMBER"))
                         }
                         else if (grepl("schedule", fname, ignore.case ="True")){
                           private$header( c("DRUG_CODE", "SCHEDULE", "SCHEDULE_F"))
                         }
                         else if (grepl("biosimilar", fname, ignore.case ="True")){
                           private$header( c("DRUG_CODE", "BIOSIMILAR", "BIOSIMILAIRE", "NUMBER"))
                         }
                         else {
                           stop("No valid file")
                         }
                       }
                     },
                     #' @description Choose the subset of the table columns
                     #' @param columns column names.
                     #' @export
                     #' @examples obj1$select()
                     select= function(columns) {   #
                       stopifnot(!is.null(private$dfm), (length(columns)<ncol(private$dfm)))
                       private$dfm <-private$dfm[,columns]
                     },

                     #' @description this function searches for fields that have a given name or substring
                     #' @param field field name or prefix/suffix.
                     #' @param b Boolean parameter to define the returned values .
                     #' @return string containing found fields or boolean value
                     #' @export
                     #' @examples obj$field_search ("STATUS") #for string searches
                     #'           bj$field_search ("STATUS", b=TRUE) #for sboolean searches
                     #'
                     #'         If b==TRUE, boolean value representing absence/presence of a name or
                     #'         prefix/infix/suffix in the set of field names.
                     #'         If b==FALSE,Name(s) of the table(s) where the field is located
                     #'
                     field_search= function(field, b=FALSE) {   #
                       tableClass<-"file(s):"
                       vnames<- c("DRUG_CODE", "CURRENT_STATUS_FLAG", "STATUS", "HISTORY_DATE", "STATUS_F", "LOT_NUMBER", "EXPIRATION_DATE")
                       fields<-vnames[(which(grepl(pattern = field, x =vnames)))]
                       if (length(fields)>0)  {
                         if (b) return (TRUE)
                         tableClass<-paste(tableClass, "status")
                       }
                       vnames<-c("DRUG_CODE", "ROUTE_OF_ADMINISTRATION_CODE", "ROUTE_OF_ADMINISTRATION", "ROUTE_OF_ADMINISTRATION_F")
                       fields<-vnames[(which(grepl(pattern = field, x =vnames)))]
                       if (length(fields)>0)  {
                         if (b) return (TRUE)
                         tableClass<-paste(tableClass, "route")
                       }
                       vnames<-c("DRUG_CODE", "ACTIVE_INGREDIENT_CODE", "INGREDIENT", "INGREDIENT_SUPPLIED_IND", "STRENGTH", "STRENGTH_UNIT", "STRENGTH_TYPE", "DOSAGE_VALUE", "BASE", "DOSAGE_UNIT", "NOTES", "INGREDIENT_F", "STRENGTH_UNIT_F", "STRENGTH_TYPE_F", "DOSAGE_UNIT_F")
                       fields<-vnames[(which(grepl(pattern = field, x =vnames)))]
                       if (length(fields)>0)  {
                         if (b) return (TRUE)
                         tableClass<-paste(tableClass, "ingred")
                       }
                       vnames<-c("DRUG_CODE", "PHARM_FORM_CODE", "PHARMACEUTICAL_FORM", "PHARMACEUTICAL_FORM_F")
                       fields<-vnames[(which(grepl(pattern = field, x =vnames)))]
                       if (length(fields)>0)  {
                         if (b) return (TRUE)
                         tableClass<-paste(tableClass, "form")
                       }
                       vnames<-c("DRUG_CODE", "MFR_CODE", "COMPANY_CODE", "COMPANY_NAME", "COMPANY_TYPE", "ADDRESS_MAILING_FLAG", "ADDRESS_BILLING_FLAG", "ADDRESS_NOTIFICATION_FLAG", "ADDRESS_OTHER", "SUITE_NUMBER", "STREET_NAME", "CITY_NAME", "PROVINCE", "COUNTRY", "POSTAL_CODE", "POST_OFFICE_BOX", "PROVINCE_F", "COUNTRY_F" )
                       fields<-vnames[(which(grepl(pattern = field, x =vnames)))]
                       if (length(fields)>0)  {
                         if (b) return (TRUE)
                         tableClass<-paste(tableClass, "comp")
                       }
                       vnames<-c("DRUG_CODE", "PRODUCT_CATEGORIZATION", "CLASS", "DRUG_IDENTIFICATION_NUMBER", "BRAND_NAME", "DESCRIPTOR", "PEDIATRIC_FLAG","ACCESSION_NUMBER","NUMBER_OF_AIS", "LAST_UPDATE_DATE", "AI_GROUP_NO","CLASS_F", "BRAND_NAME_F", "DESCRIPTOR_F")
                       fields<-vnames[(which(grepl(pattern = field, x =vnames)))]
                       if (length(fields)>0)  {
                         if (b) return (TRUE)
                         tableClass<-paste(tableClass, "drug")
                       }
                       vnames<-c("DRUG_CODE", "UPC", "PACKAGE_SIZE_UNIT", "PACKAGE_TYPE", "PACKAGE_SIZE", "PRODUCT_INFORMATION", "PACKAGE_SIZE_UNIT_F", "PACKAGE_TYPE_F")
                       fields<-vnames[(which(grepl(pattern = field, x =vnames)))]
                       if (length(fields)>0)  {
                         if (b) return (TRUE)
                         tableClass<-paste(tableClass, "package")
                       }
                       vnames<-c("DRUG_CODE", "TC_ATC_NUMBER", "TC_ATC", "TC_AHFS_NUMBER", "TC_AHFS", "TC_ATC_F", "TC_AHFS_F")
                       fields<-vnames[(which(grepl(pattern = field, x =vnames)))]
                       if (length(fields)>0)  {
                         if (b) return (TRUE)
                         tableClass<-paste(tableClass, "ther")
                       }
                       vnames<- c("DRUG_CODE", "SCHEDULE", "SCHEDULE_F")
                       fields<-vnames[(which(grepl(pattern = field, x =vnames)))]
                       if (length(fields)>0)  {
                         if (b) return (TRUE)
                         tableClass<-paste(tableClass, "schedule")
                       }
                       vnames<- c("DRUG_CODE", "VET_SPECIES", "VET_SUB_SPECIES", "VET_SPECIES_F")
                       fields<-vnames[(which(grepl(pattern = field, x =vnames)))]
                       if (length(fields)>0)  {
                         if (b) return (TRUE)
                         tableClass<-paste(tableClass, "vet")
                       }
                       print(tableClass)
                     }),
                   private = list(
                     #' @field dfm table or data frame derived from the corresponding DPD file.                     #'
                     #' @export
                     dfm= NULL,
                     #' @description function to set the names of the DPD table fields
                     #' @param vnames a vector of field names
                     #' @export
                     #' @examples
                     #' private$header(c("DRUG_CODE", "VET_SPECIES","VET_SUB_SPECIES","VET_SPECIES_F"))
                     #'
                     header= function(vnames) {
                       stopifnot(!is.null(private$dfm), (length(vnames)==ncol(private$dfm)))
                       colnames(private$dfm) <-vnames
                     },
                     #' @description Sort table by drug_code
                     #' @param
                     #' @export
                     #' @examples
                     #' private$sorted()
                     sorted= function() {
                       dfm <- dfm[order(dfm$DRUG_CODE,decreasing=FALSE),]
                     },
                     #' @description private method to determine the number of repeats                     #'
                     #' @param dfs vector of code names
                     #' @export
                     #' @examples
                     #' mci<-private$Run(dfs)
                     Run =function (dfs) {
                       udfs<-unique(dfs)
                       Ldfs<-length(dfs)
                       Ludfs<-length(udfs)
                       i<-1
                       cc<-1
                       ci<-c()
                       while(i<Ludfs) {
                         while(cc<Ldfs) {
                           if (udfs[i]==dfs[cc]){
                             ii<-0
                             while ((dfs[cc]==dfs[cc+ii+1])&&((cc+ii+1)<Ldfs)){
                               ii<-ii+1  }
                             cc<-cc+ii+1
                             ci<-c(ci, ii+1)
                             break
                           }
                           cc<-cc+1
                         }
                         i<-i+1
                       }
                       return(ci)
                     }
                   ))
##' @title status class
#'
#' @description R6 Class representing the DRUG product status. The status class has 5 methods in addition to 6 other methods and a field that inherits from the parent class (DPDfile).
#' @export
#' @details
#' A drug is considered to have 4 possible statuses: Approved, cancelled, marketed or dormant.
#' status.txt: Approved, marqueted ("Y")
#' status_ap.txt: Approved ("Y"), marqueted and cancelled
#' status_ia.txt: Approved, marqueted and cancelled  ("Y")
#' status_dr.txt: Approved, marqueted and dormant  ("Y")
#'
status <- R6Class("status",
                  portable = TRUE,
                  inherit = DPDfile,
                  public = list(
                    #' @description Create a new status object
                    #' @param fname file and path names.
                    #' @return A new status object.
                    #' @export
                    #' @examples
                    #' obj<-status$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/status.txt", "status")
                    #' obj<-status$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/status.txt")
                    initialize = function(fname){
                      super$initialize(fname)
                    },
                    #' @description Select current statuses
                    #' @export
                    #' @examples
                    #' obj<-status$Y_select()
                    Y_select=function(){
                      stopifnot(!is.null(private$dfm), ("CURRENT_STATUS_FLAG" %in% colnames(private$dfm)))
                      private$dfm<- private$dfm[private$dfm$CURRENT_STATUS_FLAG == 'Y',]
                    },
                    #' @description Choose the subset of the table columns
                    #' @param columns a vector of status column names
                    #' @export
                    #' @examples
                    #' obj<-status$select()
                    #' obj<-status$select(c("DRUG_CODE", "CURRENT_STATUS_FLAG", "STATUS"))
                    select= function(columns=c("DRUG_CODE", "STATUS")) {   #c("DRUG_CODE", "STATUS")
                      super$select(columns)
                    },
                    #' @description
                    #' calculates how long ago a drug acquired a given status
                    #' @param fdate date on which a given drug acquired a status
                    #' @export
                    #' @examples
                    #' obj<-status$DiffDate("30-JUN-2016")
                    #'
                    DiffDate= function(fdate) {
                      if (grepl('JAN', fdate, ignore.case ="True"))
                      { d0<-'JAN'
                      d1<-'01' }
                      else if (grepl('FEB', fdate, ignore.case ="True"))
                      { d0<-'FEB'
                      d1<-'02' }
                      else if (grepl('MAR', fdate, ignore.case ="True"))
                      { d0<-'MAR'
                      d1<-'03' }
                      else if (grepl('APR', fdate, ignore.case ="True"))
                      { d0<-'APR'
                      d1<-'04' }
                      else if (grepl('MAY', fdate, ignore.case ="True"))
                      { d0<-'MAY'
                      d1<-'05' }
                      else if (grepl('JUN', fdate, ignore.case ="True"))
                      { d0<-'JUN'
                      d1<-'06' }
                      else if (grepl('JUL', fdate, ignore.case ="True"))
                      { d0<-'JUL'
                      d1<-'07' }
                      else if (grepl('AUG', fdate, ignore.case ="True"))
                      { d0<-'AUG'
                      d1<-'08' }
                      else if (grepl('SEP', fdate, ignore.case ="True"))
                      { d0<-'SEP'
                      d1<-'09' }
                      else if (grepl('OCT', fdate, ignore.case ="True"))
                      { d0<-'OCT'
                      d1<-'10' }
                      else if (grepl('NOV', fdate, ignore.case ="True"))
                      { d0<-'NOV'
                      d1<-'11' }
                      else if (grepl('DEC', fdate, ignore.case ="True"))
                      { d0<-'DEC'
                      d1<-'12' }
                      dt<-gsub(d0, d1, fdate)
                      dt <- as.Date(dt,format='%d-%m-%Y')
                      ddt<-difftime(Sys.Date(),dt, units="weeks")
                      return (ddt)
                    }
                  ))

#' @title route class
#' @description R6 Class representing the DRUG product administration routes
#' @export
#'
#' @details
#' The route class has 5 methods in addition to 6 other methods and a field that inherits from the
#' parent class (DPDfile).A drug may have one or more routes of administration.

route <- R6Class(classname = "route",
                 portable = TRUE,
                 inherit = DPDfile,
                 public = list(
                   #' @description
                   #' Create a new route object
                   #' @param fname file and path names.
                   #' @param dpdlabel distinctive element of the route name.
                   #' @export
                   #' @return A new route object.
                   #' @examples
                   #' obj<-route$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/route.txt")
                   #' obj<-route$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/route_ap.txt")
                   initialize = function(fname){
                     super$initialize(fname)
                   },
                   #' @description
                   #' choose the route fields
                   #' @param columns a vector of route field names
                   #' @export
                   #' @examples
                   #' obj<-route$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/route.txt", "route")
                   #' obj$select()
                   #' obj$select(c("DRUG_CODE", "ROUTE_OF_ADMINISTRATION_CODE", "ROUTE_OF_ADMINISTRATION"))
                   select= function(columns=c("DRUG_CODE", "ROUTE_OF_ADMINISTRATION")) {   #c("DRUG_CODE", "STATUS")
                     super$select(columns)
                   },
                   #' @description
                   #' Selective transposition of drugs with at least one route of administration based
                   #' on maximum number of administration routes
                   #' @export
                   #' @examples
                   #' obj<-route$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/route.txt", "route")
                   #' obj$select()
                   #' obj$CTranspose()
                   CTranspose = function() {

                     dfs<-as.character(private$dfm[,"DRUG_CODE"])

                     udfs<-unique(private$dfm[,"DRUG_CODE"])

                     route<-private$dfm[,"ROUTE_OF_ADMINISTRATION"]

                     Ldfs<-length(dfs)
                     Ludfs<-length(udfs)
                     mci<-max(private$Run(dfs))
                     dfn<-paste0("route_", 1:mci, "_")
                     dfv = data.frame(matrix(nrow =length(udfs) , ncol =mci+1))
                     colnames(dfv)<-colnames(dfv)<-c("DRUG_CODE", dfn)
                     v<-1
                     ii<-1
                     #ii<-1
                     while(ii<Ldfs+1) {
                       c<-1
                       if ((dfs[ii+c]!=dfs[ii])){
                         dfv[v,1]<-dfs[ii]
                         dfv[v,2]<-route[ii]
                         v<-v+1
                       }
                       else {
                         while ((dfs[ii]==dfs[c+ii])&&((c+ii)<Ldfs+1)){
                           dfv[v,c+1]<-route[ii+c-1]
                           c<-c+1
                         }
                         dfv[v,c+1]<-route[ii+c-1]
                         dfv[v,1]<-dfs[ii]
                         v<-v+1
                       }
                       ii<-ii+c
                     }
                     return (dfv)
                   }
                 )
)

#' @title ingred class
#' @description R6 Class representing the active ingredients of a given drug product
#' @export
#'
#'
#' @details The ingred class has 8 methods in addition to 6 other methods and a field that inherits from the
#' parent class (DPDfile).A drug may have one or more active ingredients.
#'
ingred <- R6Class(classname = "ingred",
                  portable = TRUE,
                  inherit = DPDfile,
                  public = list(
                    #' @description
                    #' Create a new ingred object
                    #' @param fname file and path names.
                    #' @export
                    #' @return A new ingred object.
                    #' @examples
                    #' obj<-ingred$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/ingred.txt")
                    #' obj<-ingred$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/ingred_ap.txt")
                    #'
                    initialize = function(fname){
                      super$initialize(fname)
                    },
                    #' @description
                    #' choose the ingred fields
                    #' @param columns a vector of route field names
                    #' @export
                    #' @examples
                    #' obj<-ingred$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/ingred_ia.txt")
                    #' obj$select(c("DRUG_CODE", "INGREDIENT"))
                    #' obj$select()
                    select= function(columns=c("DRUG_CODE", "BASE", "INGREDIENT",  "STRENGTH", "STRENGTH_UNIT", "STRENGTH_TYPE") ) {   #c("DRUG_CODE", "STATUS")
                      super$select(columns)
                    },
                    #' @description
                    #' Selective transposition of drugs with at least one active ingredient.
                    #' based on maximum number of active ingredients
                    #' @param a indicating the relevance of antib family
                    #' @export
                    #' @examples
                    #' obj<-ingred$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/ingred.txt")
                    #' obj$CTranspose()
                    CTranspose = function(a=TRUE) {
                      dfs<-as.character(private$dfm[,"DRUG_CODE"])
                      mci<-max(private$Run(dfs))
                      #print(mci)
                      if (a)  {namet<- c("INGREDIENT","STRENGTH", "STRENGTH_UNIT","antif") }
                      else  {namet<- c("INGREDIENT","STRENGTH", "STRENGTH_UNIT")   }
                      udfs<-unique(dfs)
                      Ludfs<-length(udfs)
                      Ldfs<-length(dfs)

                      dfn<-rep(namet, each=mci)
                      dfn2<-rep(1:mci, times=4)
                      dfnn<-paste0(dfn, "_", dfn2)
                      dfnn<-c("DRUG_CODE", "BASE", dfnn)
                      dfv = data.frame(matrix(nrow =Ludfs , ncol =mci*4+2))
                      colnames(dfv)<-dfnn

                      i<-1
                      ii<-1
                      while(i<Ludfs+1) {
                        dfv[i,1]<- udfs[i]
                        ii<-1
                        while(ii<Ldfs+1) {
                          c<-0
                          while ((udfs[i]==dfs[c+ii])&&((c+ii)<Ldfs+1)){
                            #print(private$dfm[ii+c,"ing"])
                            dfv[i,2]<-private$dfm[ii+c,"BASE"]
                            dfv[i,c+3]<-private$dfm[ii+c,"INGREDIENT"]
                            dfv[i,(mci)+c+3]<-private$dfm[ii+c,"STRENGTH"]
                            dfv[i,(mci)*2+c+3]<-private$dfm[ii+c,"STRENGTH_UNIT"]
                            if (a)  {dfv[i,(mci)*3+c+3]<-private$dfm[ii+c,"antif"]  }
                            c<-c+1
                          }
                          ii<-ii+c+1

                          if (c!=0) break
                        }
                        i<-i+1
                      }
                      private$dfm<-dfv
                    },
                    #' @description
                    #' compute freq distribution of the number of active ingredients
                    #' @param dc drug code vector
                    #' @return
                    #' table of frequency of the nomber of active ingredients per drug
                    #' @export
                    #' @examples
                    #' obj<-ingred$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/ingred.txt")
                    #' dc<-obj$DPDtable[,"DRUG_CODE"]
                    #' f<-obj$freqDrugCode(dc)
                    freqDrugCode=function(dc) {
                      mr<-max(private$Run(dc))
                      freq<-table(mr)/length(mr)
                      barplot(freq)
                      return(freq)
                    },
                    #' @description
                    #' compute freq distribution of active ingredients
                    #' @param dc drug code vector
                    #' @export
                    #' @return
                    #' table of frequency of active ingredients
                    freqIngred=function(ingred) {
                      freq<-table(ingred)/length(ingred)
                      barplot(freq)
                      return(freq)
                    },
                    #' @description
                    #' select active ingredients that are antibiotics
                    #' @param Antib_list list of antibiotics.
                    #' @param AntiFam_list list of antibiotic families
                    #' @param  SCcat_list list of Canada Health categories
                    #' @param  antibacterial Boolean variable indicating the presence/absence of antibacterial activity.
                    #' @param  antiprotozoal Boolean variable indicating the presence/absence of antiprotozoal activity.
                    #' @export
                    #' @return
                    #' table with four columns of the active ingredients that are antibiotics:
                    #'  name of the active ingredient, name of the corresponding antibiotic family,
                    #'  name of the Canada Health category, as well as the antibacterial or antiprotozoal activity
                    #'  of the corresponding active ingredient
                    #' @examples
                    #' obj<-ingred$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/ingred.txt")
                    #' df<-obj$filtering(a, b, c, d)
                    #'
                    filtering = function(Antib_list, AntiFam_list, SCcat_list, antibacterial,antiprotozoal) {
                      Antib<-tolower(Antib_list)
                      AntiFam<-tolower(AntiFam_list)
                      ingred<-tolower(private$dfm[,"INGREDIENT"])
                      private$dfm<-private$dfm[which(ingred%in% Antib_list),]
                      ind<-c()
                      while(i<length(Antib)+1){
                        while(ii<length(ingred)+1){
                          if (Antib[i]==ingred[ii]){
                            ind<-c(ind, i)
                          }
                          ii<-ii+1
                        }
                        i<-i+1
                      }
                      #al<-which(Antib_list %in% ingred)
                      return (data.frame(Antib_list[ind], AntiFam_list[ind], SCcat_list[ind], antibacterial[ind], antiprotozoal[ind]))
                    },
                    #' @title  select active ingredients that are antibiotics
                    #' @description
                    #' This function selects the active ingredients that are antibiotics and prints their families.
                    #' @param Antib_list list of antibiotics.
                    #' @param AntiFam_list list of antibiotic families
                    #' @param b boolean parameter representing the method used to select the antibiotics. Recommended default value:TRUE
                    #' @param verbose boolean parameter for the complexity of the name of active ingredients. Full name:verbose=TRUE
                    #' @export
                    #' @return
                    #' table of five columns for the active ingredients that are antibiotics:
                    #' name of active ingredient, drug code, dose, dose units and antibiotic family.
                    #' @examples
                    #' obj<-ingred$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/ingred.txt")
                    #' obj$Afiltering(ab, af)
                    #' obj$Afiltering(ab, af, b=TRUE, verbose=TRUE)
                    Afiltering = function(Antib_list, AntiFam_list,b=TRUE, verbose=TRUE) {
                      Antib<-tolower(Antib_list)
                      AntiFam<-tolower(AntiFam_list)
                      ingred<-tolower(private$dfm[,"INGREDIENT"])
                      dcode<-as.character(private$dfm[,"DRUG_CODE"])
                      dsbase<-as.character(private$dfm[,"BASE"])
                      dstrength<-private$dfm[,"STRENGTH"]
                      dstrengthUnit<-private$dfm[,"STRENGTH_UNIT"]
                      i<-1
                      BASE<-c()
                      INGREDIENT<-c()
                      DRUG_CODE<-c()
                      STRENGTH<-c()
                      STRENGTH_UNIT<-c()
                      antif<-c()
                      while (i<length(ingred)+1) {
                        ii<-1
                        while (ii<length(Antib)+1) {
                          if  (b)
                          {cond<-(grepl(Antib[ii], ingred[i], ignore.case ="True")) }
                          else  {cond<- (grepl(ingred[i], Antib[ii], ignore.case ="True")||grepl(Antib[ii], ingred[i], ignore.case ="True")) }
                          if (cond)
                          {
                            if (verbose) {INGREDIENT<-c(INGREDIENT,ingred[i])  }
                            else {
                              INGREDIENT<-c(INGREDIENT, Antib[ii])}
                            BASE<-c(BASE,dsbase[i])
                            DRUG_CODE<-c(DRUG_CODE,dcode[i])
                            STRENGTH<-c(STRENGTH,dstrength[i])
                            STRENGTH_UNIT<-c(STRENGTH_UNIT, dstrengthUnit[i])
                            antif<-c(antif,AntiFam[ii])

                            break
                          }
                          ii<-ii+1
                        }
                        i<-i+1
                      }
                      private$dfm<-data.frame(DRUG_CODE,BASE,INGREDIENT,STRENGTH, STRENGTH_UNIT, antif)
                    },
                    #' @title select active ingredients that are antibiotics
                    #' @description
                    #' This function selects the active ingredients that are antibiotics, prints their health category as well as the antibiotic family.
                    #' @param Antib_list list of antibiotics.
                    #' @param AntiFam_list list of antibiotic families
                    #' @param AntiCatSC_lis list of Canada health categories
                    #' @param b boolean parameter representing the method used to select the antibiotics. Recommended default value:TRUE
                    #' @param verbose boolean parameter for the complexity of the name of active ingredients. Full name:verbose=TRUE
                    #' @return
                    #' table of 6 columns for the active ingredients that are antibiotics:
                    #' name of active ingredient, drug code, dose, dose units,  antibiotic family and Canada health category
                    #' @examples
                    #' obj<-ingred$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/ingred.txt")
                    #' obj$ACfiltering(ab, af, ac)
                    #' obj$ACfiltering(ab, af, ac, b=TRUE, verbose=TRUE)
                    #'
                    ACfiltering = function(Antib_list, AntiFam_list, AntiCatSC_list,b=TRUE, verbose=TRUE) {
                      Antib<-tolower(Antib_list)
                      AntiFam<-tolower(AntiFam_list)
                      AntiCatSC<-tolower(AntiCatSC_list)

                      ingred<-tolower(private$dfm[,"INGREDIENT"])
                      dcode<-as.character(private$dfm[,"DRUG_CODE"])
                      dstrength<-private$dfm[,"STRENGTH"]
                      dstrengthUnit<-private$dfm[,"STRENGTH_UNIT"]
                      i<-1
                      INGREDIENT<-c()
                      DRUG_CODE<-c()
                      STRENGTH<-c()
                      STRENGTH_UNIT<-c()
                      antif<-c()
                      antic<-c()

                      while (i<length(ingred)+1) {
                        ii<-1
                        while (ii<length(Antib)+1) {
                          if  (b)
                          {cond<-(grepl(Antib[ii], ingred[i], ignore.case ="True")) }
                          else  {cond<- (grepl(ingred[i], Antib[ii], ignore.case ="True")||grepl(Antib[ii], ingred[i], ignore.case ="True")) }
                          if (cond)
                          {
                            if (verbose) {INGREDIENT<-c(INGREDIENT,ingred[i])  }
                            else {
                              INGREDIENT<-c(INGREDIENT, Antib[ii])}

                            DRUG_CODE<-c(DRUG_CODE,dcode[i])
                            STRENGTH<-c(STRENGTH,dstrength[i])
                            STRENGTH_UNIT<-c(STRENGTH_UNIT, dstrengthUnit[i])
                            antif<-c(antif,AntiFam[ii])
                            antic<-c(antic,AntiCatSC[ii])

                            break
                          }
                          ii<-ii+1
                        }
                        i<-i+1
                      }
                      private$dfm<-data.frame(DRUG_CODE,INGREDIENT,STRENGTH, STRENGTH_UNIT, antif, antic)
                    }
                  )
)

#' @title comp class
#' @description R6 Class representing drug manufacturers.
#' @export
#'
#' @details
#' #' The comp class has 2  methods in addition to 6 other methods and a field that inherits from the
#' parent class (DPDfile).
#' Every drug has one and only one manufacturer. However, one manufacturer may be linked to several drugs.
#'  the comp table has 18 fields:"DRUG_CODE", "MFR_CODE", "COMPANY_CODE", "COMPANY_NAME", "COMPANY_TYPE",
#'  "ADDRESS_MAILING_FLAG", "ADDRESS_BILLING_FLAG", "ADDRESS_NOTIFICATION_FLAG", "ADDRESS_OTHER", "SUITE_NUMBER",
#'  "STREET_NAME","CITY_NAME", "PROVINCE", "COUNTRY", "POSTAL_CODE", "POST_OFFICE_BOX",
#'  "PROVINCE_F", "COUNTRY_F"
#'
comp <- R6Class(classname = "comp",
                portable = TRUE,
                inherit = DPDfile,
                public = list(
                  #' @description
                  #' Create a new comp object
                  #' @param fname file and path names.
                  #' @param dpdlabel distinctive element of the comp name.
                  #' @export
                  #' @return A new comp object.
                  #' @examples
                  #' obj<-comp$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/comp.txt")
                  #' obj<-comp$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/comp_ia.txt")
                  #'
                  initialize = function(fname){
                    super$initialize(fname)
                  },
                  #' @description
                  #' Chooses the table columns
                  #' @param columns field names.
                  #' @export
                  #' @examples
                  #' obj<-drug$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/comp.txt")
                  #' obj$select()
                  select= function(columns= c("DRUG_CODE", "COMPANY_CODE", "COMPANY_NAME") ) {   #c("DRUG_CODE", "STATUS")
                    super$select(columns)
                  } ))

#' @title drug class
#'
#' @description R6 Class representing drug products.
#' @export'
#'
#' @details  The drug class has 3  methods in addition to 6 other methods and a field that inherits from the
#' parent class (DPDfile).The drug table has 14 fields:  "DRUG_CODE", "PRODUCT_CATEGORIZATION", "CLASS", "DRUG_IDENTIFICATION_NUMBER",
#' "BRAND_NAME", "DESCRIPTOR", "PEDIATRIC_FLAG","ACCESSION_NUMBER","NUMBER_OF_AIS", "LAST_UPDATE_DATE",
#' "AI_GROUP_NO","CLASS_F","BRAND_NAME_F", "DESCRIPTOR_F"

drug <- R6Class(classname = "drug",
                portable = TRUE,
                inherit = DPDfile,
                public = list(
                  #' @description
                  #' Create a new drug object
                  #' @param fname file and path names.
                  #' @export
                  #' @return A new drug object.
                  #' @examples
                  #' obj<-drug$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/drug.txt")
                  #' obj<-drug$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/drug_ia.txt")
                  #'
                  initialize = function(fname){
                    super$initialize(fname)
                  },
                  #' @description
                  #' Chooses the table columns
                  #' @export
                  #' @param columns field names.
                  #' @examples
                  #' obj<-drug$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/drug.txt")
                  #' obj$select()
                  select= function(columns= c("DRUG_CODE", "CLASS","DRUG_IDENTIFICATION_NUMBER", "BRAND_NAME", "NUMBER_OF_AIS",  "AI_GROUP_NO") ) {   #c("DRUG_CODE", "STATUS")
                    super$select(columns)
                  },
                  #' @description
                  #' Chooses the table columns
                  #' @param drugCode code
                  #' @param fields drug table fields
                  #' @export
                  #' @examples
                  #' obj<-drug$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/drug.txt")
                  #' obj$select()
                  #'
                  Drug_search=function(drugCode, fields=c("DRUG_CODE", "CLASS","DRUG_IDENTIFICATION_NUMBER", "BRAND_NAME", "NUMBER_OF_AIS")) {
                    dfmDC <-private$dfm[private$df$DRUG_CODE == drugCode,fields ]
                    return (dfmDC)
                  }
                ))

#' @title package class
#'
#' @description R6 Class representing drug formats or packaging.
#' @export
#'
#'
#' @details The package class has 2  methods in addition to 6 other methods and a field that inherits from the
#' parent class (DPDfile).The package table has 8 fields: "DRUG_CODE", "UPC", "PACKAGE_SIZE_UNIT", "PACKAGE_TYPE", "PACKAGE_SIZE",
#' "PRODUCT_INFORMATION", "PACKAGE_SIZE_UNIT_F", "PACKAGE_TYPE_F"

package <- R6Class(classname = "package",
                   portable = TRUE,
                   inherit = DPDfile,
                   public = list(
                     #' @description
                     #' Create a new package object
                     #' @param fname file and path names.
                     #' @export
                     #' @return A new package object.
                     #' @examples
                     #' obj<-package$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/package.txt")
                     #' obj<-package$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/package_ia.txt")
                     #'
                     initialize = function(fname){
                       super$initialize(fname)
                     },
                     #' @description
                     #' Chooses the table columns
                     #' @param columns field names.
                     #' @export
                     #' @examples
                     #' obj<-package$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/package.txt")
                     #' obj$select()
                     select= function(columns=c("DRUG_CODE","PACKAGE_SIZE_UNIT", "PACKAGE_TYPE", "PACKAGE_SIZE","PRODUCT_INFORMATION") ) {   #c("DRUG_CODE", "STATUS")
                       super$select(columns)
                     } ))


#' @title form class
#' @description  R6 Class representing drug forms.
#' @export'
#'
#' @details
#' The package class has 2  methods in addition to 6 other methods and a field that inherits from the
#' parent class (DPDfile).
#' There are several pharmaceutical forms in this table: SOLUTION, TABLET,SPRAY, WIPE, LOTION,
#'  OINTMENT, POWDER, CAPSULE and CREAM.
#' The same product can have several different forms
#' the package table has 4 fields:"DRUG_CODE", "PHARM_FORM_CODE", "PHARMACEUTICAL_FORM",
#' "PHARMACEUTICAL_FORM_F"
#'
form <- R6Class(classname = "form",
                portable = TRUE,
                inherit = DPDfile,
                public = list(
                  #' @description
                  #' Create a new form object
                  #' @param fname file and path names.
                  #' @param dpdlabel distinctive element of the form name.
                  #' @export
                  #' @return A new form object.
                  #' @examples
                  #' obj<-form$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/form.txt")
                  #' obj<-form$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/form_ia.txt")
                  #'
                  initialize = function(fname){
                    super$initialize(fname)
                  },
                  #' @description
                  #' Chooses the table columns
                  #' @param columns field names.
                  #' @export
                  #' @examples
                  #' obj<-form$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/form.txt")
                  #' obj$select()
                  #'
                  select= function(columns=c("DRUG_CODE", "PHARMACEUTICAL_FORM") ) {
                    super$select(columns)
                  },
                  #' @description
                  #' Selective transposition for form table
                  #' @export
                  #' @examples
                  #' obj<-form$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/form.txt")
                  #' obj$CTranspose()
                  #
                  CTranspose = function() {

                    dfs<-as.character(private$dfm[,"DRUG_CODE"])
                    udfs<-unique(dfs)
                    formi<-private$dfm[,"PHARMACEUTICAL_FORM"]

                    Ldfs<-length(dfs)
                    Ludfs<-length(udfs)
                    mci<-max(private$Run(dfs))
                    dfn<-paste0("form_", 1:mci, "_")
                    dfv = data.frame(matrix(nrow =length(udfs) , ncol =mci+1))
                    colnames(dfv)<-colnames(dfv)<-c("DRUG_CODE", dfn)

                    v<-1
                    ii<-1
                    #ii<-1
                    while(ii<Ldfs+1) {
                      c<-1
                      if ((!is.na(dfs[ii+c]))&&(dfs[ii+c]!=dfs[ii])){
                        dfv[v,1]<-dfs[ii]
                        dfv[v,2]<-formi[ii]
                        v<-v+1
                      }
                      else {
                        while ((dfs[ii]==dfs[c+ii])&&((c+ii)<Ldfs+1)){
                          dfv[v,c+1]<-formi[ii+c-1]
                          c<-c+1
                        }
                        dfv[v,c+1]<-formi[ii+c-1]
                        dfv[v,1]<-dfs[ii]
                        v<-v+1
                      }
                      ii<-ii+c
                    }
                    return (dfv)
                  }
                ))

#' @title pharm class
#'
#' @description R6 Class representing pharmaceutical STD.
#' @export
#'
#' @details
#' The package class has 2  methods in addition to 6 other methods and a field that inherits from the
#' parent class (DPDfile).
#' the pharm table has 4 fields:"DRUG_CODE" and  "PHARMACEUTICAL_STD"
#'
pharm <- R6Class(classname = "pharm",
                 portable = TRUE,
                 inherit = DPDfile,
                 public = list(
                   #' @description
                   #' Create a new pharm object
                   #' @param fname file and path names.
                   #' @param dpdlabel distinctive element of the pharm name.
                   #' @return A new pharm object.
                   #' @export
                   #' @examples
                   #' obj<-pharm$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/pharm.txt")
                   #' obj<-pharm$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/pharm_ia.txt")
                   #'
                   initialize = function(fname){
                     super$initialize(fname)
                   },
                   #' @description
                   #' Chooses the table columns
                   #' @param columns field names.
                   #' @export
                   #' @examples
                   #' obj<-pharm$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/pharm.txt")
                   #' obj$select()
                   #'
                   select= function(columns=c("DRUG_CODE", "PHARMACEUTICAL_STD") ) {
                     super$select(columns)
                   } ))
#' @title ther class
#'
#' @description R6 Class representing therapeutic class.
#' @export
#'
#'
#' @details The package class has 2  methods in addition to 6 other methods and a field that inherits from the
#' parent class (DPDfile). The ther table has 4 fields:"DRUG_CODE", "TC_ATC_NUMBER", "TC_ATC" and "TC_AHFS_NUMBER"
#' (7 fields according to Readme section)
#'
ther <- R6Class(classname = "ther",
                portable = TRUE,
                inherit = DPDfile,
                public = list(
                  #' @description
                  #' Create a new ther object
                  #' @param fname file and path names.
                  #' @param dpdlabel distinctive element of the ther name.
                  #' @return A new ther object.
                  #' @export
                  #' @examples
                  #' obj<-ther$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/ther.txt")
                  initialize = function(fname){
                    super$initialize(fname)
                  },
                  #' @description
                  #' Chooses the table columns
                  #' @param columns field names.
                  #' @export
                  #' @examples
                  #' obj<-ther$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/ther.txt")
                  #' obj$select()
                  #'
                  select= function(columns=c("DRUG_CODE", "TC_ATC") ) {
                    super$select(columns)
                  } ))
#' @title biosimilar class
#'
#' @description R6 Class representing biosimilar drugs.
#' @export
#'
#'
#' @details The biosimilar class has 2  methods in addition to 6 other methods and a field that inherits from the
#' parent class (DPDfile).
#'
biosimilar <- R6Class(classname = "biosimilar",
                      portable = TRUE,
                      inherit = DPDfile,
                      public = list(
                        #' @description
                        #' Create a new biosimilar object
                        #' @param fname file and path names.
                        #' @param dpdlabel distinctive element of the ther name.
                        #' @export
                        #' @return A new ther object.
                        #' @examples
                        #' obj<-biosimilar$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/biosimilar.txt")
                        #'
                        initialize = function(fname){
                          super$initialize(fname)
                        },
                        #' @description
                        #' Chooses the table columns
                        #' @param columns field names.
                        #' @export
                        #' @examples
                        #' obj<-biosimilar$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/biosimilar.txt")
                        #' obj$select()
                        #'
                        select= function(columns=c("DRUG_CODE", "BIOSIMILAR") ) {
                          super$select(columns)
                        } ))

#' @title schedule class
#'
#' @description R6 Class representing schedules (over-the-counter or prescription drugs).
#' @export
#'
#' @details
#' ' The package class has 2  methods in addition to 6 other methods and a field that inherits from the
#' parent class (DPDfile). The schedule table has 3 fields: "DRUG_CODE", "SCHEDULE", "SCHEDULE_F"
#'
schedule <- R6Class(classname = "schedule",
                    portable = TRUE,
                    inherit = DPDfile,
                    public = list(
                      #' @description
                      #' Create a new schedule object
                      #' @param fname file and path names.
                      #' @param dpdlabel distinctive element of the schedule name.
                      #' @export
                      #' @return A new schedule object.
                      #' @examples
                      #' obj<-schedule$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/schedule.txt")
                      #'
                      initialize = function(fname){
                        super$initialize(fname)
                      },
                      #' @description
                      #' Chooses the table columns
                      #' @param columns field names.
                      #' @export
                      #' @examples
                      #' obj<-schedule$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/schedule.txt")
                      #' obj$select()
                      #'
                      select= function(columns=c("DRUG_CODE") ) {
                        super$select(columns)
                      } ))

#' @title vet class
#'
#' @description R6 Class representing the species on which the drugs are used.
#' @export'
#'
#' @details
#'  The package class has 4  methods in addition to 6 other methods and a field that inherits from the
#' parent class (DPDfile).
#' The vet table has 4 fields: "DRUG_CODE", "VET_SPECIES", "VET_SUB_SPECIES" and "VET_SPECIES_F"
vet<- R6Class(classname = "vet",
              portable = TRUE,
              inherit = DPDfile,
              public = list(
                #' @description
                #' Create a new vet object
                #' @param fname file and path names.
                #' @param dpdlabel distinctive element of the vet name.
                #' @export
                #' @return A new vet object.
                #' @examples
                #' obj<-schedule$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/vet.txt")
                #'
                initialize = function(fname){
                  super$initialize(fname)
                },
                #' @description
                #' Chooses the table columns
                #' @param columns field names.
                #' @examples
                #' obj<-vet$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/vet.txt")
                #' obj$select()
                #'
                select= function(columns=c("DRUG_CODE", "VET_SPECIES","VET_SUB_SPECIES") ) {
                  super$select(columns)
                },
                #' @description
                #' Selective transposition of drugs applied in at least one species
                #' based on maximum number of species
                #' @export
                #' @examples
                #' obj<-vet$new("D:/WIAAgrosante/Mapaq-Antibioticos/DPD/vet.txt")
                #' obj$CTranspose()
                #'
                CTranspose = function(ve="VET_SPECIES") {  #ve="VET_SUB_SPECIES"
                  dfs<-private$dfm[,ve]
                  cdfs<-as.character(private$dfm[,"DRUG_CODE"])
                  udfs<-unique(dfs)
                  ucdfs<-unique(cdfs)
                  dfv = data.frame(matrix(0, nrow = length(ucdfs), ncol =length(udfs)+1))
                  colnames(dfv)<-c("DRUG_CODE", udfs)
                  #print(udfs)
                  v<-1
                  vv<-1
                  while(v<length(ucdfs)+1){
                    vv<-1
                    while(vv<length(cdfs)+1){
                      cc<-0
                      while ((!is.na(cdfs[vv+cc]))&&cdfs[vv+cc]==ucdfs[v]) {  #(!is.na(cdfs[vv+cc]))
                        dfv[v,1]<-cdfs[vv+cc]
                        dfv[v,match(dfs[vv+cc],udfs)+1]<-1
                        cc<-cc+1
                      }
                      vv<-vv+1 #+cc
                      #if (cc!=0) break
                    }
                    v<-v+1
                  }
                  return (dfv)
                }
              )
)


