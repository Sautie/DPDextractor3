
#' @title kdataset function
#' @description High level function. Object selector
#' @param kfile file name.
#' @param dpdlabel dintinctive component name.
#' @return A new selected object.
#' @export
#' @examples
#' obj1<-kdataset(txt_files[k], "ingred")
#'
kdataset= function(kfile, dpdlabel) {
  if (dpdlabel=="status")  {
    obj1<-status$new(kfile)
  } else if(dpdlabel=="route"){
    obj1<-route$new(kfile)
  }else if (dpdlabel=="ingred"){
    obj1<-ingred$new(kfile)
  }else if (dpdlabel=="comp"){
    obj1<-comp$new(kfile)
  }else if (dpdlabel=="drug"){
    obj1<-drug$new(kfile)
  }else if (dpdlabel=="package"){
    obj1<-package$new(kfile)
  }else if (dpdlabel=="form"){
    obj1<-form$new(kfile)
  }else if (dpdlabel=="pharm"){
    obj1<-pharm$new(kfile)
  }else if (dpdlabel=="ther"){
    obj1<-ther$new(kfile)
  }else if (dpdlabel=="schedule"){
    obj1<-schedule$new(kfile)
  }else if (dpdlabel=="vet"){
    obj1<-vet$new(kfile)
  }
   else if (dpdlabel==""){
    obj1<-DPDfile$new()
  }
  return(obj1)
}

#' @title addCategory function
#' @description High level function. Add new (Health Canada Category) column to route table
#' @param kfile file name for route table.
#' @return New table.
#' @export
#' @examples
#'  updatedTable<-addCategory("superTab2.csv")
#'  updatedTable<-addCategory(tab)

addCategory <- function(superTabroute, n=6) {

  if (is.character(superTabroute)){
    updatedList <-read.csv(superTabroute)
  }
  else {
    updatedList <- superTabroute
  }

  dsn<-paste0("Scat_", 1:10)
  dfn<-paste0("antif_", 1:10)
  dfn<- dfn[1:n]
  dsn<-dsn[1:n]
  updatedList[dsn]<- NA


  for (ii in 1:length(dfn)) {

    if (dfn[ii] %in% colnames(updatedList))  {

      for (i in 1:nrow(updatedList)) {

        group_name <- trimws(updatedList[i,dfn[ii]])
        group_name  <-tolower(group_name)
        group_name_aux <- trimws(updatedList$ROUTE_OF_ADMINISTRATION[i])

        if (!is.na(group_name)) {
          if (group_name == "penicillins") {
            updatedList[i, dsn[ii]] <- 2
          } else if (group_name == "cephalosporins (1|2)") {
            updatedList[i, dsn[ii]] <- 2
          } else if (group_name == "aminocyclitol") {
            updatedList[i, dsn[ii]] <- 3
          } else if (group_name == "amphenicol") {
            updatedList[i, dsn[ii]] <- 3
          }
          else if (group_name == "cephalosporins (3|4)") {
            updatedList[i, dsn[ii]] <- 1
          }
          else if (group_name == "fluoroquinolones") {
            updatedList[i, dsn[ii]] <- 1
          }
          else if (group_name == "carbapenems") {
            updatedList[i, dsn[ii]] <- 1
          }
          else if (group_name == "fosfomycins") {
            updatedList[i, dsn[ii]]<- 3
          }
          else if (group_name == "fusidic acid") {
            updatedList[i, dsn[ii]] <- 2
          }
          else if (group_name == "glycopeptides") {
            updatedList[i, dsn[ii]]<- 1
          }
          else if (group_name == "glycylcylines") {
            updatedList[i, dsn[ii]] <- 1
          }
          else if (group_name == "ketolides") {
            updatedList[i, dsn[ii]]<- 1
          }
          else if (group_name == "lincosamides") {
            updatedList[i, dsn[ii]]<- 2
          }
          else if (group_name == "lipopeptides") {
            updatedList[i, dsn[ii]]<- 1
          }
          else if (group_name == "macrolides") {
            updatedList[i, dsn[ii]] <- 2
          }
          else if (group_name == "monobactams") {
            updatedList[i, dsn[ii]] <- 1
          }
          else if (group_name == "nitrofurans") {
            updatedList[i, dsn[ii]] <- 3
          }
          else if (group_name == "nitroimidazoles") {
            updatedList[i, dsn[ii]] <- 1
          }
          else if (group_name == "oxazolidinones") {
            updatedList[i, dsn[ii]] <- 1
          }
          else if (group_name == "penems") {
            updatedList[i, dsn[ii]] <- 1
          }
          else if ((group_name == "aminoglycosides (systemic)")||(group_name == "aminoglycosides")&&( group_name_aux=="TOPICAL")){
            updatedList[i, dsn[ii]]<- 2
          }
          else if ((group_name == "aminoglycosides (systemic)")||(group_name == "aminoglycosides")&&( group_name_aux!="TOPICAL")){
            updatedList[i, dsn[ii]]<- 3
          }
          else if (group_name == "phenicols") {
            updatedList[i, dsn[ii]] <- 3
          }
          else if (group_name == "polymyxins") {
            updatedList[i, dsn[ii]] <- 1
          }
          else if (group_name == "rifamycinss") {
            updatedList[i, dsn[ii]] <- 1
          }
          else if (group_name == "streptogramins") {
            updatedList[i, dsn[ii]] <- 2
          }
          else if (group_name == "sulphonamides") {
            updatedList[i, dsn[ii]]<- 3
          }
          else if (group_name == "tetracyclines") {
            updatedList[i, dsn[ii]] <- 3
          }
          else if (group_name == "therapeutic agents for tuberculosis") {
            updatedList[i, dsn[ii]] <- 1
          }
          else if (group_name == "ionophores") {
            updatedList[i, dsn[ii]] <- 4
          }
        }
      }
    }
  }
  return(updatedList)
}

#' @title  AddMergedIngredNames function.
#' @description  High level function.
#'               Add new (Health Canada Category) column  for  merged ingred names
#'
#' @param ingredfile name
#' @return
#' @export
#' @examples
#'
#' updatedTab<-AddMergedIngredNames("tabIged.csv")
#'
AddMergedIngredNames<- function(ingredfile) {
  updatedList <- read.csv(file = ingredfile, head=TRUE,sep=",")
  updatedList$Ingred0 <- NA
  ingredu<-unique(updatedList$INGREDIENT)
  i<-1
  cc<-0
  ii<-1
  mergedName<-updatedList$INGREDIENT[i]
  while(i<nrow(updatedList)) {
    if (ii<length(ingredu)) {
      ii<-ii+1
    }
    if (((i+1)<length(updatedList$DRUG_CODE))&&(updatedList$DRUG_CODE[i]==updatedList$DRUG_CODE[i+1]))
    {
      cc<-0
      while(updatedList$DRUG_CODE[i]==updatedList$DRUG_CODE[i+cc+1]&&(i+cc+1<nrow(updatedList))) {
        mergedName<-c(mergedName,updatedList$INGREDIENT[i+cc+1])
        cc<-cc+1
      }
      mergedName2<-unique(mergedName)
      mergedName<- paste0(mergedName, collapse ="+")
      if (length(mergedName2)<length(mergedName))  {
        mergedName<- paste0(mergedName2, collapse ="+")
      }
      updatedList$Ingred0[ii]<-mergedName
      i<-i+cc
    }
    else
    {
      mergedName<-updatedList$Ingred[i]
      updatedList$Ingred0[ii]<-mergedName
      cc<-0
    }
    i<-i+1
  }
  return(updatedListt)
}


#' @title  AddMergedIngredNames2 function.
#' @description  High level function for  transposed ingredient table
#'               Add new (Health Canada Category) column  for  merged ingred names
#'
#' @param ingredfile name
#' @return
#' @export
#' @examples
#'
#' updatedTab<-AddMergedIngredNames2("tabIged.csv")
#' updatedTab<-AddMergedIngredNames2(tabIged)
#'
AddMergedIngredNames2<- function(ingredfile) {

  if (is.character(ingredfile)){
    updatedList <-read.csv(ingredfile)
  }
  else {
    updatedList <- ingredfile
  }

  updatedList$MergedFamName <- NA
  updatedList$nATB<- NA
  i<-0
  cc<-0
  c<-0
  while(i<nrow(updatedList)) {
    i<-i+1
    mergedName0<-updatedList$antif_1[i]

    if (!is.na(updatedList$INGREDIENT_2[i])){
      mergedName0<- c(mergedName0, updatedList$antif_2[i])
      c<-c+1
    } else
    {
      mergedName0<- paste0(mergedName0, collapse ="+")
      updatedList$MergedFamName[i] <-mergedName0
      updatedList$nATB[i] <-c
      next
    }
    if (!is.na(updatedList$INGREDIENT_3[i])){
      mergedName0<- c(mergedName0, updatedList$antif_3[i])
    }
    else {
      mergedName0<- paste0(mergedName0, collapse ="+")
      updatedList$MergedFamName[i]<-mergedName0
      updatedList$nATB[i] <-c
      next
    }
    if (!is.na(updatedList$antif_4[i])){
      mergedName0<- c(mergedName0, updatedList$INGREDIENT_4[i])
    }
    else {
      mergedName0<- paste0(mergedName0, collapse ="+")
      updatedList$MergedFamName[i]<-mergedName0
      updatedList$nATB[i] <-c
      next
    }
    if (!is.na(updatedList$antif_5[i])){
      mergedName0<- c(mergedName0, updatedList$INGREDIENT_5[i])
    }
    else {
      mergedName0<- paste0(mergedName0, collapse ="+")
      updatedList$MergedFamName[i]<-mergedName0
      updatedList$nATB[i] <-c
      next
    }
    if (!is.na(updatedList$antif_6[i])){
      mergedName0<- c(mergedName0, updatedList$INGREDIENT_6[i])
    }
    else {
      mergedName0<- paste0(mergedName0, collapse ="+")
      updatedList$MergedFamName[i]<-mergedName0
      updatedList$nATB[i] <-c
      next
    }
  }
  return(updatedList)
}


#' @title addActivity function
#' @description High level function. Add two new columns to drogue code table.
#'              Both columns come from "Cecile's table"
#' @param Activity file name for activity table.
#' @param supertabDrogue file name for any table containing drogue codes.
#' @return A new table.
#' @export
#' @examples
#'
#' actTable <-addActivity ("activity2.txt", "supertabDrogue.csv")
#' actTable <-addActivity ("activity2.txt", DFDrogue)
#'
addActivity <- function(Activity, updatedList) {

  updatedList2 <- read.table(file = Activity, header = TRUE, sep = ";")

  updatedList$AntiB <- NA
  updatedList$AntiP <- NA

  for (i in 1:nrow(updatedList)) {
    for (j in 1:nrow(updatedList2)) {

      if ((as.character(updatedList$DRUG_CODE[i]))==(as.character(updatedList2$DRUG_CODE[j]))) {
        AntiBac <- trimws(updatedList2$Antibacterial[j])
        AntiProt<- trimws(updatedList2$Antiprotozoal[j])
        updatedList$AntiB[i]<-AntiBac
        updatedList$AntiP[i]<-AntiProt
        break;

      }
    }
  }
  return(updatedList)
}


#' @title addExtraVetcols function
#' @description High level function. Add extra vet columns
#' @param extravetcol file name for extra vet columns.
#' @param supertabVet file name for any table containing drogue codes and vet vars.
#' @return A new table.
#' @export
#' @examples
#'
#' actTable <-aaddExtraVetcols ("activity2.txt", "supertabDrogue.csv")
#' actTable <-addExtraVetcols ("activity2.txt", DFDrogue)
#'
addExtraVetcols <- function(extravetcol, supertabVet) {


  if (is.character(supertabVet)){
    updatedList <-read.csv(supertabVet)
  }
  else {
    updatedList <- supertabVet
  }

  updatedList2 <- read.csv(file =extravetcol, header = TRUE, sep = ";")

  updatedList$Cattle4n<- NA
  updatedList$Poultry5n<- NA
  updatedList$SmallRumn<- NA
  updatedList$FoodAnim13n<- NA
  updatedList$SmallAnimn<- NA
  updatedList$Others14n<- NA
  updatedList$FoodAnimCL1n<- NA
  updatedList$GoatRabn<- NA
  updatedList$TLISn<- NA


  for (i in 1:nrow(updatedList)) {
    for (j in 1:nrow(updatedList2)) {
      dc<-as.character(updatedList$DRUG_CODE[i])
      dc2<-as.character(updatedList2$DRUG_CODE[j])

      if ((dc)==(dc2)){
        updatedList$GoatRabn[i]<-as.character(updatedList2$GoatRab[j])

        updatedList$Cattle4n[i]<-as.character(updatedList2$Cattle4[j])
        updatedList$Poultry5n[i]<-as.character(updatedList2$Poultry5[j])
        updatedList$SmallRumn[i]<-as.character(updatedList2$SmallRum[j])
        updatedList$FoodAnim13n[i]<-as.character(updatedList2$FoodAnim13[j])
        updatedList$SmallAnimn[i]<-as.character(updatedList2$SmallAnim[j])
        updatedList$Others14n[i]<-as.character(updatedList2$Others14[j])
        updatedList$FoodAnimCL1n[i]<-as.character(updatedList2$FoodAnimCL1[j])
        updatedList$TLISn[i]<-as.character( updatedList$TLIS[j])

        break;

      }
    }
  }
  return(updatedList)
}

#' @title datasetVmerge function
#' @description Function for vertical merging of tables containing drugs of different statuses
#' @param txt_files file name.
#' @param dpdlabel dintinctive component name: status", "drug","route", "ingred", "comp", "package" OR "vet"
#' @param stat boolean vector to select the four drug status: marketed, approved, cancelled and dormant
#' @param filt boolean parameter to choose antibiotic filtering (filt=TRUE)
#' @param f list of antibiotics and A families
#' @return A merged table.
#' @export
#' @examples
#'
#' sta<-datasetVmerge(txt_files, dpdlabel="status")
#' ing<-datasetVmerge(txt_files, dpdlabel="ingred")
#' ing<-datasetVmerge(txt_files, dpdlabel="ingred", stat=c(1, 1, 0, 0))              # MEANING: MARKETED and APPROVED ANTIBIOTICS
#' ing<-datasetVmerge(txt_files, dpdlabel="ingred", stat=c(1, 0, 0, 0), filt=FALSE)  # MEANING: MARKETED DRUGS
#' f="listAntibiodine.txt"

datasetVmerge= function(txt_files, dpdlabel,  f="listAntibiodine.txt", stat=c(1, 1, 1, 1), filt=TRUE)  {
  path<-getwd()
  dir<-paste0(path,"/", f)
  for (k in 1:length(txt_files)){
    #print(k)
    if ((txt_files[k]!="listAntibiodine.txt")&&(txt_files[k]!="ExtraVetCols.txt")&&(txt_files[k]!="activity2.txt")){
      if (stat[1]==1) {
        if (grepl(dpdlabel, txt_files[k], ignore.case ="True")&&(!grepl("_", txt_files[k], ignore.case ="True")) ) {
          obj1<-kdataset(txt_files[k], dpdlabel)
          if (dpdlabel=="status")  { obj1$Y_select() }
          obj1$select()
          TabMarketed <- obj1$DPDtable }  }
      else { TabMarketed <- data.frame()  }

      if (stat[2]==1){
        if (grepl(paste0(dpdlabel, "_ap"), txt_files[k], ignore.case ="True")) {
          obj1ap<-kdataset(txt_files[k], dpdlabel)
          if (dpdlabel=="status")  { obj1ap$Y_select() }
          obj1ap$select()
          TabApp <- obj1ap$DPDtable }  }
      else { TabApp <- data.frame()  }

      if (stat[3]==1) {
        if (grepl(paste0(dpdlabel, "_ia"), txt_files[k], ignore.case ="True")) {
          obj1ia<-kdataset(txt_files[k], dpdlabel)
          if (dpdlabel=="status")  { obj1ia$Y_select() }
          obj1ia$select()
          TabIa <- obj1ia$DPDtable  }}
      else { TabIa <- data.frame()  }

      if (stat[4]==1) {
        if (grepl(paste0(dpdlabel, "_dr"), txt_files[k], ignore.case ="True")) {
          obj1dr<-kdataset(txt_files[k], dpdlabel)
          if (dpdlabel=="status")  { obj1dr$Y_select() }
          obj1dr$select()
          TabDr <- obj1dr$DPDtable }}
      else { TabDr <- data.frame()  }
    }
  }

  dfTable<-rbind(TabMarketed, TabIa, TabApp, TabDr)

  if (dpdlabel=="ingred") {
    obj1$DPDtable<-dfTable
    if (filt)  {
      dfmanti<- read.table(dir,sep=';', header=TRUE)
      Antib_list<-dfmanti[,"AIwhocc"]
      AntiFam_list<-dfmanti[,"GroupeChimSC1"]
      obj1$Afiltering(Antib_list, AntiFam_list)  }

    dfTable<-obj1$DPDtable
  }
  return(dfTable)

}

#' @title datasetVTransmerge
#' @description Function for vertical merging of tables containing drugs of different statuses and
#' selective transposition for "vet" and "ingred"
#' @param txt_files file name.
#' @param dpdlabel dintinctive component name: "status", "drug","route", "ingred", "comp", "package" OR "vet"
#' @param stat boolean vector to select the four drug status: marketed, approved, cancelled and dormant
#' @param filt boolean parameter to choose antibiotic filtering (filt=TRUE)
#' @param TR boolean parameter to choose transposition for ingred, vet, form and route tasbles
#' @param f list of antibiotics and A families
#' @return A merged table.
#' @export
#' @examples
#' sta<-datasetVTransmerge(txt_files, dpdlabel="status")
#' ing<-datasetVTransmerge(txt_files, dpdlabel="ingred")
#' ing<-datasetVTransmerge(txt_files, dpdlabel="ingred", stat=c(1, 1, 0, 0))              # MEANING: MARKETED and APPROVED ANTIBIOTICS
#' ing<-datasetVTransmerge(txt_files, dpdlabel="ingred", stat=c(1, 0, 0, 0), filt=FALSE)  # MEANING: MARKETED DRUGS
#'  f="listAntibiodine.txt"

datasetVTransmerge= function(txt_files, dpdlabel,  f="listAntibiodine.txt", stat=c(1, 1, 1, 1), filt=TRUE, spe=TRUE)  {

  path<-getwd()
  dir<-paste0(path,"/", f)

  for (k in 1:length(txt_files)){
    #print(k)
    if ((txt_files[k]!="listAntibiodine.txt")&&(txt_files[k]!="ExtraVetCols.txt")&&(txt_files[k]!="activity2.txt")) {
      if (stat[1]==1) {
        if (grepl(dpdlabel, txt_files[k], ignore.case ="True")&&(!grepl("_", txt_files[k], ignore.case ="True")) ) {
          obj1<-kdataset(txt_files[k], dpdlabel)
          if (dpdlabel=="status")  { obj1$Y_select() }
          obj1$select()
          TabMarketed <- obj1$DPDtable  } }
      else { TabMarketed <- data.frame()   }

      if (stat[2]==1){
        if (grepl(paste0(dpdlabel, "_ap"), txt_files[k], ignore.case ="True")) {
          obj1ap<-kdataset(txt_files[k], dpdlabel)
          if (dpdlabel=="status")  { obj1ap$Y_select() }
          obj1ap$select()
          TabApp <- obj1ap$DPDtable }  }
      else { TabApp <- data.frame()  }

      if (stat[3]==1) {
        if (grepl(paste0(dpdlabel, "_ia"), txt_files[k], ignore.case ="True")) {
          obj1ia<-kdataset(txt_files[k], dpdlabel)
          if (dpdlabel=="status")  { obj1ia$Y_select() }
          obj1ia$select()
          TabIa <- obj1ia$DPDtable  }}
      else { TabIa <- data.frame()  }

      if (stat[4]==1) {
        if (grepl(paste0(dpdlabel, "_dr"), txt_files[k], ignore.case ="True")) {
          obj1dr<-kdataset(txt_files[k], dpdlabel)
          if (dpdlabel=="status")  { obj1dr$Y_select() }
          obj1dr$select()
          TabDr <- obj1dr$DPDtable }}
      else { TabDr <- data.frame()  }
    }
  }

  dfTable<-rbind(TabMarketed, TabIa, TabApp, TabDr)

  if (dpdlabel=="ingred"){
    obj1$DPDtable<-dfTable
    if (filt)  {
      dfmanti<- read.table(dir,sep=';', header=TRUE)
      Antib_list<-dfmanti[,"AIwhocc"]
      AntiFam_list<-dfmanti[,"GroupeChimSC1"]
      obj1$Afiltering(Antib_list, AntiFam_list)
      obj1$CTranspose()
    } else   {obj1$CTranspose(a=FALSE) }
    dfTable<- obj1$DPDtable
  }

  if (dpdlabel=="vet"){
    obj1$DPDtable<-dfTable
    if (spe){
      dfTable<-obj1$CTranspose()
    } else
      dfTable<-obj1$CTranspose(ve="VET_SUB_SPECIES")
  }
  if (dpdlabel=="route"){
    obj1$DPDtable<-dfTable
    dfTable<-obj1$CTranspose()

  }
  if (dpdlabel=="form"){
    obj1$DPDtable<-dfTable
    dfTable<-obj1$CTranspose()
  }

  return(dfTable)
}

#' @title HVmerge
#' @description Function for horizontal and vertical merging in a given ordering of previously chosen tables
#' containing drugs of different statuses.
#' @param txt_files set of file names.
#' @param dpdlabel vector indicating the order in which the files are merged.
#' @param b boolean param to choose whether to transpose or not
#' @return A merged table.
#' @export
#' @examples
#' txt_files <- list.files(path = "D:/WIAAgrosante/Mapaq-Antibioticos/DPD", pattern = "txt", full.names=TRUE)
#' df<-HVmerge(txt_files)
#' df<-HVmerge(txt_files, b=FALSE)
#' df<-HVmerge(txt_files, b=TRUE,  dpdlabel=c("drug", "ingred", "vet", "package"))

HVmerge=function(txt_files, b=TRUE, dpdlabel=c("status", "drug","route", "form","ingred", "comp", "package", "ther",  "vet"),  f="listAntibiodine.txt") {

  for (d in 1:length(dpdlabel)){
    if (d==1){
      if (b) {tab<-datasetVmerge(txt_files, dpdlabel[d])}
      else {tab<-datasetVTransmerge(txt_files, dpdlabel[d])}
    }
    else {
      if (b) {tab2<-datasetVmerge(txt_files, dpdlabel[d])}
      else {tab2<-datasetVTransmerge(txt_files, dpdlabel[d])}

      tab <- merge(tab,tab2,by="DRUG_CODE")
    }
  }
  return (tab)
}
#' @title DataFromZipfiles
#' @description Function to download DPD
#' @return DPD files
#' @export
#' @examples
#'
#' DataFromZipfiles()
DataFromZipfiles=function(){

  url1<-"https://www.canada.ca/content/dam/hc-sc/documents/services/drug-product-database/allfiles.zip"
  url2<-"https://www.canada.ca/content/dam/hc-sc/documents/services/drug-product-database/allfiles_ia.zip"
  url3<-"https://www.canada.ca/content/dam/hc-sc/documents/services/drug-product-database/allfiles_ap.zip"
  url4<-"https://www.canada.ca/content/dam/hc-sc/documents/services/drug-product-database/allfiles_dr.zip"
  download.file(url1, "zfil1.zip")
  unzip("zfil1.zip")
  download.file(url2, "zfil2.zip")
  unzip("zfil2.zip")
  download.file(url3, "zfil3.zip")
  unzip("zfil3.zip")
  download.file(url4, "zfil4.zip")
  unzip("zfil4.zip")
}


#' @title FileMerge
#' @description function for merging pd files and generating new  and separate
#' csv files
#'
#' @param txt_files set of file names.
#' @param dpdlabel vector indicating the files to be merged.
#' @param b boolean vector to select whether or not to transpose the ingred, form, route and vet tables
#' @param path to the directory where the DPD files are located
#' @return Vmerged tables in DPD files
#' @export
#' @examples
#'
#' txt_files <- list.files(path = getwd(), pattern = "txt")
#' FileMerge(txt_files)
#' FileMerge(txt_files, b=c(0,0,0,0))
#' FileMerge(txt_files, b=c(1,1,0,1))
#' FileMerge(txt_files, b=c(1,1,1,1))
#' FileMerge(txt_files, b=c(0,0,1,1),  dpdlabel=c("ingred", "form", "route","vet"))


FileMerge=function(txt_files, b=c(1,0,0,1), dpdlabel=c("drug","status", "route", "form", "comp","ingred", "package", "vet"), filt=TRUE, outf=TRUE)
{
  # dir<-getwd()
  # dir<-paste0(dir,"/")
  for (k in 1:length(dpdlabel)){

    if (dpdlabel[k]=="drug"){
      print("Drug_unfiltered")
      print("merging")
      tab<-datasetVmerge(txt_files, dpdlabel[k])

      if (outf) { file1<-"TabDrugall_FM.csv"  }

    } else if (dpdlabel[k]=="ingred") {
      if (filt==TRUE)  {
        print("Ingred_filtered")
        if (b[1]==1) {
          print("merging and transposing")
          tab<-datasetVTransmerge(txt_files, dpdlabel[k])
          if (outf) { file1<-"transTabIA_FM.csv" }

        } else {
          print("merging")
          tab<-datasetVmerge(txt_files, dpdlabel[k])
          if (outf) { file1<-"TabIA_FM.csv" }
        }
      } else  {
        print("ingred_unfiltered")
        if (b[1]==1) {
          print("merging and transposing")
          tab<-datasetVTransmerge(txt_files, dpdlabel[k], f="listAntibiodine.txt", stat=c(1, 1, 1, 1), filt=FALSE)
          if (outf) { file1<-"transTabIAall_FM.csv" }

        }  else {
          print("merging")
          tab<-datasetVmerge(txt_files, dpdlabel[k], f="listAntibiodine.txt", stat=c(1, 1, 1, 1), filt=FALSE)
          if (outf) { file1<-"TabIAall_FM.csv" }

        }  }
    } else if  (dpdlabel[k]=="form") {
      print("Form_unfiltered")
      if (b[2]==1) {
        print("Merging and transposing")
        tab<-datasetVTransmerge(txt_files, dpdlabel[k])
        if (outf) { file1<-"transTabFormall_FM.csv"  }
      } else {
        print("Merging")
        tab<-datasetVmerge(txt_files,dpdlabel[k])
        if (outf) { file1<-"TabFormall_FM.csv" }
      }
    } else if (dpdlabel[k]=="route")   {
      print("Route_unfiltered")
      if (b[3]==1) {
        print("Merging and transposing")
        tab<-datasetVTransmerge(txt_files, dpdlabel[k])
        if (outf)  { file1<-"transTabRouteall_FM.csv" }
      } else {
        print("Merging")
        tab<-datasetVmerge(txt_files, dpdlabel[k])
        if (outf)    { file1<-"TabRouteall_FM.csv" }
      }
    } else if (dpdlabel[k]=="status")   {
      print("Status_unfiltered")
      print("Merging")
      tab<-datasetVmerge(txt_files, dpdlabel[k])
      if (outf)  { file1<-"TabStatusall_FM.csv" }
    } else if (dpdlabel[k]=="comp")   {
      print("Comp_unfiltered")
      print("Merging")
      tab<-datasetVmerge(txt_files, dpdlabel[k])
      if (outf)   { file1<-"TabCompall_FM.csv"  }
    }  else if (dpdlabel[k]=="package")   {
      print("Pack_unfiltered")
      print("Merging")
      tab<-datasetVmerge(txt_files, dpdlabel[k])
      if (outf)   { file1<-"TabPackall_FM.csv"  }
    }   else if (dpdlabel[k]=="vet")   {
      print("Vet_unfiltered")
      if (b[4]==1) {
        print("Merging and transposing")
        tab<-datasetVTransmerge(txt_files, dpdlabel[k])
        if (outf)    { file1<-"transTabVetall_FM.csv"  }
      } else {
        print("Merging")
        tab<-datasetVmerge(txt_files, dpdlabel[k])
        if (outf)   { file1<-"TabVetall_FM.csv"  }
      }
    }

    if (outf)  {
      print("writing in csv file")
      write.csv(tab,file1, row.names=FALSE)   }
    # paste0(dir,file1)
    else return(tab)
  }
}

#' @title CFmergeFiles
#' @description function for vertical and horizontal merging of dpd files in the order required
#' to generate Cecile and Nicolas' excel file or any other similar file. Drug and ingred must be the first two labels
#' @param dpdlabel vector indicating the order in which the files are merged.
#' @param b boolean vector to select whether or not to transpose the ingred, form, route and vet tables
#' @param path to the directory where the DPD files are located
#' @return A merged table.
#' @export
#' @examples
#'
#' CFmergeFiles()
#' CFmergeFiles(b=c(1,0,1,1),dpdlabel=c("drug","ingred", "route", "form", "status","comp", "package", "vet"), filt=TRUE, fout="superTabIngDrugrouteformCompStaVetPack_CF2.csv")

# merge tables: drug ingred route form Status (AddMergedIngred AddCategory AddActivity) Com Pack Vet

CFmergeFiles=function(b=c(1,0,0,1), dpdlabel=c("drug","ingred", "route", "form", "status","comp", "package", "vet"), filt=TRUE, fout="superTabIngDrugrouteformCompStaVetPack_CF6.csv")
{
  # file1<- rep(NA, length(dpdlabel))
  txt_files <- list.files(path = getwd(), pattern = "txt")
  for (k in 1:length(dpdlabel)){

    if (dpdlabel[k]=="drug"){
      print("Drug_unfiltered")
      print("merging")
      tab_drug<-datasetVmerge(txt_files, dpdlabel[k])
  }
  else if (dpdlabel[k]=="ingred") {

    if (filt==TRUE)  {
      print("Ingred_filtered")
      if (b[1]!=0) {
        print("merging and transposing")
        tab_filt_ingred<-datasetVTransmerge(txt_files, dpdlabel[k])
      }
      else {

        tab_filt_ingred<-datasetVmerge(txt_files, dpdlabel[k])
      }
      print("adding antibiotic families")
      tab <- merge(x=tab_drug,y=tab_filt_ingred,by="DRUG_CODE", all.y=T)
    }
    else  {
      print("ingred_unfiltered")
      if (b[1]!=0) {
        print("merging and transposing")
        tab_ingred<-datasetVTransmerge(txt_files, dpdlabel[k], f="listAntibiodine.txt", stat=c(1, 1, 1, 1), filt=FALSE)

      }  else {
        print("merging")
        tab_ingred<-datasetVmerge(txt_files, dpdlabel[k], f="listAntibiodine.txt", stat=c(1, 1, 1, 1), filt=FALSE)

      }
      tab <- merge(x=tab_drug,y=tab_ingred,by="DRUG_CODE", all.y=T)
    }
    tab[,'DRUG_CODE']<- as.character(tab[,'DRUG_CODE'])
  }
  else if  (dpdlabel[k]=="form") {
    print("Form_unfiltered")
    if (b[2]==1) {
      print("Merging and transposing")
      tab_Form<-datasetVTransmerge(txt_files, dpdlabel[k])
    } else {
      print("Merging")
      tab_Form<-datasetVmerge(txt_files,dpdlabel[k])
    }
    tab <- merge(x=tab,y=tab_Form,by="DRUG_CODE", all.y=T) #cf all.x=T  cf2 form all=T route all.x=T  cf5 form route all=T
    tab[,'DRUG_CODE']<- as.character(tab[,'DRUG_CODE'])  # all.y=T form cf3 all=T route cf3
                                                          # all.x=T form cf4 all=T route cf4
                                                           # cf6 all.y =T  all.y=T
  }
  else if (dpdlabel[k]=="route")   {
    print("Route_unfiltered")
    if (b[3]==1) {
      print("Merging and transposing")
      tab_Route<-datasetVTransmerge(txt_files, dpdlabel[k])
    } else {
      print("Merging")
      tab_Route<-datasetVmerge(txt_files, dpdlabel[k])
    }
    tab <- merge(x=tab,y=tab_Route,by="DRUG_CODE", all.y=T)  #all.x=T
    tab[,'DRUG_CODE']<- as.character(tab[,'DRUG_CODE'])
    print("Adding merged antibiotic family names, HC categories and activities")
    updatedTab<-AddMergedIngredNames2(tab)
    updatedTable<-addCategory(updatedTab)
    actTable<-addActivity ("activity2.txt",updatedTable)


  }
  else if (dpdlabel[k]=="status")   {
    print("Status_unfiltered")
    print("Merging")
    tab_Status<-datasetVmerge(txt_files, dpdlabel[k])
    tab <- merge(x=actTable,y=tab_Status,by="DRUG_CODE", all=T)

  } else if (dpdlabel[k]=="comp")   {
    print("Comp_unfiltered")
    print("Merging")
    tab_Comp<-datasetVmerge(txt_files, dpdlabel[k])
    tab <- merge(x=tab,y=tab_Comp,by="DRUG_CODE", all=T)

  }  else if (dpdlabel[k]=="package")   {
    print("Pack_unfiltered")
    print("Merging")
    tab_Pack<-datasetVmerge(txt_files, dpdlabel[k])
    tab <- merge(x=tab,y=tab_Pack,by="DRUG_CODE", all=T)

  }   else if (dpdlabel[k]=="vet")
    {
    print("Vet_unfiltered")
    if (b[4]==1) {
      print("Merging and transposing")
      tab_Vet<-datasetVTransmerge(txt_files, dpdlabel[k])

    } else {
      print("Merging")
      tab_Vet<-datasetVmerge(txt_files, dpdlabel[k])
    }
    tab <- merge(x=tab, y= tab_Vet,by="DRUG_CODE", all=T)
    tab<-tab[!is.na(tab$CLASS),]
  }
  }
  dir<-getwd()
  dir<-paste0(dir,"/", fout)
  write.csv(tab, dir, row.names=FALSE)

  return(tab)

}



#' @title CFmergeFiles2
#' @description function for vertical and horizontal merging of dpd files in the order required
#' to generate a file similar to the Cecile and Nicolas' excel file. Drug and ingred must be the first two labels
#' @param dpdlabel vector indicating the order in which the files are merged.
#' @param b boolean vector to select whether or not to transpose the ingred, form, route and vet tables
#' @param path to the directory where the DPD files are located
#' @param bm1  this parameter allows access to the argument (0: all=T, 1: all.x=T, 2: all.y=T) of the merge  function for the table Form
#' @param bm2  this parameter allows access to the argument (0: all=T, 1: all.x=T, 2: all.y=T)  of the merge  function for the table Route
#' @param extrac Boolean parameters to add extra columns to vet subtable
#' @return A merged table.
#' @export
#' @examples
#'
#' CFmergeFiles2() #sintaxis to generate a file very similar to the "Cecile and Nicolas' excel file"

# merge tables: drug ingred route form Status (AddMergedIngred AddCategory AddActivity) Com Pack Vet

CFmergeFiles2=function(b=c(1,0,0,1), dpdlabel=c("drug","ingred", "form","route",  "status","comp", "package", "vet"), filt=TRUE, fout="superTabIngDrugrouteformCompStaVetPack_CF0.csv", bm1=1, bm2=1, extrac=TRUE)
{
  # file1<- rep(NA, length(dpdlabel))
  txt_files <- list.files(path = getwd(), pattern = "txt")
  for (k in 1:length(dpdlabel)){

    if (dpdlabel[k]=="drug"){
      print("Drug_unfiltered")
      print("merging")
      tab_drug<-datasetVmerge(txt_files, dpdlabel[k])
  }
  else if (dpdlabel[k]=="ingred") {

    if (filt==TRUE)  {
      print("Ingred_filtered")
      if (b[1]!=0) {
        print("merging and transposing")
        tab_filt_ingred<-datasetVTransmerge(txt_files, dpdlabel[k])
      }
      else {

        tab_filt_ingred<-datasetVmerge(txt_files, dpdlabel[k])
      }
      print("adding antibiotic families")
      tab <- merge(x=tab_drug,y=tab_filt_ingred,by="DRUG_CODE", all.y=T)
    }
    else  {
      print("ingred_unfiltered")
      if (b[1]!=0) {
        print("merging and transposing")
        tab_ingred<-datasetVTransmerge(txt_files, dpdlabel[k], f="listAntibiodine.txt", stat=c(1, 1, 1, 1), filt=FALSE)

      }  else {
        print("merging")
        tab_ingred<-datasetVmerge(txt_files, dpdlabel[k], f="listAntibiodine.txt", stat=c(1, 1, 1, 1), filt=FALSE)

      }
      tab <- merge(x=tab_drug,y=tab_ingred,by="DRUG_CODE", all.y=T)
    }
    tab[,'DRUG_CODE']<- as.character(tab[,'DRUG_CODE'])
  }
  else if  (dpdlabel[k]=="form") {
    print("Form_unfiltered")
    if (b[2]==1) {
      print("Merging and transposing")
      tab_Form<-datasetVTransmerge(txt_files, dpdlabel[k])
    } else {
      print("Merging")
      tab_Form<-datasetVmerge(txt_files,dpdlabel[k])
    }

                                                         #cf all.x=T  cf2 form all=T route all.x=T  cf5 form route all=T
    tab[,'DRUG_CODE']<- as.character(tab[,'DRUG_CODE'])  # all.y=T form cf3 all=T route cf3
                                                          # all.x=T form cf4 all=T route cf4
                                                           # cf6 all.y =T  all.y=T
    if (bm1==0)  {
      tab <- merge(x=tab,y=tab_Form,by="DRUG_CODE", all=T)
    } else if (bm1==1) {
      tab <- merge(x=tab,y=tab_Form,by="DRUG_CODE", all.x=T)
    } else if (bm1==2) {
      tab <- merge(x=tab,y=tab_Form,by="DRUG_CODE", all.y=T)
    }

  }
  else if (dpdlabel[k]=="route")   {
    print("Route_unfiltered")
    if (b[3]==1) {
      print("Merging and transposing")
      tab_Route<-datasetVTransmerge(txt_files, dpdlabel[k])
    } else {
      print("Merging")
      tab_Route<-datasetVmerge(txt_files, dpdlabel[k])
    }

    if (bm2==0)  {
      tab <- merge(x=tab,y=tab_Route,by="DRUG_CODE", all=T)
    } else if (bm2==1) {
      tab <- merge(x=tab,y=tab_Route,by="DRUG_CODE", all.x=T)
    } else if (bm2==2) {
      tab <- merge(x=tab,y=tab_Route,by="DRUG_CODE", all.y=T)
    }

    tab[,'DRUG_CODE']<- as.character(tab[,'DRUG_CODE'])

    print("Adding merged antibiotic family names, HC categories and activities")
    tab<-AddMergedIngredNames2(tab)
    tab<-addCategory(tab)
    tab<-addActivity("activity2.txt", tab)

  }
  else if (dpdlabel[k]=="status")   {
    print("Status_unfiltered")
    print("Merging")
    tab_Status<-datasetVmerge(txt_files, dpdlabel[k])
    tab <- merge(x=tab,y=tab_Status,by="DRUG_CODE", all=T)

  } else if (dpdlabel[k]=="comp")   {
    print("Comp_unfiltered")
    print("Merging")
    tab_Comp<-datasetVmerge(txt_files, dpdlabel[k])
    tab <- merge(x=tab,y=tab_Comp,by="DRUG_CODE", all=T)

  }  else if (dpdlabel[k]=="package")   {
    print("Pack_unfiltered")
    print("Merging")
    tab_Pack<-datasetVmerge(txt_files, dpdlabel[k])
    tab <- merge(x=tab,y=tab_Pack,by="DRUG_CODE", all=T)

  }   else if (dpdlabel[k]=="vet")
    {
    print("Vet_unfiltered")
    if (b[4]==1) {
      print("Merging and transposing")
      tab_Vet<-datasetVTransmerge(txt_files, dpdlabel[k])

    } else {
      print("Merging")
      tab_Vet<-datasetVmerge(txt_files, dpdlabel[k])
    }
    tab <- merge(x=tab, y= tab_Vet,by="DRUG_CODE", all=T)
    if (extrac) {
      print("Adding extra vet columns")
      tab_Vet2<-datasetVTransmerge(txt_files, dpdlabel[k], f="listAntibiodine.txt", stat=c(1, 1, 1, 1), filt=TRUE, spe=FALSE)
      tab_Vet2<-tab_Vet2[,c("DRUG_CODE","CALVES","CHICKENS","GEESE","DUCKS","TURKEYS","DAIRY CATTLE","BEEF CATTLE","COMPANION BIRDS")]
      tab_Vet2<-tab_Vet2[,-c("LAMBS", "RABBIT", "WILD ANIMALS (DEER, MOOSE, BISON)")]
      tab <- merge(x=tab, y= tab_Vet2,by="DRUG_CODE", all=T)
      tab<-addExtraVetcols("ExtraVertCols.csv",tab)

    }
    tab<-tab[!is.na(tab$CLASS),]
  }
  }
  dir<-getwd()
  dir<-paste0(dir,"/", fout)
  write.csv(tab, dir, row.names=FALSE)

  return(tab)

}

