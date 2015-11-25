#Hook function .onAttach is called when attach package.
#' @importFrom Rcmdr putRcmdr getRcmdr closeCommander Commander
.onAttach <- function(libname, pkgname){
  if (!interactive()) return()
  putRcmdr("slider.env", new.env())    
  Rcmdr <- options()$Rcmdr
  plugins <- Rcmdr$plugins
  if (!pkgname %in% plugins) {
    Rcmdr$plugins <- c(plugins, pkgname)
    options(Rcmdr=Rcmdr)
    if("package:Rcmdr" %in% search()) {
      if(!getRcmdr("autoRestart")) {
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
      }
    }
    else {
      Commander()
    }        
  }
}

#'Generalized Waring Regression Model
#'Using the selected data show a window asking for the GWRM parameters
#'
#' @return a new model in the model list
#' 
#' @importFrom Rcmdr gettextRcmdr getDialog initializeDialog ActiveModel formulaFields ActiveDataSet modelFormula UpdateModelNumber tclVar tkframe ttkentry subsetBox variableComboBox Numeric tclvalue errorCondition gettextRcmdr trim.blanks is.valid.name checkReplace UpdateModelNumber closeDialog getSelection putDialog ActiveDataSet doItAndPrint activeModel tkfocus CommanderWindow OKCancelHelp buttonRcmdr tkgrid labelRcmdr getFrame tklabel dialogSuffix
#' @import GWRM RcmdrMisc
#' 
#' @export

generalizedWaringModel<-function () 
{
  if (! ("gw" %in% getRcmdr("modelClasses")))
    putRcmdr("modelClasses",c(getRcmdr("modelClasses"),"gw"))
  
  defaults <- list(initial.weight = gettextRcmdr("<no variable selected>"))
  dialog.values <- getDialog("generalizedWaringModel", defaults)
  initializeDialog(title = gettextRcmdr("Generalized Waring Model"))
  .activeModel <- ActiveModel()
  currentModel <- if (!is.null(.activeModel)) 
    class(get(.activeModel, envir = .GlobalEnv))[1] == "gw"
  else FALSE
  if (currentModel) {
    currentFields <- formulaFields(get(.activeModel, envir = .GlobalEnv), 
                                   glm = FALSE)
    if (currentFields$data != ActiveDataSet()) 
      currentModel <- FALSE
  }
  if (isTRUE(getRcmdr("reset.model"))) {
    currentModel <- FALSE
    putRcmdr("reset.model", FALSE)
  }
  modelFormula()
  UpdateModelNumber()
  modelName <- tclVar(paste("GW.", getRcmdr("modelNumber"), 
                            sep = ""))  
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
  max.height <- getRcmdr("variable.list.height")
  
  kFrame <- tkframe(top)
  kValueVariable<-tclVar("")
  kValue<- ttkentry(kFrame, width = "6",textvariable=kValueVariable)
  
  
  subsetWeightFrame <- tkframe(top)
  subsetBox(window = subsetWeightFrame, model = TRUE)
  weightComboBox <- variableComboBox(subsetWeightFrame, variableList=Numeric(), 
                                     initialSelection=dialog.values$initial.weight,
                                     title=gettextRcmdr("Weights"))
  onOK <- function() {
    check.empty <- gsub(" ", "", tclvalue(lhsVariable))
    if ("" == check.empty) {
      errorCondition(recall = generalizedWaringModel, model = TRUE, 
                     message = gettextRcmdr("Left-hand side of model empty."))
      return()
    }
    check.empty <- gsub(" ", "", tclvalue(rhsVariable))
    if ("" == check.empty) {
      errorCondition(recall = generalizedWaringModel, model = TRUE, 
                     message = gettextRcmdr("Right-hand side of model empty."))
      return()
    }
    
    check.not.empty <- gsub(" ", "", tclvalue(kValueVariable))
    if ("" == check.not.empty) {
      k<-""
    }else if(as.numeric(check.not.empty)){
      k <-paste(", k=",as.numeric(check.not.empty))
    }else{
      errorCondition(recall = generalizedWaringModel, model = TRUE, 
                     message = gettextRcmdr("k must be numeric."))
      return()
    }
    
    modelValue <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(modelValue)) {
      errorCondition(recall = generalizedWaringModel, model = TRUE, 
                     message = sprintf(gettextRcmdr("\"%s\" is not a valid name."), 
                                       modelValue))
      return()
    }
    if (is.element(modelValue, listGeneralizedWaringModels())) {
      if ("no" == tclvalue(checkReplace(modelValue, type = gettextRcmdr("Model")))) {
        UpdateModelNumber(-1)
        closeDialog()
        generalizedWaringModel()
        return()
      }
    }
    formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), 
                     sep = " ~ ")
    
    subset <- tclvalue(subsetVariable)
    closeDialog()
    if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || 
        trim.blanks(subset) == "") {
      subset <- ""
      putRcmdr("modelWithSubset", FALSE)
    }
    else {
      subset <- paste(", subset=", subset, sep = "")
      putRcmdr("modelWithSubset", TRUE)
    }
    weight.var <- getSelection(weightComboBox)
    putDialog("generalizedWaringModel", list(initial.weight = weight.var))
    weights <- if (weight.var == gettextRcmdr("<no variable selected>")) 
      ""
    else paste(", weights=", weight.var, sep = "")
    command <- paste("gw(", formula, ", data=", ActiveDataSet(), subset, weights, k,
                     ")", sep = "")
    doItAndPrint(paste(modelValue, " <- ", command, sep = ""))
    doItAndPrint(paste("summary(", modelValue, ")", sep = ""))
    activeModel(modelValue)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "generalizedWaringModel", model = TRUE, 
               reset = "resetGW", apply = "generalizedWaringModel")
  helpButton <- buttonRcmdr(buttonsFrame, text = "Help", width = "12", 
                            command = onHelp)
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model:")), 
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(kFrame, text=gettextRcmdr("Model parameters (optional)"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w",columnspan=2)
  tkgrid(labelRcmdr(kFrame, text = gettextRcmdr("k:")), kValue, sticky = "w")
  tkgrid(kFrame, sticky = "w")
  tkgrid(getFrame(xBox), sticky = "w")
  tkgrid(outerOperatorsFrame, sticky = "w")
  tkgrid(formulaFrame, sticky = "w")
  tkgrid(subsetFrame, tklabel(subsetWeightFrame, text = "   "), 
         getFrame(weightComboBox), sticky = "nw")
  tkgrid(subsetWeightFrame, sticky = "w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(focus = lhsEntry, preventDoubleClick = TRUE)
}

#' @export
listGeneralizedWaringModels<-function (envir = .GlobalEnv, ...) 
{
  objects <- ls(envir = envir, ...)
  if (length(objects) == 0) 
    NULL
  else objects[sapply(objects, function(.x) "gw" == (class(get(.x, 
                                                               envir = envir))[1]))]
}

#' @export
resetGW<- function () 
{
  putRcmdr("reset.model", TRUE)
  putDialog("generalizedWaringModel", NULL)
  putDialog("generalizedWaringModel", NULL, resettable = FALSE)
  generalizedWaringModel()
}

#' @export
activeModelGW<- function(){
  if (is.null(ActiveModel())){
    return (FALSE)
  }else{
    if(class(get(activeModel(), envir = .GlobalEnv))[1] == "gw"){
         #Disabled some menus when activeModel is gw
         menus=Rcmdr:::getRcmdr("Menus")
         modelsMenus=menus[mapply(function(a){names(a$position)}=="modelsMenu",menus)]
         .Tcl(paste(modelsMenus[[1]]$ID," entryconfigure 8 -state disabled",sep=""))
         .Tcl(paste(modelsMenus[[1]]$ID,".1"," entryconfigure 0 -state disabled",sep=""))
         .Tcl(paste(modelsMenus[[1]]$ID,".1"," entryconfigure 1 -state disabled",sep=""))
         .Tcl(paste(modelsMenus[[1]]$ID,".1"," entryconfigure 2 -state disabled",sep=""))
      return (TRUE)
    }else{
      return (FALSE)  
    }
  }
}


#' @importFrom Rcmdr tkdestroy
#' @export
showTable<-function(...){
  tkdestroy(getRcmdr("gwrmNewDataTable"))
  newDataTable<- tkframe(getRcmdr("gwrmNewDataFrame"))
  nrows <- as.numeric(tclvalue(getRcmdr("gwrmRowsValue")))
  colNames<-getRcmdr("gwrmColNames")
  ncols<-length(colNames)
  putRcmdr("gwrmNewDataTable",newDataTable)
  header<-"tklabel(newDataTable, text='')"
  for (col in seq(length.out=ncols)) {
    putRcmdr(colNames[col],c())
    header <- paste(header, ", ", "tklabel(newDataTable, width='12', text=\'", 
                    colNames[col], "\')", sep="")
  }
  if (ncols > 0) {
    eval(parse(text=paste("tkgrid(", header, ")", sep="")))
    for (row in seq(length.out=nrows)) { 
      rowSource <- paste("tklabel(newDataTable, width='6', text=\'",
                         row, "\')", sep="")
      for (col in 1:ncols){
        varName=paste("gwrm_",colNames[col],"_",row,"_",col,sep="")
        putRcmdr(varName,tclVar(""))
        rowSource <- paste(rowSource, ", ", "tkentry(newDataTable, width='12',background='#ffffff', textvariable=getRcmdr(\'",varName,"\'))", sep="")
      }      
      eval(parse(text=paste("tkgrid(", rowSource, ")", sep="")))
    }
  }
  tkgrid(newDataTable, sticky="w")
}

#' @export
extractNewData <-function(nrows,ncols,colNames){
  textCreateNewData<-"data.frame("
  for (col in 1:ncols){
    colName=paste("gwrm_",colNames[col],sep="")
    putRcmdr(colName,c())
    colData=eval(parse(text=paste(ActiveModel(),"$", "data","$",colNames[col], sep="")))
    for(row in 1:nrows){
      cellName=paste("gwrm_",colNames[col],"_",row,"_",col,sep="")
      if (is.factor(colData)||is.character(colData)){
        cellValue <- tclvalue(getRcmdr(cellName))
      }else{
        cellValue <- as.numeric(tclvalue(getRcmdr(cellName)))
      }
      putRcmdr(colName,c(getRcmdr(colName),cellValue))
    }
    textCreateNewData<-paste(textCreateNewData,colNames[col],
                             "=getRcmdr(\'",colName,"\'), ",sep="")
  }
  textCreateNewData<-paste(substring(textCreateNewData,1,nchar(textCreateNewData)-2),")",sep="")
  newData<-eval(parse(text=textCreateNewData))
  newData <- na.omit(newData)
  
  tt <- terms(get(ActiveModel()))
  Terms <- delete.response(tt)
  m <- model.frame(Terms, newData, xlev = get(ActiveModel())$xlevels)
  if (!is.null(cl <- attr(Terms, "dataClasses"))) 
    .checkMFClasses(cl, m)
  return (newData)
}

#'Generalized Waring Regression Model Partition Varianze
#'Using the selected gw model show a window asking for de Partition Varianze values 
#'
#' @return Partition Varianze of the model
#' 
#' @importFrom Rcmdr tkframe tkscale
#' 
#' @export
gwrmPartVar <-function(){
  
  initializeDialog(title=gettextRcmdr("Variance Partition"))
  newDataFrame <- tkframe(top)
  putRcmdr("gwrmNewDataFrame",newDataFrame)
  tkgrid(tklabel(newDataFrame, text="Enter New Data (optional)",fg="blue"), sticky="w")
  newDataRowsFrame <- tkframe(newDataFrame)
  rowsValue <- tclVar("0")
  putRcmdr("gwrmRowsValue",rowsValue)
  rowsText<- tklabel(newDataRowsFrame, text="Number of data rows")
  rowsSlider <- tkscale(newDataRowsFrame, from=0, to=10, showvalue=FALSE, variable=rowsValue,
                        resolution=1, orient="horizontal", command=showTable)
  rowsShow <- tklabel(newDataRowsFrame, textvariable=rowsValue, background="white", width=2, justify="right")
  tkgrid(rowsText,rowsShow,rowsSlider, sticky="we")
  tkgrid(newDataRowsFrame, sticky="w")
  
  colNames <- if (is.null(ActiveModel())) NULL else all.vars(delete.response(terms(get(ActiveModel()))))
  putRcmdr("gwrmColNames",colNames)
  ncols<-length(colNames)
  
  newDataTable<- tkframe(newDataFrame)
  putRcmdr("gwrmNewDataTable",newDataTable)
  header<-"tklabel(newDataTable, text='')"
  for (col in seq(length.out=ncols)) {
    putRcmdr(colNames[col],c())
    header <- paste(header, ", ", "tklabel(newDataTable, width='12', text=\'", 
                    colNames[col], "\')", sep="")
  }   
  
  tkgrid(newDataTable, sticky="w")
  tkgrid(newDataFrame, sticky="w")
  onOK <- function() {
    
    if (is.null(ActiveModel())) {
      errorCondition(recall=gwrmPartVar, message=sprintf(gettextRcmdr("No active model.  Please press Cancel.")))
      return()
    }
    if (!inherits(get(ActiveModel()), "gw")){ 
      errorCondition(recall=gwrmPartVar, message=sprintf(gettextRcmdr("No gw active model.  Please press Cancel.")))
      return()
    }
    nrows <- as.numeric(tclvalue(rowsValue))
    ncols<-length(colNames)
    newData<-NULL
    if(ncols>0 && nrows>0){
      newData<-extractNewData(nrows,ncols,colNames)
      putRcmdr("gwrmNewData",newData)
      doItAndPrint("gwrmNewData<-getRcmdr('gwrmNewData')")
      doItAndPrint("gwrmNewData")
    }
    closeDialog()
    setBusyCursor()
    newdata <- if (! is.null(newData)) 
      ", newdata=gwrmNewData"
    else ""
    command <- paste("partvar(",ActiveModel() , newdata,")", sep = "")
    doItAndPrint(command)
    on.exit(setIdleCursor())
    tkfocus(CommanderWindow())
  }
  onCancel<-function(){
    closeDialog()
  }
  OKCancelHelp(helpSubject="gwrmPartVar")
  
  tkgrid(buttonsFrame, columnspan=2, sticky="w")
  #dialogSuffix(rows=7, columns=2)  
  dialogSuffix()
}

#'Generalized Waring Regression Model Residuals
#'Using the selected gw model show a window asking for the residuals function parameters
#'
#' @return Residuals plot
#' 
#' @importFrom Rcmdr variableListBox
#' 
#' @export
gwrmResiduals<-function(){
  
  typeSets <- c("pearson","deviance","response")
  initializeDialog(title=gettextRcmdr("Simulated Envelope of Residuals"))
  
  newDataFrame <- tkframe(top)
  putRcmdr("gwrmNewDataFrame",newDataFrame)
  
  typeSetsBox <- variableListBox(top, typeSets, title=gettextRcmdr("Select the residuals type"),
                                 initialSelection=0)
  repFrame <- tkframe(top)
  repValueVariable<-tclVar(19)
  repValue<- ttkentry(repFrame, width = "6",textvariable=repValueVariable)
  tkgrid(labelRcmdr(repFrame, text = gettextRcmdr("Repeats")), repValue, sticky = "w")
  tkgrid(repFrame, sticky = "w")
  tkgrid(getFrame(typeSetsBox), sticky="nw")
  
  tkgrid(tklabel(newDataFrame, text=gettextRcmdr("Enter New Data (optional)"),fg="blue"), sticky="w")
  newDataRowsFrame <- tkframe(newDataFrame)
  rowsValue <- tclVar("0")
  putRcmdr("gwrmRowsValue",rowsValue)
  rowsText<- tklabel(newDataRowsFrame, text="Number of data rows")
  rowsSlider <- tkscale(newDataRowsFrame, from=0, to=10, showvalue=FALSE, variable=rowsValue,
                        resolution=1, orient="horizontal", command=showTable)
  rowsShow <- tklabel(newDataRowsFrame, textvariable=rowsValue, background="white", width=2, justify="right")
  tkgrid(rowsText,rowsShow,rowsSlider, sticky="we")
  tkgrid(newDataRowsFrame, sticky="w")
  
  
  colNames <- if (is.null(ActiveModel())) NULL else all.vars(delete.response(terms(get(ActiveModel()))))
  putRcmdr("gwrmColNames",colNames)
  ncols<-length(colNames)
  
#   newDataTable<- tkframe(newDataFrame)
#   putRcmdr("gwrmNewDataTable",newDataTable)
#   header<-"tklabel(newDataTable, text='')"
#   for (col in seq(length.out=ncols)) {
#     putRcmdr(colNames[col],c())
#     header <- paste(header, ", ", "tklabel(newDataTable, width='12', text=\'", 
#                     colNames[col], "\')", sep="")
#   }   
#   
#   tkgrid(newDataTable, sticky="w")
#   tkgrid(newDataFrame, sticky="w")
  
  onOK <- function(){
    if (is.null(ActiveModel())) {
      errorCondition(recall=gwrmResiduals, message=sprintf(gettextRcmdr("No active model.  Please press Cancel.")))
      return()
    }
    if (!inherits(get(ActiveModel()), "gw")){ 
      errorCondition(recall=gwrmResiduals, message=sprintf(gettextRcmdr("No GW active model.  Please press Cancel.")))
      return()
    }
    nrows <- as.numeric(tclvalue(rowsValue))
    ncols<-length(colNames)
    newData<-NULL
    if(ncols>0 && nrows>0){
      newData<-extractNewData(nrows,ncols,colNames)
      putRcmdr("gwrmNewData",newData)
      doItAndPrint("gwrmNewData<-getRcmdr('gwrmNewData')")
      doItAndPrint("gwrmNewData")
    }
    stringInteger<- gsub(" ", "", tclvalue(repValueVariable))
    check.integerGT18 <-if (suppressWarnings(!is.na(as.numeric(stringInteger)))) 
      (as.numeric(stringInteger)==round(as.numeric(stringInteger))&&as.numeric(stringInteger)>18) 
    else FALSE
    if (!check.integerGT18)  {
      errorCondition(recall = gwrmResiduals, model = TRUE, 
                     message = gettextRcmdr("Repeats must be integer greater than 18."))
      return()
    }
    
    selectionType <- getSelection(typeSetsBox)
    closeDialog()
    setBusyCursor()
    repeatsEnvelope<-paste(",rep=",stringInteger,sep="")
    newdata <- if (! is.null(newData)) 
      ", newdata=gwrmNewData"
    else ""
    typeText<-paste(", type=\"", selectionType,"\"", sep = "")
    command <- paste("residuals(",ActiveModel(),typeText , newdata,", envelope=TRUE",repeatsEnvelope,")", sep = "")
    doItAndPrint(command)
    on.exit(setIdleCursor())
    tkfocus(CommanderWindow())
  }
  onCancel<-function(){
    closeDialog()
  }
  
  tkgrid(tklabel(top, text=gettextRcmdr("This procedure can last several minutes"),fg="red"), columnspan=2, sticky="w")
  OKCancelHelp(helpSubject="gwrmPartVar")
  
  tkgrid(buttonsFrame, columnspan=2, sticky="w")
  #dialogSuffix(rows=7, columns=2)  
  dialogSuffix()
}

#'Add observationStatisticsGWRM
#'Using gw model active add observations data
#'
#' @return Residuals plot
#' 
#' @importFrom Rcmdr variableListBox
#' 
#' @export
gwrmAddObservationStatistics <- function () 
{
  #Is the same as Rcommader with a bug solved.
  .activeDataSet <- ActiveDataSet()
  .activeModel <- ActiveModel()
  if (is.null(.activeModel)) 
    return()
  
  if (activeModelGW()){
    addVariable <- function(name) {
      variable <- paste(name, ".", .activeModel, sep = "")
      if (is.element(variable, .variables)) {
        ans <- checkReplace(variable)
        if (tclvalue(ans) == "no") 
          return()
      }
      paste(variable, " <- ", name, "(", .activeModel, ")", 
            sep = "")
    }
    if (getRcmdr("modelWithSubset")) {
      Message(message = gettextRcmdr("Observation statistics not available\nfor a model fit to a subset of the data."), 
              type = "error")
      tkfocus(CommanderWindow())
      return()
    }
    defaults <- list(initial.fitted = 1, initial.residuals = 1, initial.obsNumbers = 1)
    dialog.values <- getDialog("addObservationStatistics", defaults)
    initializeDialog(title = gettextRcmdr("Add Observation Statistics to Data"))
    .variables <- Variables()
    obsNumberExists <- is.element("obsNumber", .variables)
    checkBoxes(frame = "selectFrame", boxes = c("fitted", "residuals", "obsNumbers"), 
               labels = gettextRcmdr("Fitted values", "Residuals", "Observation indices"), 
               initialValues = c(dialog.values$initial.fitted, dialog.values$initial.residuals, dialog.values$initial.obsNumbers))
    command <- paste(.activeDataSet, "<- within(", .activeDataSet, ", {", sep = "")
    onOK <- function() {
      closeDialog()
      if (tclvalue(fittedVariable) == 1) 
        command <- paste(command, "\n  ", addVariable("fitted"), 
                         sep = "")
      if (tclvalue(residualsVariable) == 1) 
        command <- paste(command, "\n  ", addVariable("residuals"), 
                         sep = "")
      obsNumbers <- tclvalue(obsNumbersVariable)
      putDialog("addObservationStatistics", list(initial.fitted = tclvalue(fittedVariable), 
                                                 initial.residuals = tclvalue(residualsVariable), 
                                                 initial.obsNumbers = obsNumbers))
      if (tclvalue(obsNumbersVariable) == 1) {
        proceed <- if (obsNumberExists) 
          tclvalue(checkReplace("obsNumber"))
        else "yes"
        if (proceed == "yes") {
          command <- paste(command, "\n  obsNumber <- 1:nrow(", 
                           .activeDataSet, ")", sep = "")
        }
      }
      command <- paste(command, "\n})")
      result <- doItAndPrint(command)
      if (class(result) != "try-error") 
        activeDataSet(.activeDataSet, flushModel = FALSE, 
                      flushDialogMemory = FALSE)
      tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "influence.measures", reset = "addObservationStatistics")
    tkgrid(selectFrame, sticky = "w")
    tkgrid(buttonsFrame, sticky = "w")
    dialogSuffix()
  } else{
    Rcmdr:::addObservationStatistics()
  }
}
