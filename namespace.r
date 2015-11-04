stepwise
stepwise(   )
stepwise()
RcmdrMisc::stepwise
library("RcmdrPlugin.GWRM", lib.loc="~/R/win-library/3.2")
nrow(model.matrix(GW.1))
GW.1
model.matrix()
model.matrix
model.matrix.default()
model.matrix.default
eenvironment(GW.1)
environment(GW.1)
environment(GLM.2)
stepwise(GLM.2, direction='backward/forward', criterion='BIC')
stepwise(GW.1, direction='backward/forward', criterion='BIC')
GLM.2$data
GW.1$data
GW.1$terms
GLM.2$terms
stepwise(GW.1, direction='backward/forward', criterion='BIC')
GLM.2$position
GLM.2$R
GLM.2$xlevels
GW.2$xlevels
GW.1$xlevels
stepwise
source('~/.active-rstudio-document')
stepwise(GW.1, direction='backward/forward', criterion='BIC')
debugSource('~/GIT/RcmdrPlugin.GWRM/R/stepwise.r')
stepwise(GW.1, direction='backward/forward', criterion='BIC')
debugSource('~/GIT/RcmdrPlugin.GWRM/R/stepwise.r')
stepwise(GW.1, direction='backward/forward', criterion='BIC')
source('~/GIT/RcmdrPlugin.GWRM/R/stepwise.r')
source('~/GIT/RcmdrPlugin.GWRM/R/stepwise.r')
stepwise(GW.1, direction='backward/forward', criterion='BIC')
debugSource('~/GIT/RcmdrPlugin.GWRM/R/stepwise.r')
source('~/GIT/RcmdrPlugin.GWRM/R/RcmdrPlugin.GWRM.r')
debugSource('~/GIT/RcmdrPlugin.GWRM/R/stepwise.r')
source('~/GIT/RcmdrPlugin.GWRM/R/RcmdrPlugin.GWRM.r')
debugSource('~/GIT/RcmdrPlugin.GWRM/R/stepwise.r')
source('~/GIT/RcmdrPlugin.GWRM/R/RcmdrPlugin.GWRM.r')
stepwise(GW.1, direction='backward/forward', criterion='BIC')
model.matrix.default
GWRM:::model.matrix.gw()
GWRM:::model.matrix.gw
model.frame(GW.1)
model.frame(GW.1)[1:10,]
model.frame(GLM.2)[1:10,]
GWRM:::model.matrix.gw
n_match <- match("x", names(object), 0L)
n_match <- match("x", names(GW.2), 0L)
n_match <- match("x", names(GW.1), 0L)
n_match <- match("x", names(GLM.2), 0L)
if (n_match <- match("x", names(GLM.2), 0L))
print(1)
data <- model.frame(object, xlev = object$xlevels, ...)
object<-GW.1
data <- model.frame(object, xlev = object$xlevels, ...)
data <- model.frame(object, xlev = object$xlevels)
GWRM:::model.frame.gw()
GWRM:::model.frame.gw
object$formula
object$model
formula<-object
k <- if (criterion == "BIC")
fcall <- formula$call
fcall <- formula$call
m <- match(c("formula", "data", "subset", "weights", "na.action", "offset"), names(fcall), 0L)
fcall <- fcall[c(1L, m)]
fcall$drop.unused.levels <- TRUE
fcall[[1L]] <- quote(stats::model.frame)
fcall$xlev <- formula$xlevels
fcall$formula <- terms(formula)
fcall[names(nargs)] <- nargs
model.frame.gw<-function (formula, ...) {
dots <- list(...)
nargs <- dots[match(c("data", "na.action", "subset"), names(dots), 0)]
if (length(nargs) || is.null(formula$model)) {
fcall <- formula$call
m <- match(c("formula", "data", "subset", "weights", "na.action", "offset"), names(fcall), 0L)
fcall <- fcall[c(1L, m)]
fcall$drop.unused.levels <- TRUE
fcall[[1L]] <- quote(stats::model.frame)
fcall$xlev <- formula$xlevels
fcall$formula <- terms(formula)
fcall[names(nargs)] <- nargs
env <- environment(formula$terms)
if (is.null(env))
env <- parent.frame()
eval(fcall, env, parent.frame())
}
else formula$model
}
model.frame.gw(object)
debugSource('~/GIT/RcmdrPlugin.GWRM/R/stepwise.r')
prueba(GW.1)
model.matrix.gw
GWRM::model.matrix.gw
GWRM:::model.matrix.gw
GWRM:::model.matrix.gw(GW.1)
help(".onAttach")
options()
library(RcmdrPlugin.GWRM)
stepwise(GW.1, direction='backward/forward', criterion='BIC')
stepwise(GW.1, criterion='BIC')
model.frame(GW.1)
model.matrix(GW.1)
library(RcmdrPlugin.GWRM)
Rcmdr:::linearModel
.activeDataSet <- ActiveDataSet()
.activeModel <- ActiveModel()
if (is.null(.activeModel))
return()
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
defaults <- list(initial.fitted = 1, initial.residuals = 1,
initial.rstudent = 1, initial.hatvalues = 1, initial.cookd = 1,
initial.obsNumbers = 1)
dialog.values <- getDialog("addObservationStatistics", defaults)
initializeDialog(title = gettextRcmdr("Add Observation Statistics to Data"))
.variables <- Variables()
obsNumberExists <- is.element("obsNumber", .variables)
activate <- c(checkMethod("fitted", .activeModel, default = TRUE,
reportError = FALSE), checkMethod("residuals", .activeModel,
default = TRUE, reportError = FALSE), checkMethod("rstudent",
.activeModel, reportError = FALSE), checkMethod("hatvalues",
.activeModel, reportError = FALSE), checkMethod("cooks.distance",
.activeModel, reportError = FALSE))
checkBoxes(frame = "selectFrame", boxes = c(c("fitted", "residuals",
"rstudent", "hatvalues", "cookd")[activate], "obsNumbers"),
labels = c(gettextRcmdr(c("Fitted values", "Residuals",
"Studentized residuals", "Hat-values", "Cook's distances"))[activate],
gettextRcmdr("Observation indices")), initialValues = c(dialog.values$initial.fitted,
dialog.values$initial.residuals, dialog.values$initial.rstudent,
dialog.values$initial.hatvalues, dialog.values$initial.cookd,
dialog.values$initial.obsNumbers))
command <- paste(.activeDataSet, "<- within(", .activeDataSet,
", {", sep = "")
onOK <- function() {
closeDialog()
if (activate[1] && tclvalue(fittedVariable) == 1)
command <- paste(command, "\n  ", addVariable("fitted"),
sep = "")
if (activate[2] && tclvalue(residualsVariable) == 1)
command <- paste(command, "\n  ", addVariable("residuals"),
sep = "")
if (activate[3] && tclvalue(rstudentVariable) == 1)
command <- paste(command, "\n  ", addVariable("rstudent"),
sep = "")
if (activate[4] && tclvalue(hatvaluesVariable) == 1)
command <- paste(command, "\n  ", addVariable("hatvalues"),
sep = "")
if (activate[5] && tclvalue(cookdVariable) == 1)
command <- paste(command, "\n  ", addVariable("cooks.distance"),
sep = "")
obsNumbers <- tclvalue(obsNumbersVariable)
putDialog("addObservationStatistics", list(initial.fitted = tclvalue(fittedVariable),
initial.residuals = tclvalue(residualsVariable),
initial.rstudent = tclvalue(rstudentVariable), initial.hatvalues = tclvalue(hatvaluesVariable),
initial.cookd = tclvalue(cookdVariable), initial.obsNumbers = obsNumbers))
.activeDataSet <- ActiveDataSet()
.activeModel <- ActiveModel()
if (is.null(.activeModel))
return()
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
defaults <- list(initial.fitted = 1, initial.residuals = 1,
initial.rstudent = 1, initial.hatvalues = 1, initial.cookd = 1,
initial.obsNumbers = 1)
dialog.values <- getDialog("addObservationStatistics", defaults)
initializeDialog(title = gettextRcmdr("Add Observation Statistics to Data"))
.variables <- Variables()
obsNumberExists <- is.element("obsNumber", .variables)
activate <- c(checkMethod("fitted", .activeModel, default = TRUE,
reportError = FALSE), checkMethod("residuals", .activeModel,
default = TRUE, reportError = FALSE), checkMethod("rstudent",
.activeModel, reportError = FALSE), checkMethod("hatvalues",
.activeModel, reportError = FALSE), checkMethod("cooks.distance",
.activeModel, reportError = FALSE))
.activeDataSet <- ActiveDataSet()
.activeModel <- ActiveModel()
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
defaults <- list(initial.fitted = 1, initial.residuals = 1,
initial.rstudent = 1, initial.hatvalues = 1, initial.cookd = 1,
initial.obsNumbers = 1)
dialog
Ç
}
.activeDataSet <- ActiveDataSet()
.activeModel <- ActiveModel()
if (is.null(.activeModel))
return()
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
activate <- c(checkMethod("fitted", .activeModel, default = TRUE,
reportError = FALSE), checkMethod("residuals", .activeModel,
default = TRUE, reportError = FALSE), checkMethod("rstudent",
.activeModel, reportError = FALSE), checkMethod("hatvalues",
.activeModel, reportError = FALSE), checkMethod("cooks.distance",
.activeModel, reportError = FALSE))
if (activate[1] && tclvalue(fittedVariable) == 1)
command <- paste(command, "\n  ", addVariable("fitted"),
sep = "")
if (activate[2] && tclvalue(residualsVariable) == 1)
command <- paste(command, "\n  ", addVariable("residuals"),
sep = "")
if (activate[3] && tclvalue(rstudentVariable) == 1)
command <- paste(command, "\n  ", addVariable("rstudent"),
sep = "")
if (activate[4] && tclvalue(hatvaluesVariable) == 1)
command <- paste(command, "\n  ", addVariable("hatvalues"),
sep = "")
if (activate[5] && tclvalue(cookdVariable) == 1)
command <- paste(command, "\n  ", addVariable("cooks.distance"),
sep = "")
command
tclvalue(rstudentVariable)
obsNumbers <- tclvalue(obsNumbersVariable)
putDialog("addObservationStatistics", list(initial.fitted = tclvalue(fittedVariable),
initial.residuals = tclvalue(residualsVariable),
initial.rstudent = tclvalue(rstudentVariable), initial.hatvalues = tclvalue(hatvaluesVariable),
initial.cookd = tclvalue(cookdVariable), initial.obsNumbers = obsNumbers))
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
.activeDataSet <- ActiveDataSet()
.activeModel <- ActiveModel()
if (is.null(.activeModel))
return()
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
defaults <- list(initial.fitted = 1, initial.residuals = 1,
initial.rstudent = 1, initial.hatvalues = 1, initial.cookd = 1,
initial.obsNumbers = 1)
dialog.values <- getDialog("addObservationStatistics", defaults)
initializeDialog(title = gettextRcmdr("Add Observation Statistics to Data"))
.variables <- Variables()
obsNumberExists <- is.element("obsNumber", .variables)
activate <- c(checkMethod("fitted", .activeModel, default = TRUE,
reportError = FALSE), checkMethod("residuals", .activeModel,
default = TRUE, reportError = FALSE), checkMethod("rstudent",
.activeModel, reportError = FALSE), checkMethod("hatvalues",
.activeModel, reportError = FALSE), checkMethod("cooks.distance",
.activeModel, reportError = FALSE))
checkBoxes(frame = "selectFrame", boxes = c(c("fitted", "residuals",
"rstudent", "hatvalues", "cookd")[activate], "obsNumbers"),
labels = c(gettextRcmdr(c("Fitted values", "Residuals",
"Studentized residuals", "Hat-values", "Cook's distances"))[activate],
gettextRcmdr("Observation indices")), initialValues = c(dialog.values$initial.fitted,
dialog.values$initial.residuals, dialog.values$initial.rstudent,
dialog.values$initial.hatvalues, dialog.values$initial.cookd,
dialog.values$initial.obsNumbers))
command <- paste(.activeDataSet, "<- within(", .activeDataSet,
", {", sep = "")
onOK <- function() {
closeDialog()
if (activate[1] && tclvalue(fittedVariable) == 1)
command <- paste(command, "\n  ", addVariable("fitted"),
sep = "")
if (activate[2] && tclvalue(residualsVariable) == 1)
command <- paste(command, "\n  ", addVariable("residuals"),
sep = "")
if (activate[3] && tclvalue(rstudentVariable) == 1)
command <- paste(command, "\n  ", addVariable("rstudent"),
sep = "")
if (activate[4] && tclvalue(hatvaluesVariable) == 1)
command <- paste(command, "\n  ", addVariable("hatvalues"),
sep = "")
if (activate[5] && tclvalue(cookdVariable) == 1)
command <- paste(command, "\n  ", addVariable("cooks.distance"),
sep = "")
obsNumbers <- tclvalue(obsNumbersVariable)
putDialog("addObservationStatistics", list(initial.fitted = tclvalue(fittedVariable),
initial.residuals = tclvalue(residualsVariable),
initial.rstudent = tclvalue(rstudentVariable), initial.hatvalues = tclvalue(hatvaluesVariable),
initial.cookd = tclvalue(cookdVariable), initial.obsNumbers = obsNumbers))
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
View(goals)
residuals(GW.2)
library("RcmdrPlugin.GWRM", lib.loc="~/R/win-library/3.2")
stepwise
stepAIC
help(residuals)
help(residuals.gw)
library(RcmdrPlugin.GWRM)
library(RcmdrPlugin.GWRM)
library("RcmdrPlugin.GWRM", lib.loc="~/R/win-library/3.2")
addObservationStatistics
Rcmdr::addObservationStatistics
Rcmdr:::addObservationStatistics
library("RcmdrMisc", lib.loc="~/R/win-library/3.2")
library("RcmdrPlugin.GWRM", lib.loc="~/R/win-library/3.2")
source('~/GIT/RcmdrPlugin.GWRM/R/prueba.r')
debugSource('~/GIT/RcmdrPlugin.GWRM/R/prueba.r')
debugSource('~/GIT/RcmdrPlugin.GWRM/R/prueba.r')
debugSource('~/GIT/RcmdrPlugin.GWRM/R/prueba.r')
library("RcmdrPlugin.GWRM", lib.loc="~/R/win-library/3.2")
detach("package:RcmdrPlugin.GWRM", unload=TRUE)
Commander()
detach("package:RcmdrPlugin.GWRM", unload=TRUE)
detach("package:RcmdrMisc", unload=TRUE)
detach("package:Rcmdr", unload=TRUE)
library("Rcmdr", lib.loc="~/R/win-library/3.2")
detach("package:RcmdrPlugin.GWRM", unload=TRUE)
library("Rcmdr", lib.loc="~/R/win-library/3.2")
RcmdrTclSet()
RcmdrTclSet
activateMenus()
activateMenus
getRcmdr("suppress.menus")
getRcmdr("Menus")
kk=getRcmdr("Menus")
kk[1:10]
kk[11:30]
kk$summariesMenu
typeof(kk)
kk[17]
kk[[17]]
kk[[17]]$ID
kk[[17]]$position
kk[[17]]$position[1]
names(kk[[17]]$position)
names(kk[[17]]$position)(variablesMenu)
names(kk[[17]]$position)[variablesMenu]
kk[[17]]$position[variablesMenu]
kk[[17]]$position["variablesMenu"]
kk[[17]]$position$variablesMenu
kk[[17]]$position
kk[[17]]$position.variablesMenu
kk[[17]]$position
tipeof(kk[[17]]$position)
typeof(kk[[17]]$position)
kk
subset(kk)
select.list()
CommanderWindow()$ID
Commander()
CommanderWindow()$ID
CommanderWindow()$ID
CommanderWindow()$ID
CommanderWindow()$ID
CommanderWindow()$ID
as.numeric(c(tclvalue(.Tcl(paste("winfo rootx", ID))),
tclvalue(.Tcl(paste("winfo rooty", ID)))))
tclvalue(.Tcl(paste("winfo rootx", ID)
)
)
activateMenus()
getRcmdr("suppress.menus")
gettextMenus()
Rcmdr:::gettextMenus()
parent
getRcmdr("Menus")
getRcmdr("Menus")[[106]]
aactiveDataSetP()
activeDataSetP()
getRcmdr("Menus")[[106]]$activation
getRcmdr("Menus")[[106]]$activation<-function() return FALSE
getRcmdr("Menus")[[106]]$activation<-function(){ return FALSE}
getRcmdr("Menus")[[106]]$activation<-function(){ FALSE)}
getRcmdr("Menus")[[106]]$activation<-function(){ FALSE}
getRcmdr("Menus")[[106]]$activation<-function() FALSE
false
FALSE
getRcmdr("Menus")[[106]]$activation<-function() return (FALSE)
kk<-function(){return (FALSE)}
getRcmdr("Menus")[[106]]$activation= function() kk()
getRcmdr("Menus")[[106]]$activation
typeof(getRcmdr("Menus")[[106]]$activation)
for (item in getRcmdr("Menus")){item}
item
item$activation()
item$activation()
item$activation=FALSE
item$activation=function() return FALSE
item$activation=function() return (FALSE)
.Tcl(.1.1.9)
.Tcl(".1.1.9")
.Tcl(".1.1.9 entryconfigure ")
.Tcl(".1.1.9 entryconfigure 6 -state disable")
.Tcl(".1.1.9 entryconfigure 6 -state disable")$
q
.Tcl(".1.1.9 entryconfigure 6 -state disable")
.Tcl(".1.1.9 entryconfigure 6 -state normal")
help(.Tcl)
activateMenus()
help(.Tcl)
.Tcl(".1.1.9 entryconfigure 6 -state normal")
.Tcl(".1.1.9 entryconfigure 6 -state disable")
help(.Tcl)
activateMenus()
.Tcl(".1.1.9 entryconfigure 6 -state disable")
savehistory("~/GIT/RcmdrPlugin.GWRM/namespace.r")
