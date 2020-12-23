library(tidyr)
library(dplyr)
library(plyr)
library(reshape)
library(ggplot2)

setwd(".")

############################
# Read the dataCollapsed
############################

dataCollapsed = read.csv("./dataCollapsed.csv", stringsAsFactors = TRUE)
# dataCollapsed = na.omit(dataCollapsed)
data = read.csv("./data.csv", stringsAsFactors = TRUE)

############################
# Reshape the dataCollapsed
############################

##### RQ1

venues <- data %>% 
  gather("venue", "value", c("conference", "journal", "workshop", "bookChapter"), na.rm = TRUE, convert = TRUE) %>% 
  dplyr::filter(grepl("x",value)) %>% dplyr::rename(ID = X) %>% select(ID, venue) 
venues$venue <- as.factor(venues$venue)

researchTypes <- data %>% 
  gather("researchType", "value", c("validation", "evaluation", "solutionProposal", "experience"), na.rm = FALSE, convert = TRUE) %>% 
  dplyr::filter(grepl("x",value)) %>% dplyr::rename(ID = X) %>% select(ID, researchType)
researchTypes$researchType <- as.factor(researchTypes$researchType)

contributionTypes <- data %>% 
  gather("contributionType", "value", c("model", "method", "metric", "tool", "openItems"), na.rm = FALSE, convert = TRUE) %>% 
  dplyr::filter(grepl("x",value)) %>% dplyr::rename(ID = X) %>% select(ID, contributionType)
contributionTypes$contributionType <- as.factor(contributionTypes$contributionType)

##### RQ2

robotTypesApproach <- dataCollapsed %>% 
  gather("robotTypeApproach", "value", c("terrestrialApproach", "aerialApproach", "spatialApproach", "genericApproach"), na.rm = FALSE, convert = TRUE) %>% 
  dplyr::filter(grepl("x",value)) %>% select(ID, robotTypeApproach)
robotTypesApproach$robotTypeApproach <- as.factor(robotTypesApproach$robotTypeApproach)

multiRobotBoolApproach <- ifelse(dataCollapsed$multiRobotApproach=="x",TRUE,FALSE)
multiRobotApproach <- cbind(dataCollapsed, multiRobotBoolApproach) %>% select(ID, multiRobotBoolApproach)
colnames(multiRobotApproach)[colnames(multiRobotApproach)=="multiRobotBoolApproach"] <- "multiRobotApproach"

years <- dataCollapsed %>% select(ID, year)

##### RQ3

languages <- dataCollapsed %>% 
  gather("language", "value", c("dsl", "uml"), na.rm = FALSE, convert = TRUE) %>% 
  dplyr::filter(grepl("x",value)) %>% select(ID, language)
languages$language <- as.factor(languages$language)

automationTypes <- dataCollapsed %>% 
  gather("automation", "value", c("transformation", "codeGeneration", "modelsRuntime", "analysis"), na.rm = FALSE, convert = TRUE) %>% 
  dplyr::filter(grepl("x",value)) %>% select(ID, automation)
automationTypes$automation <- as.factor(automationTypes$automation)

aspects <- dataCollapsed %>% 
  gather("aspect", "value", c("behaviour", "navigation", "structure"), na.rm = FALSE, convert = TRUE) %>% 
  dplyr::filter(grepl("x",value)) %>% select(ID, aspect)
aspects$aspect <- as.factor(aspects$aspect)

##### RQ4.1

validationTypes <- dataCollapsed %>% 
  gather("validationType", "value", c("experiment", "caseStudy", "example", "realProject", "simulation"), na.rm = FALSE, convert = TRUE) %>% 
  dplyr::filter(grepl("x",value)) %>% select(ID, validationType)
validationTypes$validationType <- as.factor(validationTypes$validationType)

##### RQ4.2

robotTypesEval <- dataCollapsed %>% 
  gather("robotTypeEval", "value", c("terrestrialEval", "aerialEval", "aquaticEval", "spatialEval", "genericEval"), na.rm = FALSE, convert = TRUE) %>% 
  dplyr::filter(grepl("x",value)) %>% select(ID, robotTypeEval)
robotTypesEval$robotTypeEval <- as.factor(robotTypesEval$robotTypeEval)

multiRobotBoolEval <- ifelse(dataCollapsed$multiRobotEval=="x",TRUE,FALSE)
multiRobotEval <- cbind(dataCollapsed, multiRobotBoolEval) %>% select(ID, multiRobotBoolEval)
colnames(multiRobotEval)[colnames(multiRobotEval)=="multiRobotBoolEval"] <- "multiRobotEval"

############################
# It's plotting time!
############################

plot <- function(var, fileName, labels, width, height, leftMargin) {

  filePath <- paste("./output/", fileName, ".pdf", sep="")
  pdf(filePath, width=width, height=height)
  
  par(mar=c(3, leftMargin, 1, 1))
  par(mfrow=c(1, 1))
  par(las=1)
  
  var <- as.factor(var)
  levels(var) <- labels
    
  dataToPlot <- table(var)
  dataToPlot <- dataToPlot[order(dataToPlot, decreasing=FALSE)]

  plot <- barplot(dataToPlot, main="", cex.main=1, xlim=c(0, nrow(data) + 5), cex=1.5, cex.names=1.5, las=1, horiz=TRUE)
  text(x=as.numeric(dataToPlot) + 3, y = plot, label = dataToPlot, cex = 1.5, col = "black")
  
  dev.off()
}  

# RQ1
plot(venues$venue, "RQ1_venues", c("Book chapter", "Conference", "Journal", "Workshop"), 10, 5, 13)
plot(researchTypes$researchType, "RQ1_researchTypes", c( "Evaluation research", "Experience paper", "Solution proposal", "Validation research"), 10, 5, 13)
plot(contributionTypes$contributionType, "RQ1_contributionTypes", c("Method", "Metric", "Model", "Open items", "Tool"), 10, 5, 13)

# RQ2
plot(robotTypesApproach$robotTypeApproach, "RQ2_robotTypesApproach", c("Aerial robots", "Generic robots", "Space robots", "Terrestrial robots", "Aquatic robots"), 10, 5, 13)
plot(multiRobotApproach$multiRobotApproach, "RQ2_multiRobotApproach", c("Single robot", "Multiple robots"), 10, 5, 13)

# RQ3
umlDialects <- dataCollapsed$umlDialect[which(dataCollapsed$umlDialect != "")]
umlDialects <- droplevels(umlDialects)

plot(languages$language, "RQ3_1_languages", c("DSL", "UML"), 10, 5, 13)
plot(umlDialects, "RQ3_1_umlDialects", levels(umlDialects), 10, 5, 13)
plot(automationTypes$automation, "RQ3_1_automationTypes", c("Analysis", "Code generation", "Models at runtime", "Transformation"), 10, 5, 13)
plot(aspects$aspect, "RQ3_2_aspects", c("Behaviour", "Navigation", "Structure"), 10, 5, 13)

# RQ4.1
plot(validationTypes$validationType, "RQ4_1_validationTypes", c( "Case study", "Example", "Experiment", "Real project", "Simulation"), 10, 5, 13)

# RQ4.2
plot(robotTypesEval$robotTypeEval, "RQ4_2_robotTypesEval", c("Aerial robots", "Aquatic robots", "Generic robots", "Space robots", "Terrestrial robots"), 10, 5, 13)
plot(multiRobotEval$multiRobotEval, "RQ4_2_multiRobotEval", c("Single robot", "Multiple robots"), 10, 5, 13)

# RQ5
lwb <- dataCollapsed$languageWorkbench[which(dataCollapsed$languageWorkbench != "")]
lwb <- droplevels(lwb)

plot(lwb, "RQ5_lwbs", levels(lwb), 10, 5, 13)

# Horizontal analysis

pdf("./output/horizontals.pdf", width=6, height=4)
par(mar=c(5, 5, 5, 5))
par(mfrow=c(1, 1))
par(las=1)

plotBubbles <- function(var1, var2, var1Label, var2Label, var1Labels, var2Labels, plotTitle) {
    
  currentData <- merge(var1, var2, by="ID", all = T)
  currentData <- na.omit(currentData)
  var1Name <- names(currentData)[2]
  var2Name <- names(currentData)[3]
  counts <- count(currentData, c(var1Name, var2Name))
  # print(counts)
  
  if(var1Label == "" && var2Label == "") {
    var1Label = var1Name
    var2Label = var2Name
  }
  if(plotTitle != "") {
    plotTitle = paste(var1Name, "___", var2Name)
  }
  
  plot <- ggplot(counts, aes(counts[,2], counts[,1], color=freq,  alpha = 0.7)) + geom_point(aes(size = freq)) +
    theme_bw() +
    scale_color_gradient2(low="white", high="gray13", name="freq") +
    scale_size_continuous(range=c(0, 30)) + geom_text(aes(label = freq), color="black") +
    theme(legend.position = "none") +
    # labs(x=var2Name, y=var1Name) +
    # ggtitle(paste(var1Name, "___", var2Name))
    labs(x=var2Label, y=var1Label) +
    ggtitle(plotTitle)
  
  if(var1Labels != "" && var2Labels != "") {
   plot <- plot + scale_y_discrete(labels= var1Labels) 
   plot <- plot + scale_x_discrete(labels= var2Labels) 
  }
  
  print(plot)
}

plotBubbles(contributionTypes, venues, "", "", "", "", "")
plotBubbles(researchTypes, venues, "", "", "", "", "")
plotBubbles(robotTypesEval, venues, "", "", "", "", "")
plotBubbles(multiRobotEval, venues, "", "", "", "", "")
plotBubbles(multiRobotApproach, venues, "", "", "", "", "")
plotBubbles(languages, venues, "", "", "", "", "")
plotBubbles(automationTypes, venues, "", "", "", "", "")
plotBubbles(aspects, venues, "", "", "", "", "")

plotBubbles(researchTypes, contributionTypes, "Research type (RQ1)", "Contribution type (RQ1)", c( "Evaluation research", "Experience paper", "Solution proposal", "Validation research"), c("Method", "Metric", "Model", "Open items", "Tool"), "")
plotBubbles(robotTypesEval, contributionTypes, "", "", "", "", "")
plotBubbles(multiRobotEval, contributionTypes, "", "", "", "", "")
plotBubbles(languages, contributionTypes, "Type of modelling language (RQ3)", "Contribution type (RQ1)", c("DSL", "UML"), c("Method", "Metric", "Model", "Open items", "Tool"), "")
plotBubbles(automationTypes, contributionTypes, "", "", "", "", "")
plotBubbles(aspects, contributionTypes, "", "", "", "", "")

plotBubbles(robotTypesEval, researchTypes, "", "", "", "", "")
plotBubbles(multiRobotEval, researchTypes, "", "", "", "", "")
plotBubbles(languages, researchTypes, "", "", "", "", "")
plotBubbles(automationTypes, researchTypes, "", "", "", "", "")
plotBubbles(aspects, researchTypes, "", "", "", "", "")

plotBubbles(multiRobotEval, robotTypesEval, "Cardinality of robots (RQ4)", "Robot type (RQ4)", c("Single robot", "Multiple robots"), c("Aerial robots", "Aquatic robots", "Generic robots", "Space robots", "Terrestrial robots"), "")
plotBubbles(multiRobotApproach, robotTypesApproach, "Cardinality of robots (RQ4)", "Robot type (RQ4)", c("Single robot", "Multiple robots"), c("Aerial robots", "Generic robots", "Space robots", "Terrestrial robots"), "")
plotBubbles(languages, robotTypesEval, "", "", "", "", "")
plotBubbles(automationTypes, robotTypesEval, "", "", "", "", "")
plotBubbles(aspects, robotTypesEval, "", "", "", "", "")

plotBubbles(multiRobotApproach, robotTypesApproach, "", "", "", "", "")
plotBubbles(languages, robotTypesApproach, "", "", "", "", "")
plotBubbles(automationTypes, robotTypesApproach, "", "", "", "", "")
plotBubbles(aspects, robotTypesApproach, "", "", "", "", "")

plotBubbles(languages, multiRobotApproach, "", "", "", "", "")
plotBubbles(automationTypes, multiRobotApproach, "", "", "", "", "")
plotBubbles(aspects, multiRobotApproach, "", "", "", "", "")
plotBubbles(years, multiRobotApproach, "", "", "", "", "")

plotBubbles(automationTypes, languages, "", "", "", "", "")
plotBubbles(aspects, languages, "Engineered aspect (RQ3)", "Modelling language (RQ3)", c("Behaviour", "Navigation", "Structure"), c("DSL", "UML"), "")

plotBubbles(aspects, automationTypes, "Engineered aspect (RQ3)", "Automation (RQ3)", c("Behaviour", "Navigation", "Structure"), c("Analysis", "Code generation", "Models at runtime", "Transformation"), "")

dev.off()
  