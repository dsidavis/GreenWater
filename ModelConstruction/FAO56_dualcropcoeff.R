#TO-DO:
  #1.  Re-run all 30% and 80% allowable depletion scenarios because of correction of KsCalc function.
  #2.  Correct Ei and Ep functions such that TEW cannot be exceeded by Dei or Dep, respectively, in the upper layer [DONE].  
  #3.  Re-run all almond and walnut scenarios, since model codes were changed when tomatoes, etc. were added to the mix [DONE]
  #5.  Modify winter Kcb calculation for alfalfa: allow for fall growth in intermountain region before frost termination; allow for higher peak values in Fall then decline to 0.8 before rising again in Feb [DONE]
  #6.  Modify Fc equation for alfalfa, so that it is directly based off of Kcb values [DONE]
  #7.  Allow for winter residual "mulch" in intermountain alfalfa production (for every 10% effective surface coverage results in 5% reduction in TEW)
  #8.  Modify irrigation decision so that irrigations can't occur before 2/?? in Central Valley, even with drought stress but ensure this doesn't adversely affect results calculations that depend on definitions of Jdev [DONE]
  #9. Add alfalfa zone to model scaffold [DONE]
  #10. Add grape zone to model scaffold [DONE]
#modified KeiCalc and KepCalc on 9/11/17 to correct for overestimation of evaporation from sandy soils with very low TEW (i.e. <12 mm)
  #11. Simple tests or monte-Carlo simulation of effect of following effects (1) climatic adjustment of Kcb values (2) bloom and leaf-drop assumptions (3) Kcb values themselves
  #12. Test effect of varying alfalfa height on Kcb calcs
#changed order of DPei and DPep on 9/11/17  
# changed order of Ir and Ks calculation on 8/23/17
# changed Ir decision function on 8/23/17 to accomodate different irrigation decisions for wine grapes
# concept for wine irrigation decisions is to set a min Ks threshold and irrigate to allowable depletion when that threshold is crossed, as opposed to irrigating to field capacity when allowable depletion is crossed
#script to implement the FAO56 dual crop coefficient ET routine
#modelscaffoldDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/model_scaffold/run_model/Oct2017' #location of input data; copied from Sep2017 on 10/18/17
#resultsDir <- 'C:/Users/smdevine/Desktop/Allowable_Depletion/results/Oct2017'

modelscaffoldDir = "Data/Oct2017"
resultsDir  = "Results"
resultsDir = file.path(modelscaffoldDir, resultsDir)
rounding_digits <- 3
scenarios.to.run <- 1000

odir = getwd()
setwd(modelscaffoldDir)

if(FALSE) {
irrigation.parameters <- read.csv('irrigation_parameters.csv', stringsAsFactors = F)
crop.parameters.df <- read.csv('crop_parameters.csv', stringsAsFactors = F) #necessary parameters by crop to run the FAO56 dual crop coefficient ET model
P.df <- read.csv('PRISM.precip.data.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of precip from 10/1/2003-6/25/17 from 'free' daily PRISM 4km resolution for cells of interest in California, created in download_PRISM.R script (from 6/26/17 download); blanks checked for in 'data_QA_QC.R'
U2.df <- read.csv('SpatialCIMIS.U2.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of wind data from download of spatial CIMIS data, created in spatialCIMIS.R script.  No missing data except for cell 148533
RHmin.df <- read.csv('SpatialCIMIS.RHmin.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of minimum relative humidity, estimated from download of spatial CIMIS Tdew and Tmax data, created in spatialCIMIS.R script.  Blanks filled on "12_08_2011" in data_QA_QC.R.  Now, no missing data except for cell 148533
ETo.df <- read.csv('SpatialCIMIS.ETo.updated9.13.17.csv', stringsAsFactors = F) #this is a daily summary of reference ET from download of spatial CIMIS data, created in spatialCIMIS.R script.  Blanks filled on multiple days in data_QA_QC.R.  Now, no missing data except for cell 148533
model.scaffold <- read.csv('model_scaffold_majcomps.v2.csv', stringsAsFactors = F)
cropscape_legend <- read.csv('cropscape_legend.txt', stringsAsFactors = FALSE)


setwd(odir)

#temporary pruning of the model scaffold for Matt and Duncan
almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
model.scaffold <- model.scaffold[which(model.scaffold$crop_code==almond_code), ]
model.scaffold <- model.scaffold[order(model.scaffold$unique_model_code), ]
model.scaffold <- model.scaffold[1:scenarios.to.run, ]
model.scaffold$longitude_AEA <- NULL
model.scaffold$latitude_AEA <- NULL
model.scaffold$grape.zone[which(model.scaffold$grape.zone=='Sonoran Basin and Range')] <- 'Central California Valley' #this zone will be treated like 'Central California Valley,' that is, assumed table grape, raisin, or low-quality wine grape production

# sum(model.scaffold$crop_code==grape_code) #80312
# sum(model.scaffold$grape.zone=='Central California Valley' | model.scaffold$grape.zone=='Central California Foothills and Coastal Mountains', na.rm=TRUE) #79767
# dim(model.scaffold) #387970 rows

#now merge SpCIMIS data updates and write to disk when necessary
# setwd(file.path(modelscaffoldDir, 'SpCIMIS'))
# U2.update <- read.csv('SpatialCIMIS.U2update.rounded.csv')
# ETo.update <- read.csv('SpatialCIMIS.EToupdate.rounded.csv')
# RHmin.update <- read.csv('SpatialCIMIS.minRHupdate.rounded.csv', stringsAsFactors = F)
# setwd(modelscaffoldDir)
# P.update <- read.csv('PRISM_precip_data_update.csv', stringsAsFactors = FALSE)
# dim(U2.update)
# dim(U2.df)
# U2.df <- rbind(U2.df, U2.update)
# dim(U2.df)
# dim(ETo.df)
# dim(ETo.update)
# ETo.df <- rbind(ETo.df, ETo.update)
# dim(RHmin.df)
# dim(RHmin.update)
# RHmin.df <- rbind(RHmin.df, RHmin.update)
# dim(P.df)
# P.df[nrow(P.df),1:20] #last date was 6/25/17 from June 26 2017 download
# P.df <- P.df[-(which(P.df$dates=='12_01_2016'):nrow(P.df)), ]
# dim(P.df)
# P.df[nrow(P.df),1:20]
# P.df <- rbind(P.df, P.update)
# write.csv(P.df, 'PRISM.precip.data.updated9.13.17.csv', row.names=FALSE)
# write.csv(U2.df, 'SpatialCIMIS.U2.updated9.13.17.csv', row.names=FALSE)
# write.csv(ETo.df, 'SpatialCIMIS.ETo.updated9.13.17.csv', row.names=FALSE)
# write.csv(RHmin.df, 'SpatialCIMIS.RHmin.updated9.13.17.csv', row.names=FALSE)

#define functions implement FA56 dual crop coefficients
#includes subroutine that separates evaporable water as P vs. Irr sourced
#some results abbreviations
#E=evaporation
#T=transpiration
#ET=evapotranspriation
#Irr.1=first irrigation of year
#Irr.Last=last irrigation of year
#GW.ET.to.Irr1=ET sourced from green water from flowering to first irrigation (should subtract previous year's carryover irrigation storage)
#ET.growing=total ET from flowering (Jdev) to leaf-drop (Jharv)
#ET.WY=total annual ET on water-year basis
#deep.perc is annual deep percolation ()
#GW.capture.net is net change in soil root zone depletion from Jharv (leaf drop) to Jdev (flowering and development)
#end.season.Dr is soil root zone depletion at Jharv (leaf drop)

#temporary arguments for working inside the function when necessary
# cropname <- 'alfalfa.intermountain'
# cropcode <- alfalfa_code
# AD.percentage <- 30
# root_depth <- '1.0m'
# irr.type <- 'Border'
# results_file <- 'new'
# row_start <- 1
# RDI.min <- NA
# alfalfa.zone <- 'Intermountain'
# alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa'] #75380 total
# grape_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
# almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
# walnut_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']
# pistachio_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Pistachios']

}

FAO56DualCropCalc <- function(cropname, cropcode, AD.percentage, root_depth, irr.type, crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file, row_start, RDI.min, alfalfa.zone, grape.zone) {
  alfalfa_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Alfalfa']
  grape_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Grapes']
  almond_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Almonds']
  walnut_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Walnuts']
  pistachio_code <- cropscape_legend$VALUE[cropscape_legend$CLASS_NAME=='Pistachios']

#browser()  
  #do.call(rbind, lapply(split(model.result, model.result$water.year), GreenWaterCaptureCalc))
  if (length(U2.df$DOY)==length(RHmin.df$DOY) & length(U2.df$DOY)==length(ETo.df$DOY)) {
    doys.model <- U2.df$DOY
    dates <- as.Date(U2.df$dates, format='%m_%d_%Y')
    days <- U2.df$day
    months <- U2.df$month
    years <- U2.df$year
    water.year <- years
    water.year[which(months >= 10)] <- years[which(months >= 10)] + 1
    print('Temporal coverages match in Spatial CIMIS.')
  } else {
    print('There are differing temporal coverages in the Spatial CIMIS data.')
  }
  model.length <- nrow(ETo.df)
  #get precip and spatial CIMIS data to same temporal end point
  last.date <- ETo.df$dates[nrow(ETo.df)]
  P.df <- P.df[1:which(P.df$dates==last.date), ]
  cropname.dir <-  paste0(cropname, '_majcomps')
  scenario.name <- if (cropname == 'grapes.wine') { #this was modified after the fact so that AD or RDI.min precedes root depth
    paste0(cropname.dir, '/scenario_', root_depth, as.character(RDI.min), 'RDI.min')} else {
        paste0(cropname.dir, '/scenario_', root_depth, as.character(AD.percentage), 'AD')
      } #need to add irr.type here
  if (cropname=='alfalfa.intermountain' | cropname=='alfalfa.CV' | cropname == 'alfalfa.imperial') {
    paw.var <- paste0('z', root_depth, '_cmH2O_unmodified_comp') 
  } else {
      paw.var <- paste0('z', root_depth, '_cmH2O_modified_comp')
  }
  if (!dir.exists(file.path(resultsDir, cropname.dir))) 
    dir.create(file.path(resultsDir, cropname.dir))

  if (!dir.exists(file.path(resultsDir, scenario.name))) 
    dir.create(file.path(resultsDir, scenario.name))

#initialization assumptions
  first_irrigation <- 0
  TEW.fraction <- 0.5
#limit model.scaffold to cropname
  if (cropname=='alfalfa.intermountain' | cropname=='alfalfa.CV' | cropname=='alfalfa.imperial') {
    model.scaffold.crop <- model.scaffold[which(model.scaffold$crop_code==cropcode & model.scaffold$alfalfa.zone==alfalfa.zone), ] #further refine model.scaffold.crop if cropname == alfalfa according to geography
  } else if (cropname=='grapes.table' | cropname=='grapes.wine') {
    model.scaffold.crop <- model.scaffold[grepl(grape.zone, model.scaffold$grape.zone), ] #further refine model.scaffold.crop if cropname == grape according to geography; wine.grapes will be run with 'Central California Foothills and Coastal Mountains'; grapes.table will be run for 'Central California Valley' and 'Sonora Basin and Range', the latter is forced to 'Central California Valley' after reading in model scaffold for simplicity's sake
  } else {
      model.scaffold.crop <- model.scaffold[which(model.scaffold$crop_code==cropcode), ] #80,401 unique crop, soil, and climate combinations for almond (spatially, this is the equivalent of only 7,236 ha)
  }
#make a results data.frame
  if (results_file == 'new') {
    model.length.yrs <- max(ETo.df$year) - min(ETo.df$year) + 1 #data starts 10/2003
    paw.vector <- model.scaffold.crop[[paw.var]]
  #print(head(paw.vector))
    #mukey	crop_code	PRISMcellnumber	CIMIScellnumber	unique_model_code	full_matrix_rownum	n_compkeys	cokey	compname	comppct_r	majcompflag	TEW	REW	surface.depth	z2.0m_cmH2O_modified_comp	Model.Year
    model.scaffold2 <- model.scaffold.crop[ ,c(-11:-22, -26:-27)] #takes out paw data, grape.zone, and alfalfa.zone from model scaffold for pasting results later
    model.scaffold2$paw <- paw.vector
    colnames(model.scaffold2)[14] <- paw.var
    model.scaffold.results <- model.scaffold2[rep(seq.int(1, nrow(model.scaffold2)), model.length.yrs), 1:ncol(model.scaffold2)] #makes a new data.frame with each row repeated model.length.yrs number of times
    model.scaffold.results <- model.scaffold.results[order(model.scaffold.results$unique_model_code, model.scaffold.results$cokey), ] #for some reason it is produced out of order
    model.scaffold.results <- data.frame(model.scaffold.results, Model.Year=rep(seq(from=min(ETo.df$year), to=max(ETo.df$year), by=1), times=nrow(model.scaffold.crop)), Irr.1=as.Date('1900-01-01'), Irr.Last=as.Date('1900-01-01'), RAW.end.season=NA, PAW.end.season=NA, Dr.end.season=NA, P.end.season=NA, Irr.end.storage=NA, GW.ET.growing=NA, Irr.app.total=NA, Irr.app.last=NA, ET.growing=NA, E.growing=NA, T.growing=NA, H2O.stress=NA, GW.ET.to.Irr1=NA, GW.E.to.Irr1=NA, GW.T.to.Irr1=NA, ET.annual=NA, E.annual=NA, T.annual=NA, deep.perc.annual=NA, winter.deep.perc=NA, post.Irr1.deep.perc=NA, fall.deep.perc=NA, GW.capture.net=NA)
    model.scaffold.results$unique_model_code_final <- paste0(as.character(model.scaffold.results$unique_model_code), as.character(model.scaffold.results$cokey)) #need to use as.character to preserve integrity of long integers
#model.scaffold.results[which(model.scaffold.results$unique_model_code==100058 & model.scaffold.results$cokey==13094564), ]
    rm(model.scaffold2)
  } else {
    setwd(file.path(resultsDir, scenario.name))
    #fname <- list.files(pattern = glob2rx('*_FAO56results.csv'))
    #model.scaffold.results <- read.csv(fname, stringsAsFactors = FALSE)
    model.scaffold.results <- read.csv(results_file, stringsAsFactors = FALSE)
    model.scaffold.results$Irr.1 <- as.Date(model.scaffold.results$Irr.1)
    model.scaffold.results$Irr.Last <- as.Date(model.scaffold.results$Irr.Last)
  }
  crop.parameters <- CropParametersDefine(crop.parameters.df, cropname)
  Kcb.std <- KcbDefine(doys.model, cropname, crop.parameters) #this will be substituted with a crop code
  fc <- fcCalc(doys.model, crop.parameters, cropname) #TO-DO: implement alternative fc calculation in accordance with Eq. 11 from Allen et al. 2005: ((Kcb-Kcmin)/(Kcmax-Kcmin))^(1+0.5*h).  However, this produced a strange result in spreadsheet model for almonds, where increasing h decreases fc.
  if (cropname == 'alfalfa.intermountain' | cropname == 'alfalfa.CV' | cropname == 'alfalfa.imperial') {
    harvest.days <- HarvestDays(cropname)
  }
  if (cropname=='alfalfa.intermountain') {
    Jdev <- crop.parameters$Jini[which(crop.parameters$crop==cropname)] #Jdev is intended to represent start of growing season as DOY
  } else if (cropname=='alfalfa.CV' | cropname == 'alfalfa.imperial') {
    Jdev <- 1
  } else {
      Jdev <- crop.parameters$Jdev[which(crop.parameters$crop==cropname)]
  }
  Jmid <- crop.parameters$Jmid[which(crop.parameters$crop==cropname)]
  Jlate <- crop.parameters$Jlate[which(crop.parameters$crop==cropname)]
  if (cropname=='alfalfa.intermountain') {
    Jharv <- harvest.days[length(harvest.days)] + crop.parameters$Lfrost.kill[which(crop.parameters$crop==cropname)] #Jharv is intended to represent end of growing season as DOY where irrigation is no longer applicable
  } else if (cropname == 'alfalfa.imperial' | cropname=='alfalfa.CV') {
    Jharv <- 365 #what about leap years?
  } else {
      Jharv <- crop.parameters$Jharv[which(crop.parameters$crop==cropname)]
  }
  fw <- fwSelect(irrigation.parameters, irr.type)
  fewi <- fewiCalc(fc, fw)
  fewp <- fewpCalc(fc, fewi)
#loop through all rows of model scaffold but only do these operations once for each model.scaffold.crop row
  set.seed(461980)
  rows.to.sample <- sample(1:nrow(model.scaffold.crop), 0.005*nrow(model.scaffold.crop))
  if (nrow(model.scaffold.crop) > 10000) {
    save.times <- seq(from=10000, to=nrow(model.scaffold.crop), by=10000)
  } else {save.times <- NA}

tm1 = proc.time()  
  for (n in row_start:nrow(model.scaffold.crop)) {
    model.code <- model.scaffold.crop$unique_model_code[n]
    PAW <- model.scaffold.crop[[paw.var]][n]*10
    AD <- (AD.percentage/100)*PAW
    cokey <- model.scaffold.crop$cokey[n]
    REW.parameter <- model.scaffold.crop$REW[n]
    TEW.parameter <- model.scaffold.crop$TEW[n]
    if (is.na(AD) | is.na(REW.parameter) | is.na(TEW.parameter)) {
      next(print(paste('Soils data is missing for cokey ', as.character(cokey)))) #TO-DO: write this result to separate file of NAs
    }
    if (AD==0 | TEW.parameter==0 | REW.parameter==0) {
      print(paste('AD, TEW, or REW is 0 for cokey ', as.character(cokey)))
      next
    }
    if (REW.parameter > TEW.parameter) { #there are several hundered instances where this was the result of the SSURGO aggregation work
      REW.parameter <- TEW.parameter
    }
  #identify climatic and soil parameters
    spCIMIScell <- model.scaffold.crop$CIMIScellnumber[n]
    PRISMcell <- model.scaffold.crop$PRISMcellnumber[n]
    comppctr <- model.scaffold.crop$comppct_r[n]
    mukey <- model.scaffold.crop$mukey[n]
    P <- P.df[ , which(colnames(P.df)==paste0('cell_', as.character(PRISMcell)))]
    ETo <- ETo.df[ , which(colnames(ETo.df)==paste0('cell_', as.character(spCIMIScell)))]
    U2 <- U2.df[ ,which(colnames(U2.df)==paste0('cell_', as.character(spCIMIScell)))]
    RHmin <- RHmin.df[ ,which(colnames(RHmin.df)==paste0('cell_', as.character(spCIMIScell)))]
    if (all(is.na(P)) | all(is.na(ETo))) {
      print(paste('Climate data is missing for scenario number', as.character(n))) #TO-DO: write this result to separate file of NAs
      next
    }
    Kcb.df <- KcbAdj(Kcb.std, crop.parameters=crop.parameters, cropname, U2, RHmin) #object 'crop.parameters' not found
    Kcb.adjusted <- Kcb.df$Kcb.climate.adj
    days.no.irr <- DaysNoIrr(P, ETo, Kcb.adjusted, AD, doys.model, years, Jmid, Jharv, cropname = cropname)
    if (is.null(days.no.irr)) {
      print(paste0('Null value returned for days.no.irr for model.scaffold.crop n = ', as.character(n), ', likely a result of a low PAW: ', as.character(PAW), ' mm.'))
      next
  }

#print(proc.time() - tm1)
    
    Kcmax <- Kcb.df$Kc.max
    Dei.initial <- numeric(length = model.length)
    DPei <- numeric(length = model.length)
    Dep.initial <- numeric(length = model.length)
    Kri <- numeric(length = model.length)
    Kei <- numeric(length = model.length)
    Ei <- numeric(length = model.length)
    Dei.end <- numeric(length = model.length)
    Krp <- numeric(length = model.length)
    Kep <- numeric(length = model.length)
    Ep <- numeric(length = model.length)
    DPep <- numeric(length = model.length)
    Dep.end <- numeric(length = model.length)
    W <- numeric(length = model.length)
    Kc.ns <- numeric(length = model.length)
    ETc.ns <- numeric(length = model.length)
    Dr.initial <- numeric(length = model.length)
#assumes depletion is zero for root zone except for half of TEW in upper layer
    Ir <- numeric(length = model.length)
    DPr <- numeric(length = model.length)
    Ks <- numeric(length = model.length)
    Kc.act <- numeric(length = model.length)
    Dr.end <- numeric(length = model.length)
    ETc.act <- numeric(length = model.length)
  #now for i=1, which will have slighly modified execution as it is the model initialization
    Dei.initial[1] <- max(TEW.parameter * TEW.fraction - P[1], 0) #this is an initial estimate of 
  #the water balance for the exposed surface soil where irrigation is not applied 
  #that assumes all daily precip occurs early in the morning so as to estimate Ke 
  #and Kr.  Same done for Dep.initial
    Dep.initial[1] <- max(TEW.parameter * TEW.fraction - P[1], 0)
    Kri[1] <- KrCalc(TEW.parameter, REW.parameter, Dei.initial, 1)
    Krp[1] <- KrCalc(TEW.parameter, REW.parameter, Dep.initial, 1)
    W[1] <- WCalc(TEW.parameter, Dei.initial, Dep.initial, fewp, fewi, 1)
    DPei[1] <- max(P[1] - TEW.parameter * TEW.fraction, 0) #initial estimate assumes irrigation is zero on previous day
    DPep[1] <- DPepCalc(P, Dep.initial, 1)
    Kei[1] <- min(Kri[1] * W[1] * (Kcmax[1] - Kcb.adjusted[1]), fewi[1] * Kcmax[1])
    Kep[1] <- min(Krp[1] * (1 - W[1]) * (Kcmax[1] - Kcb.adjusted[1]), fewp[1] * Kcmax[1])
    Ep[1] <- EpCalc(ETo, Kep, 1)
    Ei[1] <- EiCalc(ETo, Kei, 1)
    Dep.end[1] <- min(TEW.parameter, max(TEW.parameter * TEW.fraction - P[1] + Ep[1] / fewp[1] + DPep[1], 0)) #replaces Dep.end[i-1] with TEW.parameter * TEW.fraction
    Dei.end[1] <- min(TEW.parameter, max(TEW.parameter * TEW.fraction - P[1] + Ei[1] / fewi[1] + DPei[1], 0)) #replaces Dei.end[i-1] with Dei.intial[i]
    Kc.ns[1] <- KcnsCalc(Kcb.adjusted, Kei, Kep, 1)
    ETc.ns[1] <- ETcnsCalc(Kc.ns, ETo, 1)
    Dr.initial[1] <- max(TEW.parameter * TEW.fraction - P[1] + ETc.ns[1], 0) #initial calc
    Ks[1] <- KsCalc(Dr.initial=Dr.initial, PAW=PAW, i=1)
    Ir[1] <- IrCalc(Ks, RDI.min, AD, Dr.initial, doys.model, Jdev, Jharv, days.no.irr, 1, cropname)
    DPr[1] <- max(max(P[1] + Ir[1] - TEW.parameter * TEW.fraction - ETc.ns[1], 0)) #initial calc
    Kc.act[1] <- KcactCalc(Ks, Kcb.adjusted, Kei, Kep, 1)
    ETc.act[1] <- ETcactCalc(Kc.act, ETo, 1)
    Dr.end[1] <- TEW.parameter * TEW.fraction - P[1] + Kc.act[1] * ETo[1] + DPr[1] #initial calc


if(FALSE) {    
    ans = loopMain(AD, cropname, days.no.irr, doys.model, ETo, fewi, fewp, fw, Jdev, Jharv, Kcb.adjusted, Kcmax, model.length, P, PAW, RDI.min, REW.parameter, TEW.parameter, Dei.end, Ir, Dei.initial, Dep.end, Dep.initial, Kri, Krp, W, DPep, DPei, Kei, Kep, Ep, Ei, Kc.ns, ETc.ns, Dr.end, Dr.initial, Ks, DPr, Kc.act, ETc.act)

Dei.initial = ans$Dei.initial
Dep.initial = ans$Dep.initial
Kri = ans$Kri
Krp = ans$Krp
W = ans$W
DPep = ans$DPep
DPei = ans$DPei
Kei = ans$Kei
Kep = ans$Kep
Ep = ans$Ep
Ei = ans$Ei
Dep.end = ans$Dep.end
Dei.end = ans$Dei.end
Kc.ns = ans$Kc.ns
ETc.ns = ans$ETc.ns
Dr.initial = ans$Dr.initial
Ks = ans$Ks
Ir = ans$Ir
DPr = ans$DPr
Kc.act = ans$Kc.act
Dr.end = ans$Dr.end
ETc.act = ans$ETc.act    
} else {

# tm1 = proc.time()
if(FALSE) {    
   for (i in 2:model.length) { #now for days 2...model.length after initialization
      Dei.initial[i] <- DeiInitialCalc(Dei.end, P, Ir, fw, i)
      Dep.initial[i] <- DepInitialCalc(Dep.end, P, i)
      Kri[i] <- KrCalc(TEW.parameter, REW.parameter, Dei.initial, i)
      Krp[i] <- KrCalc(TEW.parameter, REW.parameter, Dep.initial, i)
      W[i] <- WCalc(TEW.parameter, Dei.initial, Dep.initial, fewp, fewi, i)
      DPep[i] <- DPepCalc(P, Dep.initial, i)
      DPei[i] <- DPeiCalc(P, Ir, fw, Dei.initial, i)
      Kei[i] <- KeiCalc(Kri, W, Kcmax, Kcb.adjusted, fewi, TEW.parameter, Dei.end, DPei, P, ETo, i, Ir, fw)
      Kep[i] <- KepCalc(Krp, W, Kcmax, Kcb.adjusted, fewp, TEW.parameter, Dep.end, DPep, P, ETo, i)
      Ep[i] <- EpCalc(ETo, Kep, i)
      Ei[i] <- EiCalc(ETo, Kei, i)
      Dep.end[i] <- DepEndCalc(Dep.end, P, Ep, fewp, DPep, i)
      Dei.end[i] <- DeiEndCalc(Dei.end, P, Ir, fw, fewi, Ei, DPei, i)
      Kc.ns[i] <- KcnsCalc(Kcb.adjusted, Kei, Kep, i)
      ETc.ns[i] <- ETcnsCalc(Kc.ns, ETo, i)
      Dr.initial[i] <- DrInitialCalc(Dr.end, ETc.ns, P, Ir, i)
      Ks[i] <- KsCalc(Dr.initial=Dr.initial, PAW=PAW, i=i) #corrected on 10/18/17
      Ir[i] <- IrCalc(Ks, RDI.min, AD, Dr.initial, doys.model, Jdev, Jharv, days.no.irr, i, cropname)
      DPr[i] <- DPrCalc(P, Ir, ETc.ns, Dr.end, i)
      Kc.act[i] <- KcactCalc(Ks, Kcb.adjusted, Kei, Kep, i)
      Dr.end[i] <- DrEndCalc(Dr.end, P, Ir, Kc.act, ETo, DPr, i)
      ETc.act[i] <- ETcactCalc(Kc.act, ETo, i) #could take this out of loop
  }
} else {

    .Call("loopMain", AD, cropname, days.no.irr, doys.model, ETo, fewi, fewp, fw, Jdev, Jharv, Kcb.adjusted, Kcmax, model.length, P, PAW, RDI.min, REW.parameter, TEW.parameter, Dei.end, Ir,
                      Dei.initial, Dep.end, Dep.initial, Kri, Krp, W, DPep, DPei, Kei, Kep, Ep, Ei, Kc.ns, ETc.ns, Dr.end, Dr.initial, Ks, DPr, Kc.act, ETc.act, integer()) # harvest.days is not defined in our situation.
}
#tm2 = proc.time()
#cat("Loop\n")
#print(tm2 - tm1)
}


tm1 = proc.time()
    model.result <- data.frame(dates, months, days, years, water.year, doys.model, P, ETo, RHmin, U2,
                               lapply(X=list(Kcb.std=Kcb.std, Kcb.adjusted=Kcb.adjusted, Kcmax=Kcmax, fceff=fc, fw=fw, fewi=fewi, fewp=fewp,
                                          Dei.initial=Dei.initial, Dep.initial=Dep.initial, Kri=Kri, Krp=Krp, W=W, Kei=Kei, Kep=Kep,
                                          Ei=Ei, Ep=Ep, Dpei=DPei, DPep=DPep, Dei.end=Dei.end, Dep.end=Dep.end, Kc.ns=Kc.ns, ETc.ns=ETc.ns,
                                          Dr.initial=Dr.initial, Ir=Ir, DPr=DPr, Ks=Ks, Kc.act=Kc.act, ETc.act=ETc.act, Dr.end=Dr.end),
                                      round, digits=rounding_digits))

    ridx = which(model.scaffold.results$unique_model_code==model.code & model.scaffold.results$cokey == cokey)
    colidx = which(colnames(model.scaffold.results)=='Irr.1'):(which(colnames(model.scaffold.results)=='GW.capture.net'))
    model.scaffold.results[ridx, colidx] <- merge(cbind(
                                                       do.call(rbind, lapply(split(model.result, model.result$years), IrDateCalc, Jdev = Jdev, Jharv = Jharv, days.no.irr = days.no.irr)),
                                                       do.call(rbind, lapply(split(model.result, model.result$years), WaterBalanceCalc, AD, days.no.irr, Jdev, Jharv, PAW)),
                                                       do.call(rbind, lapply(split(model.result, model.result$years), GreenWaterIrr1Calc, Jdev)),
                                                       do.call(rbind, lapply(split(model.result, model.result$years), DeepPercCalc, days.no.irr, Jdev, Jharv))),
                                                  do.call(rbind, lapply(split(model.result, model.result$water.year), GreenWaterCaptureCalc, Jdev, Jharv, Jmid, cropname)),
                                                  by="row.names", all=TRUE)[ ,2:26]
         
#tm2 = proc.time()
#cat("Time for model.result")
#print(tm2 - tm1)    

    
    print(paste(scenario.name, as.character(n)))
    
    if (FALSE && (n == 1 | n %in% rows.to.sample)) {
#        setwd(file.path(resultsDir, scenario.name))
      dir = file.path(resultsDir, scenario.name)
      filename = if (cropname=='grapes.wine') 
                    paste0(cropname, root_depth, 'RDI.min', as.character(RDI.min), '_', as.character(model.code), '_', as.character(cokey), '_', Sys.Date(), '.csv')
                 else
                    paste0(cropname, root_depth, 'AD', as.character(AD.percentage), '_', as.character(model.code), '_', as.character(cokey), '_', Sys.Date(), '.csv')
      write.csv(model.result, file.path(dir, filename), row.names=FALSE)      
    }
    if (n %in% save.times) { #was (n==100 | n %in% save.times), changed for Matt and Duncan 10/20/17
#      setwd(file.path(resultsDir, scenario.name))
      filename = if (cropname=='grapes.wine') 
                     paste0(cropname, root_depth, 'RDI.min', as.character(RDI.min), '_FAO56results.csv')
                 else
                     paste0(cropname, root_depth, 'AD', as.character(AD.percentage), '_FAO56results.csv')

      filename = file.path(file.path(resultsDir, scenario.name), filename)
      write.csv(model.scaffold.results, filename, row.names = FALSE)
    } else {next}
}
  
  dir = file.path(resultsDir, scenario.name)
  filename = if (cropname=='grapes.wine') 
                 paste0(cropname, root_depth, 'RDI.min', as.character(RDI.min), '_FAO56results.csv')
             else
                 paste0(cropname, root_depth, 'AD', as.character(AD.percentage), '_FAO56results.csv')
  write.csv(model.scaffold.results, file.path(dir, filename), row.names=FALSE)


  metadata <- cbind(data.frame(date.run=Sys.Date(), crop=cropname, cropscape.code=cropcode,
                               AD.percentage=AD.percentage, rooting.depth=root_depth, irrigation.type=irr.type,
                               paw.varname = paw.var, model.days=model.length, first.day=dates[1],
                               last.day=dates[length(dates)], n.models=nrow(model.scaffold.crop)),
                     crop.parameters[which(crop.parameters$crop==cropname), 2:ncol(crop.parameters)],
                     irrigation.parameters[which(irrigation.parameters$irrigation.type==irr.type), 'fw'])
  colnames(metadata)[ncol(metadata)] <- 'fw'
  filename = if (cropname == 'grapes.wine')
                 paste0(cropname, root_depth, 'RDI.min', as.character(RDI.min), '_model_metadata.csv')
             else
                 paste0(cropname, root_depth, 'AD', as.character(AD.percentage), '_model_metadata.csv')

  write.csv(metadata, paste(dir, filename, sep = .Platform$file.sep), row.names = FALSE)
}

#function call for Matt and Duncan
#tm = system.time(FAO56DualCropCalc('almond.mature', almond_code, 50, '2.0m', "Microspray, orchards", crop.parameters.df, model.scaffold, U2.df, P.df, ETo.df, RHmin.df, results_file='new', row_start=1, RDI.min = NA, alfalfa.zone = NA, grape.zone=NA))

#legend for FAO56 abbreviations
#De,j=De,j-1 - P,j - Ij/fw + Ej/fewi + DPei,j (again, ignoring tranpiration from upper 10 cm and runoff, eqn. 21) 
#pseudo-code outline of 'separate prediction of evaporation from soil wetted by precipitation only' following Allen et al. 2005, except ignoring runoff, essentially assuming that runoff will really only occur when soils are near field capacity, so partitioning this as 'deep percolation' is acceptable and is consciously preferred over introduced errors from the curve number approach
#Ke = Kei + Kep (eqn. 14)
#where Kei=evaporation coefficient for the exposed fraction of the soil wetted by both irrigation and by precipitation and Kep=evaporation coefficient for the exposed fraction of the soil wetted by precipitation only
#Kei = Kri*W*(Kcmax-Kcb) <= fewi*Kcmax (eqn. 15)
#Kep = Krp*(1-W)*(Kcmax-Kcb) <= fewp*Kcmax (eqn. 16)
#where fewi=fraction of soil wetted by both irrigation and precipitation and is exposed to rapid drying due to exposure to solar radiation and/or ventilation and is calculated as: min(1-fc, fw) (eqn. 18);
#where fewp=fraction of soil exposed to rapid drying and is wetted by precipitation only and is calculated as: 1-fc-fewi (eqn. 17)
#where Kri and Krp=evaporation reduction coefficients for the fewi and fewp fractions, respectively
#where W=weighting coefficient for partitioning the energy available for evaporation in the fewi and fewp soil fractions, depending on water availability, and is calculated as: 1/(1+(fewp/fewi)*(TEW-Dep)/(TEW-De)) (eqn. 19);
#where De=cumulative depletion depth (mm) from the evaporating layer for the fewi fraction of soil
#where Dep=cumulative depletion depth (mm) from the evaporating layer for the fewp fraction of soil
#finally, the water balance formation for fraction of soil only wetted by precipitation:
#Dep,j=Dep,j-1 - Pj + Ep,j/fewp + DPep,j (ignoring transpiration from upper 10 cm and runoff, eqn. 20)
#where Dep,j-1 and Dep,j are the cumulative depletion depths at the ends of days j-1 and j in the fewp fraction of the surface (mm)
#where Ep,j=evaporation from kewp fraction on day j and is calculated as: Kep*ETo in mm
#where DPep,j=deep percolation from the fewp fraction of the evaporation layer on day j if soil water content exceeds field capacity
#where 0 <= Dep, j <= TEW
#TEW=total evaporable water in upper layer and is calculated as: 1000*(field_capacity-0.5*wilting_point)*Ze
#where Ze=effective depth of wetting and field capacity and wilting point is assumed to be moisture content at 1/3 and 15 bars, respectively, determined from the SSURGO database
#and the water balance formulation for the fraction of soil wetted by both precipitation and irrigation
#De,j=De,j-1 - P,j - Ij/fw + Ej/fewi + DPei,j (again, ignoring tranpiration from upper 10 cm and runoff, eqn. 21)
#where 0 <= De,j <= TEW
#where Kri=(TEW-De,j-1)/(TEW-REW) (eqn. 22)
#where Krp=(TEW-Dep,j-1)/(TEW-REW) (eqn. 23)
#where Dpei,j=Pj + Ij/fw - Dei,j-1 >= 0
#where DPep,j=Pj-Dep,j-1 >= 0
#Kr depends on daily water balance of surface layer (upper 10-15 cm); Few will also depend on daily water balance if separate calculations are done for soil wetted by precipitation vs. by both precip and irrigation (see "Separate Prediction of Evaporation from Soil Wetted by Precipitation Only" in Allen et al. 2005)

