# SRIT Modules 

## settings & libraries
    generalSettings.R <-
    libraries.R <-
    customFunctions.R <-

## occurrences (general module)
    occurrences.R <- download occurrence data for different taxa from GBIF or other sources

## traits (general module)
(general module - no need to run every simulation)   
    externalData.R <- reads input trait data from external sources
    traits.R <- (v2 being updated by Afonso)

## suitability (general module)
(general module - no need to run every simulation)   
    globalSuitability.R <- produces input suitability landscapes with species specific resolution for the different models
        (old simulatedLandscapes.R) 

## disturbance
(simulation-specific module)
    activityArea <- identifies the area the area disturbed by human activity
    species.R <- identifies species for which the IUCN range overlaps with the activity area
    disturbance.R <- translates disturbance within the activity area to suitability landscapes (e.g. conversion to bare land)
    landscapeResolution.R <- transform suitability landscapes into landscapes with species-specific modelling resolution. Can also create test cells for quick simulation tests. 

## dataframes
(simulation-specific module)
    rangeShifterDataframe.R <-
    metaRangeDataframe.R <- (being created by Afonso)

## simulation
(simulation-specific module)
    simulationSettings.R <- 
    rangeshifter_localcalibrationb.R <-
    simulation_beforeAfterDisturbance.R <-
    scenarioProjections.R <-
    modelValidation.R <-

## resilience
    resilienceMetrics.R <-


SRIT Data storage can be found at: https://drive.google.com/drive/folders/1wkmRrCmzI3pzR7jNDEA-jypK6BHyxZ5c?usp=drive_link
