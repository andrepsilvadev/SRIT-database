# latest updates
- SRIT database (trait data collection and habitat suitability is now done separately from the SRIT pipeline). Allows to run SRIT faster. Pipeline has been tested. 
- selected species are those for which activity area overlaps with IUCN range (not the bufffer as previously done) - too strict?
- Buffer of activity area is now species-specific (based on home-range)

# suitability module
#20250108
- Incorporate global and local drivers of suitability - for instance climatic suitability is not yet incorporated (global driver) 
run sdms for suitability landscapes
- a mechanistic estimation of the niche see stefan example with metarange
- how would the suitability scores be calculated if we run sdms

#202501007
- globalSuitability.R needs to be tested - crashed in the gaia step