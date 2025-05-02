# trait data updates
#20250502 (MIS)
Oii tava a ver coisas minhas e lembrei que isto talvez ajude a Filipa.
Não sei o quão comum isto de haver varios nomes para a msm sps é nos mammals, mas para trees e birds talvez sirva.
Neste repo eles tem um script para, antes de fazer a search nas databases, criar uma lista de sinonimos para cada espécies, usando o rfishbase package. Talvez no caso do NatPoKe se possa usar o taxize or rgibf?
vê-se achas util para integrar na pipeline do natPoKe e  procurar mais traits para esses target groups https://github.com/genner-lab/meta-fish-lib/blob/main/scripts/load-synonyms.R
https://onlinelibrary.wiley.com/doi/10.1111/jfb.14852 (escusas de ler o paper mas fica só pq sim)

#20250328 (MIS)
OpenForests - https://www.cambridge.org/core/journals/environmental-data-science/article/openforest-a-data-catalog-for-machine-learning-in-forest-monitoring/F62FBEADFF8E3A10C6EDA789D7D180C6
#20250327 (MIS)
- Tree traits are still imcomplete see next links for more info on databases (used and to still look at)
    - Script: https://github.com/andrepsilvadev/SRIT-database/blob/4e124479c5dd238a17ad78b4e3c4f069a4e388a9/src/TreeTraits_MIS.R
    - Databases files: https://drive.google.com/drive/folders/1VppQMKqJOlJigZMws-3BWcO2tUUmQ9KA?usp=sharing
    - Tree traits to still look for: https://docs.google.com/spreadsheets/d/1JHLHvyIlQKSPwfPvWB8TYRV6UA62H5cb5gUQIZzmPEE/edit?usp=sharing
- Mammals traits are good (130 sps with complete trait data for tropical and boreal forests together)
- Bird traits could still benefit from a search for max ages (or longevity) and ages at maturity/reproduction ages, so we can get more than woodpeckers and the tucan
    
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
