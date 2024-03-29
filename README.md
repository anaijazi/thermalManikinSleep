# Passive and low-energy strategies to improve sleep thermal comfort and energy resilience during heat waves and cold snaps

[Arfa Aijazi](https://github.com/anaijazi), [Thomas Parkinson](https://github.com/tom-parkinson), [Stefano Schiavon](https://github.com/stefanoschiavon), and Hui Zhang

## Background
Sleep is a pillar of human health and wellbeing. In high- and middle-income countries, there is a great reliance on heating ventilation and air conditioning systems (HVAC) to control the interior thermal environment in the bedroom. However, these systems are problematic as they are expensive to buy and operate while being energy and environmentally intensive - problems that may increase due to climate change. Passive and low-energy strategies, such as fans and electrical blankets, may address these challenges but their comparative effectiveness for providing comfort in sleep environments has not been studied. We used a thermal manikin to experimentally show that many passive and low-energy strategies are highly effective in supplementing or replacing HVAC systems during sleep. Using passive strategies in combination with low-energy strategies that elevate air movement like ceiling or pedestal fans can enhance the cooling effect by three times compared to only passive options. We applied our experimentally measured heating and cooling effects to two historical case studies: the 2015 Pakistan heat wave and the 2021 Texas power crisis. Passive and low-energy strategies can reduce the sleep time heat or cold exposure by as much as 90%. The low-energy strategies we tested require one to two orders of magnitude less energy than HVAC systems, and the passive strategies require no energy input. Our results demonstrate that these strategies can also help reduce peak load surges in extreme temperature events. This reduces the need for utility loadshedding, which can put individuals at risk of hazardous heat or cold exposure. Our results may serve as a starting point for evidence-based public health guidelines on how individuals can sleep better during heat waves and cold snaps without HVAC.

## Code
This repository contains all the data files and source code to reporduce the forthcoming paper. All code is written in R and follows the `{tidyverse}` syntax. 

The project directory is structured as follows:

```
└── doc
    ├── 20230920_ThermalManikin_Manuscript_Clean.docx # manuscript file
    └── figures
        └── components 
            ├── attributions.xlsx # source and attributions for images not created by the author
            ├── ... # image files used to create composite figures in varying formats
        └── image_license
            ├── ... # documentation of permission to use licensed images from product manufacturer
        ├── ...  # final versions of figures that appear in the manuscript in varying formats
└── code
    ├── thermalManikinSleep.Rproj # RStudio project file
    ├── 01_hobocalibration.Rmd # calculates coefficients of linear model to calibrate each HOBO temperature probe
    ├── 02_erroruncertainty.Rmd # calculates error and uncertainty on thermal manikin data, i.e. not the combined uncertainty which is calculated in 04_Teq.Rmd
    ├── 03_h.rmd # calculates the heat transfer coefficient, h, from reference conditions
    ├── 04_Teq.Rmd # calculates the equivlent temperature, the heating or cooling effect, as well as the combined uncertainty
    ├── 05_casestudy.Rmd # calculates the heat and cold exposure with and without top performing interventions from the laboratory data
    ├── 06_fanspeed.Rmd # calculates the spatially averaged fan air speed over the experimental bed
    ├── 07_casestudy_map.Rmd # products a plot of the case study locations, not used in final manuscript
    ├── 08_clo.Rmd # calculates the Clo value for mattress, bedding, and clothing interventions
└── data
    └── calibration
        └── hobo 
            ├── ... # raw output from HOBO data loggers
        ├── startstop.csv # start and end times for steady state period during calibration
        ├── HOBO_Calibration.csv # coefficients of linear model to calibrate each temperature probe calculated in 01_hobocalibration.Rmd
    └── case_study
        └── dallas_energymodel
            └── weather
                └── # output generated from EnergyPlus 
                ├── USA_TX_Dallas-Fort.Worth.Intl.AP.722590_KDFW.epw # historical weather data from Dallas airport weather station for February 2021
            └── Dallas_Feb2021_Outage_KDFW
                ├── Dallas_Feb2021_Outage_KDFW.idf # EnergyPlus input data file
                ├── Dallas_Feb2021_Outage_KDFW.csv # energy model output file containining interior temperature data
                ├── ... # all other energy model output files
           └──  Dallas_Feb2021_NoOutage_TMY3
                ├── ... # original energy model from Pacific Northwest National Lab (PNNL)
           ├── CaseStudyLatLong.csv # input for 07_casestudy_map.Rmd
           ├── Karachi_June2015_OPKC.csv # historical weather data from Karachi airport weather station for June 2015
    └── experiment
        ├── ExperimentalMatrix.csv # table of experimental trials and the start and stop time for each run. 
        └── hobo
            ├── ... # raw data from HOBO temperature probe
        └── manikin
            ├── ... # raw data from thermal manikin
    └── fan_speed
      ├── FanSpeed.csv # measured air speed data
    └── h
        ├── h.csv # output from 03_h.rmd containing the heat transfer coefficient for each body segment in three diferent postures
    └── ir_images
        ├── ... # image files from infrared camera. The corresponding experimental condition is listed in ../experiment/ExperimentalMatrix.csv
    └── power
        ├── ... # power consumption for low-energy interventions 
    └── teq
        ├── SurfaceArea.csv # surface area of each body segment
        ├── thermal.effect_labels.csv # x-axis plot labels
    └── uncertainty
        ├── Uncertainty.xlsx # breakdown of uncertainty for each measurement and calculation of combined uncertainty
        ├── uReference.csv #combined uncertainty for reference cases by body part as calculated in Uncertainty.xlsx
        ├── uExperiment.csv #combined uncertainty for experimental cases by body part as calculated in Uncertainty.xlsx
```

Knitting the R markdown files in the 'code' directory in sequence is important to generate all necessary input parameters. 

1. '01_hobocalibration.rmd'
2. '02_erroruncertainty.rmd'
3. '03_h.rmd'
4. '04_Teq.rmd'
5. '05_casestudy.rmd'
6. '06_fanspeed.rmd'
7. '08_clo.rmd'

## Acknowledgements
At the time of the study, Arfa Aijazi was supported by a Doctoral Completion Fellowship through the Graduate Division at the University of California, Berkeley. This research was also in part funded by the Center for the Built Environment (CBE) at University of California, Berkeley. CBE with which the authors are affiliated CBE is advised by and funded by many partners that represent a diversity of organizations from the building industry, including manufacturers, building owners, facility managers, contractors, architects, engineers, government agencies, and utilities. Specifically, the authors also acknowledge in-kind equipment donations to CBE from Big Ass Solutions and Sleepme. The authors also thank Professor Ed Arens, Charlie Huizenga, Dr. Yingdong He, and visiting PhD student Thomas Hirn with the CBE at the University of California, Berkeley for their assistance with using the Controlled Environmental Chamber (CEC) and the thermal manikin, Dr. Federico Tartarini, formerly affiliated with the Berkeley Education Alliance for Research in Singapore (BEARS), for his help with the case study methodology, Dr. Carlos Duarte with CBE for his help estimating the power rating of conventional HVAC systems, and Professor Gail Brager with CBE for review of a portion of the manuscript text. We also acknowledge that this study significantly builds upon a thermal manikin study conducted by Professor Young-Hum Cho, affiliated with the School of Architecture, Yeungnam University, Republic of Korea and formerly a CBE visiting scholar.  