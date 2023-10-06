# Estimating tax effects from reductions in Commercial Real Estate in downtown Chicago

This repository contains code, data, and output files for estimating the tax effect


**Software tools:** The analysis utilized to the R package [PTAXSIM](https://github.com/ccao-data/ptaxsim/) developed by the data team in the Cook County Assessor's Office to estimate the effect of reductions in downtown Commercial Real Estate values on tax bills in Chicago. The basic workflow was based on the code vignette available [here](https://ccao-data.github.io/ptaxsim/articles/reassessment.html#future-reassessments).

**Geographic definitions:** Downtown Chicago is based on the [CBD delineation in the Chicago Data Portal](https://data.cityofchicago.org/api/geospatial/tksj-nvsw). The analysis is restricted to only property tax bills for residents in the City of Chicago.

**Projected real estate values:**
The 10 to 40% reduction range in reductions in commercial real estate value is based on this [BCG report](https://www.bcg.com/publications/2023/countering-the-surge-of-zombie-buildings), which estimates a loss in building office value of $20-25 billion (35-45%) and a loss of annual rent revenue $2-3.5 billion (25-35%) in Chicago. Reductions are 

**Projected Levies:**
All l
Property tax revenue from [Chicago Public Schools](https://www.cps.edu/about/finance/budget/)
> 2019 $2,984,300,000
2020 $3,134,500,000
2021 $3,264,912,413
2022 $3,374,173,082
2023 $3,685,311,455
2024 $3,816,007,845

Property tax revenue from [Chicago Department of Finance](https://www.chicago.gov/content/dam/city/depts/fin/supp_info/CAFR/2022CAFR/ACFR_2022.pdf#page=216)
> 2018 $1,446,971,000 
2019 $1,514,102,000  
2020 $1,539,811,000 
2021 $1,633,162,000 
2022 $1,709,390,000 
2023 $1,734,390,000 ([see here](https://www.chicago.gov/content/dam/city/depts/COFA/ProposedBudget/COFA_AnalysisOfAnnualProposedBudget_FY2023.pdf#page=5) and [here](https://www.chicago.gov/content/dam/city/depts/COFA/ProposedBudget/COFA_AnalysisOfAnnualProposedBudget_FY2023.pdf#page=5))
2024 $1,824,390,000 ([see here](https://www.chicago.gov/content/dam/city/depts/COFA/ProposedBudget/Presentations_ProposedBudget/Mid-Year-Budget-Forecast-COFA-Analysis.pdf#page=3))

**Property Classes:**
Property Class Dictionary available [here](https://prodassets.cookcountyassessor.com/s3fs-public/form_documents/classcode.pdf).

| Property grouping         | Class codes                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|---------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Residential               | 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 218, 219, 224, 225, 234, 236, 239, 240, 241, 278, 288, 290, 295, 297, 299                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| Multi-story office/retail | 591, 592                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Commercial/industrial     | 100, 190, 300, 301, 313, 314, 315, 318, 390, 391, 396, 397, 399, 400, 401, 417, 418, 422, 423, 426, 427, 428, 429, 430, 431, 432, 433, 435, 480, 481, 483, 487, 489, 490, 491, 492, 493, 496, 497, 499, 500, 501, 516, 517, 522, 523, 526, 527, 528, 529, 530, 531, 532, 533, 535, 550, 580, 581, 583, 587, 589, 590, 593, 597, 599, 637, 638, 650, 651, 654, 655, 663, 666, 668, 669, 670, 671, 673, 677, 679, 680, 681, 683, 687, 689, 693, 700, 701, 716, 717, 722, 723, 726, 727, 728, 729, 730, 731, 732, 733, 735, 742, 743, 745, 746, 747, 748, 752, 753, 756, 757, 758, 760, 761, 762, 764, 765, 767, 772, 774, 790, 791, 792, 797, 798, 799, 800, 801, 816, 817, 822, 823, 826, 827, 828, 829, 830, 831, 832, 833, 835, 850, 880, 881, 883, 887, 889, 890, 891, 892, 893, 897, 899, 900, 901, 913, 914, 915, 918, 959, 990, 991, 996, 997 |


