This README file was generated on 2026-02-11 by Antoni Vivó Pons.

GENERAL INFORMATION

1. Title of Dataset: Disentangling the effects of abiotic and biotic processes on non-indigenous species dominance

2. Author Information
	A. Principal Investigator Contact Information
  	 Name: Antoni Vivó-Pons	
  	 Institution: Marine Ecology department, Centre for Advanced Studies of Blanes (CEAB-CSIC)
  	 Address:  Blanes, Girona, Spain
  	 Email: antoni.vivo@ceab.csic.es

3. Date of data collection (single date, range, approximate date): N/A

4. Geographic location of data collection: Worldwide

SHARING/ACCESS INFORMATION

1. Licenses/restrictions placed on the data: CC0 1.0 Universal (CC0 1.0) Public Domain

2. Links to publications that cite or use the data:

	Vivó-Pons, A., Jaspers, C., Seebens, H., Thorson, J., Lusseau, D., Lindegren, M. (2026). Data from: Global patterns and processes of establishment risk by non-indigenous marine fish.

3. Links to other publicly accessible locations of the data: 
	- AquaMaps: https://www.aquamaps.org/
	- FishLife: https://github.com/James-Thorson-NOAA/FishLife
	- MarInvaders:  https://marinvaders.atlantis-erc.eu/
	- Ocean Health Index, cumulative human impacts: https://oceanhealthindex.org/resources/data/cumulative-human-impacts/
	- Marine Ecoregions of the World: https://www.marineregions.org/
	- World Register of Introduced Marine Species: [https://www.marinespecies.org/introduced/index.php](https://www.marinespecies.org/introduced/index.php)

4. Links/relationships to ancillary data sets: None

5. Was data derived from another source? No

6. Recommended citation for this dataset: 

Vivó-Pons, A., Jaspers, C., Seebens, H., Thorson, J., Lusseau, D., Lindegren, M. (2026). Data from: Global patterns and processes of establishment risk by non-indigenous marine fish. Dryad Digital Repository. 

CODE INFORMATION

**Libraries and functions**. List of required libraries.

**Figures data**. Needed data files and objects to produce all figures.

**Update aquamapsdata**. This script contains the code to update the local SQL data repository containing the data from AquaMaps.

**1. Environmental suitability and degree of niche overlap**. This script contains the code needed to obtain the environmental suitability (Component II) and degree of niche overlap (Component III), for each NIS at each half-degree cell.

**2.  Connectivity**. This script corresponds to the calculation of the connectivity between source and recipient areas (Component I), from single port-to-port connections to aggregated values at a province level. Calculations were based on the work of Seebens et al (2013).

**3. NIS establishment risk calculation**. This script corresponds to the calculation of NIS establishment risk integrating the three previous components. 

**4. Global patterns of risk (Fig 1-4; Fig S1-3; Table S1)**. This script contains all the code related with the exploration of global risk patterns (**Fig. 1**), summary of risk in each realm (**Fig. 2**), decomposing the risk (**Fig. 3**) and mapping the patterns of contribution for each single component (**Fig. 4**). In addition, supplementary figures and tables showing other descriptors of risk (mean and sd; Fig. S1), the main source regions of NIS and cells included (Fig. S2), relationship between aggregated and mean risk (Fig. S3) and summary of risk and components values for each NIS (**Table S1**)

**5. Cumulative impacts, MPAs and risk (Fig 5; Fig S4; Table S2)**. This script contains all the code needed to produce the GAMMs exploring the relationship between cumulative human impacts and establishment risk (**Fig. 5, Table S2**), as well as the relationship between risk and MPA presence (**Fig. S4**).

 **6. Comparison of risk with established NIS (Fig 6, Fig S5, Table S3)**. This script contains all the code needed for the comparison with the risk scores obtained and the current ranges of established NIS extracted from WRiMS (**Fig. 6, Fig. S5, and Table S3**)
