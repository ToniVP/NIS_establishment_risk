This README file was generated on 2025-07-07 by Antoni Vivó Pons.

CODE INFORMATION

**Libraries and functions**. List of required libraries.

**Figures data**. Needed data files and objects to produce all figures.

**Update aquamapsdata**. This script contains the code to update the local SQL data repository containing the data from AquaMaps.

**1. Environmental suitability and degree of niche overlap**. This script contains the code needed to obtain the environmental suitability (Component II) and degree of niche overlap (Component III), for each NIS at each half-degree cell.

**2.  Connectivity**. This script corresponds to the calculation of the connectivity between source and recipient areas (Component I), from single port-to-port connections to aggregated values at a province level. Calculations were based on the work of Seebens et al (2013).

**3. NIS establishment risk calculation**. This script corresponds to the calculation of NIS establishment risk integrating the three previous components. 

**4. Global patterns of risk (Fig 1-4; Fig S1-3; Table S1)**. This script contains all the code related with the exploration of global risk patterns (**Fig. 1**), summary of risk in each realm (**Fig. 2**), decomposing the risk (**Fig. 3**) and mapping the patterns of contribution for each single component (**Fig. 4**). In addition, supplementary figures and tables showing other descriptors of risk (mean and sd; Fig. S1), the main source regions of NIS and cells included (Fig. S2), relationship between aggregated and mean risk (Fig. S3) and summary of risk and components values for each NIS (**Table S1**)

**5. Cumulative impacts, MPAs and risk (Fig 5; Fig S4; Table S2)**. This script contains all the code needed to produce the GAMMs exploring the relationship between cumulative human impacts and establishment risk (**Fig. 5, Table S2**), as well as the relationship between risk and MPA presence (**Fig. S4**).

*******************************************************************************************************************************************************************************************
DATA AND FOLDERS GENERAL INFORMATION
1. File List:
	1. In *Data* folder:
		1. All_cells_AquaMaps.txt
		2. All_traits_clean.txt
		3. Aquaculture_fish.txt
		4. cumulative_impacts.txt
		5. dist_matrix_gawdis.R -- could not be uploaded but can be generated with the code
		6. Final_traits_num.txt
		7. fish_sp_filtered.txt
		8. Initial_metrics.txt -- could not be uploaded but can be generated with the code
		9. Joint_components.txt -- could not be uploaded but can be generated with the code
		10. NIS_risk_metrics.txt -- could not be uploaded but can be generated with the code
	2. In *All_species_metrics* subfolder: Generated metrics for all NIS in their potential range. Variable names are the same as in *Initial_metrics.txt*. The contents in this folder could 	   not be uploaded but can be generated with the code
	3. In *All_species_metrics_native* subfolder: Generated metrics for all NIS in their native range, used for posterior range corrections. Variable names are the same as in *Initial_metrics.txt*. The contents in this folder could not be uploaded but can be generated with the code
	4. In *Connectivity* subfolder:
		1. matrix_invasion_risk.txt
		2. Native_regions.R
		3. ports_total_risk.txt
		4. ports_link_list.txt
		5. port-to-port_risk.txt
		6. Regions.R
		7. risk_matr_ecoreg.R
		8. risk_matr_province.R
		9. risk_matr_realm.R
	5. In *Cumul_impact* subfolder: All TIFF objects corresponding to cumulative human impacts, could not be uploaded but can be extracted from Halpern et al (2019) and [Ocean Health Index](https://oceanhealthindex.org/resources/data/cumulative-human-impacts/)
	6. In *MEOW* subfolder: Shapefiles corresponding to the [Marine Ecoregions of the World ](https://www.marineregions.org/)(Spalding et. al., 2007)
	7. In *Models* subfolder: GAMM objects derived from Script *5. Cumulative impacts, MPAs and risk (Fig 5; Fig S4; Table S2)*. The contents in this folder could not be uploaded but can be generated with the code

2. Relationship between files: 
	- *Initial_metrics.txt* is derived from all files within the *Data* subfolders *All_species_metrics* and *All_species_metrics_native*. From the *Initial_metrics.txt* file, first we derive the *Joint_components.txt* and finally we obtain the *NIS_risk_metrics.txt* file, which is the final file used to produce all figures. 
	- *All_traits_clean.txt* is derived from the *Final_traits_num.txt* file

3. Additional related data collected that was not included in the current data package: 
	- Data found within the *aquamapsdata* folder was provided by the AquaMaps team. Data descriptions can be found at [aquamapsdata](https://raquamaps.github.io/aquamapsdata/articles/intro.html) or by contacting the associated team. Inside the folder:
		1. *aquamaps_sql_files* subfolder: Metadata for all datasets found in the R package and in the main *aquamapsdata* folder.
		2. *am* Data Base file: Local SQL data repository where all data from AquaMaps is stored and accessed with the package functions. See script *Update aquamapsdata* for further details. 
		3. hcaf: Characteristics for all individual cells. Stored in SQL format.
		4. hcaf_species_native_fish: All native range cells for global fishes. Stored in SQL format.
		5. hcaf_species_native_NIS_list: All native range cells for the list of NIS. Stored in SQL format.
		6. hcaf_species_suitable_NIS_list: All potential range cells for the list of NIS. Stored in SQL format.
		7. hspen_fish: Input data used to generate a species’environmental envelopes and the envelopes themselves. Stored in SQL format.
		8. speciesoccursum_fish: Summary of species occurrences. Stored in SQL format.

#########################################################################

DATA-SPECIFIC INFORMATION FOR: *All_cells_AquaMaps.txt*

Specific information for each half-degree cell extracted from [aquamapsdata](https://raquamaps.github.io/aquamapsdata/articles/intro.html)

1. Number of variables: 58
2. Number of cases/rows: 259200
3. Variable List:
	- ID: Unique HCAF ID, for internal use only.  
	- CsquareCode: A unique identifier for every half-degree cell in the global map based on the c-square method - a hierarchical cell labelling system developed at CSIRO Oceans and Atmosphere (then CSIRO Marine Research). Example: 3414:227:3  
	- LOICZID: LOICZ ID numbers are long integers from 1 to 259200. They begin with the cell centered at 89.75°N latitude and 179.75°W longitude and proceed west to east...  
	- NLimit: Northern boundary of cell in decimal degrees latitude (positive in N hemisphere, negative in S hemisphere)...  
	- Slimit: Southern boundary of cell in decimal degrees latitude (positive in N hemisphere, negative in S hemisphere)...  
	- WLimit: Western boundary of cell in decimal degrees latitude (positive in E hemisphere, negative in W hemisphere)...  
	- ELimit: Eastern boundary of cell in decimal degrees latitude (positive in E hemisphere, negative in W hemisphere)...  
	- CenterLat: The center point of the cell in decimal degrees latitude.  
	- CenterLong: The center point of the cell in decimal degrees longitude.  
	- CellArea: The total area inside the cell in square kilometers, using WGS84 and Miller cylindrical projection.  
	- OceanArea: The area in the cell that is normally covered by sea water or permanent ice.  
	- PWater: Proportion of water in each cell.  
	- ClimZoneCode: Climate zone to which the cell belongs based on climate zone shape file in SAU database.  
	- FAOAreaM: Code number of FAO statistical area to which the cell belongs, for all oceanic and coastal cells.  
	- FAOAreaIn: Code number of FAO statistical area to which the cell belongs, for all inland and coastal cells.  
	- CountryMain: UN code number of country, island or area to which the largest land area of the cell belongs.  
	- CountrySecond: UN code number of country, island or area to which the second largest land area of the cell belongs.  
	- CountryThird: UN code number of country, island or area to which the third largest land area of the cell belongs.  
	- CountrySubMain: ISO code number of state, province, region to which the largest land area of the cell belongs.  
	- CountrySubSecond: ISO code number of state, province, region to which the second largest land area of the cell belongs.  
	- CountrySubThird: ISO code number of state, province, region to which the third largest land area of the cell belongs.  
	- EEZ: Code number of country, island or area to which the EEZ area in the cell belongs.  
	- LME: Code number of the large marine ecosystem to which the cell belongs, as given by NOAA.
	- LMEBorder: Tags whether or not cell lies along the border of an LME. 0 = No, 1 = Yes.  
	- MEOW: 5-digit code referring to the marine ecoregion the cell belongs to, as assigned by MEOW.  
	- OceanBasin: Major ocean basins of the world, with north and south sub-basins separated by latitude.  
	- IslandsNo: Number of coastal or oceanic islands contained in cell.  
	- Area0_20: Area in cell from 0–20 m depth, in square kilometers.  
	- Area20_40: Area in cell from 20–40 m depth, in square kilometers.  
	- Area40_60: Area in cell from 40–60 m depth, in square kilometers.  
	- Area60_80: Area in cell from 60–80 m depth, in square kilometers.  
	- Area80_100: Area in cell from 80–100 m depth, in square kilometers.  
	- AreaBelow100: Area in cell below 100 m depth, in square kilometers.  
	- ElevationMin: Minimum elevation above sea level in meters.  
	- ElevationMax: Maximum elevation above sea level in meters.  
	- ElevationMean: Mean elevation above sea level in meters.  
	- ElevationSD: Standard deviation of elevation above sea level in meters.  
	- DepthMin: Minimum bathymetric depth in the cell (negative values).  
	- DepthMax: Maximum bathymetric depth in the cell (negative values).  
	- DepthMean: Mean bathymetric depth in the cell (negative values).  
	- DepthSD: Standard deviation of depth below sea level in meters.  
	- SSTAnMean: Mean annual sea surface temperature (2000–2014) in °C.  
	- SBTAnMean: Mean annual sea bottom temperature (2000–2014) in °C.  
	- SalinityMean: Mean annual surface salinity (2000–2014), practical salinity scale.  
	- SalinityBMean: Mean annual bottom salinity (2000–2014), practical salinity scale.  
	- PrimProdMean: Annual surface primary production in mg C·m⁻³·day⁻¹.  
	- IceConAnn: Mean annual sea ice concentration (fraction 0–1).  
	- OxyMean: Mean annual surface oxygen concentration in mmol/m³.  
	- OxyBMean: Mean annual bottom oxygen concentration in mmol/m³.  
	- LandDist: Distance (km) to the nearest coastal cell (for water cells).  
	- Shelf: Water area within the shelf zone (0–200 m depth).  
	- Slope: Water area within the slope zone (200–4000 m depth).  
	- Abyssal: Water area within the abyssal zone (>4000 m depth).  
	- TidalRange: Scaled discrete class for tidal range from LOICZ.  
	- Coral: Proportion of cell covered by coral.  
	- Estuary: Area covered by estuaries in the cell.  
	- Seamount: Number of known seamounts in the cell.  
	- MPA: Proportion of the cell covered by a Marine Protected Area. 

#########################################################################

DATA-SPECIFIC INFORMATION FOR: *All_traits_clean.txt*

Clean trait data for all included species of fish

1. Number of variables: 25

2. Number of cases/rows: 9452

3. Variable List: In this case each column accounts for a specific trait modality (further described in Thorson et. al., 2023), showing the specific values associated to each taxon. 

#########################################################################

DATA-SPECIFIC INFORMATION FOR: Aquaculture_fish.txt

1. Number of variables: 1

2. Number of cases/rows: 151

3. Variable List:
	- Valid_name: Species names

#########################################################################

DATA-SPECIFIC INFORMATION FOR: cumulative_impacts.txt

1. Number of variables: 1500

2. Number of cases/rows: 6

3. Variable List:
   - CsquareCode: A unique identifier for every half-degree cell in the global map based on the c-square method - a hierarchical cell labelling system developed at CSIRO Oceans and Atmosphere (then CSIRO Marine Research). Example: 3414:227:3 
   - CenterLong: The center point of the cell in decimal degrees longitude.
   - CenterLat: The center point of the cell in decimal degrees latitude.
   - REALM: Name of the Marine Realm where that cell belongs
   - PROVINCE: Name of the Marine Province where that cell belongs
   - ECOREGION: Name of the Marine Ecoregion where that cell belongs
   - MEOW: 5-digit code referring to the marine ecoregion the cell belongs to, as assigned by MEOW. 
   - art_fish: Tonnes of artisanal fisheries catch standardized by Net Primary Productivity
   - demersal_destructive: Tonnes of catch using demersal destructive gear types, standardized by Net Primary Productivity
   - demersal_low_bycatch: Tonnes of demersal fisheries catch using nondestructive and low bycatch gear types, standardized by Net Primary Productivity
   - pelagic_high_bycatch: Tonnes of pelagic fisheries catch using high bycatch gear types, standardized by Net Primary Productivity
   - cumul_imp: The cumulative impact of all 14 stressors on 21 marine habitats.
   - direct_human: Magnitude of direct human interactions on coastal and near-coastal habitats, such as trampling
   - shipping: Relative intensity of global shipping traffic
   - sst: Frequency of extreme temperature events relative to a historical baseline period

#########################################################################

DATA-SPECIFIC INFORMATION FOR: dist_matrix_gawdis
Overall pairwise matrix of functional distances between species

1. Number of variables: 9452

2. Number of cases/rows: 9452

#########################################################################

DATA-SPECIFIC INFORMATION FOR: *All_traits_clean.txt*

Raw worldwide fish trait data from [FishLife](https://github.com/James-Thorson-NOAA/FishLife)

1. Number of variables: 33

2. Number of cases/rows: 30017

3. Variable List: In this case each column accounts for a specific trait modality (further described in Thorson et. al., 2023), showing the specific values associated to each taxon. 

#########################################################################

DATA-SPECIFIC INFORMATION FOR: fish_sp_filtered.txt*

List of fish species included, with data from FishBase and AquaMaps

1. Number of variables: 13

2. Number of cases/rows: 9861

3. Variable List:
   - SpeciesID: AquaMaps’ unique identifier for a valid species used by the Catalogue of Life Annual Checklist (www.catalogueoflife.org). Example for the whale shark: Fis-30583
   - SpecCode: Species identifier used in FishBase or SeaLifeBase
   - Species: Complete species name
   - Genus: Genus to which the species belongs
   - AphiaID: AphiaID unique identifyer. 
   - Class: Class to which the species belongs
   - Order: Order to which the species belongs
   - Family: Family to which the species belongs
   - Fresh: Yes/no fields that indicate whether the species occurs in the freshwater environment, at any stage of its development.
   - Brack: Yes/no fields that indicate whether the species occurs in the brackish environment, at any stage of its development.
   - Saltwater: Yes/no fields that indicate whether the species occurs in the marine environment, at any stage of its development.
   - DemersPelag: Indicates the particular environment preferred by the species.
   - AnaCat: Migration spawning patterns of the species.

#########################################################################

DATA-SPECIFIC INFORMATION FOR: Initial_metrics.txt

Database with environmental suitability (Component II) and degree of niche ovelap (Component III) for each NIS, in each cell for their potential and native range

1. Number of variables: 36

2. Number of cases/rows: 911555

3. Variable List:
   - MEOW: 5-digit code referring to the marine ecoregion the cell belongs to, as assigned by MEOW. 
   - SpeciesID: AquaMaps’ unique identifier for a valid species used by the Catalogue of Life Annual Checklist (www.catalogueoflife.org). Example for the whale shark: Fis-30583
   - Valid_name: Complete species name.
   - CsquareCode: A unique identifier for every half-degree cell in the global map based on the c-square method - a hierarchical cell labelling system developed at CSIRO Oceans and Atmosphere (then CSIRO Marine Research). Example: 3414:227:3
   - uniq_coords: Joint longitude and latitude for that cell
   - Probability: Relative habitat suitability scores from AquaMaps (i.e. environmental suitability)
   - ID: Unique HCAF ID, for internal use only.
   - CenterLat: The center point of the cell in decimal degrees latitude.
   - CenterLong: The center point of the cell in decimal degrees longitude.
   - NLimit: Northern boundary of cell in decimal degrees latitude (positive in N hemisphere, negative in S hemisphere)...  
   - Slimit: Southern boundary of cell in decimal degrees latitude (positive in N hemisphere, negative in S hemisphere)...  
   - WLimit: Western boundary of cell in decimal degrees latitude (positive in E hemisphere, negative in W hemisphere)...  
   - ELimit: Eastern boundary of cell in decimal degrees latitude (positive in E hemisphere, negative in W hemisphere)...>)
   - LME: Code number of the large marine ecosystem to which the cell belongs, as given by NOAA ([http://www.lme.noaa.gov](http://www.lme.noaa.gov/)), for all coastal and oceanic cells.
   - LMEBorder: Tags whether or not cell lies along the border of an LME. 0=No, 1=Yes
   - DepthMin: Minimum bathymetric depth in the cell (negative values).  
   - DepthMax: Maximum bathymetric depth in the cell (negative values).  
   - DepthMean: Mean bathymetric depth in the cell (negative values).  
   - DepthSD: Standard deviation of depth below sea level in meters.  
   - SSTAnMean: Mean annual sea surface temperature (2000–2014) in °C.  
   - SBTAnMean: Mean annual sea bottom temperature (2000–2014) in °C.  
   - SalinityMean: Mean annual surface salinity (2000–2014), practical salinity scale.  
   - SalinityBMean: Mean annual bottom salinity (2000–2014), practical salinity scale.  
   - PrimProdMean: Annual surface primary production in mg C·m⁻³·day⁻¹.  
   - OxyMean: Mean annual surface oxygen concentration in mmol/m³.  
   - OxyBMean: Mean annual bottom oxygen concentration in mmol/m³.  
   - Shelf: The water area of the cell that lies within the shelf zone (0 - 200m depth); based on min/max elevation and proportion in depth zone.
   - MPA: Proportion of cell covered by a Marine Protected Area.
   - Di: Functional distinctiveness of each NIS in each cell compared to the potential native community, not scaled
   - Di_scaled_cell: Functional distinctiveness of each NIS in each cell compared to the potential native community, with the functional distances matrix scaled for each cell
   - rich: Native species richness
   - rich_with_trait: Native species richness with available trait information
   - range: Potential or native range of NIS.
   - ECOREGION: Name of the Marine Ecoregion where that cell belongs
   - PROVINCE: Name of the Marine Province where that cell belongs
   - REALM: Name of the Marine Realm where that cell belongs

#########################################################################

DATA-SPECIFIC INFORMATION FOR: Joint_components.txt

Database with all components together: Connectivity (Component I), environmental suitability (Component II) and degree of niche ovelap (Component III) for each NIS, in each cell for their potential and native range

1. Number of variables: 30

2. Number of cases/rows: 891104

3. Variable List:
   - MEOW: 5-digit code referring to the marine ecoregion the cell belongs to, as assigned by MEOW. 
   - SpeciesID: AquaMaps’ unique identifier for a valid species used by the Catalogue of Life Annual Checklist (www.catalogueoflife.org). Example for the whale shark: Fis-30583
   - Valid_name: Complete species name.
   - CsquareCode: A unique identifier for every half-degree cell in the global map based on the c-square method - a hierarchical cell labelling system developed at CSIRO Oceans and Atmosphere (then CSIRO Marine Research). Example: 3414:227:3
   - uniq_coords: Joint longitude and latitude for that cell
   - Probability: Relative habitat suitability scores from AquaMaps (i.e. environmental suitability)
   - ID: Unique HCAF ID, for internal use only.
   - CenterLat: The center point of the cell in decimal degrees latitude.
   - CenterLong: The center point of the cell in decimal degrees longitude.
   - MPA: Proportion of cell covered by a Marine Protected Area.
   - Di: Functional distinctiveness of each NIS in each cell compared to the potential native community, not scaled
   - Di_scaled_cell: Functional distinctiveness of each NIS in each cell compared to the potential native community, with the functional distances matrix scaled for each cell
   - rich: Native species richness
   - rich_with_trait: Native species richness with available trait information
   - range: Potential or native range of NIS.
   - ECOREGION: Name of the Marine Ecoregion where that cell belongs
   - PROVINCE: Name of the Marine Province where that cell belongs
   - REALM: Name of the Marine Realm where that cell belongs
   - Di_scaled: Functional distinctiveness of each NIS in each cell compared to the potential native community, with the distinctiveness values scaled for each NIS’ potential range
   - inv_risk: Port-to-port connectivity values aggregated by Marine Ecoregion
   - native_area: Native ecoregions for that NIS.
   - inv_risk_pr: Port-to-port connectivity values aggregated by Marine Province
   - inv_risk_rlm: Port-to-port connectivity values aggregated by Marine Realm
   - inv_risk_scaled: Scaled connectivity values for the potential range of each NIS. In this case it was the connectivity values aggregated by province. 
   - rescaled_cell_Di: Functional distinctiveness of each NIS in each cell compared to the potential native community, with the distinctiveness values scaled for each NIS’ potential range. Duplicated from Di_scaled, none were used at the end. 
   - status: introduced status of NIS, with three options; range expansion, introd, uncertain; used to re-classify NIS
   - pathway: Possible introduction pathway of NIS
   - aquaculture: Binary identifier if a certain NIS has interest in aquaculture (YES/NO) 
   - lessepsian: Binary identifier if a certain NIS is a lessepsian migrant (YES/NO) 
   - ornamental: Binary identifier if a certain NIS has an ornamental interest (YES/NO) 

#########################################################################

DATA-SPECIFIC INFORMATION FOR: NIS_risk_metrics.txt

Database with all components and the computed establishment risk metrics for each NIS, in each cell for their potential and native range

1. Number of variables: 34

2. Number of cases/rows: 891104

3. Variable List:
   - MEOW: 5-digit code referring to the marine ecoregion the cell belongs to, as assigned by MEOW. 
   - SpeciesID: AquaMaps’ unique identifier for a valid species used by the Catalogue of Life Annual Checklist (www.catalogueoflife.org). Example for the whale shark: Fis-30583
   - Valid_name: Complete species name.
   - CsquareCode: A unique identifier for every half-degree cell in the global map based on the c-square method - a hierarchical cell labelling system developed at CSIRO Oceans and Atmosphere (then CSIRO Marine Research). Example: 3414:227:3
   - uniq_coords: Joint longitude and latitude for that cell
   - Probability: Relative habitat suitability scores from AquaMaps (i.e. environmental suitability)
   - ID: Unique HCAF ID, for internal use only.
   - CenterLat: The center point of the cell in decimal degrees latitude.
   - CenterLong: The center point of the cell in decimal degrees longitude.
   - MPA: Proportion of cell covered by a Marine Protected Area.
   - Di: Functional distinctiveness of each NIS in each cell compared to the potential native community, not scaled
   - Di_scaled_cell: Functional distinctiveness of each NIS in each cell compared to the potential native community, with the functional distances matrix scaled for each cell
   - rich: Native species richness
   - rich_with_trait: Native species richness with available trait information
   - range: Potential or native range of NIS.
   - ECOREGION: Name of the Marine Ecoregion where that cell belongs
   - PROVINCE: Name of the Marine Province where that cell belongs
   - REALM: Name of the Marine Realm where that cell belongs
   - Di_scaled: Functional distinctiveness of each NIS in each cell compared to the potential native community, with the distinctiveness values scaled for each NIS’ potential range
   - inv_risk: Port-to-port connectivity values aggregated by Marine Ecoregion
   - native_area: Native ecoregions for that NIS.
   - inv_risk_pr: Port-to-port connectivity values aggregated by Marine Province
   - inv_risk_rlm: Port-to-port connectivity values aggregated by Marine Realm
   - inv_risk_scaled: Scaled connectivity values for the potential range of each NIS. In this case it was the connectivity values aggregated by province. 
   - rescaled_cell_Di: Functional distinctiveness of each NIS in each cell compared to the potential native community, with the distinctiveness values scaled for each NIS’ potential range. Duplicated from Di_scaled, none were used at the end. 
   - status: introduced status of NIS, with three options; range expansion, introd, uncertain; used to re-classify NIS
   - pathway: Possible introduction pathway of NIS
   - aquaculture: Binary identifier if a certain NIS has interest in aquaculture (YES/NO) 
   - lessepsian: Binary identifier if a certain NIS is a lessepsian migrant (YES/NO) 
   - ornamental: Binary identifier if a certain NIS has an ornamental interest (YES/NO) 
   - risk_sum: Establishment risk obtained as the sum of all components. Risk in native range was set to 0.
   - risk_prod: Establishment risk obtained as the product of all components. Risk in native range was set to 0.
   - risk_scaled: Standardized establishment risk based on the sum of components for each NIS across their potential range. Risk in native range was set to 0.
   - risk_scaled_prod: Standardized establishment risk based on the product of components for each NIS across their potential range. Risk in native range was set to 0.

#########################################################################

DATA-SPECIFIC INFORMATION FOR: *matrix_invasion_risk.txt*

Matrix of mutual invasion risks between ecoregions obtained from [Seebens et al (2013)](https://onlinelibrary.wiley.com/doi/full/10.1111/ele.12111)

1. Number of variables: 16

2. Number of cases/rows: 18

3. Variable List: Pairwise invasion risk matrix between ecoregions, corresponding to Table S3 from [Seebens et al (2013)](https://onlinelibrary.wiley.com/doi/full/10.1111/ele.12111) 

#########################################################################

DATA-SPECIFIC INFORMATION FOR: Native_regions.R

List of all native marine ecoregions for each specific NIS

1. Number of variables: 127 elements

#########################################################################

DATA-SPECIFIC INFORMATION FOR: *port_total_risk.txt*

Total aggregated invasion risk for each port

1. Number of variables: 2

2. Number of cases/rows: 1469

3. Variable List: 
	- V1: Corresponds to the port name. This is corrected in the script.
	- V2: Corresponds to the aggregated risk throughout all connections. 

#########################################################################

DATA-SPECIFIC INFORMATION FOR: *ports_link_list.txt*

Information of each port included in the connectivity component, based on [Seebens et al (2013)](https://onlinelibrary.wiley.com/doi/full/10.1111/ele.12111)

1. Number of variables: 12

2. Number of cases/rows: 1512

3. Variable List:
   - Country: Country where each port is found
   - Port: Port’s name
   - lon: Longitude of the port in decimal degrees
   - lat: Latitude of the port in decimal degrees
   - Temp: Surface water temperatures from the World Ocean Atlas (WOA, http://www.nodc.noaa.gov)
   - Sal: Surface water salinity from Lloyd’s Register Fairplay (http://www.portguide.com)
   - Nit: Nitrate concentration from the World Ocean Atlas (WOA, http://www.nodc.noaa.gov)
   - Phos: Phospate concentration from the World Ocean Atlas (WOA, http://www.nodc.noaa.gov)
   - Sil: Silicate concentration from the World Ocean Atlas (WOA, http://www.nodc.noaa.gov)
   - Tmin: Minimum surface water temperature from the World Ocean Atlas (WOA, http://www.nodc.noaa.gov)
   - Ecoregion: Marine Ecoregion where that port belongs
   - Continent: Continent where that port belongs

#########################################################################

DATA-SPECIFIC INFORMATION FOR: *port-to-port_risk.txt*

Pairwise connectivity values between all ports, based on [Seebens et al (2013)](https://onlinelibrary.wiley.com/doi/full/10.1111/ele.12111)

1. Number of variables: 3

2. Number of cases/rows: 512603

3. Variable List:
   - port_orig: Source port
   - port_dest: Destination port
   - risk: Connectivity value between them

#########################################################################

DATA-SPECIFIC INFORMATION FOR: Regions.R

List of all marine ecoregions within the potential range for each specific NIS

1. Number of variables: 127 elements

#########################################################################

DATA-SPECIFIC INFORMATION FOR: *risk_matr_ecoreg.R*

Aggregated connectivity values between Ecoregions

1. Number of variables: 9

2. Number of cases/rows: 6335

3. Variable List:
   - ecoreg_origin: Source Ecoregion name
   - MEOW_origin: 5-digit code referring to the marine ecoregion acting as source of NIS, as assigned by MEOW. 
   - ecoreg_dest: Destination Ecoregion name
   - MEOW_dest: 5-digit code referring to the destination marine ecoregion of NIS, as assigned by MEOW. 
   - province_orig: Source Province name
   - province_dest: Destination Province name
   - realm_orig: Source Realm name
   - realm_dest: Destination Realm name
   - risk: Aggregated connectivity value between Ecoregions

#########################################################################

DATA-SPECIFIC INFORMATION FOR: *risk_matr_province.R*

Aggregated connectivity values between Provinces

1. Number of variables: 5

2. Number of cases/rows: 1217

3. Variable List:
   - province_orig: Source Province name
   - province_dest: Destination Province name
   - realm_orig: Source Realm name
   - realm_dest: Destination Realm name
   - risk: Aggregated connectivity value between Provinces

#########################################################################

DATA-SPECIFIC INFORMATION FOR: *risk_matr_realm.R*

Aggregated connectivity values between Realms

1. Number of variables: 3

2. Number of cases/rows: 117

3. Variable List:
   - realm_orig: Source Realm name
   - realm_dest: Destination Realm name
   - risk: Aggregated connectivity value between Realms
