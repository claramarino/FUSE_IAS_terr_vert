# Conservation priorities for terrestrial vertebrates threatened by biological invasions
Scripts and data for reproducing the results obtained by Marino, Soares & Bellard in the paper "Conservation priorities for terrestrial vertebrates that are functionally unique, specialized, and endangered by biological invasions. "

## Description of the Data and file structure
Data folder contains two files related to species threats and traits.

### 1. IAS threat associated with terrestrial vertebrates

Data file Species_threat_IAS_terr_vert.xlsx contains threat information from the IUCN Red List of threatened species for amphibians, birds, mammals, and lizards at the species level (34,597 lines x 7 columns)

Column description:
- className	(character): class name of species
- orderName	(character): order name of species
- binomial (character): binomial name of species as in IUCN Red List
- cate_threat	(factor): species extinction risk (NTh: Not-threatened, Th: Threatened, Ex: Extinct, DD: Data Deficient)
- cate_p_ext (factor): IUCN Red List categories (LC: Least Concern, NT: Near Threatened, VU: Vulnerable, EN: Endangered, CR: Critically Endangered, EX: Extinct, DD: Data Deficient)
- ias_8.1 (categorical): the species is associated with IAS threat as 8.1 in IUCN (1) or not (0)
- AS (categorical): impact magnitude of IAS threat (0: low impact, 1: high impact, DD: data defficient, NA: not concerned)

### 2. Traits of terrestrial vertebrates

Data file Species_traits_terr_vert.xlsx contains traits for terrestrial vertebrates. Each sheet corresponds to one taxonomic group, lines refer to each species. 

File description:
Amphibians (6,492 lines x 5 columns):
- binomial (character): binomial name of species as in IUCN Red List
- Habitat (ordered factor): number of habitat types (1, 2, 3, 4, 5+)
- Repro.mode (factor): reproductive mode (Direct, Larval, Viviparous)
- For.niche (factor): preferred forraging stratum (Aqu: Aquatic, Arb: Arboreal, Fos: Fossorial, Ter: Terrestrial, MAqu: Multiple with aquatic, MNoAqu: Multiple without aquatic)
- ln.size.mm (continuous): body length in mm (log-transformed)

Birds (10,943 lines x 11 columns):
- binomial (character): binomial name of species as in IUCN Red List
- Habitat (ordered factor): number of habitat types (1, 2, 3, 4, 5+)
- Hand.Wing.Index (continuous): Kipp’s distance corrected for wing size in mm 
- Tail.Length (continuous): tail length in mm
- volant (binary): flying (1) or non-flying (0) bird
- Trophic.Niche (factor): main diet (Aquatic predator, Frugivore, Granivore, Herbivore aquatic, Herbivore terrestrial, Invertivore, Nectarivore, Omnivore, Scavenger, Vertivore)
- Primary.Lifestyle (factor):  preferred forraging stratum (Aerial, Aquatic, Generalist, Insessorial, Terrestrial)
- ln.Mass (continuous): body mass in g (log-transformed)
- ln.Clutch (numeric): number of eggs per clutch (log-transformed)
- ln.Beak.Depth (continuous): beak depth in mm (log-transformed)
- ln.Beak.Length_Nares (continuous): beak length in mm (log-transformed)

Mammals (5,505 lines x 6 columns): 
- binomial (character): binomial name of species as in IUCN Red List
- Habitat (ordered factor): number of habitat types (1, 2, 3, 4, 5+)
- Main.diet (factor): main diet (Herbi: herbivore, Inv: invertebrates, Multi_anim: mixed carnivore, Omni: omnivore, Vert: vertebrates)
- Activity (factor): period of activity (all: crepuscular-diurnal-nocturnal, C: crepuscular, CD: crepucular-diurnal, CN: crepuscular-nocturnal, D: diurnal, N: nocturnal)
- For.niche (factor): preferred forraging stratum (Aerial, Arboreal, Ground, Marine, Scansorial)
- ln.Mass.g (continuous): body mass in g (log-transformed)

Lizards (4,901 lines x 6 columns):
- binomial (character): binomial name of species as in IUCN Red List
- Habitat (ordered factor): number of habitat types (1, 2, 3, 4, 5+)
- Repro.mode (factor): reproductive mode (Oviparous, Viviparous, Mixed)
- Activity (factor): period of activity (Cathemeral, Diurnal, Nocturnal)
- For.niche (factor): preferred forraging stratum (Aqu: aquatic, Arb: arboreal, Fos: fossorial, Multi: multiple, Sax: saxicolous, Ter: terrestrial)
- max_log10_BM_g (continuous): body mass in g (log-transformed)

## How to use the scripts

### Calculate FUSE-IAS score

Script 01_Proba_extinction_IAS.R computes the probability of extinction of all species associated with IAS threat. Script 02_Functional_spaces_FUn_FSp.R builds the functional space based on species traits for each taxonomic group and calculates the functional metrics related to specialization and uniqueness. Script 03_FUSE_IAS_scores_and_lists.R computes the final FUSE-IAS score for each species and derives the priority lists.

### Evaluate FUSE-IAS species in priority lists

Script 04_Details_Core_list_species.R explores the characteristics of species listed in the Core list. Script XXXXX maps the species richness of species in each priority list.

