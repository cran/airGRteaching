
# airGRteaching: Tools to Simplify the Use of the airGR Hydrological Package for Education (Including a Shiny Application)

## Overview

'airGRteaching' is an add-on package to the [airGR](https://CRAN.R-project.org/package=airGR) package that simplifies its use and is teaching-oriented.
It allows to use with very low programming skills the rainfall-runoff models (**GR4H**, **GR5H**, **GR4J**, **GR5J**, **GR6J**, **GR2M**, **GR1A**) and a snow melt and accumulation model (CemaNeige). This package also provides graphical devices to help students to explore data and modelling results.

The 'airGRteaching' package has been designed to fulfil a major requirement: facilitating the use of the [airGR](https://CRAN.R-project.org/package=airGR) functionalities by students. The names of the functions and their arguments were chosen to this end. 

The package is mostly based on three families of functions:

- the functions that allow to complete very simply a hydrological modelling exercise;
- plotting functions to help students to explore observed data and to interpret the results of calibration and simulation of the GR models;
- a function which runs a 'Shiny' graphical user interface that allows for displaying in real-time model parameters impacts on hydrographs.

This package brings into R the hydrological modelling tools developed at INRAE-Antony ([Catchment Hydrology research group](https://webgr.inrae.fr/eng) of the HYCAR Research Unit, France).


## Installation

### Release version

To install the version of the package that is on the CRAN, you just have to use the following command line:

``` r
install.packages("airGRteaching")
```

### Unrelease version

To use the development version of the package that is on GitLab, you have first install the 'remotes' package. Then you can install the 'airGRteaching' package in the R environment:

``` r
install.packages("remotes")
remotes::install_gitlab(repo = "HYCAR-Hydro/airgrteaching", 
                        host = "https://gitlab.irstea.fr", 
                        dependencies = TRUE, 
                        build_vignettes = TRUE)
```


## Modelling Functions

Three functions allow to complete very simply a hydrological modelling exercise:

- preparation of data: `PrepGR()`;
- calibration of the models: `CalGR()`;
- simulation with the models: `SimGR()`.



## Plotting Functions

'airGRteaching' provides two types of plotting functions that allow to produce static (`plot()`) or dynamic (`dyplot()`) graphics (incl. mouse events and interactive graphics).
The devices allow to explore observed data and to interpret the results of calibration and simulation of the GR models.



## Graphical user interface

The package also provides the `ShinyGR()` function, which allows to launch a Shiny interface. Thus its is possible to perform:

- interactive flow simulations with parameters modifications;
- automatic calibration;
- display of internal variables evolution;
- time period selection.

A demonstrator of the graphical user interface is available for free online on the [Sunshine](https://sunshine.inrae.fr/) platform.


## Models

The six hydrological models and the snow melt and accumulation model already available in airGR are available in 'airGRteaching'.
These models can be called in 'airGRteaching' using the following model names (&#42;: available in the Shiny interface): 

- `GR4H`: four-parameter hourly lumped hydrological model (Mathevet, 2005)
- `GR5H`: five-parameter hourly lumped hydrological model (Ficchi, 2017; Ficchi *et al.*, 2019)
- `GR4J`&#42;: four-parameter daily lumped hydrological model (Perrin et al., 2003)
- `GR5J`&#42;: five-parameter daily lumped hydrological model (Le Moine, 2008)
- `GR6J`&#42;: six-parameter daily lumped hydrological model (Pushpalatha et al., 2011)
- `GR2M`&#42;: two-parameter monthly lumped hydrological model (Mouelhi, 2003; Mouelhi et al., 2006a)
- `GR1A`: one-parameter annual lumped hydrological model (Mouelhi, 2003; Mouelhi et al., 2006b)
- `CemaNeige`: two-parameter degree-day snow melt and accumulation daily model (combined with GR4H, GR5H, GR4J, GR5J or GR6J) (Valéry et al., 2014)


For more information and to get started with the package, you can refer to the vignette (`vignette("get_started", package = "airGRteaching")`) and go on the ['airGRteaching' website](https://hydrogr.github.io/airGRteaching/).


## References

- Coron, L., G. Thirel, O. Delaigue, C. Perrin and V. Andréassian (2017). The Suite of Lumped GR Hydrological Models in an R Package. Environmental Modelling and Software, 94, 166–171, doi: [10.1016/j.envsoft.2017.05.002](https://www.doi.org/10.1016/j.envsoft.2017.05.002).
- Ficchi, A. (2017). An adaptive hydrological model for multiple time-steps: Diagnostics and improvements based on fluxes consistency. PhD thesis, Irstea (Antony), GRNE (Paris), France.
- Ficchi, A., C. Perrin and V. Andréassian (2019). Hydrological modelling at multiple sub-daily time steps: model improvement via flux-matching. Journal of Hydrology, 575, 1308-1327, doi: [10.1016/j.jhydrol.2019.05.084](https://www.doi.org/10.1016/j.jhydrol.2019.05.084).
- Le Moine, N. (2008). Le bassin versant de surface vu par le souterrain : une voie d'amélioration des performances et du réalisme des modèles pluie-débit ?, PhD thesis (in French), UPMC - Cemagref Antony, Paris, France, 324 pp.
- Mathevet, T. (2005). Quels modèles pluie-débit globaux pour le pas de temps horaire ? Développement empirique et comparaison de modèles sur un large échantillon de bassins versants, PhD thesis (in French), ENGREF - Cemagref Antony, Paris, France, 463 pp.
- Mouelhi S. (2003). Vers une chaîne cohérente de modèles pluie-débit conceptuels globaux aux pas de temps pluriannuel, annuel, mensuel et journalier, PhD thesis (in French), ENGREF - Cemagref Antony, Paris, France, 323 pp.
- Mouelhi, S., C. Michel, C. Perrin and V. Andréassian (2006a). Stepwise development of a two-parameter monthly water balance model, Journal of Hydrology, 318(1-4), 200-214, doi: [10.1016/j.jhydrol.2005.06.014](https://www.doi.org/10.1016/j.jhydrol.2005.06.014).
- Mouelhi, S., C. Michel, C. Perrin. and V. Andreassian (2006b). Linking stream flow to rainfall at the annual time step: the Manabe bucket model revisited, Journal of Hydrology, 328, 283-296, doi: [10.1016/j.jhydrol.2005.12.022](https://www.doi.org/10.1016/j.jhydrol.2005.12.022).
- Perrin, C., C. Michel and V. Andréassian (2003). Improvement of a parsimonious model for streamflow simulation, Journal of Hydrology, 279(1-4), 275-289, doi: [10.1016/S0022-1694(03)00225-7](https://www.doi.org/10.1016/S0022-1694(03)00225-7).
- Pushpalatha, R., C. Perrin, N. Le Moine, T. Mathevet and V. Andréassian (2011). A downward structural sensitivity analysis of hydrological models to improve low-flow simulation, Journal of Hydrology, 411(1-2), 66-76, doi: [10.1016/j.jhydrol.2011.09.034](https://www.doi.org/10.1016/j.jhydrol.2011.09.034).
- Valéry, A., V. Andréassian and C. Perrin (2014). "As simple as possible but not simpler": What is useful in a temperature-based snow-accounting routine? Part 2 - Sensitivity analysis of the Cemaneige snow accounting routine on 380 catchments, Journal of Hydrology, 517(0): 1176-1187, doi: [10.1016/j.jhydrol.2014.04.058](https://www.doi.org/10.1016/j.jhydrol.2014.04.058).



