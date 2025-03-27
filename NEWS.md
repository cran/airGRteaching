## Release History of the airGRteaching Package





### 0.3.5 Release Notes (2025-03-27)

#### Bug fixes

- the YAML metadata values of the markodown files used in the GUI has been updated to be consistent with YAML updates. ([#81](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/81))

____________________________________________________________________________________


### 0.3.4 Release Notes (2025-02-11)

#### User-visible changes

- the GUI, launched by the `ShinyGR()` function, now displays fact sheets of catchments again if the dataset name contains the station code (10 characters: 1 letter and 9 digits) from the Hydroportail website (so it is available only for datasets from this French database). ([#79](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/79))
- the GUI, launched by the `ShinyGR()` function, has its authors' list removed and the screenshots optimized to reduce their file size. ([#80](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/80))

____________________________________________________________________________________

### 0.3.3 Release Notes (2024-07-23)

#### User-visible changes

- the figure of the "Pessimistic zero precipitation scenario" section is now corrected in the 'Low-flow forecasting' vignette. ([#78](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/78))
- the DOI used in the CITATION file is now the one provided by the CRAN. ([#77](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/77))
- the `ShinyGR()` now returns a message to explain how to diconnect the GUI from R. ([#76](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/76))
- small modifications made in vignettes. ([#75](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/75), [#74](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/74))

#### CRAN-compatibility updates

- private utility functions are no longer exported by the namespace (`.CheckUrl()`, `.DiagramGR()`, `.DyShadingMulti()`, `.StartStop()`, `.TypeModelGR()`). ([#73](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/73))

____________________________________________________________________________________


### 0.3.2 Release Notes (2023-07-11)

#### New features

- `plot.CalGR()` and `plot.SimGR()` gain a `log_scale` argument to display flow time series with a logarithmic scale. ([#71](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/71))

#### Bug fixes

- bug fixed in `plot.CalGR()` and `plot.SimGR()`. The functions work well when `log_scale = TRUE` to display flow time series with a logarithmic scale. ([#71](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/71))

____________________________________________________________________________________

### 0.3.1 Release Notes (2022-12-15)

#### New features

- added the `GetCrit` and `GetParam` S3 methods in order to get respectively the error criterion value and the model parameter set from the outputs of `CalGR()` and `SimGR()` functions. ([#62](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/62))
- added 4 vignettes to explain more deeply the package options (1 vignette: "Teaching hydrology"), and to offer example tutorials as hydrological teaching projects (3 vignettes: "Flow reconstruction", "Forecasting", and "Climate change"; available both in English and French). ([#47](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/47))


#### Bug fixes

- bug fixed in `plot.CalGR()`, now returns no more the `"synth"` graph when `which = "perf"`. ([#63](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/63))


#### User-visible changes

- add the possibility to disable the warm-up period of the model using the `CalGR()` and `SimGR()` functions (if `WupPer = 0L`). ([#52](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/52))
- add the possibility to choose other graphical outputs in the functions `plot.CalGR()` and `plot.SimGR()` using the `which` argument . ([#6](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/6))
- the `dyplot` S3 method  has been cleaned. The internal `.dyplot()` replace `dyplot.default()` function. The `dyplot.PrepGR()`, `dyplot.CalGR()` and `dyplot.SimGR()` aliases and usages have been added the `dyplot()` help page ([#61](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/61))
- the Font Awesome icon names used in the GUI have been updated to avoid warning messages during the launch by the `ShinyGR()` function. ([#61](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/61))

____________________________________________________________________________________


### 0.2.13 Release Notes (2022-03-03)


#### Bug fixes

- bug fixed in `PrepGR()` and by consequence in `ShinyGR()` when the dataset given to the `ObsDF` argument is a `tibble` object. ([#49](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/49))


#### User-visible changes

- in order to avoid duplicated warning messages, the use of the `.onAttach()` command has been removed from the `ShinyGR()` function, because it is already call from the `dyplot()` function which is use in `ShinyGR()`. ([#50](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/50))
- the 'Model diagram' plot exported from the GUI launched by `ShinyGR()` has been resized to avoid to wide title when the the parameter values are large. ([#54](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/54))
- the deprecated icon names from 'Font Awesome' have been updated to avoid warning messages when the `ShinyGR()` function is used. ([#51](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/51))


#### Version control and issue tracking

- the CI/CD runner now uses a R docker image. ([#55](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/55))


#### CRAN-compatibility updates

- The `FUN_CRIT` deprecated argument (since 'airGR' 1.2.13.16) has been removed from the use of `Calibration()` call by the `CalGR()` function. ([#56](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/56))

____________________________________________________________________________________


### 0.2.12 Release Notes (2021-08-06)


#### Bug fixes

- bug fixed in the GUI launched by `ShinyGR()`, it is possible to the export the GR2M model diagram PNG file again. ([#40](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/40))
- bug fixed in `SimGR()` which can now simulate a period with no observed flow. Previously, it could only be run in this case when the observed flow was missing from the output of `PrepGR()` on the whole period and not just on a subperiod. ([#43](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/43))


#### User-visible changes

- displaying a message when the package is attached from RStudio and also when the GUI is launched by `ShinyGR()` in order to warn users about GUI instability problems. ([#46](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/46))
- added explanation of the use of the GR5H model in the `PrepGR()` help page. ([#41](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/41))
- the text relating to the GR2M model in the 'GUI Help' image displayed by the GUI launched by `ShinyGR()` is corrected. ([#42](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/42))
- replace the use of `class()` by `inherits()` in order to test the classes of inputs in all functions. ([#45](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/45))


#### Version control and issue tracking

- automatic tests implemented in the package. ([#44](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/44))

____________________________________________________________________________________


### 0.2.11 Release Notes (2021-06-07)


#### Bug fixes

- bug fixed in `ShinyGR()` when the date column of `ObsDF` has another name as 'DatesR'. ([#38](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/38))


#### User-visible changes

- the `as.data.frame` S3 methods have been set for classes `PrepGR`, `CalGR` and `SimGR`. ([#39](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/39))
- the useless `airGRt` class and the `as.data.frame.airGRt` S3 method no longer exist ([#39](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/39))

____________________________________________________________________________________


### 0.2.10.112 Release Notes (2021-01-23)


#### New features

- the GUI, launched by the `ShinyGR()` function, can now run on monthly time series, using the GR2M model ([#14](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/14))


#### Bug fixes

- bug fixed in `plot.PrepGR()` when all `Qobs` are missing. The function now displays an empty plot for the observed discharges ([#35](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/35))


#### User-visible changes

- `theme` agument of the `ShinyGR()` function now uses partial matching ([#12](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/12))
- `as.data.frame.airGRt()`, `plot`, `plot.PrepGR()`, `plot.CalGR()` and `plot.SimGR()` functions are no longer exported by the namespace ([#30](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/30))


#### Version control and issue tracking

- implement automatic tests in the package ([#29](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/29))


#### CRAN-compatibility updates

- now depends on R >= 3.6.0 in order to be sure to have the packages 'shiny' >= 1.1.0 and 'htmlwidgets' >= 1.5.3 available ([#5](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/5))
- now depends on 'airGR' >= 1.6.9.27. 'airGRteaching' uses the new 'Ps' output of the `RunModel_GR2M` function ([#51](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/51)) and the new `SeriesAggreg()` function ([#25](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/25), [#41](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/41), [#43](https://gitlab.irstea.fr/HYCAR-Hydro/airgr/-/issues/43)) from 'airGR'
- now suggests 'htmlwidgets' >= 1.5.3, available on the CRAN. It avoids troubles with the use of dynamic graphics of the 'dygraphs' package (called by the `dyplot*()` and the `ShinyGR()` functions) ([#5](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/5))

____________________________________________________________________________________


### 0.2.9.25 Release Notes (2020-10-19)


#### New features

- GUI, launched by the `ShinyGR()` function, now displays a new tab panel that shows a summary sheets of basin if the name of the dataset contains the code station (8 characters : 1 letter and 7 digits) of the Banque Hydro French database (so it is available only for the dataset of this database) ([#10](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/10))


#### Version control and issue tracking

- users can now track [changes](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching) and [issues](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues)


#### User-visible changes

- the `theme` agument of the `ShinyGR()` function now works even if a wrong character case is used


#### CRAN-compatibility updates

- when the package is attached or when the `dyplot()` and the `ShinyGR()` function are used, a message warns the users if they use a version of 'htmlwidgets' < 1.5.2.9000. The latest version of this package, available on GitHub, avoids troubles with the use of dynamic graphics of the 'dygraphs' package (called by the `dyplot*()` and the `ShinyGR()` functions) ([#5](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/5))

____________________________________________________________________________________


### 0.2.8.69 Release Notes (2020-02-28)


#### New features

- added `as.data.frame.airGRt()` method in order to create a `data.frame` from outputs of `PrepGR()`, `CalGR()` and `SimGR()` functions. This `data.frame` always presents the same structure and contains observed flow, simulated flow, simulated solid precipitation fraction, etc. When it does not make sense, the concerned column is assigned with `NA` values (e.g. Qsim with the `PrepGR()` function)
- a digital object identifier (DOI) now allows to identify the manual of the 'airGRteaching' package. When you use airGRteaching in your work, please always cite both the article and the manual. The last one allows to know the version of the package that is used in order to enhance reproducible research. The references can be displayed with the `citation("airGRteaching")` command
- two themes of alternative stylesheet are available (`"Inrae"` and `"Saclay"`) using the `theme` agument of the `ShinyGR()` function


#### Deprecated and defunct

- `CalGR` argument is now deprecated in the `SimGR()` function. It has been replaced by the use of the `Param` argument which can be set by an object of the class `CalGR` or by a vector of parameters


#### Bug fixes

- `ShinyGR()` now runs when independent arguments (`DatesR`, `Precip`, etc.) are used instead of the `ObsDF` argument


#### User-visible changes

- it is now possible to use the GR4H and GR5H hourly models with or without CemaNeige. For that, in the `PrepGR()`, the `HydroModel` argument could be set to `"GR4H"` or `"GR5H"`. In the GUI, launched by `ShinyGR()` function, nothing changed, only the daily models are available. So, now airGRteaching depends on the version of 'airGR' >= 1.4.3.52) ([#7](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/7))
- it is now possible to run the `PrepGR()` function when discharge is not provided in `Qobs`. If it is the case, the `CalGR()` function will return an error message because it is not possible to calibrate the model. The `SimGR()` function will return a warning message because it is not possible to compute any efficiency criterion
- it is now possible to run the `ShinyGR()` function when discharge is not provided in `Qobs`
- when observed discharge is provided in `ShinyGR()`, the first plotting panel now draws the flow error time series ([#4](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/4))
- `plot()` function is now exported
- `dyplot.PrepGR()`, `dyplot.CalGR()` and `dyplot.SimGR()` functions are no longer exported
- there is now only one help page for all `plot.*()` functions (use `?plot` to call it)


#### CRAN-compatibility updates

- when the package is loaded, a message warns the users if they use a version of 'htmlwidgets' < 1.5.1.9000. The latest version of this package, available on GitHub, avoids troubles with the use of dynamic graphics of the 'dygraphs' package (called by the `dyplot*()` and the `ShinyGR()` functions) ([#5](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/5))

____________________________________________________________________________________


### 0.2.6.29 Release Notes (2019-05-02)


#### Bug fixes

- `ShinyGR()` can export the csv table again. It was broken due to modifications to the version 1.2.13.16 of the 'airGR' package


#### User-visible changes

- it is now possible to export the diagram plot of the model from the `ShinyGR()` interface

____________________________________________________________________________________


### 0.2.6.27 Release Notes (2019-04-23)


#### Bug fixes

- `ShinyGR()` may now run for any timezone
- `ShinyGR()` takes into account the fact that on 1 time step `airGR::ErrorCrit_KGE` do not return CritName (temporary patch)
- `ShinyGR()` takes into account the fact that the previous simulation could have a missing value criterion
- `ShinyGR()` takes into account the fact the prevuous Qsim is sometimes to long of one value (temporary patch)


#### User-visible changes

- the WarmUp element returned by `CalGR()` and `SimGR()` now presents a timezone defined as UTC

____________________________________________________________________________________


### 0.2.6.14 Release Notes (2019-04-03)


#### User-visible changes

- the CemaNeige model is now allowed when the model diagram is drawn in `ShinyGR()`
- `.DiagramGR()` and `.TypeModelGR()` are now private functions
- time format of "Period" and "Event" sliders of the `ShinyGR()` function is now `"%Y-%m-%d"` with the latest versions of the 'shiny' package (like it was with th old versions)


#### CRAN-compatibility updates

- now depends on the latest version (1.2.13.16) of the 'airGR' package: `CalGR()`, `SimGR()` and `ShinyGR()` have been updated
- the 'htmlwidgets' package is no longer imported

____________________________________________________________________________________


### 0.2.3.2 Release Notes (2018-08-08)


#### User-visible changes

- the article reference is updated
- `.DiagramGR()` no longer returns errors when inputs are not yet available in `ShinyGR()`


#### CRAN-compatibility updates

- now depends on the latest version (1.1.1.6) of the 'dygraphs' package from CRAN (embeded 'dygraphs' functions have been removed)

____________________________________________________________________________________


### 0.2.2.2 Release Notes (2018-03-21)


#### Bug fixes

- bug fixed in `ShinyGR()`, the criteria values are now right on Unix system


#### User-visible changes

- vignette added

____________________________________________________________________________________


### 0.2.0.9 Release Notes (2018-03-16)


#### CRAN-compatibility updates

- embeding 'dygraphs' functions to avoid user to install the last version of this package from GitHub (import of devtools not necessary)

____________________________________________________________________________________


### 0.1.11.26 Release Notes (2018-02-01)


#### Bug fixes
- bug fixed in `ShinyGR()` when C1 (or C2) is modified after calibration; the calibration button is now reset
- bug fixed in warm-up, calibration and simulation periods checks in `CalGR()` and `SimGR()` functions


#### Deprecated and defunct

- `ObsBV` argument has been renamed `ObsDF` in `PrepGR()` and `ShinyGR()` functions


#### User-visible changes

- update and homogenization of the unit of time abbreviation in `.TypeModelGR()` and `ShinyGR()`
- graphical parameters recorded and executed when the `plot.PrepGR()`, `plot.CalGR()` and `plot.SimGR()` functions exit

____________________________________________________________________________________


### 0.1.10.0 Release Notes (2018-01-30)


#### Deprecated and defunct

- `ObsGR()` function (and relatives arguments in `CalGR()` and `SimGR()` has been renamed PrepGR()

____________________________________________________________________________________


### 0.1.9.29 Release Notes (2018-01-30)


#### Bug fixes

- missing exchange added on exp. store when plotting GR6J model diagram in `ShinyGR()`
- exp store now appears exported png file of state variables plot in `ShinyGR()` when GR6J is used
- animate button fixed in `ShinyGR()`
- bug fixed in `ShinyGR()` to show previous sim. when model or dataset changes
- bug fixed in `ShinyGR()` to show previous sim. when time window changes but keeps the same length
- bug fixed to disable calibration when there is no Qobs in `ShinyGR()`


#### User-visible changes

- it is now possible to draw the model diagram in `ShinyGR()` using the GR6J model
- exp. store now appears in state variables plot in `ShinyGR()` when GR6J is used
- update and homogenization of the unit of time abbreviation in `.TypeModelGR()` and `ShinyGR()`
- write "< - 99.99" in the criteria table of `ShinyGR()` when a criterion is very low
- `ShinyGR()` now allows a list format for `ObsBV` data.frame and CemaNeige inputs

____________________________________________________________________________________


### 0.1.8.14 Release Notes (2017-11-29)


#### Bug fixes

- `ObsGR()` function now returns an error if the time zone is not defined as `"UTC"`
- in `ShinyGR()` background color defined to black when the Flatly theme is used
- bug fixed in `ShinyGR()` when inputs are defined in vectors (not in a data.frame)
- Psol et Pliq bars are reversed in `dyplot.default()`


#### User-visible changes

- new reactive to prepare data for plotting in the `ShinyGR()` interface

____________________________________________________________________________________


### 0.1.7.6 Release Notes (2017-10-05)


#### User-visible changes

- `SimGR()` now runs only once to compute all `ErrorCrit`
- it is now possible to show the table of the last simulation criteria in the `ShinyGR()` interface
- it is now possible to export state variable plot from the `ShinyGR()` interface

____________________________________________________________________________________


### 0.1.6.15 Release Notes (2017-09-29)


#### New features

- `dyplot()` now allows to draw an additional time series of flow


#### Bug fixes

- bug fixed when zooming after changing snow model on plot to see the last simulation in `ShinyGR()`
- `dyplot()` now plots Pliq and Psol when CemaNeige is used (`dyStackedBarGroup()` instead of the plotter argument)


#### User-visible changes

- disable and enable buttons in `ShinyGR()` interface (using the 'shinyjs' package)
- it is now possible to register the last simulation and draw it on Model diagram of `ShinyGR()`

____________________________________________________________________________________


### 0.1.5.22 Release Notes (2017-09-14)


#### New features

- it is now possible to export some plots and tables from `ShinyGR()` interface
- in `ShinyGR()`, `TypeModel` inputIds renamed into `HydroModel`


#### Bug fixes

- period slider is linked to the 'dygraphs' selected period


#### Deprecated and defunct

- deprecated `Param` arguments in `ShinyGR()`


#### User-visible changes

- the size of the "Model performance" plot is now adapted if CemaNeige is used or not


#### CRAN-compatibility updates

- `dyplot()` updated to be compatible with 'dygraphs' >= 1.1.1.4 (available only on GitHub)

____________________________________________________________________________________


### 0.1.4.0 Release Notes (2017-07-21)


#### New features

- `ShinyGR()` now use 'dygraphs' devices (except for model perf.)


#### User-visible changes

- `dyplot.default()` now draws precipitation as a true bar plot and not a step plot

____________________________________________________________________________________


### 0.1.3.9 Release Notes (2017-06-22)


#### New features

- added GR5J in the Model diagram of `ShinyGR()`
- it is now possible to choose the objective function to calibrate the model


#### Bug fixes

- NA values can be drawn by `dyplot*()` functions

____________________________________________________________________________________


### 0.1.2.52 Release Notes (2017-06-02)


#### New features

- `shiny.SimGR()` now presents a theme argument that allows to change the stylesheet


#### Deprecated and defunct

- the `shiny.SimGR()` function has been renamed into `ShinyGR()`


#### Bug fixes

- bug fixed in `ShinyGR()` to plot state variables with GRJ


#### User-visible changes

- the `shiny.SimGR()` function has been renamed into `ShinyGR()`

____________________________________________________________________________________


### 0.1.1.20 Release Notes (2017-03-21)


#### User-visible changes

- in `shiny.SimGR()` if the model diagram is plotted, the animation can be run only from the Event slider and no more from the "Period" slider
- `SimGR()` now also returns the `OptionsCrit` value


#### CRAN-compatibility updates

- the package now depends on 'airGR' version 1.0.5.22

____________________________________________________________________________________

	 
### 0.0.3.15 Release Notes (2017-01-19)


#### New features

- it is now possible to calibrate the model in `shiny.SimGR()` and to draw new plots
- `dyplot.default()` gains a `Roller` period argument


#### Deprecated and defunct

- `TypeModelGR()` is now a private function


#### Bug fixes

- bug fixed, it is now possible to run `CalGR()` with `verbose = FALSE`
- bug fixed in `plot.CalGR()` (it does not use anymore a global variable)
