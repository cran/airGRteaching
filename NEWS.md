## Release History of the airGRteaching Package





### 5 Release Notes (2020-10-19)


#### New features

- in the <code>ShinyGR()</code> interface, there is a new tab panel that shows a summary sheets of basin if the name of the dataset contains the code station (8 characters : 1 letter and 7 numbers) of the Banque Hydro French database (so it is available only for the dataset of this database) ([#10](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/10))


#### User-visible changes

- the <code>theme</code> agument of the <code>ShinyGR()</code> function now works even if a wrong character case


#### CRAN-compatibility updates

- when the package is loaded or when the <code>dyplot()</code> and the <code>ShinyGR()</code> function are used, a message warns the users if they use a version of 'htmlwidgets' < 1.5.2.9000. The latest version of this package, available on GitHub, avoids troubles with the use of dynamic graphics of the 'dygraphs' package (called by the <code>dyplot&#42;()</code> and the <code>ShinyGR()</code> functions) ([#5](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/5))


____________________________________________________________________________________


### 0.2.8.69 Release Notes (2020-02-28)


#### New features

- added <code>as.data.frame.airGRt()</code> method in order to create a <code>data.frame</code> from outputs of <code>PrepGR()</code>, <code>CalGR()</code> and <code>SimGR()</code> functions. This <code>data.frame</code> always presents the same structure and contains observed flow, simulated flow, simulated solid precipitation fraction, etc. When it does not make sense, the concerned column is assigned with <code>NA</code> values (e.g. Qsim with the <code>PrepGR()</code> function)

- a digital object identifier (DOI) now allows to identify the manual of the 'airGRteaching' package. When you use airGRteaching in your work, please always cite both the article and the manual. The last one allows to know the version of the package that is used in order to enhance reproducible research. The references can be displayed with the <code>citation("airGRteaching")</code> command

- two themes of alternative stylesheet are available (<code>"Inrae"</code> and <code>"Saclay"</code>)


#### Deprecated and defunct

- The <code>CalGR</code> argument is now deprecated in the <code>SimGR()</code> function. It has been replaced by the use of the <code>Param</code> argument which can be set by an object of the class <code>CalGR</code> or by a vector of parameters


#### Bug fixes

- <code>ShinyGR()</code> now runs when independent arguments (<code>DatesR</code>, <code>Precip</code>, etc.) are used instead of the <code>ObsDF</code> argument


#### User-visible changes

- it is now possible to use the GR4H and GR5H hourly models with or without CemaNeige. For that, in the <code>PrepGR()</code>, the <code>HydroModel</code> argument could be set to <code>"GR4H"</code> or <code>"GR5H"</code>. In the GUI, launched by <code>ShinyGR()</code> function, nothing changed, only the daily models are available. So, now airGRteaching depends on the version of airGR >= 1.4.3.52) ([#7](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/7))

- it is now possible to run the <code>PrepGR()</code> function when discharge is not provided in <code>Qobs</code>. If it is the case, the <code>CalGR()</code> function will return an error message because it is not possible to calibrate the model. The <code>SimGR()</code> function will return a warning message because it is not possible to compute any efficiency criterion

- it is now possible to run the <code>ShinyGR()</code> function when discharge is not provided in <code>Qobs</code>

- when observed discharge is provided in <code>ShinyGR()</code>, the first plotting panel now draws the flow error time series ([#4](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/4))

- the <code>plot()</code> function is now exported

- the <code>dyplot.PrepGR()</code>, <code>dyplot.CalGR()</code> and <code>dyplot.SimGR()</code> functions are no longer exported

- there is now only one help page for all <code>plot.&#42;()</code> functions (use <code>?plot</code> to call it)


#### CRAN-compatibility updates

- when the package is loaded, a message warns the users if they use a version of 'htmlwidgets' < 1.5.1.9000. The latest version of this package, available on GitHub, avoids troubles with the use of dynamic graphics of the 'dygraphs' package (called by the <code>dyplot&#42;()</code> and the <code>ShinyGR()</code> functions) ([#5](https://gitlab.irstea.fr/HYCAR-Hydro/airgrteaching/-/issues/5))

____________________________________________________________________________________


### 0.2.6.29 Release Notes (2019-05-02)

#### Bug fixes

- <code>ShinyGR()</code> can export the csv table again. It was broken due to modifications to the version 1.2.13.16 of the 'airGR' package


#### User-visible changes

- it is now possible to export the diagram plot of the model from the <code>ShinyGR()</code> interface

____________________________________________________________________________________


### 0.2.6.27 Release Notes (2019-04-23)


#### Bug fixes

- <code>ShinyGR()</code> may now run for any timezone

- <code>ShinyGR()</code> takes into account the fact that on 1 time step <code>airGR::ErrorCrit_KGE</code> do not return CritName (temporary patch)

- <code>ShinyGR()</code> takes into account the fact that the previous simulation could have a missing value criterion

- <code>ShinyGR()</code> takes into account the fact the prevuous Qsim is sometimes to long of one value (temporary patch)


#### User-visible changes

- The WarmUp element returned by <code>CalGR()</code> and <code>SimGR()</code> now presents a timezone defined as UTC

____________________________________________________________________________________


### 0.2.6.14 Release Notes (2019-04-03)


#### CRAN-compatibility updates

- now depends on the latest version (1.2.13.16) of the 'airGR' package: <code>CalGR()</code>, <code>SimGR()</code> and <code>ShinyGR()</code> have been updated

- the 'htmlwidget' package is no longer imported


#### User-visible changes

- the CemaNeige model is now allowed when the model diagram is drawn in <code>ShinyGR()</code>

- .DiagramGR() and <code>.TypeModelGR()</code> are now private functions

- time format of "Period" and "Event" sliders of the <code>ShinyGR()</code> function is now <code>"%Y-%m-%d"</code> with the latest versions of the 'shiny' package (like it was with th old versions)

____________________________________________________________________________________


### 0.2.3.2 Release Notes (2018-08-08)


#### CRAN-compatibility updates

- now depends on the latest version (1.1.1.6) of the 'dygraphs' package from CRAN (embeded 'dygraphs' functions have been removed)


#### User-visible changes

- the article reference is updated

- .DiagramGR() no longer returns errors when inputs are not yet available in <code>ShinyGR()</code>

____________________________________________________________________________________


### 0.2.2.2 Release Notes (2018-03-21)


#### Bug fixes

- bug fixed in <code>ShinyGR()</code>, the criteria values are now right on Unix system


#### User-visible changes

- vignette added

____________________________________________________________________________________


### 0.2.0.9 Release Notes (2018-03-16)


#### CRAN-compatibility updates

- embeding 'dygraphs' functions to avoid user to install the last version of this package from GitHub (import of devtools not necessary)

____________________________________________________________________________________


### 0.1.11.26 Release Notes (2018-02-01)


#### Bug fixes
- bug fixed in <code>ShinyGR()</code> when C1 (or C2) is modified after calibration; the calibration button is now reset

- bug fixed in warm-up, calibration and simulation periods checks in <code>CalGR()</code> and <code>SimGR()</code> functions


#### Deprecated and defunct

- <code>ObsBV</code> argument has been renamed <code>ObsDF</code> in <code>PrepGR()</code> and <code>ShinyGR()</code> functions


#### User-visible changes

- update and homogenization of the unit of time abbreviation in <code>.TypeModelGR()</code> and <code>ShinyGR()</code>

- graphical parameters recorded and executed when the <code>plot.PrepGR()</code>, <code>plot.CalGR()</code> and <code>plot.SimGR()</code> functions exit

____________________________________________________________________________________


### 0.1.10.0 Release Notes (2018-01-30)


#### Deprecated and defunct

- <code>ObsGR()</code> function (and relatives arguments in <code>CalGR()</code> and <code>SimGR()</code> has been renamed PrepGR()

____________________________________________________________________________________


### 0.1.9.29 Release Notes (2018-01-30)


#### Bug fixes

- missing exchange added on exp. store when plotting GR6J model diagram in <code>ShinyGR()</code>

- exp store now appears exported png file of state variables plot in <code>ShinyGR()</code> when GR6J is used

- animate button fixed in <code>ShinyGR()</code>

- bug fixed in <code>ShinyGR()</code> to show previous sim. when model or dataset changes

- bug fixed in <code>ShinyGR()</code> to show previous sim. when time window changes but keeps the same length

- bug fixed to disable calibration when there is no Qobs in <code>ShinyGR()</code>



#### User-visible changes

- it is now possible to draw the model diagram in <code>ShinyGR()</code> using the GR6J model

- exp. store now appears in state variables plot in <code>ShinyGR()</code> when GR6J is used

- update and homogenization of the unit of time abbreviation in <code>.TypeModelGR()</code> and <code>ShinyGR()</code>

- write "< - 99.99" in the criteria table of <code>ShinyGR()</code> when a criterion is very low

- <code>ShinyGR()</code> now allows a list format for <code>ObsBV</code> data.frame and CemaNeige inputs

____________________________________________________________________________________


### 0.1.8.14 Release Notes (2017-11-29)


#### Bug fixes

- <code>ObsGR()</code> function now returns an error if the time zone is not defined as <code>"UTC"</code>

- in <code>ShinyGR()</code> background color defined to black when the Flatly theme is used

- bug fixed in <code>ShinyGR()</code> when inputs are defined in vectors (not in a data.frame)

- Psol et Pliq bars are reversed in <code>dyplot.default()</code>



#### User-visible changes

- new reactive to prepare data for plotting in the <code>ShinyGR()</code> interface

____________________________________________________________________________________


### 0.1.7.6 Release Notes (2017-10-05)


#### User-visible changes

- <code>SimGR()</code> now runs only once to compute all <code>ErrorCrit</code>

- it is now possible to show the table of the last simulation criteria in the <code>ShinyGR()</code> interface

- it is now possible to export state variable plot from the <code>ShinyGR()</code> interface

____________________________________________________________________________________


### 0.1.6.15 Release Notes (2017-09-29)


#### New features

- <code>dyplot()</code> now allows to draw an additional time series of flow


#### Bug fixes

- bug fixed when zooming after changing snow model on plot to see the last simulation in <code>ShinyGR()</code>

- <code>dyplot()</code> now plots Pliq and Psol when CemaNeige is used (<code>dyStackedBarGroup()</code> instead of the plotter argument)


#### User-visible changes

- disable and enable buttons in <code>ShinyGR()</code> interface (using the 'shinyjs' package)

- it is now possible to register the last simulation and draw it on Model diagram of <code>ShinyGR()</code>

____________________________________________________________________________________


### 0.1.5.22 Release Notes (2017-09-14)


#### New features

- it is now possible to export some plots and tables from <code>ShinyGR()</code> interface

- in <code>ShinyGR()</code>, <code>TypeModel</code> inputIds renamed into <code>HydroModel</code>


#### Bug fixes

- period slider is linked to the dygraphs selected period


#### Deprecated and defunct

- deprecated <code>Param</code> arguments in <code>ShinyGR()</code>


#### User-visible changes

- the size of the "Model performance" plot is now adapted if CemaNeige is used or not


#### CRAN-compatibility updates
- <code>dyplot()</code> updated to be compatible with dygraphs >= 1.1.1.4 (available only on GitHub)

____________________________________________________________________________________


### 0.1.4.0 Release Notes (2017-07-21)


#### New features

- <code>ShinyGR()</code> now use dygraph devices (except for model perf.)


#### User-visible changes

- <code>dyplot.default()</code> now draws precipitation as a true bar plot and not a step plot

____________________________________________________________________________________


### 0.1.3.9 Release Notes (2017-06-22)

#### New features

- added GR5J in the Model diagram of <code>ShinyGR()</code>

- it is now possible to choose the objective function to calibrate the model


#### Bug fixes

- NA values can be drawn by <code>dyplot*()</code> functions

____________________________________________________________________________________


### 0.1.2.52 Release Notes (2017-06-02)


#### New features

- <code>shiny.SimGR()</code> now presents a theme argument that allows to change the stylesheet


#### Deprecated and defunct

- the <code>shiny.SimGR()</code> function has been renamed into <code>ShinyGR()</code>


#### Bug fixes

- bug fixed in <code>ShinyGR()</code> to plot state variables with GRJ


#### User-visible changes

- the <code>shiny.SimGR()</code> function has been renamed into <code>ShinyGR()</code>

____________________________________________________________________________________


### 0.1.1.20 Release Notes (2017-03-21)


#### User-visible changes

- in <code>shiny.SimGR()</code> if the model diagram is plotted, the animation can be run only from the Event slider and no more from the "Period" slider

- <code>SimGR()</code> now also returns the <code>OptionsCrit</code> value


#### CRAN-compatibility updates

- the package now depends on 'airGR' version 1.0.5.22

____________________________________________________________________________________

	 
### 0.0.3.15 Release Notes (2017-01-19)


#### New features

- it is now possible to calibrate the model in <code>shiny.SimGR()</code> and to draw new plots

- <code>dyplot.default()</code> gains a <code>Roller</code> period argument


#### Deprecated and defunct

- <code>TypeModelGR()</code> is now a private function


#### Bug fixes

- bug fixed, it is now possible to run <code>CalGR()</code> with <code>verbose = FALSE</code>

- bug fixed in <code>plot.CalGR()</code> (it does not use anymore a global variable)
