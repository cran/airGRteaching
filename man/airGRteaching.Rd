\name{airGRteaching}
\alias{airGRteaching}
\alias{airGRteaching-package}
\docType{package}
\encoding{UTF-8}


\title{\packageTitle{airGRteaching}}


\description{
airGRteaching is an add-on package to the airGR package that simplifies its use and is teaching oriented.
It allows to use with very low programming skills several lumped rainfall-runoff models (GR4H, GR5H, GR4J, GR5J, GR6J, GR2M and GR1A) and a snow melt and accumulation model (CemaNeige). This package also provides graphical devices to help students to explore data and modelling results.
\cr

The airGRteaching package has been designed to fulfil a major requirement: facilitating the use of the airGR functionalities by students. The names of the functions and their arguments were chosen to this end.

The package is mostly based on three families of functions:
\itemize{
  \item the functions that allow to complete very simply a hydrological modelling exercise;
  \item plotting functions to help students to explore observed data and to interpret the results of calibration and simulation of the GR models;
  \item a function which runs a 'Shiny' graphical user interface that allows for displaying in real-time the impacts of model parameters on hydrographs.
}

This package brings into R the hydrological modelling tools developed at INRAE-Antony (\href{https://webgr.inrae.fr/eng}{Catchment Hydrology research group} of the HYCAR Research Unit, France).
\cr\cr


# --- Modelling Functions

Three functions allow to complete very simply a hydrological modelling exercise:
\itemize{
  \item preparation of data: \code{\link{PrepGR}()};
  \item calibration of the models: \code{\link{CalGR}()};
  \item simulation with the models: \code{\link{SimGR}()}.
}


# --- Plotting Functions

airGRteaching provides two types of plotting functions that allow to produce static (\code{plot}()) or dynamic (\code{\link{dyplot}()}) graphics (incl. mouse events and interactive graphics).
The devices allow to explore observed data and to interpret the results of calibration and simulation of the GR models.
\cr


# --- Graphical user interface

The package also provides the \code{\link{ShinyGR}()} function, which allows to launch a Shiny interface. Thus it is possible to perform:
\itemize{
  \item interactive flow simulations with parameters modifications;
  \item automatic calibration;
  \item display of internal variables evolution;
  \item time period selection.
}

A demonstrator of the graphical interface is available for free online on the \href{https://sunshine.inrae.fr/}{Sunshine} platform.
\cr


# --- Models

The six hydrological models and the snow melt and accumulation model already available in airGR are accessible from airGRteaching. \cr
These models can be called within airGRteaching using the following model names (*: available in the Shiny interface):
\itemize{
  \item \code{GR4H}: four-parameter hourly lumped hydrological model (Mathevet, 2005)
  \item \code{GR5H}: five-parameter hourly lumped hydrological model (Ficchi, 2017; Ficchi \emph{et al.}, 2019)
  \item \code{GR4J}*: four-parameter daily lumped hydrological model (Perrin et al., 2003)
  \item \code{GR5J}*: five-parameter daily lumped hydrological model (Le Moine, 2008)
  \item \code{GR6J}*: six-parameter daily lumped hydrological model (Pushpalatha et al., 2011)
  \item \code{GR2M}*: two-parameter monthly lumped hydrological model (Mouelhi, 2003; Mouelhi et al., 2006a)
  \item \code{GR1A}: one-parameter annual lumped hydrological model (Mouelhi, 2003; Mouelhi et al., 2006b)
  \item \code{CemaNeige}: two-parameter degree-day snow melt and accumulation daily model (combined with GR4H, GR5H, GR4J, GR5J or GR6J) (Valéry et al., 2014)
}

For more information and to get started with the package, you can refer to the vignette (\code{vignette("get_started", package = "airGRteaching")}) and go on the \href{https://hydrogr.github.io/airGRteaching/}{airGRteaching website}.
\cr


# --- References

\itemize{
\item Coron, L., G. Thirel, O. Delaigue, C. Perrin and V. Andréassian (2017). The Suite of Lumped GR Hydrological Models in an R Package. Environmental Modelling and Software, 94, 166–171, \doi{10.1016/j.envsoft.2017.05.002}.
\item Ficchi, A. (2017). An adaptive hydrological model for multiple time-steps: Diagnostics and improvements based on fluxes consistency. PhD thesis, Irstea (Antony), GRNE (Paris), France.
\item Ficchi, A., C. Perrin and V. Andréassian (2019). Hydrological modelling at multiple sub-daily time steps: model improvement via flux-matching. Journal of Hydrology, 575, 1308-1327, \doi{10.1016/j.jhydrol.2019.05.084}.
\item Le Moine, N. (2008). Le bassin versant de surface vu par le souterrain : une voie d'amélioration des performances et du réalisme des modèles pluie-débit ?, PhD thesis (in French), UPMC - Cemagref Antony, Paris, France, 324 pp.
\item Mathevet, T. (2005). Quels modèles pluie-débit globaux pour le pas de temps horaire ? Développement empirique et comparaison de modèles sur un large échantillon de bassins versants, PhD thesis (in French), ENGREF - Cemagref Antony, Paris, France, 463 pp.
\item Mouelhi S. (2003). Vers une chaîne cohérente de modèles pluie-débit conceptuels globaux aux pas de temps pluriannuel, annuel, mensuel et journalier, PhD thesis (in French), ENGREF - Cemagref Antony, Paris, France, 323 pp.
\item Mouelhi, S., C. Michel, C. Perrin and V. Andréassian (2006a). Stepwise development of a two-parameter monthly water balance model, Journal of Hydrology, 318(1-4), 200-214, \doi{10.1016/j.jhydrol.2005.06.014}.
\item Mouelhi, S., C. Michel, C. Perrin. and V. Andreassian (2006b). Linking stream flow to rainfall at the annual time step: the Manabe bucket model revisited, Journal of Hydrology, 328, 283-296, \doi{10.1016/j.jhydrol.2005.12.022}.
\item Perrin, C., C. Michel and V. Andréassian (2003). Improvement of a parsimonious model for streamflow simulation, Journal of Hydrology, 279(1-4), 275-289, \doi{10.1016/S0022-1694(03)00225-7}.
\item Pushpalatha, R., C. Perrin, N. Le Moine, T. Mathevet and V. Andréassian (2011). A downward structural sensitivity analysis of hydrological models to improve low-flow simulation, Journal of Hydrology, 411(1-2), 66-76, \doi{10.1016/j.jhydrol.2011.09.034}.
\item Valéry, A., V. Andréassian and C. Perrin (2014). "As simple as possible but not simpler": What is useful in a temperature-based snow-accounting routine? Part 2 - Sensitivity analysis of the Cemaneige snow accounting routine on 380 catchments, Journal of Hydrology, 517(0): 1176-1187, \doi{10.1016/j.jhydrol.2014.04.058}.
}

}


\concept{hydrology}
\concept{model}
\concept{efficiency criterion}
\concept{calibration}
\concept{GR4J}
\concept{airGR}
\concept{student}
\concept{teaching}
\concept{shiny}
