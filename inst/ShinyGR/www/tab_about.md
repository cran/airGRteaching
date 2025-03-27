---
output: 
  html_document: 
    keep_md: true
    self_contained: true
---





<table  width="100%">
<tbody>
  <tr>
  <td width="125"><img src="fig/logo_airGRteaching_CMJN_square_0125x0121.png" width="90%" height="90%"></td>
  <td><h4>
<font color="#0BA6AA">
To obtain help regarding the use of the <strong><font color="#62003C">airGRteaching</font></strong> or <strong><font color="#62003C">airGR</font></strong> packages, or to suggest modifications, send an email to <font color="#62003C"><strong>airGR@inrae.fr</strong></font>
</font>
</h4>
  </tr>
</tbody>
</table>

<br>

<strong><font color="#0BA6AA">airGRteaching</font></strong> is a package developed in the <img src="fig/logo_R_CMJN.svg" height="20"> language devoted to the use of the <strong><font color="#0BA6AA"><a href = 'https://webgr.inrae.fr/en/models/' title='GR models' rel='noopener noreferrer' onclick='window.open(this.href); return false;'>GR</a></font></strong> rainfall-runoff models and the <strong><font color="#0BA6AA"><a href = 'https://webgr.inrae.fr/en/models/snow-model/' title='CemaNeige' rel='noopener noreferrer' onclick='window.open(this.href); return false;'>CemaNeige</a></font></strong> snowmelt and accumulation model by students and teachers.
<br>
<br><strong><font color="#0BA6AA">airGRteaching</font></strong> is an add-on package of the <strong><font color="#0BA6AA"><a href = 'https://hydrogr.github.io/airGR/' title='airGR' rel='noopener noreferrer' onclick='window.open(this.href); return false;'>airGR</a></font></strong> hydrological package.
<br>It simplifies the use of the airGR functionalities as it only requires a basic level of programming.
<br>
<br>
The <strong><font color="#0BA6AA"><a href = 'https://webgr.inrae.fr/en/models/' title='GR models' rel='noopener noreferrer' onclick='window.open(this.href); return false;'>GR</a></font></strong> hydrological models in a few words:

* lumped conceptual rainfall-runoff models (<strong><font color="#0BA6AA"><a href = 'https://webgr.inrae.fr/en/models/annual-hydrologic-model-gr1a/' title='GR1A' rel='noopener noreferrer' onclick='window.open(this.href); return false;'>GR1A</a></font></strong>, <strong><font color="#0BA6AA"><a href = 'https://webgr.inrae.fr/en/models/monthly-model-gr2m/' title='GR2M' rel='noopener noreferrer' onclick='window.open(this.href); return false;'>GR2M</a></font></strong>, <strong><font color="#0BA6AA"><a href = 'https://webgr.inrae.fr/en/models/daily-hydrological-model-gr4j/' title='GR4J' rel='noopener noreferrer' onclick='window.open(this.href); return false;'>GR4J</a></font></strong>, **GR5J**, **GR6J**, **GR4H** and **GR5H**)
* designed with the objective to be as efficient as possible for flow simulation at various time steps (from annual to hourly)
* their structures were developed to have warranted complexity and limited data requirements
* can be applied on a wide range of conditions, including snowy catchments (thanks to the <strong><font color="#0BA6AA"><a href = 'https://webgr.inrae.fr/en/models/snow-model/' title='CemaNeige' rel='noopener noreferrer' onclick='window.open(this.href); return false;'>CemaNeige</a></font></strong> snow model)
<br><br>


#### <strong><font color="#0BA6AA">airGRteaching</font></strong> features

* Only three simple functions for a full modelling exercise:
    + data preparation
    + model calibration
    + model simulation
* Pre-defined graphical plots:
    + static plotting functions
    + mouse events and interactive graphics (using the *dygraphs* JavaScript charting library)
* Graphical user interface based on *Shiny*:
    + interactive flow simulation and plotting with parameters modifications
    + automatic calibration button
    + internal variables evolution graphs
    + time period selection
    + only monthly and daily models are currently available (<strong><font color="#0BA6AA"><a href = 'https://webgr.inrae.fr/en/models/monthly-model-gr2m/' title='GR2M' rel='noopener noreferrer' onclick='window.open(this.href); return false;'>GR2M</a></font></strong>, <strong><font color="#0BA6AA"><a href = 'https://webgr.inrae.fr/en/models/daily-hydrological-model-gr4j/' title='GR4J' rel='noopener noreferrer' onclick='window.open(this.href); return false;'>GR4J</a></font></strong>, **GR5J**, **GR6J** + <strong><font color="#0BA6AA"><a href = 'https://webgr.inrae.fr/en/models/snow-model/' title='CemaNeige' rel='noopener noreferrer' onclick='window.open(this.href); return false;'>CemaNeige</a></font></strong>)
    + a demonstrator of the graphical interface is available for free online on the <strong><font color="#0BA6AA"><a href = 'https://sunshine.inrae.fr/' title='sunshine.inrae.fr' rel='noopener noreferrer' onclick='window.open(this.href); return false;'>Sunshine</a></font></strong> website
    
<font color="#0BA6AA">See the "Get started" tab for examples including <img src="fig/logo_R_CMJN.svg" height="17"> commands.</font>
<br><br>


#### <strong><font color="#0BA6AA">airGR</font></strong> features

- Easy implementation on numerous catchments 
- Data requirements limited to lumped precip., temp. and streamflow time series
- One automatic calibration procedure
- A set of efficiency criteria
- Limited computation times (use of Fortran routines to run the models)
- Pre-defined graphical plots
- Outputs include simulated flow time series and internal variables
- User can implement its own models, efficiency criteria or optimization algorithms
- Possibility to run semi-distributed versions of the models (use facilitated by the <strong><font color="#0BA6AA"><a href = 'https://airgriwrm.g-eau.fr/' title='airGRiwrm' rel='noopener noreferrer' onclick='window.open(this.href); return false;'>airGRiwrm</a></font></strong> package)

<br><center><img src="fig/airGR_graphe_fonctions_EN.svg" width="500"></center>
