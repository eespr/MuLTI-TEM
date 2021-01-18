# MuLTI-TEM

Multimodal Layered Transdimensional Inversion of Transient Electromagnetic data with Depth Constraints

(Can be used for all ground-based TEM survey designs, see parameter list below)

The MuLTI-TEM algorithm was writted by Siobhan Killingbeck and Phil Livermore in 2019, School of Earth and Environemnt, The University of Leeds. It is based on Matlab code written by Thomas Bodin. MuLTI-TEM was developed in Matlab version 2017a, therefore all Matlab codes supplied will work with this version or subsequent Matlab versions. This code can be run on Linux or Windows based platforms. This github repository includes: 

1. 1D and 2D MuLTI-TEM Matlab scripts 
2. Two matlab functions called in the main scripts, “thicknesses_and_priors” and “whichnuclei”,
3. 1D and 2D example dataset used to test MuLTI-TEM 1D and 2D codes,
4. Adapted Leroi forward modelling code (called LEROI_TEM.F90), written in Fortran 95, and it’s corresponding LEROI_TEM mex files used to be called in Matlab within the MuLTI-TEM script, for both Linux and Windows based platforms. The original Leroi code can be downloaded from the CSIRO/AMIRA P223F project at: http://p223suite.sourceforge.net/.

The mex interface has been tested on CentOS 7, and Windows 7, with Matlab 2017a. Further details and instructions on how to compile and use the LEROI_TEM mex file can be found in the corresponding folders and at https://github.com/cemac/MEX-LEROI_TEM. If working on a Linux based platform the LEROI_TEM mex file created for Linux specifically must be compiled in the Matlab working directory and then the LEROI_TEM function can be called in Matlab. If working on a Windows platform the LEROI_TEM mex file created for windows specifically does not need compiled and can just be called from the Matlab working directory. 

To run the MuLTI-TEM Matlab codes, .m files, the corresponding platform based LEROI_TEM mex file must be in the active Matlab working directory along with the Matlab functions ‘thicknesses_and_priors’ and ‘whichnuclei’ and the input TEM data files.
For the 1D code: The input TEM data files are .mat files with the received voltages saved as a column vector variable called “data” with column 1 and 2 being the start and end times of the time gates in ms, column 3 being the time gate centre in ms and column 4 being the received voltages in nanoVolts. The weighting is determined from the variance of the measured data points, and can be a percentage of the data or inputted as a column vector.

For the 2D code: The input TEM data files are .mat files with the received voltages saved as a cell array variable called “data_2D”. Each cell is populated with a 1D sounding, ordered by acquisition along the line e.g., the first sounding acquired at the start of the line will be in the first cell and the last sounding at the end of the line will be in the last cell. Within each cell, the 1D soundings are column vectors with column 1 being the time gate centre in ms and column 2 being the received voltages in nanoVolts.

IMPORTANT NOTE ABOUT THE FORWARD MODELLING CODE: 

In MuLTI-TEM, the Leroi code has been adapted, from its original code, for ground-based TEM survey designs only.  The TEM survey parameters are defined in MuLTI-TEM as follows (we try to keep each parameter name the same as the original in Leroi where possible):

NCHNL = Number of time gates (normally either 20 or 30, depending on repetition frequency)

REFTYM = Time (in ms) from which TMS or TOPN & TCLS are measured. For example, this could be signal off-time or start of downward ramp.

OFFTYM = time (milliseconds) between end of one pulse and the start of the next pulse (of opposite sign) since a bipolar waveform is assumed (lambda/4). For systems which have a signal which is always on, OFFTIME = 0.

TXON   = digitised time (in milliseconds) of each point in the waveform (set at 4 points). In most cases, TXON(1) = 0, TXON(2) = pulse on-time, TXON(3) = pulse off-time, TXON(4) = REFTYM where TXON(4) - TXON(3) = turn off time e.g., [0.0, 0.001, 1.0492, 1.05];

TXAMP  = transmitter current in amps at time TXON(J), normally signal is normalised e.g., [0.0, 1.0, 1.0, 0.0];

TOPN   = Start times (in ms) of receiver windows, (1x20);

TCLS   = End times (in ms) of receiver windows, (1x20);

SXE    = east coordinate of vertex I for loop position J (m), fixed at 4 vertices e.g., [5, -5, -5, 5]; Note the transmitter is fixed on the ground (Z=0) in this adapted Leroi code, for airborne data more parameters will need to be passed through the mex file to model.

SXN    = north coordinate of vertex I for loop position J (m), fixed at 4 vertices e.g., [5, 5, -5, -5]; Note the transmitter is fixed on the ground (Z=0) in this adapted Leroi code, for airborne data more parameters will need to be passed through the mex file to model.

RXE    = receiver easting (m);

RXN    = receiver northing (m);

RXZ    = receiver z (always be 0 for ground based TEM);

However, the original Leroi code can model almost any EM system and survey design, therefore this main code should be downloaded and adapted to the user’s specific EM survey method. A corresponding mex file will need to be created for the users specific adapted Leroi code, our mex file can be used as a guideline for this process. 

DOI 10.5281/zenodo.3471638

# MuLTI-TEMp

The MuLTI-TEMp algorithm was adapted from the original MuLTI TEM code. The petrophysical (p) version was created in 2021 by Siobhan Killingbeck, Department of Geography & Environmental Management, The University of Waterloo funded by the W. Garfield Weston Foundation as part of the Canadian SEARCHArtic project. 

MuLTI-TEMp additionally derives salinity of subsurface fluids. It uses Archie’s mixing law and Fofonoff and Millard’s 1983 internationally recognized conductivity to salinity  conversion method, outputting salinity in terms of its practical salinity from the Practical Salinity Scale 1978, (Perkin and Lewis, 1980). Archie's law and Fofonoff and Millard’s 1983 conversion are applied to each resistivity-depth model within the ensemble of the posterior distribution to output an additional PDF of practical salinity, provided that a prior distribution of porosity can be reliably estimated, for example, from nearby borehole measurements or detailed seismic velocity analysis. A prior distribution of pressure and temperature is also required for the conductivity to salinity conversion; this can be obtained from borehole measurements or modelling, for example, using a 1D steady-state advection-diffusion model. The code is set up to use a Gaussian distribution to randomly sample each petrophysical parameter (temperature, pressure, cementation factor and porosity) from a prior mean and standard deviation, however this can be changed to a uniform or skewed distribution with depth, depending on the observed data. Note the mixing law can also be easily changed if required. 

ADDITIONAL FUNCTIONS TO BE USED WITH MuLTI-TEMp

* "salinity.m" matlab script written by Edward T Peltzer, MBARI in 2007

ADDITIONAL INPUTS TO BE USED WITH MuLTI-TEMp

* porosity = estimate of porosity model from known data, if you want to calcuate pore fluid salinity.
* sigma_porosity = estimate of porosity uncertainty
* Rfluid = estimate of pore fluid resistivity model from known data e.g., borehole or lake measurements, if you want to calcuate porosity.
* sigma_Rfluid = estimate of pore fluid uncertainty
* cementation = m (cementation factor) Archie's law
* sigma_cementation = estimate of uncertainty in m
* temp = temperature degrees C
* sigma_temp = temperature uncertainty degrees C
* pressure = pressure in dbars
* sigma_pressure = uncertainty in dbars

MULTIPLE BASE FREQUENCIES INVERSION
* MuLTI-TEMp has also been developed to invert for multiple base frequencies. The version: MuLTI-TEMp_III is currently written to invert for THREE base frequencies. In this version, the likelihood of a proposed model is calculated for all three base frequencies and only models with acceptable likelihoods for all base frequencies are added to the chain, and applied in the next iteration as the current model; if a proposed model is not accepted by all frequencies the existing model is retained as the current model.

References: 

Archie GE. (1952) Classification of carbonate reservoir rocks and petrophysical considerations, AAPG Bull., 36, 278–298. https://doi.org/10.1306/3D9343F7-16B1-11D7-8645000102C1865D
Fofonoff NP and Millard RC (1983) Algorithms for the computation of fundamental properties of seawater. Paris, France, UNESCO, 53pp. (UNESCO Technical Papers in Marine Sciences; 44), http://hdl.handle.net/11329/109 
Perkin R and Lewis E (1980). The practical salinity scale 1978: Fitting the data. IEEE Journal of Oceanic Engineering, 5(1), pp.9-16. doi.org/10.1109/JOE.1980.1145441

DOI: 10.5281/zenodo.4446266
