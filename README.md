# MuLTI-TEM

Multimodal Layered Transdimensional Inversion of Transient Electromagnetic data with Depth Constraints

(Can be used for all ground-based TEM survey designs, see parameter list below)

The MuLTI-TEM algorithm was developed in Matlab version 2017a, therefore all Matlab codes supplied will work with this version or subsequent Matlab versions. This code can be run on Linux or Windows based platforms. This github repository includes: 

1.	1D and 2D MuLTI-TEM Matlab scripts along with two matlab functions called in the main scripts, “thicknesses_and_priors” and “whichnuclei”,
2.	1D and 2D example dataset used to test MuLTI-TEM 1D and 2D codes,
3.	adapted Leroi forward modelling code (called LEROI_TEM.F90), written in Fortran 95, and it’s corresponding LEROI_TEM mex files used to be called in Matlab within the MuLTI-TEM script, for both Linux and Windows based platforms. The original Leroi code can be downloaded from the CSIRO/AMIRA P223F project at: http://p223suite.sourceforge.net/.

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
