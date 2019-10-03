!   PROGRAM Leroi       Version 8.0.    16 January 2008       Language: ANSI Standard Fortran 95
!   ======= =====       ===========     ===============       ----------------------------------
!
!
!            Developed by:  Art Raiche
!                     For:  AMIRA project P223F
!
!    Leroi 8.0 represents a major improvrment in Leroi capabilities.
!    Plates can be in any layer but cannot cross layers.
!
!    Errors, which have been present since the major revision of the P223E version,
!    have been discovered and corrected as part of the major new development of
!    The Green;s function subroutines.
!
!***************************************************************************
!***************************************************************************
!
!    Original source file : Leroi_FM_TEM_subs.f90
!
!    Code modified to be compiled / run from Matlab, via MEX interface
!    circa April 2019
!
!    to compile:
!
!      mex LEROI_TEM.F90
!
!    The code will only work with the type of model input provided.
!    For example, different SURVEY_TYPE values ('1' in provided input) will
!    require additional / differing input values.
!
!***************************************************************************
!***************************************************************************
!
!** RECORD 1:  TITLE - up to 200 characters
!
!** RECORD 2:  TDFD, DO3D, ISYS, PRFL, ISTOP
!
!      TDFD = 1 => time-domain modelling - STANDARD OPTION
!
!==========================================================================
!
!      DO3D = 0  compute layered earth model only
!
!      ISYS = 0 : Standard output processing
!
!      PRFL = 1  prints response in profile mode.
!                Each column contains the response for one channel
!                (or frequency) for all stations on the profile.

!     ISTOP = 0  read the input data and run the specified models.
!   ---------------------------------------------------------------------------------------
!
!         WAVEFORM & WINDOWS FOR NORMAL TIME-DOMAIN SYSTEMS
!         -------------------------------------------------
!
!** RECORD 3 (time-domain):  STEP, NSX, NCHNL, KRXW, REFTYM, OFFTIME
!
!      STEP = 0 : Compute dB/dt for all magnetic dipole receivers.
!
!      NSX =  number of points needed to describe 1/2 cycle of the transmitter
!             waveform.  A bipolar waveform is assumed.  Thus for a system
!             like Sirotem or EM37, NSX = 4, one point each for the start
!             and end of the two ramps.
!             For an ideal step turnoff system set NSX = 1
!
!      NCHNL - number of receiver channels
!
!      KRXW = 1 : receiver channels will be read in terms of start and end
!                 times in ms relative to REFTYM.
!
!      REFTYM - Time (in ms) from which TMS or TOPN & TCLS are measured.  For
!               example, this could be signal off-time or start of downward ramp.
!
!      OFFTIME - time (milliseconds) between end of one pulse and the start of
!                the next pulse (of opposite sign) since a bipolar waveform is
!                assumed.  For systems which have a signal which is always on,
!                OFFTIME = 0.
!
!** RECORD 4.1 (time-domain):  (TXON(J), TXAMP(J), J = 1,NSX)
!
!      TXON(J) = digitised time (in milliseconds)
!                In most cases, TXON(1) = 0, TXON(NSX) = pulse on-time
!
!      TXAMP(J) = transmitter current in amps at time TXON(J)
!
!
!   For KRXW = 1
!** RECORD 4.2 (time-domain):  (TOPN(J), TCLS(J), J=1, NCHNL)
!
!              Start and end times (in ms) of receiver windows.
!              (measured from REFTYM.)
!
!         SURVEY INFORMATION
!         ------------------
!
!** RECORD 5:  SURVEY_TYPE
!
!      SURVEY_TYPE = 1 : GENERAL OPTION for separate setup of transmitter and
!                        receiver arrays.  Open and closed loops are not shape
!                        restricted.   Inductive and galvanic sources &
!                        receivers are permitted.
!                        This would be the correct choice for downhole surveys
!                        using surface loop transmitters or for CSAMT..
!
!===========================================================================
!
!          OUTPUT UNITS DEFINITION FOR ALL SURVEY OPTIONS: if SURVEY_TYPE = 1
!          units can be specified individually for each receiver group.
!          ------------------------------------------------------------------
!
!            UNITS = 1 : volts
!                    2 : millivolts
!                    3 : microvolts
!                    4 : nanovolts
!
!              Only for un-normalised response from magnetic dipole receivers
!
!          ----------------------------------------------
!
!
!  DEFINITION OF MAGNETIC DIPOLE RECEIVER COMPONENT & PLOT CONVENTIONS FOR ALL SURVEY OPTIONS:
!  ------------------------------------------------------------------------------------------
!
!    Define CMP(J), the component selection for magnetic dipole and point electric receivers.
!    For inversion, this will define the data components of Line J that are to be inverted.
!    For modelling, these will govern output for Line J.
!
!    For coincident loop or electric dipole receivers or Sampo, Leroi sets CMP = 1
!
!    In what follows, depending upon the value of IDH:
!
!      X is shorthand for the X, U or N component
!      Y is shorthand for the Y, V or S component
!      Z is shorthand for the Z, A or W component
!
!      CMP(J) =   1 : model or invert on X (U,N) data only for Line(J)
!             =   2 : model or invert on Y (V,S) data only for Line(J)
!             =   3 : model or invert on Z (A,W) data only for Line(J)
!             =  12 : model or invert on X (U,N) and Y (V,S) data for Line(J)
!             =  13 : model or invert on Z (A,W) and X (U,N) data for Line(J)
!             =  23 : model or invert on Z (A,W) and Y (V,S) data for Line(J)
!             = 123 : model or invert on all three Line(J) components
!
!        The order of the integers is unimportant; eg, CMP = 312 will give the
!        same result as 123.
!
!
!          For vertical coaxial dipole surveys,
!              the X component is computed: set CMP = 1
!          For vertical coplanar dipole surveys (broadside),
!             the Y component is computed: set CMP = 2
!          For horizontal coplanar dipole surveys,
!             the Z component is computed: set CMP = 3
!
!    Specify ground or downhole surveys
!
!      IDH = 0 for all surface surveys.
!
!
!          Set IDH = 0 for Surface Receiver Components
!          -------------------------------------------
!
!    The X component is horizontal and lies along the nominated survey line azimuth
!    The Y component is horizontal and lies perpendicular to the survey line azimuth
!    The Z component is vertical.
!
!          PLOT CONVENTION
!          ---------------
!
!      IPLT(J) = 1 : plot response of Line J at receiver location
!              = 2 : plot response of Line J at transmitter-receiver midpoint
!              = 3 : plot response of Line J at transmitter location
!
!      This definition allows the user to specify different plot conventions
!      for each component by defining separate lines (and hence different IPLT)
!      for each component.
!
!
!*******************************************
!
! RECORDS 6-9 for SURVEY_TYPE = 1 : Fixed Tx
!===========================================
!
!** RECORD 6:  NLINES, MRXL, NTX, SOURCE_TYPE, MXVRTX, TXMNT
!
!      NLINES - number of lines of data to be modelled or inverted.
!               For this option, a line of data consists of specifying a
!               single transmitter plus a line of receivers.
!
!               Different RECEIVER types are allowed for different lines
!               but ALL receivers in any line must be of the same type:
!               magnetic dipoles, electric dipoles or rectangular loops.
!
!               The same SOURCE type must be used for all lines in the
!               modelling or inversion project
!
!      MRXL - maximum number of receiver positions per line.
!
!      NTX = number of distinct transmitter positions for source
!            (loop, magnetic dipole, or electric bipole or dipole)
!
!            Note that the same transmitter position can be used for more than one
!            line by combining it with different receiver groups.  For example one
!            line might consist of a transmitter position and a group of surface
!            receivers and another might consist of the same transmitter position
!            and a group of downhole receivers.
!
!            If only one line of receivers is used for every transmitter position,
!            NLINES = NTX
!
!      SOURCE_TYPE = 1 : general loop    - vertex locations will be specified
!
!      MXVRTX - maximum number of vertices for all sources.
!               If SOURCE_TYPE = 3, magnetic dipole, set MXVRTX = 1
!
!      TXMNT = NTRN, number of turns for closed loop source (SOURCE_TYPE = 1) = 1
!
!
!  TRANSMITTER LOCATIONS FOR SURVEY_TYPE = 1
!  ==========================================
!
!** For SOURCE_TYPE = 1 or 2 (closed loop & grounded wire)
!   ------------------------------------------------------
!
!     For each source position, specify the number of vertices followed by the
!     easting, northing of each vertex.
!     Loops are restricted to lie horizontally; ie one depth for each open or closed loop.
!
!     If SOURCE_TYPE = 1, the program will connect the first vertex
!                         to the last to complete the loop.
!                         DON'T SPECIFY THE SAME VERTEX MORE THAN ONCE.
!
!   SPECIFY_TX: DO FOR J = 1 TO NTX
!
!** RECORD 7.J.0: NVRTX(J), TXZ(J)
!
!         NVRTX(J) = number of vertices for transmitter loop J
!           TXZ(J) = elevation of loop J
!             (TXZ > 0 for loops in air
!             (TXZ < 0 for loops below air-earth or air-sea interface)
!
!     SPECIFY_VERTICES: DO FOR I = 1 TO NVRTX
!**** RECORDS 7.J.I: SXE(I,J), SXN(I,J)
!
!       SXE(I,J) = east coordinate of vertex I for loop position J
!       SXN(I,J) = north coordinate of vertex I for loop position J
!
!     END SPECIFY_VERTICES FOR TX POSITION J
!
!     Note that the current will flow in the direction specified
!     by the order of the vertices.  Clockwise order will yield a
!     positive time-domain magnetic field in the loop centre.
!
!   END SPECIFY_TX
!----------------------------------------------------------------------------
!
!   LINE & RECEIVER SPECIFICATION FOR SURVEY_TYPE = 1
!   -------------------------------------------------
!
!     For each Line J:  J = 1, NLINES, RECORDS 8.J and 9.J are read in pairs
!     -------------------------------
!
!** RECORD 8.J.1: LINE(J), IDTX(J), RX_TYPE(J), NRX(J), UNITS(J)
!        LINE(J) - line number for Line J
!        IDTX(J) - transmitter index for Line J; ie, IDTX is an integer from 1 to NTX.
!                  It specifies which of the NTX transmitter positions is to be used
!                  for LINE(J).  In the case where the same transmitter position is
!                  to be used for more than one line of receivers, it is important to
!                  group the lines such that all the lines corresponding to the first
!                  transmitter position are followed by all the lines for the
!                  second position etc.
!
!         NRX(J) = number of receivers for Line J
!
!     RX_TYPE(J) = 1 for magnetic dipole receivers -
!
!
!       UNITS(J) - defined above at end of RECORD 5 description
!
!
!   ---------------------------------------------------------
!** RECORD 8.J.2: (RX_TYPE = 1 only) CMP(J), SV_AZM(J),KNORM(J), IPLT(J), IDH(J), RXMNT(J)
!
!          CMP(J) : Component selection. See definitions below RECORD 5 (SURVEY_TYPE)
!
!       SV_AZM(J) : azimuth of survey line J.  SV_AZM = 0 pointing north.
!                   It is positive clockwise.
!
!           For surface surveys, SV_AZM orients the X & Y components.
!               The X (radial) component lies along SV_AZM
!               The Y (transverse) component is perpendicular to SV_AZM
!               Z = vertical component.
!
!           If IDH = 2, SV_AZM orients the U & V components.
!
!       KNORM(J) = 0 : output is not normalised.  UNITS(J) < 30
!
!       It is not allowed for time-domain dB/dt output.
!
!
!       IPLT(J) = 1 : plot response of Line J at receiver location
!               = 2 : plot response of Line J at transmitter-receiver midpoint
!               = 3 : plot response of Line J at transmitter midpoint
!
!           IDH = 0 : surface receivers
!       RXMNT(J) - dipole receiver moment (area * turns) for Line J
!                  (magnetic dipole only)
!
!
!     SPECIFY_RECEIVER_LOCATIONS
!     --------------------------
!    Under each line 8.J, enter records I = 1, NRX(J) records for each receiver on Line J.
!
!     For magnetic dipole receivers:
!     -----------------------------
!     Enter the easting (RXE), northing (RXN) and ground clearance
!     (positive RXZ = above, negative implies below ground)
!     for each receiver I on Line J.  For downhole processing (IDH > 0)
!     BHDIP(I,J) and BHAZM(I,J) are also required.
!
!     For IDH = 0 (X,Y,Z) processing
!**   RECORD 9.J.I  RXE(I,J), RXN(I,J), RXZ(I,J)
!
!=============================================================================
!
!          Lithology & structure for Leroi & LeroiAir
!          ==========================================
!
!** RECORD 10:  NLAYER, NPLATE, NLITH
!
!      NLAYER - number of layers including basement.
!
!      NPLATE - number of thin plates
!
!       NLITH - number of layer plus plate lithologies.  Any number of
!               lithologies may be defined.  Be careful not to use
!               layer lithologies for plates and vice versa
!
!          DEFINE LITHOLOGIES
!          ------------------
!
!** RECORD 11.1: RES(1), SIG_T(1), RMU(1), REPS(1), CHRG(1), CTAU(1), CFREQ(1)
!** RECORD 11.2: RES(2), SIG_T(2), RMU(2), REPS(2), CHRG(2), CTAU(2), CFREQ(2)
!     .
!
!** RECORD 11.N: RES(N), SIG_T(N), RMU(N), REPS(N), CHRG(N), CTAU(N), CFREQ(N)
!
!           N = NLITH
!      RES(I) - layer resistivity
!    SIG_T(I) - Conductance (conductivity-thickness product)
!      RMU(I) - relative layer magnetic permeability for LITH_INDEX(I)
!     REPS(I) - relative layer dielectric constant (permittivity for LITH_INDEX(I)
!     CHRG(I) - Cole-Cole layer chargeability for LITH_INDEX(I)
!     CTAU(I) - Cole-Cole layer time constant for LITH_INDEX(I)
!    CFREQ(I) - Cole-Cole layer frequency constant for LITH_INDEX(I)
!
!    Default values:  RMU = 1   REPS = 1   CHRG = 0   CTAU = 0   CFREQ = 1
!
!    The default means no magnetic permeability contrast (MU = 4 PI * 10^(-7))
!                      no dielectric constant contrast  (EPSILON = 8.854215E-12)
!                      and no IP effects (no Cole-Cole)
!
!
!     NOTE:  For layers, SIG_T must equal -1
!
!
!          LAYERED EARTH STRUCTURE
!          -----------------------
!
!   (Don't enter 12.1 for a uniform half-space)
!** RECORD 12.1: LITH(1), THK(1)
!** RECORD 12.2: LITH(2), THK(2)
!
!** RECORD 12.NLAYER: LITH (NLAYER)
!
!      LITH(J) = integer which assigns the resistivity and other
!                physical properties from the list of RECORD 10
!                to layer J.
!
!         THK  = thickness of layers above basement
!
!                *********************************************
!                !   DATA ENTRY IS FINISHED IF DO3D = 0,     !
!                *********************************************
!
!=======================================
!                                      =
!     END OF ENTRIES FOR Leroi.cfl     =
!         (Logical unit NR = 3)        =
!                                      =
!                                      =
!=======================================
!==========================================================
!                                                         =
!     END DATA ENTRY DESCRIPTIOM                          =
!                                                         =
!**********************************************************
!
!============================================================================

 MODULE FILTER_COEFFICIENTS
!--------------------------

 IMPLICIT NONE

 INTEGER, PARAMETER :: JNLO=-250, JNHI=150, NDEC_JN=15, QL=SELECTED_REAL_KIND(12,80)
 INTEGER J9
 REAL(KIND=QL) WJ0(JNLO:JNHI), WJ1(JNLO:JNHI), WCOS(-200:99), DELCOS, SHFTJN
 SAVE

!  Filter restored to original LeroiAir 7 February, 2000 (artificial shift removed)

!  J0 filter coefficients computed from the Niels Christensen program, FILCOA
!  for the following parameters:
!
!   ANY =  0      AMY =  0      NDEC = 15       NLO = -250        NHI =  150
!   IOPT = 1   ISHIFT = 0      OMEGA = .3 PI    EPS = 1.0E-12      SC = 3.257209
!      A = 0.162875             DEL0 = 0.14314998               ERROR =  1.4032E-08

 DATA SHFTJN /0.14314998_QL/
 DATA (WJ0(J9), J9= -250,-161)/ &
  2.86608135867D-18,  3.34160553102D-18,  3.89602601168D-18,  4.54243283439D-18,  5.29608785801D-18,  6.17478510356D-18, &
  7.19927087644D-18,  8.39373359283D-18,  9.78637487555D-18,  1.14100754027D-17,  1.33031712306D-17,  1.55103589191D-17, &
  1.80837508313D-17,  2.10841055215D-17,  2.45822622636D-17,  2.86608135867D-17,  3.34160553102D-17,  3.89602601168D-17, &
  4.54243283439D-17,  5.29608785801D-17,  6.17478510356D-17,  7.19927087644D-17,  8.39373359283D-17,  9.78637487555D-17, &
  1.14100754027D-16,  1.33031712306D-16,  1.55103589191D-16,  1.80837508313D-16,  2.10841055215D-16,  2.45822622636D-16, &
  2.86608135867D-16,  3.34160553102D-16,  3.89602601168D-16,  4.54243283439D-16,  5.29608785801D-16,  6.17478510356D-16, &
  7.19927087644D-16,  8.39373359283D-16,  9.78637487555D-16,  1.14100754027D-15,  1.33031712306D-15,  1.55103589191D-15, &
  1.80837508313D-15,  2.10841055215D-15,  2.45822622636D-15,  2.86608135867D-15,  3.34160553102D-15,  3.89602601168D-15, &
  4.54243283439D-15,  5.29608785801D-15,  6.17478510356D-15,  7.19927087644D-15,  8.39373359283D-15,  9.78637487555D-15, &
  1.14100754027D-14,  1.33031712306D-14,  1.55103589191D-14,  1.80837508313D-14,  2.10841055215D-14,  2.45822622636D-14, &
  2.86608135867D-14,  3.34160553102D-14,  3.89602601168D-14,  4.54243283439D-14,  5.29608785801D-14,  6.17478510356D-14, &
  7.19927087644D-14,  8.39373359283D-14,  9.78637487555D-14,  1.14100754027D-13,  1.33031712306D-13,  1.55103589191D-13, &
  1.80837508313D-13,  2.10841055215D-13,  2.45822622636D-13,  2.86608135867D-13,  3.34160553102D-13,  3.89602601168D-13, &
  4.54243283439D-13,  5.29608785801D-13,  6.17478510356D-13,  7.19927087644D-13,  8.39373359283D-13,  9.78637487555D-13, &
  1.14100754027D-12,  1.33031712306D-12,  1.55103589191D-12,  1.80837508313D-12,  2.10841055215D-12,  2.45822622636D-12/
 DATA (WJ0(J9),J9= -160,-71)/ &
  2.86608135867D-12,  3.34160553102D-12,  3.89602601168D-12,  4.54243283439D-12,  5.29608785801D-12,  6.17478510356D-12, &
  7.19927087644D-12,  8.39373359283D-12,  9.78637487555D-12,  1.14100754027D-11,  1.33031712306D-11,  1.55103589191D-11, &
  1.80837508313D-11,  2.10841055215D-11,  2.45822622636D-11,  2.86608135867D-11,  3.34160553102D-11,  3.89602601168D-11, &
  4.54243283439D-11,  5.29608785801D-11,  6.17478510356D-11,  7.19927087644D-11,  8.39373359283D-11,  9.78637487555D-11, &
  1.14100754027D-10,  1.33031712306D-10,  1.55103589191D-10,  1.80837508313D-10,  2.10841055215D-10,  2.45822622636D-10, &
  2.86608135867D-10,  3.34160553102D-10,  3.89602601168D-10,  4.54243283439D-10,  5.29608785801D-10,  6.17478510356D-10, &
  7.19927087644D-10,  8.39373359283D-10,  9.78637487555D-10,  1.14100754027D-09,  1.33031712306D-09,  1.55103589191D-09, &
  1.80837508313D-09,  2.10841055215D-09,  2.45822622636D-09,  2.86608135867D-09,  3.34160553102D-09,  3.89602601168D-09, &
  4.54243283439D-09,  5.29608785801D-09,  6.17478510356D-09,  7.19927087644D-09,  8.39373359283D-09,  9.78637487555D-09, &
  1.14100754027D-08,  1.33031712306D-08,  1.55103589191D-08,  1.80837508313D-08,  2.10841055215D-08,  2.45822622636D-08, &
  2.86608135867D-08,  3.34160553102D-08,  3.89602601168D-08,  4.54243283439D-08,  5.29608785801D-08,  6.17478510356D-08, &
  7.19927087644D-08,  8.39373359283D-08,  9.78637487555D-08,  1.14100754027D-07,  1.33031712306D-07,  1.55103589191D-07, &
  1.80837508313D-07,  2.10841055215D-07,  2.45822622635D-07,  2.86608135866D-07,  3.34160553102D-07,  3.89602601167D-07, &
  4.54243283438D-07,  5.29608785799D-07,  6.17478510354D-07,  7.19927087640D-07,  8.39373359277D-07,  9.78637487545D-07, &
  1.14100754026D-06,  1.33031712304D-06,  1.55103589187D-06,  1.80837508307D-06,  2.10841055205D-06,  2.45822622620D-06/
 DATA (WJ0(J9),J9= -70,19)/ &
  2.86608135842D-06,  3.34160553063D-06,  3.89602601105D-06,  4.54243283340D-06,  5.29608785643D-06,  6.17478510107D-06, &
  7.19927087248D-06,  8.39373358656D-06,  9.78637486561D-06,  1.14100753870D-05,  1.33031712056D-05,  1.55103588795D-05, &
  1.80837507685D-05,  2.10841054221D-05,  2.45822621060D-05,  2.86608133369D-05,  3.34160549143D-05,  3.89602594894D-05, &
  4.54243273495D-05,  5.29608770041D-05,  6.17478485378D-05,  7.19927048056D-05,  8.39373296541D-05,  9.78637388116D-05, &
  1.14100738267D-04,  1.33031687328D-04,  1.55103549604D-04,  1.80837445571D-04,  2.10840955776D-04,  2.45822465035D-04, &
  2.86607886087D-04,  3.34160157229D-04,  3.89601973751D-04,  4.54242289050D-04,  5.29607209800D-04,  6.17476012564D-04, &
  7.19923128912D-04,  8.39367085119D-04,  9.78627543681D-04,  1.14099178031D-03,  1.33029214523D-03,  1.55099630479D-03, &
  1.80831234191D-03,  2.10831111434D-03,  2.45806862870D-03,  2.86583158466D-03,  3.34120966900D-03,  3.89539861933D-03, &
  4.54143849891D-03,  5.29451197347D-03,  6.17228756167D-03,  7.19531268313D-03,  8.38746058912D-03,  9.77643350230D-03, &
  1.13943208262D-02,  1.32782050079D-02,  1.54707967971D-02,  1.80210634703D-02,  2.09847837166D-02,  2.44249145050D-02, &
  2.84115778193D-02,  3.30213524808D-02,  3.83353639832D-02,  4.44353673090D-02,  5.13965627145D-02,  5.92752031985D-02, &
  6.80880607240D-02,  7.77794366644D-02,  8.81696149649D-02,  9.88766639298D-02,  1.09202052802D-01,  1.17971700371D-01, &
  1.23332521049D-01,  1.22530035854D-01,  1.11753240889D-01,  8.62569960973D-02,  4.11899187108D-02, -2.61456504772D-02, &
 -1.11691705121D-01, -1.97411432453D-01, -2.44254055664D-01, -1.95918893763D-01, -1.49300191739D-02,  2.33634698676D-01, &
  3.13582629541D-01, -4.47760615930D-03, -3.86535797015D-01, -3.87589109967D-03,  4.18653972543D-01, -4.16298788795D-01/
 DATA (WJ0(J9),J9= 20,109)/ &
  2.34448877498D-01, -9.52158343728D-02,  3.09020778713D-02, -8.49535839509D-03,  2.06835506815D-03, -4.67185821059D-04, &
  1.02086153218D-04, -2.20830053233D-05,  4.76413760468D-06, -1.02705545675D-06,  2.21421979164D-07, -4.77750910705D-08, &
  1.03340738634D-08, -2.25102276694D-09,  4.99715357680D-10, -1.16500471179D-10,  3.03986897639D-11, -9.72611811870D-12, &
  3.99994042396D-12, -2.00348565820D-12,  1.11608417099D-12, -6.50767639555D-13,  3.86180817012D-13, -2.30659587418D-13, &
  1.38093695980D-13, -8.27455585993D-14,  4.95961642994D-14, -2.97302965597D-14,  1.78224472343D-14, -1.06841897105D-14, &
  6.40498685290D-15, -3.83968417568D-15,  2.30182896520D-15, -1.37991039489D-15,  8.27234374391D-16, -4.95913890248D-16, &
  2.97292643817D-16, -1.78222228351D-16,  1.06841401468D-16, -6.40497544674D-17,  3.83968128138D-17, -2.30182807939D-17, &
  1.37991004842D-17, -8.27234560136D-18,  4.95913797287D-18, -2.97292590016D-18,  1.78222272891D-18, -1.06841382487D-18, &
  6.40497431324D-19, -3.83968224515D-19,  2.30182767120D-19, -1.37990980321D-19,  8.27234414081D-20, -4.95914134387D-20, &
  2.97292537295D-20, -1.78222241286D-20,  1.06841455108D-20, -6.40497317742D-21,  3.83968156424D-21, -2.30182923671D-21, &
  1.37990955793D-21, -8.27234267383D-22,  4.95914046240D-22, -2.97292739490D-22,  1.78222209690D-22, -1.06841436161D-22, &
  6.40497753124D-23, -3.83968088314D-23,  2.30182784256D-23, -1.37991049701D-23,  8.27234475022D-24, -4.95913958682D-24, &
  2.97292559305D-24, -1.78222330828D-24,  1.06841371450D-24, -6.40497639510D-25,  3.83968184851D-25, -2.30182842033D-25, &
  1.37990966066D-25, -8.27234682962D-26,  4.95914083158D-26, -2.97292634049D-26,  1.78222222810D-26, -1.06841489841D-26, &
  6.40497251344D-27, -3.83968281228D-27,  2.30182702533D-27, -1.37991000702D-27,  8.27234181627D-28, -4.95914207635D-28/
 DATA WJ0(110:150)/ &
  2.97292963477D-28, -1.78222420371D-28,  1.06841425086D-28, -6.40497412376D-29,  3.83968377606D-29, -2.30182957681D-29, &
  1.37991153609D-29, -8.27235098582D-30,  4.95914332316D-30, -2.97292528486D-30,  1.78222312353D-30, -1.06841451903D-30, &
  6.40498122076D-31, -3.83968474142D-31,  2.30183015458D-31, -1.37991188353D-31,  8.27234597206D-32, -4.95914031749D-32, &
  2.97292858145D-32, -1.78222357152D-32,  1.06841478804D-32, -6.40498282844D-33,  3.83968570659D-33, -2.30182876031D-33, &
  1.37991104718D-33, -8.27234805187D-34,  4.95914156225D-34, -2.97292932767D-34,  1.78222401887D-34, -1.06841414093D-34, &
  6.40497895409D-35, -3.83968338099D-35,  2.30182933903D-35, -1.37991139355D-35,  8.27235013127D-36, -4.95914281087D-36, &
  2.97292752582D-36, -1.78222294016D-36,  1.06841440910D-36, -6.40498056176D-37,  3.83968434477D-37/

!  J1 filter coefficients computed from the Niels Christensen program, FILCOA
!  for the following parameters:
!
!   ANY =  1      AMY =  0      NDD+C = 15       NLO = -250        NHI =  150
!   IOPT = 1   ISHIFT = 0      OMD+GA = .3 PI    D+PS = 1.0D-12      SC = 3.257209
!      A = 0.162875             DD+L0 = 0.14314998               D+RROR =  1.4032D-08

 DATA (WJ1(J9),J9= -250,-161)/ &
  2.67560875879D-35,  3.63710586576D-35,  4.94412310292D-35,  6.72082533724D-35,  9.13599687416D-35,  1.24190757379D-34, &
  1.68819499732D-34,  2.29485865865D-34,  3.11953078380D-34,  4.24055410750D-34,  5.76442432690D-34,  7.83590704850D-34, &
  1.06517903247D-33,  1.44795792522D-33,  1.96829085937D-33,  2.67560875879D-33,  3.63710586576D-33,  4.94412310292D-33, &
  6.72082533724D-33,  9.13599687416D-33,  1.24190757379D-32,  1.68819499732D-32,  2.29485865865D-32,  3.11953078380D-32, &
  4.24055410750D-32,  5.76442432690D-32,  7.83590704850D-32,  1.06517903247D-31,  1.44795792522D-31,  1.96829085937D-31, &
  2.67560875879D-31,  3.63710586576D-31,  4.94412310292D-31,  6.72082533724D-31,  9.13599687416D-31,  1.24190757379D-30, &
  1.68819499732D-30,  2.29485865865D-30,  3.11953078380D-30,  4.24055410750D-30,  5.76442432690D-30,  7.83590704850D-30, &
  1.06517903247D-29,  1.44795792522D-29,  1.96829085937D-29,  2.67560875879D-29,  3.63710586576D-29,  4.94412310292D-29, &
  6.72082533724D-29,  9.13599687416D-29,  1.24190757379D-28,  1.68819499732D-28,  2.29485865865D-28,  3.11953078380D-28, &
  4.24055410750D-28,  5.76442432690D-28,  7.83590704850D-28,  1.06517903247D-27,  1.44795792522D-27,  1.96829085937D-27, &
  2.67560875879D-27,  3.63710586576D-27,  4.94412310292D-27,  6.72082533724D-27,  9.13599687416D-27,  1.24190757379D-26, &
  1.68819499732D-26,  2.29485865865D-26,  3.11953078380D-26,  4.24055410750D-26,  5.76442432690D-26,  7.83590704850D-26, &
  1.06517903247D-25,  1.44795792522D-25,  1.96829085937D-25,  2.67560875879D-25,  3.63710586576D-25,  4.94412310292D-25, &
  6.72082533724D-25,  9.13599687416D-25,  1.24190757379D-24,  1.68819499732D-24,  2.29485865865D-24,  3.11953078380D-24, &
  4.24055410750D-24,  5.76442432690D-24,  7.83590704850D-24,  1.06517903247D-23,  1.44795792522D-23,  1.96829085937D-23/
 DATA (WJ1(J9),J9= -160,-71)/ &
  2.67560875879D-23,  3.63710586576D-23,  4.94412310292D-23,  6.72082533724D-23,  9.13599687416D-23,  1.24190757379D-22, &
  1.68819499732D-22,  2.29485865865D-22,  3.11953078380D-22,  4.24055410750D-22,  5.76442432690D-22,  7.83590704850D-22, &
  1.06517903247D-21,  1.44795792522D-21,  1.96829085937D-21,  2.67560875879D-21,  3.63710586576D-21,  4.94412310292D-21, &
  6.72082533724D-21,  9.13599687416D-21,  1.24190757379D-20,  1.68819499732D-20,  2.29485865865D-20,  3.11953078380D-20, &
  4.24055410750D-20,  5.76442432690D-20,  7.83590704850D-20,  1.06517903247D-19,  1.44795792522D-19,  1.96829085937D-19, &
  2.67560875879D-19,  3.63710586576D-19,  4.94412310292D-19,  6.72082533724D-19,  9.13599687416D-19,  1.24190757379D-18, &
  1.68819499732D-18,  2.29485865865D-18,  3.11953078380D-18,  4.24055410750D-18,  5.76442432690D-18,  7.83590704850D-18, &
  1.06517903247D-17,  1.44795792522D-17,  1.96829085937D-17,  2.67560875879D-17,  3.63710586576D-17,  4.94412310292D-17, &
  6.72082533724D-17,  9.13599687416D-17,  1.24190757379D-16,  1.68819499732D-16,  2.29485865865D-16,  3.11953078380D-16, &
  4.24055410750D-16,  5.76442432690D-16,  7.83590704850D-16,  1.06517903247D-15,  1.44795792522D-15,  1.96829085937D-15, &
  2.67560875879D-15,  3.63710586576D-15,  4.94412310292D-15,  6.72082533724D-15,  9.13599687416D-15,  1.24190757379D-14, &
  1.68819499732D-14,  2.29485865865D-14,  3.11953078380D-14,  4.24055410750D-14,  5.76442432690D-14,  7.83590704849D-14, &
  1.06517903247D-13,  1.44795792522D-13,  1.96829085938D-13,  2.67560875878D-13,  3.63710586577D-13,  4.94412310288D-13, &
  6.72082533728D-13,  9.13599687406D-13,  1.24190757380D-12,  1.68819499729D-12,  2.29485865868D-12,  3.11953078372D-12, &
  4.24055410758D-12,  5.76442432666D-12,  7.83590704871D-12,  1.06517903240D-11,  1.44795792527D-11,  1.96829085917D-11/
 DATA (WJ1(J9),J9= -70,19)/ &
  2.67560875891D-11,  3.63710586515D-11,  4.94412310317D-11,  6.72082533541D-11,  9.13599687462D-11,  1.24190757324D-10, &
  1.68819499736D-10,  2.29485865695D-10,  3.11953078363D-10,  4.24055410221D-10,  5.76442432542D-10,  7.83590703194D-10, &
  1.06517903172D-09,  1.44795791998D-09,  1.96829085611D-09,  2.67560874206D-09,  3.63710585268D-09,  4.94412304898D-09, &
  6.72082528725D-09,  9.13599669890D-09,  1.24190755523D-08,  1.68819493996D-08,  2.29485859113D-08,  3.11953059487D-08, &
  4.24055386543D-08,  5.76442370102D-08,  7.83590618983D-08,  1.06517882412D-07,  1.44795762309D-07,  1.96829016283D-07, &
  2.67560770231D-07,  3.63710352883D-07,  4.94411942636D-07,  6.72081747305D-07,  9.13598412795D-07,  1.24190492063D-06, &
  1.68819059152D-06,  2.29484968860D-06,  3.11951559104D-06,  4.24052372735D-06,  5.76437203602D-06,  7.83580400571D-06, &
  1.06516106220D-05,  1.44792293329D-05,  1.96822917833D-05,  2.67548981332D-05,  3.63689436167D-05,  4.94371845248D-05, &
  6.72010067340D-05,  9.13461935181D-05,  1.24165945005D-04,  1.68772580859D-04,  2.29400955289D-04,  3.11793204874D-04, &
  4.23764974965D-04,  5.75897507579D-04,  7.82597702990D-04,  1.06332133421D-03,  1.44456435715D-03,  1.96195766368D-03, &
  2.66401748131D-03,  3.61551958902D-03,  4.90456094796D-03,  6.64729428357D-03,  9.00112880743D-03,  1.21689223295D-02, &
  1.64231258930D-02,  2.20996958736D-02,  2.96400942278D-02,  3.95385050500D-02,  5.24078149405D-02,  6.87615215337D-02, &
  8.91013723344D-02,  1.13192375541D-01,  1.40192739735D-01,  1.66618485339D-01,  1.87030308669D-01,  1.89612379729D-01, &
  1.61380285157D-01,  8.29859362099D-02, -4.46335736689D-02, -2.01737898138D-01, -2.84006740802D-01, -1.90854624427D-01, &
  1.45861570853D-01,  3.42338340245D-01,  5.72930699760D-02, -4.71068534718D-01,  2.63969067746D-01,  8.25956507901D-02/
 DATA (WJ1(J9),J9= 20,109)/ &
 -2.22236420794D-01,  2.04428998525D-01, -1.44401888321D-01,  9.24618900674D-02, -5.69896615248D-02,  3.45697730305D-02, &
 -2.08227940873D-02,  1.25054653306D-02, -7.50178808640D-03,  4.49828025678D-03, -2.69688071237D-03,  1.61678766116D-03, &
 -9.69249547051D-04,  5.81052166908D-04, -3.48332124427D-04,  2.08819730575D-04, -1.25184162926D-04,  7.50459390809D-05, &
 -4.49888596104D-05,  2.69701130091D-05, -1.61681580285D-05,  9.69255610555D-06, -5.81053473294D-06,  3.48332405883D-06, &
 -2.08819791213D-06,  1.25184175990D-06, -7.50459418954D-07,  4.49888602168D-07, -2.69701131398D-07,  1.61681580566D-07, &
 -9.69255611161D-08,  5.81053473425D-08, -3.48332405911D-08,  2.08819791219D-08, -1.25184175991D-08,  7.50459418957D-09, &
 -4.49888602168D-09,  2.69701131398D-09, -1.61681580566D-09,  9.69255611161D-10, -5.81053473425D-10,  3.48332405911D-10, &
 -2.08819791219D-10,  1.25184175991D-10, -7.50459418957D-11,  4.49888602168D-11, -2.69701131398D-11,  1.61681580566D-11, &
 -9.69255611161D-12,  5.81053473425D-12, -3.48332405911D-12,  2.08819791219D-12, -1.25184175991D-12,  7.50459418957D-13, &
 -4.49888602168D-13,  2.69701131398D-13, -1.61681580566D-13,  9.69255611161D-14, -5.81053473425D-14,  3.48332405911D-14, &
 -2.08819791219D-14,  1.25184175991D-14, -7.50459418957D-15,  4.49888602168D-15, -2.69701131398D-15,  1.61681580566D-15, &
 -9.69255611161D-16,  5.81053473425D-16, -3.48332405911D-16,  2.08819791219D-16, -1.25184175991D-16,  7.50459418957D-17, &
 -4.49888602168D-17,  2.69701131398D-17, -1.61681580566D-17,  9.69255611161D-18, -5.81053473425D-18,  3.48332405911D-18, &
 -2.08819791219D-18,  1.25184175991D-18, -7.50459418957D-19,  4.49888602168D-19, -2.69701131398D-19,  1.61681580566D-19, &
 -9.69255611161D-20,  5.81053473425D-20, -3.48332405911D-20,  2.08819791219D-20, -1.25184175991D-20,  7.50459418957D-21/
 DATA WJ1(110:150)/ &
 -4.49888602168D-21,  2.69701131398D-21, -1.61681580566D-21,  9.69255611161D-22, -5.81053473425D-22,  3.48332405911D-22, &
 -2.08819791219D-22,  1.25184175991D-22, -7.50459418957D-23,  4.49888602168D-23, -2.69701131398D-23,  1.61681580566D-23, &
 -9.69255611161D-24,  5.81053473425D-24, -3.48332405911D-24,  2.08819791219D-24, -1.25184175991D-24,  7.50459418957D-25, &
 -4.49888602168D-25,  2.69701131398D-25, -1.61681580566D-25,  9.69255611161D-26, -5.81053473425D-26,  3.48332405911D-26, &
 -2.08819791219D-26,  1.25184175991D-26, -7.50459418957D-27,  4.49888602168D-27, -2.69701131398D-27,  1.61681580566D-27, &
 -9.69255611161D-28,  5.81053473425D-28, -3.48332405911D-28,  2.08819791219D-28, -1.25184175991D-28,  7.50459418957D-29, &
 -4.49888602168D-29,  2.69701131398D-29, -1.61681580566D-29,  9.69255611161D-30, -5.81053473425D-30/

!  Niels Christensen shifted cosine filter:
!  12 points per decade, OMEGA = .3 PI

  DATA DELCOS /.00632173D0 /
  DATA (WCOS (J9), J9 = -200, -21)/ &
  3.27764748749D-18,  3.97096058632D-18,  4.81092858166D-18,  5.82857304036D-18,  7.06147744874D-18,  8.55517523993D-18, &
  1.03648314276D-17,  1.25572799515D-17,  1.52134919784D-17,  1.84315663162D-17,  2.23303523839D-17,  2.70538395400D-17, &
  3.27764748749D-17,  3.97096058632D-17,  4.81092858166D-17,  5.82857304036D-17,  7.06147744874D-17,  8.55517523993D-17, &
  1.03648314276D-16,  1.25572799515D-16,  1.52134919784D-16,  1.84315663162D-16,  2.23303523839D-16,  2.70538395400D-16, &
  3.27764748749D-16,  3.97096058632D-16,  4.81092858166D-16,  5.82857304036D-16,  7.06147744874D-16,  8.55517523993D-16, &
  1.03648314276D-15,  1.25572799515D-15,  1.52134919784D-15,  1.84315663162D-15,  2.23303523839D-15,  2.70538395400D-15, &
  3.27764748749D-15,  3.97096058632D-15,  4.81092858166D-15,  5.82857304036D-15,  7.06147744874D-15,  8.55517523993D-15, &
  1.03648314276D-14,  1.25572799515D-14,  1.52134919784D-14,  1.84315663162D-14,  2.23303523839D-14,  2.70538395400D-14, &
  3.27764748749D-14,  3.97096058632D-14,  4.81092858166D-14,  5.82857304036D-14,  7.06147744874D-14,  8.55517523993D-14, &
  1.03648314276D-13,  1.25572799515D-13,  1.52134919784D-13,  1.84315663162D-13,  2.23303523839D-13,  2.70538395400D-13, &
  3.27764748749D-13,  3.97096058632D-13,  4.81092858166D-13,  5.82857304036D-13,  7.06147744874D-13,  8.55517523993D-13, &
  1.03648314276D-12,  1.25572799515D-12,  1.52134919784D-12,  1.84315663162D-12,  2.23303523839D-12,  2.70538395400D-12, &
  3.27764748749D-12,  3.97096058632D-12,  4.81092858166D-12,  5.82857304036D-12,  7.06147744874D-12,  8.55517523993D-12, &
  1.03648314276D-11,  1.25572799515D-11,  1.52134919784D-11,  1.84315663162D-11,  2.23303523839D-11,  2.70538395400D-11, &
  3.27764748749D-11,  3.97096058632D-11,  4.81092858166D-11,  5.82857304036D-11,  7.06147744874D-11,  8.55517523993D-11, &
  1.03648314276D-10,  1.25572799515D-10,  1.52134919784D-10,  1.84315663162D-10,  2.23303523839D-10,  2.70538395400D-10, &
  3.27764748749D-10,  3.97096058632D-10,  4.81092858166D-10,  5.82857304036D-10,  7.06147744874D-10,  8.55517523993D-10, &
  1.03648314276D-09,  1.25572799515D-09,  1.52134919784D-09,  1.84315663162D-09,  2.23303523839D-09,  2.70538395400D-09, &
  3.27764748749D-09,  3.97096058632D-09,  4.81092858166D-09,  5.82857304036D-09,  7.06147744874D-09,  8.55517523993D-09, &
  1.03648314276D-08,  1.25572799515D-08,  1.52134919784D-08,  1.84315663162D-08,  2.23303523839D-08,  2.70538395400D-08, &
  3.27764748749D-08,  3.97096058632D-08,  4.81092858166D-08,  5.82857304036D-08,  7.06147744874D-08,  8.55517523992D-08, &
  1.03648314276D-07,  1.25572799515D-07,  1.52134919784D-07,  1.84315663162D-07,  2.23303523839D-07,  2.70538395400D-07, &
  3.27764748748D-07,  3.97096058631D-07,  4.81092858163D-07,  5.82857304032D-07,  7.06147744866D-07,  8.55517523979D-07, &
  1.03648314273D-06,  1.25572799511D-06,  1.52134919777D-06,  1.84315663149D-06,  2.23303523815D-06,  2.70538395358D-06, &
  3.27764748674D-06,  3.97096058499D-06,  4.81092857928D-06,  5.82857303614D-06,  7.06147744122D-06,  8.55517522657D-06, &
  1.03648314038D-05,  1.25572799093D-05,  1.52134919033D-05,  1.84315661826D-05,  2.23303521464D-05,  2.70538391177D-05, &
  3.27764741237D-05,  3.97096045276D-05,  4.81092834413D-05,  5.82857261799D-05,  7.06147669760D-05,  8.55517390427D-05, &
  1.03648290523D-04,  1.25572757278D-04,  1.52134844670D-04,  1.84315529598D-04,  2.23303286305D-04,  2.70537973035D-04, &
  3.27763997594D-04,  3.97094723005D-04,  4.81090482791D-04,  5.82853080445D-04,  7.06140233231D-04,  8.55504167951D-04, &
  1.03645938870D-03,  1.25568576016D-03,  1.52127408052D-03,  1.84302307509D-03,  2.23279769616D-03,  2.70496162210D-03/

  DATA (WCOS (J9), J9= -20, 99)/ &
  3.27689631886D-03,  3.96962511374D-03,  4.80855324839D-03,  5.82435024343D-03,  7.05396657593D-03,  8.54182367870D-03, &
  1.03410843805D-02,  1.25150721296D-02,  1.51384287367D-02,  1.82981828574D-02,  2.20932012652D-02,  2.66326428704D-02, &
  3.20280504750D-02,  3.83817031757D-02,  4.57529090015D-02,  5.41138165506D-02,  6.32336060872D-02,  7.25429239280D-02, &
  8.07814005943D-02,  8.56648215301D-02,  8.29754131995D-02,  6.61728839009D-02,  2.49099879313D-02, -5.25662370332D-02, &
 -1.77257695902D-01, -3.38275600250D-01, -4.82415902998D-01, -4.55992280486D-01, -7.52812327135D-02,  6.65970979261D-01, &
  8.99170503986D-01, -3.96592370781D-01, -1.38198747238D+00,  1.66395693227D+00, -9.30334922154D-01,  3.30012032268D-01, &
 -8.19311720454D-02,  1.48662188728D-02, -2.13960121462D-03,  2.89777944084D-04, -4.10252655190D-05,  5.96303531789D-06, &
 -8.72916816254D-07,  1.28031659199D-07, -1.87886052472D-08,  2.75763186999D-09, -4.04758530392D-10,  5.94101668614D-11, &
 -8.72020580969D-12,  1.27995006152D-12, -1.87869546474D-13,  2.75750390141D-14, -4.04729332639D-15,  5.94004630834D-16, &
 -8.70764639675D-17,  1.27459963186D-17, -1.82944370627D-18,  2.67836880337D-19, -3.04833935943D-20,  1.64313000801D-21, &
  3.01142825752D-21, -5.21478596825D-22,  1.37002813677D-21, -6.52797182652D-22,  1.40079856288D-22, -1.40667671784D-22, &
  1.70033730143D-23, -2.74453364807D-23,  2.41787117103D-23, -1.78716987481D-23,  4.99883433782D-24, -4.06084044984D-24, &
  2.89670334941D-24, -8.77965537372D-25,  1.21194987045D-25, -1.74181776862D-25,  1.50307641169D-25, -1.09826064382D-25, &
  3.14586965779D-26, -2.51308231025D-26,  1.77594485992D-26, -1.17543940755D-26,  8.42024121640D-28, -1.10510759608D-27, &
  9.31619291992D-28, -6.75339996352D-28,  1.97531071217D-28, -1.55371775135D-28,  1.08953022579D-28, -7.17780762223D-29, &
  2.55398099963D-29, -6.99012347840D-30,  5.76787420019D-30, -4.15016624873D-30,  1.23507827864D-30, -9.59703688264D-31, &
  6.68070421281D-31, -4.37770918800D-31,  1.57257106203D-31, -1.06708053061D-31,  3.57322505765D-32, -2.54887457918D-32, &
  7.72541668811D-33, -5.92277283725D-33,  4.09438835539D-33, -1.32259081936D-33,  1.67919911757D-33, -2.76812163102D-34, &
  2.21131777864D-34,  5.28010221339D-35,  1.03429563330D-34, -7.40916006860D-36,  9.72409086858D-36, -8.19752817047D-36, &
 -2.58911797964D-36, -3.98829026336D-36,  1.78104494324D-37, -3.32579083872D-37,  3.00732538418D-37, -2.24730545742D-37/

 END MODULE FILTER_COEFFICIENTS

 MODULE FREQUENCY_SELECT
!-----------------------

 IMPLICIT NONE
 INTEGER, PARAMETER :: NF_MD2=46, NF_6PDE=67
 REAL FRQ_MD2(NF_MD2), FRQ_6PDE(NF_6PDE)
 SAVE

!  FRQ_MD2 has 3 frequencies / decade from 0.001 to 1 Hz &
!              6 frequencies / decade from 1.0 to 1MHz

 DATA FRQ_MD2(1:NF_MD2)/ &
  0.10000000E-02, 0.21544347E-02, 0.46415888E-02, 0.10000000E-01, 0.21544347E-01, 0.46415888E-01, &
  0.10000000E+00, 0.21544347E+00, 0.46415888E+00, 0.10000000E+01, 0.14677993E+01, 0.21544347E+01, &
  0.31622777E+01, 0.46415888E+01, 0.68129207E+01, 0.10000000E+02, 0.14677993E+02, 0.21544347E+02, &
  0.31622777E+02, 0.46415888E+02, 0.68129207E+02, 0.10000000E+03, 0.14677993E+03, 0.21544347E+03, &
  0.31622777E+03, 0.46415888E+03, 0.68129207E+03, 0.10000000E+04, 0.14677993E+04, 0.21544347E+04, &
  0.31622777E+04, 0.46415888E+04, 0.68129207E+04, 0.10000000E+05, 0.14677993E+05, 0.21544347E+05, &
  0.31622777E+05, 0.46415888E+05, 0.68129207E+05, 0.10000000E+06, 0.14677993E+06, 0.21544347E+06, &
  0.31622777E+06, 0.46415888E+06, 0.68129207E+06, 0.10000000E+07/

!  FRQ_6PDE has 6 frequencies / decade from 0.001 to 100 MHz

 DATA FRQ_6PDE(1:NF_6PDE)/ &
  0.10000000E-02, 0.14677993E-02, 0.21544347E-02, 0.31622777E-02, 0.46415888E-02, 0.68129207E-02, &
  0.10000000E-01, 0.14677993E-01, 0.21544347E-01, 0.31622777E-01, 0.46415888E-01, 0.68129207E-01, &
  0.10000000E+00, 0.14677993E+00, 0.21544347E+00, 0.31622777E+00, 0.46415888E+00, 0.68129207E+00, &
  0.10000000E+01, 0.14677993E+01, 0.21544347E+01, 0.31622777E+01, 0.46415888E+01, 0.68129207E+01, &
  0.10000000E+02, 0.14677993E+02, 0.21544347E+02, 0.31622777E+02, 0.46415888E+02, 0.68129207E+02, &
  0.10000000E+03, 0.14677993E+03, 0.21544347E+03, 0.31622777E+03, 0.46415888E+03, 0.68129207E+03, &
  0.10000000E+04, 0.14677993E+04, 0.21544347E+04, 0.31622777E+04, 0.46415888E+04, 0.68129207E+04, &
  0.10000000E+05, 0.14677993E+05, 0.21544347E+05, 0.31622777E+05, 0.46415888E+05, 0.68129207E+05, &
  0.10000000E+06, 0.14677993E+06, 0.21544347E+06, 0.31622777E+06, 0.46415888E+06, 0.68129207E+06, &
  0.10000000E+07, 0.14677993E+07, 0.21544347E+07, 0.31622777E+07, 0.46415888E+07, 0.68129207E+07, &
  0.10000000E+08, 0.14677993E+08, 0.21544347E+08, 0.31622777E+08, 0.46415888E+08, 0.68129207E+08, &
  0.10000000E+09/

 END MODULE FREQUENCY_SELECT

 MODULE INPUT_DATA_FOR_LEROI
!--------------------------

!** CONTAINS: READ_SYSTEM_DATA, READ_MODEL_DATA, SET_FRQ

 IMPLICIT NONE

! SYSTEM & LITHOLOGY DIMENSIONS
! -----------------------------

 INTEGER, PARAMETER :: NPROP=7, QL=SELECTED_REAL_KIND(12,80)
 REAL, PARAMETER :: PI=3.141592654, PI2=PI/2., R2D=180./PI,  D2R=PI/180.
 INTEGER FVN,NR,NW,ND,NLG,NRI,NW1,MSG,MXERR,DO3D,TDFD,IPPD,STEP,NSX,PRFL,ISTOP,KRXW, &
         MCHNL,NCHNL,NFRQ,NFT,CMPMT(4),KMPMT(4),MCMP,KHSQ,SOURCE_TYPE,SURVEY_TYPE,   &
         NLINES,MLINES,NTX,MXVRTX,MQVR,MXRS,ISYS,KTX,K1,MXTX,NTXL,J,JS,JT,JF,JV,JR,  &
         MRXTX,MRXL,NLITH,NPULS,NTYRP,NTYPLS,NPPD,MD1,MD2,NHID
 INTEGER, ALLOCATABLE, DIMENSION(:) :: LINE,IPLT,IDH,NVRTX,UNITS,KNORM,NRX,RX_TYPE,CMP,NRGTX,NRXTX, &
                                       HEADER_ID,KHID
 INTEGER, ALLOCATABLE, DIMENSION(:,:)  :: KRGTX,RXID,KNORM2,NCTD,LNTR
 REAL MIN_FREQ,MAX_FREQ,T0,T0SX,OFFTYM,REFTYM,PULSE,RXFMNT,TXFREQ,TXMNT,ZMAX,ZMIN,SV_AZM
 REAL, ALLOCATABLE, DIMENSION(:) :: TXON,WAVEFORM,CURNT,TRP,TMS,WTMS,TOPN,TCLS,FREQ,SWX,TXZ,SXZ,SVAZM,SDZ0, &
                                    TXLNGTH,TXWDTH,TXCLN,TXAZM,SXDIP,SXAZM,RHOTRP,RXMNT,RXOFF,RXOTX
 REAL, ALLOCATABLE, DIMENSION(:,:) :: SWY,LYTH,SXE,SXN,ZRXTX,RXDIP,RXAZM,BHDIP,BHAZM,RXZ,XRXOF,YRXOF,ZRXOF,DSTAT
 REAL, ALLOCATABLE, DIMENSION(:,:,:) :: XRXTX,YRXTX,RXE,RXN
 REAL, ALLOCATABLE :: BFTL(:,:,:,:)
 REAL(KIND=QL) ECNTRD,NCNTRD,QD,QFRQ1,QFRQ2,FQQ
 REAL(KIND=QL), ALLOCATABLE :: SDN0(:),SDE0(:)
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:,:)   :: SXED,SXND,CLCD
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:,:,:) :: YXZPLT,RXED,RXND
 LOGICAL PRTSEC, INVERT
 CHARACTER (LEN=200) INP,TITLE
 CHARACTER(LEN=60) PVC
 CHARACTER(LEN=10) TIME,DATE,ZONE
 CHARACTER(LEN=3) MONTH(12)
 DATA NR,NW,ND,NLG,NRI,NW1 /3,4,7,9,13,14/
 DATA MONTH /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/
 DATA PVC, FVN /'Leroi - Version 8.0   16 January 2008',690/

! Inversion specific parameters
! -----------------------------
 INTEGER KPRT,NDATA,KCHNL,MAXITS,CNVRG,INVPRT
 INTEGER, ALLOCATABLE :: RWTS(:,:,:,:)
 INTEGER, ALLOCATABLE, DIMENSION(:) :: CXPAR,NCMPL,KMP,PLYR
 REAL PCTCNV,PARPCT
 REAL, ALLOCATABLE, DIMENSION(:) :: XDATA,XMODL,UBND,LBND,ELAS
 REAL, ALLOCATABLE :: RDATA(:,:,:,:)
 LOGICAL, ALLOCATABLE :: SINGLE(:)

! Specific parameters for Leroi
! ------------------------------
 INTEGER NLYR,NPLT,JL,JP,MXAB,BEG,FIN,JA,JB,JAB,NAB,NAJ,NBJ,JP2,JAB2, &
         NAB2,MXRHO,NRMGT,MXCL2,NPAR,NDSTP
 INTEGER,ALLOCATABLE, DIMENSION(:) :: LITHP,KPCT
 REAL CELLW
 REAL, ALLOCATABLE, DIMENSION(:) :: RES,THK,RMU,REPS,CHRG,CTAU,CFREQ,SIG_T,CHRGP,CTAUP, &
                                    CFREQP,PLTOP,XCNTR,YCNTR,PLNGTH,PLWDTH,DZM,PLAZM,PLDIP,  &
                                    DIP,PLG,PLUNJ,MPAR
 REAL, ALLOCATABLE, DIMENSION(:,:) :: XCELL,YCELL,ZCELL
 REAL(KIND=QL),ALLOCATABLE, DIMENSION(:) :: RMUD,THKD
 LOGICAL INTRUDE

 CONTAINS

   SUBROUTINE READ_SYSTEM_AND_SURVEY_DATA(MXTDFD, MXDO3D, MXISYS, MXPRFL, MXISTOP,                &
                                          MXSTEP, MXNSX, MXNCHNL, MXKRXW, MXREFTYM, MXOFFTYM,     &
                                          MXTXON, MXWAVEFORM,                                     &
                                          MXSURVEY_TYPE,                                          &
                                          MXNLINES, MXMRXL, MXNTX, MXSOURCE_TYPE, MXMXVRTX, MXA1, &
                                          MXNVRTX, MXTXZ,                                         &
                                          MXSXED, MXSXND,                                         &
                                          MXLINE, MXLNTR, MXRX_TYPE, MXNRX, MXUNITS,              &
                                          MXCMP, MXSV_AZM, MXKNORM, MXIPLT, MXIDH, MXRXMNT,       &
                                          MXQD1, MXQD2, MXRXZ,                                    &
                                          MXTOPN, MXTCLS)

!  --------------------------------------

!***  Called by MAIN
!***  Calls CUBSPL, CALL CONFIG_ID, WRITE_LOG_FILE


! DESCRIPTION OF SURVEY VARIABLES REQUIRED FOR MODELLING ENGINES
! --------------------------------------------------------------

!    SURVEY_TYPE = 1 : general survey
!                = 2 : moving rectangular loop Tx with fixed offset MD Rx
!                = 3 : magnetic dipole Tx with fixed offset MD Rx
!                = 4 : coincident loop
!                = 5 : borehole MD Tx with constant offset Rx
!
!    SOURCE_TYPE = 1 : general loop
!                = 2 : grounded wire
!                = 3 : magnetic dipole
!                = 4 : coincident loop
!
!            NTX = number of transmitters specified
!       NVRTX(J) = number of vertices for transmitter J
!         MXVRTX - maximum number of vertices for any transmitter
!       SXE(K,J) = local east coordinate of vertex K for loop position J (taken from SXED)
!       SXN(K,J) = local coordinate of vertex K for loop position J
!       TXZ(J)   = ground clearance of dipole Tx J
!       SXZ(J)   = depth of dipole Tx J
!       SXDIP(J) = dip (in radians) of dipole J (eg; vertical = 0, horizontal = 90)
!       SXAZM(J) = azimuth (in radians) of dipole J (north = 0, east = 90)
!
!       NRXTX(J) - number of receivers for transmitter J
!          MRXTX - maximum number of receivers for any transmitter
!      RXID(I,J) - RX_TYPE of receiver I for transmitter J. 1 => MD; 2 => ED; 3 => point E field;  4=> cdnt loop.
!           MQVR - maximum number of vertices for all receivers
!   XRXTX(I,J,K) - north coordinate of the Kth vertex of the Ith receiver of transmitter J
!   YRXTX(I,J,K) - east coordinate of the Kth vertex of the Ith receiver of transmitter J
!     ZRXTX(I,J) - depth of the Ith receiver of transmitter J
!     XRXOF(I,L) - X offset along SV_AZM of Rx I on Tx line L.
!     YRXOF(I,L) - Y offset perpendicular to SV_AZM of Rx I on Tx line L.
!     ZRXOF(I,L) - ground clearance of Rx I on Tx line L.
!
!                  SURVEY_TYPE = 1
!                  ---------------
!      LNTR(1,L) : Tx index for Line L.   LNTR(2,L) = LNTR(1,L)
!      LNTR(3,L) : Rx index  for start of Line L
!      LNTR(4,L) : Rx index for finish of Line L
!
!                  SURVEY_TYPE > 1
!                  ---------------
!      LNTR(1,L) : Tx index for start of Line L
!      LNTR(2,L) : Tx index for finish of Line L
!      LNTR(3,L) : Rx index for Line L.   LNTR(4,L) = LNTR(3,L)
!
!      The Rx index for variable LNTR is defined with reference to specific
!      transmitters rather than the global number.  Tx indices are global.
!
!
! ADDITIONAL SURVEY VARIABLES REQUIRED FOR OUTPUT
! -----------------------------------------------
!
!    NLINES          - number of survey lines
!    NRX(I)          - the number of receivers in Line I.
!    RX_TYPE(I)      - Receiver type for Line I
!    RXMNT(I)        - dipole receiver moment (area * turns) for Line I
!    UNITS(I)        - units for line I
!    KNORM(I)        - normalisation indicator for line I
!    YXZPLT(1:3,I,L) - GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L
!
!    TXMNT - moment for dipole transmitter
!
! INTERMEDIATE SURVEY VARIABLES USED IN READ_SYSTEM_AND_SURVEY_DATA
! -----------------------------------------------------------------
!
!     SV_AZM     - survey azimuth in degrees
!     RXN(I,J,K) - local north coordinate of vertex K of Rx I of Group J
!     RXE(I,J,K) - local east coordinate of the Kth vertex of the Ith receiver of receiver group J
!     RXZ(I,J)   - depth (z is positive down) of Rx I of Group J

 IMPLICIT NONE
 REAL, PARAMETER :: MIN_RXED=0.001   ! Minimum source & receiver electrode depths
 INTEGER NTRN,KTXP,IDH5
 REAL A1
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:,:,:) :: QD1,QD2

 ! Matlab / MEX input:
 REAL(KIND = 8), INTENT(IN)                   :: MXTDFD, MXDO3D, MXISYS, MXPRFL, MXISTOP
 !
 REAL(KIND = 8), INTENT(IN)                   :: MXSTEP, MXNSX, MXNCHNL, MXKRXW, MXREFTYM, MXOFFTYM
 !
 REAL(KIND = 8), DIMENSION(:), INTENT(IN)     :: MXTXON, MXWAVEFORM
 !
 REAL(KIND = 8), DIMENSION(:), INTENT(IN)     :: MXTOPN, MXTCLS
 !
 REAL(KIND = 8), INTENT(IN)                   :: MXSURVEY_TYPE
 !
 REAL(KIND = 8), INTENT(IN)                   :: MXNLINES, MXMRXL, MXNTX, MXSOURCE_TYPE, MXMXVRTX, MXA1
 !
 REAL(KIND = 8), DIMENSION(:), INTENT(IN)     :: MXNVRTX, MXTXZ
 !
 REAL(KIND = 8), DIMENSION(:,:), INTENT(IN)   :: MXSXED, MXSXND
 !
 REAL(KIND = 8), DIMENSION(:), INTENT(IN)     :: MXLINE, MXRX_TYPE, MXNRX, MXUNITS
 REAL(KIND = 8), DIMENSION(:,:), INTENT(IN)   :: MXLNTR
 !
 REAL(KIND = 8), DIMENSION(:), INTENT(IN)     :: MXCMP, MXKNORM, MXIPLT, MXIDH, MXRXMNT
 REAL(KIND = 8), INTENT(IN)                   :: MXSV_AZM
 !
 REAL(KIND = 8), DIMENSION(:,:,:), INTENT(IN) :: MXQD1, MXQD2
 REAL(KIND = 8), DIMENSION(:,:), INTENT(IN)   :: MXRXZ

!      Initialise some variables.

 T0SX = 100.
 MXERR = 0             !  Initialise input error flag
 PRTSEC = .FALSE.
 REFTYM = 0.
 INVERT = .FALSE.
 INTRUDE = .FALSE.
 MRXTX = 1
 MXVRTX = 4
 SV_AZM = 0.
 CMPMT = 1
 KMPMT = 1
 MCMP = 3               !  Number of possible spatial components

!  Reproduce input data with no assignments and rewind file.

 !!!WRITE(NW,1) PVC
 !WRITE(*,1) PVC
 !!!WRITE(NW,'(T11,A/T11,A/)') 'INPUT DATA', '----------'
 !!!REFLECT_DATA: DO J = 1,200
 !!!  READ(NR,'(A)',END = 100) INP
 !!!  WRITE(NW,'(1X,A)') INP
 !!!END DO REFLECT_DATA

 !!!100 REWIND NR
 !!!    WRITE(NW,2)

 !!!READ(NR,'(A)') TITLE
 !!!WRITE(NW,'(/1X,A)') TRIM (ADJUSTL (TITLE))

! Read model control & print parameters

 TDFD  = INT(MXTDFD)
 DO3D  = INT(MXDO3D)
 ISYS  = INT(MXISYS)
 PRFL  = INT(MXPRFL)
 ISTOP = INT(MXISTOP)
 !!!READ(NR,*)  TDFD, DO3D, ISYS, PRFL, ISTOP
 !!!WRITE(NW,3) TDFD, DO3D, ISYS, PRFL, ISTOP
 IF (DO3D > 0) THEN
   IF (PRFL > 9) THEN
     PRTSEC = .TRUE.
     PRFL = PRFL - 10
   END IF
 ELSE IF (DO3D < 0) THEN
   INVERT = .TRUE.
   DO3D = 1
 END IF
 IF (DO3D > 2) THEN
   INTRUDE = .TRUE.
   DO3D = 1
   IF (DO3D > 3) CALL WRITE_LOG_FILE (NLG,3,MXERR,1)
 END IF

!   TDFD = 0, 1 or 2 for user controlled TD, standard TD or FD respectively.
!   DO3D = 2 => use old FD data from Leroi.frq.
!        = 1 or 3 => compute new  plate model.
!        = 0 => compute layered 1/2 space model only.
!   PRFL - indicates profile or decay curve output plus scattered field output option.
!  ISTOP - read data and stop if ISTOP = 1

 KHSQ = 0
 IF (TDFD < 2) THEN
   IF (ISYS == 2) THEN
     CALL WRITE_LOG_FILE (NLG,40,MXERR,2)
   ELSE IF (ISYS == 6) THEN
     CALL WRITE_LOG_FILE (NLG,20,MXERR,2)
   ELSE IF (ISYS /= 0 .AND. ISYS /= 4) THEN
     CALL WRITE_LOG_FILE (NLG,16,MXERR,2)
   END IF
 ELSE
   IF (ISYS /= 0 .AND. ISYS /= 2 .AND. ISYS /= 6) CALL WRITE_LOG_FILE (NLG,17,MXERR,2)
 END IF

 IF (TDFD == 0) THEN
   READ(NR,*) MIN_FREQ,MAX_FREQ,IPPD,KHSQ
   WRITE(NW,7) KHSQ,IPPD,MIN_FREQ,MAX_FREQ
   IF (KHSQ == 1) THEN
     WRITE(NW,16)
     WRITE(*,16)
   END IF
 END IF

 IF (PRFL /= 0 .AND. PRFL /= 1 .AND. PRFL /= 10 .AND. PRFL /= 11) THEN
   CALL WRITE_LOG_FILE (NLG,2,MXERR,1)
   PRFL = 1
 END IF

 STEP = 2
 IF (TDFD < 2) THEN
   IF (DO3D == 1) THEN
     IF (.NOT. INVERT) OPEN(ND,FILE = 'Leroi.frq',STATUS = 'REPLACE')
   ELSE IF (DO3D == 2) THEN
     OPEN(ND,FILE = 'Leroi.frq',STATUS = 'OLD')
   ELSE IF (DO3D > 3) THEN
     DO3D = 1
     CALL WRITE_LOG_FILE (NLG,3,MXERR,1)
   END IF

   NFT = 1                                      ! Used for dimensioning similar TD & FD arrays
   IF(.NOT. ALLOCATED(CURNT)) ALLOCATE (CURNT(NFT))

   IF (ISYS == 0) THEN                    ! Time-domain parameters
     STEP   = INT(MXSTEP)
     NSX    = INT(MXNSX)
     NCHNL  = INT(MXNCHNL)
     KRXW   = INT(MXKRXW)
     REFTYM = MXREFTYM
     OFFTYM = MXOFFTYM
     !!!READ(NR,*)  STEP, NSX, NCHNL, KRXW, REFTYM, OFFTYM
     !!!WRITE(NW,4) STEP, NSX, NCHNL, KRXW, REFTYM, OFFTYM
     IF (KRXW /= 1 .AND. KRXW /= 2) CALL WRITE_LOG_FILE (NLG,5,MXERR,2)
   ELSE IF (ISYS == 4) THEN   ! UTEM: dB/dt from triangular current or
     READ (NR,*) TXFREQ,NCHNL
     WRITE(NW,40) TXFREQ,NCHNL
     IF (NCHNL /=10 .AND. NCHNL /= 20) CALL WRITE_LOG_FILE (NLG,18,MXERR,2)
     NSX = 1
     CURNT(1) = 1.            ! Step B response for full duty cycle rectangular pulse
     STEP = 4
     OFFTYM = 0
     REFTYM = 0.
     PULSE = .5 / TXFREQ
   END IF
   MCHNL = NCHNL
   IF(.NOT. ALLOCATED( TXON)) ALLOCATE (TXON(NSX))
   IF(.NOT. ALLOCATED( WAVEFORM )) ALLOCATE( WAVEFORM(NSX) )
   IF( .NOT. ALLOCATED(TMS)) ALLOCATE(TMS(NCHNL))
   IF( .NOT. ALLOCATED( WTMS)) ALLOCATE( WTMS(NCHNL))
   IF(.NOT. ALLOCATED( TOPN)) ALLOCATE( TOPN(NCHNL))
   IF(.NOT. ALLOCATED(TCLS)) ALLOCATE( TCLS(NCHNL))
   IF(.NOT. ALLOCATED( SWX)) ALLOCATE(SWX(NSX))
   IF( .NOT. ALLOCATED( SWY)) ALLOCATE( SWY(NSX,3))
   TXON=0.; WAVEFORM=0.; TMS=0.; WTMS=0.; TOPN=0.; TCLS=0; SWX=0; SWY=0

   IF (ISYS == 0) THEN                    ! Time-domain parameters
     TXON     = MXTXON
     WAVEFORM = MXWAVEFORM
     !!!READ(NR,*) (TXON(J),WAVEFORM(J), J = 1,NSX)  ! Read in source waveform.
     CURNT(1) = MAXVAL (WAVEFORM)

     !!!WRITE(NW,5)
     !!!DO J = 1, NSX
     !!!  WRITE(NW,'(3X,I4,F13.3,5X,G13.4)') J,TXON(J),WAVEFORM(J)
     !!!END DO

     IF (KRXW == 1) THEN
       TOPN = MXTOPN
       TCLS = MXTCLS
       !!!READ(NR,*) (TOPN(J) ,TCLS(J), J = 1,NCHNL)
       CALL SET_TIME_ORDER (NCHNL,TOPN,TCLS)       ! Ensure that channels go from early to late
       TMS = (TOPN + TCLS) / 2.
       WTMS = TCLS - TOPN
     ELSE
       READ(NR,*) TMS(1:NCHNL)
       READ(NR,*) WTMS(1:NCHNL)
       CALL SET_TIME_ORDER (NCHNL,TMS,WTMS)
       TCLS= TMS + WTMS /2.
       TOPN= TMS - WTMS /2.
     END IF
     PULSE = 1.E-3 * (OFFTYM + MAXVAL (TXON) )
     SWX = 1.E-3 * TXON
     SWY(1:NSX,3) = WAVEFORM(1:NSX)

   ELSE IF (ISYS == 4) THEN   ! UTEM: dB/dt from triangular current or
     SWX(1) = 0.              !       B from full duty rectangular pulse
     SWY(1,1) = 1

     TCLS(NCHNL) = 1000. * PULSE
     TOPN(NCHNL) = TCLS(NCHNL) / 2.
     DO J = NCHNL-1,1,-1
       TCLS(J) = TCLS(J+1) / 2.
       TOPN(J) = TCLS(J) / 2.
     END DO
     TMS = (TOPN + TCLS) / 2.
     WTMS = TCLS - TOPN

   END IF

   !!!WRITE(NW,6) REFTYM

   TOPN = TOPN + REFTYM
   TCLS = TCLS + REFTYM
   !!!DO J = 1,NCHNL
   !!!  WRITE(NW,'(7X,I4,2F12.3,F11.3,F12.3,F11.3)') J,TOPN(J),TCLS(J),WTMS(J),TMS(J)+REFTYM,TMS(J)
   !!!  IF ( TOPN(J) <= 0) CALL WRITE_LOG_FILE (NLG,7,MXERR,2)
   !!!END DO
   TOPN = 1.E-3 * TOPN
   TCLS = 1.E-3 * TCLS

 ELSE IF (TDFD == 2) THEN          ! Frequency-domain systems
   STEP = 2
   READ(NR,*)  NFRQ
   WRITE(NW,8) NFRQ
   NFT = NFRQ                        ! Used for dimensioning similar TD & FD arrays
   MCHNL = 2*NFRQ
   IF (ISYS == 2) MCHNL = NFRQ
   ALLOCATE( FREQ(NFRQ), CURNT(NFT),SWX(1),SWY(1,1),TRP(1),TOPN(1),TCLS(1))

! Read & write source waveform.

   IF (ISYS /= 6) THEN
     WRITE(NW,9)
     DO J = 1, NFRQ
       READ(NR,*) FREQ(J),CURNT(J)
       WRITE(NW,'(3X,I4,G13.4,5X,G13.4)') J,FREQ(J),CURNT(J)
     END DO
   ELSE
     WRITE(NW,39)
     DO J = 1, NFRQ
       READ(NR,*) FREQ(J)
       WRITE(NW,'(3X,I4,G13.4)') J,FREQ(J)
     END DO
   END IF
 END IF


! Survey Information
! ------------------
! Read in absolute locations in double precision variables and convert to
! REAL body centred coordinates before entering the computation realm.

 NTRN = 1; TXMNT = 1; NTXL = 1

 SURVEY_TYPE = INT(MXSURVEY_TYPE)
 !!!READ(NR,*) SURVEY_TYPE
 !!!WRITE(NW,10) SURVEY_TYPE
 IF (ISYS == 4) THEN              ! Reverse channels for UTEM output
   CALL SET_TMSR (NCHNL,TMS)      ! (late to early)
   CALL SET_TMSR (NCHNL,WTMS)
 END IF

 SELECT CASE (SURVEY_TYPE)                        ! Set NLINES
 CASE (1)                                         ! General Source-Receiver Option
   NLINES      = INT(MXNLINES)
   MRXL        = INT(MXMRXL)
   NTX         = INT(MXNTX)
   SOURCE_TYPE = INT(MXSOURCE_TYPE)
   MXVRTX      = INT(MXMXVRTX)
   A1          = MXA1
   !!!READ(NR,*) NLINES,MRXL,NTX,SOURCE_TYPE,MXVRTX,A1
   NTXL = NTX
   IF (SOURCE_TYPE < 3) THEN
     NTRN = INT (A1)
     !!!WRITE(NW,11) NLINES,MRXL,NTX,SOURCE_TYPE,MXVRTX,NTRN
   ELSE IF (SOURCE_TYPE == 3) THEN
     TXMNT = A1
     WRITE(NW,21) NLINES,NTX,SOURCE_TYPE,MXVRTX,TXMNT
   END IF
   IF(.NOT. ALLOCATED( HEADER_ID)) ALLOCATE (HEADER_ID(NLINES))
   HEADER_ID = 100
   IF (TDFD == 2) HEADER_ID = 200

   IF (NTX > NLINES) CALL WRITE_LOG_FILE (NLG,11,MXERR,2)
   MXTX = NTX
   NTXL = NTX

 CASE (2)                                  !  Moving Loop Tx - Fixed MD Rx offset
   SOURCE_TYPE = 1
   READ(NR,*) NTXL, MRXL, MRXTX, NTRN
   TXMNT = REAL (NTRN)
   WRITE(NW,23) NTXL, MRXL, MRXTX, NTRN
   NLINES = NTXL * MRXTX
   IF (MRXTX == 0) NLINES = NTXL
   MXTX = NTXL * MRXL
   MXVRTX = 4
   ALLOCATE (RXE(MRXL,NLINES,1),RXN(MRXL,NLINES,1))
   RXN = 0.; RXE = 0.
   NHID = 1
   IF(.NOT. ALLOCATED( HEADER_ID)) ALLOCATE (HEADER_ID(NLINES))
   HEADER_ID = 110                         ! 110 & 210 imply magnetic dipole receivers on the surface
   IF (TDFD == 2) HEADER_ID = 210

 CASE (3)                                  !  Moving MD Tx   - Fixed MD Rx offset
   SOURCE_TYPE = 3
   READ(NR,*)   NTXL, MRXL, MRXTX, TXMNT
   WRITE(NW,24) NTXL, MRXL, MRXTX, TXMNT
   NLINES = NTXL * MRXTX
   MXTX = NTXL * MRXL
   MXVRTX = 1
   ALLOCATE (RXE(MRXL,NLINES,1),RXN(MRXL,NLINES,1))
   RXN = 0.; RXE = 0.
   NHID = 1
   IF(.NOT. ALLOCATED( HEADER_ID)) ALLOCATE (HEADER_ID(NLINES))
   HEADER_ID = 110                         ! 110 & 210 imply magnetic dipole receivers on the surface
   IF (TDFD == 2) HEADER_ID = 210

 CASE (4)                                  !  Moving Coincident Loop Tx - Rx
   SOURCE_TYPE = 4
   READ(NR,*) NLINES, MRXL, NTRN
   NTXL = NLINES
   TXMNT = REAL (NTRN)
   WRITE(NW,26) NLINES, MRXL, NTRN
   MXTX = NLINES * MRXL
   MXVRTX = 4
   MRXTX = 1
   ALLOCATE (RXE(MRXL,NLINES,1),RXN(MRXL,NLINES,1))
   RXN = 0.; RXE = 0.
   NHID = 1
   IF(.NOT. ALLOCATED( HEADER_ID)) ALLOCATE (HEADER_ID(NLINES))
   HEADER_ID = 140

 CASE (5)                                  !  Downhole MD Tx_Rx probe
   SOURCE_TYPE = 3
   READ(NR,*)  NLINES, MRXL, IDH5, TXMNT
   WRITE(NW,25) NLINES, MRXL, IDH5, TXMNT
   ALLOCATE (RXE(MRXL,NLINES,1),RXN(MRXL,NLINES,1))
   MXTX = NLINES * MRXL
   MXVRTX = 1
   MRXTX = 1
   NTXL = NLINES
   RXN = 0.; RXE = 0.
   NHID = 1
   IF(.NOT. ALLOCATED( HEADER_ID)) ALLOCATE (HEADER_ID(NLINES))
   HEADER_ID = 110                     ! 110 & 210 imply downhole magnetic dipole output : X-Y-Z
   IF (TDFD == 2) HEADER_ID = 210      ! 111 & 211 imply downhole magnetic dipole output : U-V-A
   HEADER_ID = HEADER_ID + IDH5        ! 112 & 212 imply downhole magnetic dipole output : W-N-S

 CASE (6)
   CALL WRITE_LOG_FILE (NLG,20,MXERR,2)   !  Write fatal error message
   WRITE(*,50)
   STOP
 CASE DEFAULT
   CALL WRITE_LOG_FILE (NLG,10,MXERR,2)   !  Write fatal error message
 END SELECT

 ALLOCATE (LINE(NLINES),UNITS(NLINES),KNORM(NLINES),SVAZM(NLINES),NRX(NLINES),CMP(NLINES),NCMPL(NLINES), &
           IDH(NLINES),IPLT(NLINES),RXMNT(NLINES),RX_TYPE(NLINES),LNTR(4,NLINES),SDN0(NTXL),SDE0(NTXL),  &
           SDZ0(NTXL),TXLNGTH(NTXL),TXWDTH(NTXL),RXOFF(NTXL),XRXOF(MRXTX,NTXL),YRXOF(MRXTX,NTXL),        &
           ZRXOF(MRXTX,NTXL),NVRTX(MXTX),RXOTX(MXTX),SXDIP(MXTX),SXAZM(MXTX),TXZ(MXTX),SXZ(MXTX),        &
           NRXTX(MXTX),TXCLN(MXTX),TXAZM(MXTX),SXND(MXVRTX,MXTX),SXED(MXVRTX,MXTX),DSTAT(MRXL,NTXL),     &
           RXZ(MRXL,NLINES))

 LINE=0;  UNITS=0;  KNORM = 0;  SVAZM=0.;  NRX=1; CMP=1; RXZ=0.; IPLT=1; RXMNT=1.; RX_TYPE=1
 SDN0=0.D0;  SDE0=0.D0;  SDZ0=0.;  TXLNGTH=1.;  TXWDTH=1.;  RXOFF=0.;  XRXOF=0.;  YRXOF=0.;  ZRXOF=0.
 NVRTX=4; NRXTX=0;  SXDIP=0.;  SXAZM=0.;  TXCLN=0.;  TXAZM=0.;  TXZ=0.;  SXZ=0.;  SXND=0.D0
 SXED=0.D0;  DSTAT=0.; RXOTX=0.;  MXRS = 1;  MQVR = 1; IDH = 0; LNTR=1

! MXRS = maximum number of subnet receivers
!      = 1 for magnetic dipole; = 5 for electric dipole;  = 100 for coincident loop
! MQVR = maximum number of vertices for a receiver
!      = 1 for magnetic dipole; = 2 for electric dipole

 SELECT CASE (SURVEY_TYPE)

 CASE (1)                                         !  General Source-Receiver Option
   SELECT CASE (SOURCE_TYPE)
   CASE (1:2)                                     ! Closed or open loops
     !!!IF (SOURCE_TYPE == 1) WRITE(NW,12)
     !!!IF (SOURCE_TYPE == 2) WRITE(NW,13)

     NVRTX = INT(MXNVRTX)
     TXZ   = MXTXZ
     SXED  = MXSXED
     SXND  = MXSXND
     DO JS = 1,NTX
       !!!READ(NR,*) NVRTX(JS),TXZ(JS)
       !!!WRITE(NW,14) JS,NVRTX(JS)
       DO JV = 1, NVRTX(JS)
         !!!READ(NR,*) SXED(JV,JS),SXND(JV,JS)
         !!!WRITE(NW,'(I5,3F14.2)') JV,SXED(JV,JS),SXND(JV,JS),TXZ(JS)
       END DO
     END DO
     SXZ = -TXZ           ! Convert from elevation to depth

   CASE(3)                                        !  Magnetic dipole source
     NVRTX = 1
     WRITE(NW,15)
     DO JS = 1,NTX
       READ(NR,*) SXED(1,JS),SXND(1,JS),TXZ(JS),TXCLN(JS),TXAZM(JS)
       WRITE(NW,'(I5,3F14.2,2F9.1,F10.1)') JS,SXED(1,JS),SXND(1,JS),TXZ(JS),TXCLN(JS),TXAZM(JS)
       SXAZM(JS) = TXAZM(JS) * D2R
       SXDIP(JS) = TXCLN(JS) * D2R
       IF (ABS (SXDIP(JS)) < 1.E-3) SXDIP(JS) = 0.
       IF (ABS (SXAZM(JS)) < 1.E-3) SXAZM(JS) = 0.
     END DO
     SXZ = -TXZ           ! Convert from elevation to depth

   CASE DEFAULT
     CALL WRITE_LOG_FILE (NLG,8,MXERR,2)
   END SELECT

   ALLOCATE (QD1(MRXL,NLINES,2),QD2(MRXL,NLINES,2),BHDIP(MRXL,NLINES),BHAZM(MRXL,NLINES))

   QD1 = 0.D0; QD2 = 0.D0; BHDIP = 90.; BHAZM = 0.

   RXMNT = 1.
   CMP = 1
   NCMPL = 1
   LINE    = INT(MXLINE)
   LNTR    = INT(MXLNTR)
   RX_TYPE = INT(MXRX_TYPE)
   NRX     = INT(MXNRX)
   UNITS   = INT(MXUNITS)
   DO JL = 1,NLINES
     !!!READ(NR,*) LINE(JL),LNTR(1,JL),RX_TYPE(JL),NRX(JL),UNITS(JL)
     !!!WRITE(NW,17) LINE(JL),LNTR(1,JL),RX_TYPE(JL),NRX(JL),UNITS(JL)

     IF (NRX(JL) > MRXL) THEN
       CALL WRITE_LOG_FILE (NLG,12,MXERR,1)
       NRX(J) = MRXL
       WRITE (NLG,33) JL,MRXL
     END IF
     HEADER_ID(JL) = HEADER_ID(JL) + 10*RX_TYPE(JL)

     IF (SOURCE_TYPE == 3 .AND. RX_TYPE(JL) > 1) THEN
       CALL WRITE_LOG_FILE (NLG,6,MXERR,2)
       WRITE(NLG,'(T3,A,I3)') 'Rx selection error for Line',JL
     END IF

     IF (TDFD /= 2 .AND. RX_TYPE(JL) == 3) THEN
       CALL WRITE_LOG_FILE (NLG,30,MXERR,2)
       WRITE(NLG,'(T3,A,I3)') 'Rx selection error for Line',JL
     END IF

     LNTR(2,JL) = LNTR(1,JL)                     !  transmitter index for Line JL
     KTX = LNTR(1,JL)
     NRXTX(KTX) = NRXTX(KTX) + NRX(JL)

     SELECT CASE (RX_TYPE(JL))
     CASE (1)                                         ! Magnetic dipole receiver
       CMP    = INT(MXCMP)
       SV_AZM = MXSV_AZM
       KNORM  = INT(MXKNORM)
       IPLT   = INT(MXIPLT)
       IDH    = INT(MXIDH)
       RXMNT  = MXRXMNT
       !!!READ(NR,*) CMP(JL),SV_AZM,KNORM(JL),IPLT(JL),IDH(JL),RXMNT(JL)
       CALL VALIDATE_CMP (NLG,MXERR,JL,NLINES,CMP,NCMPL,ISYS)
       !!!WRITE(NW,22) CMP(JL),KNORM(JL),IPLT(JL),IDH(JL),SV_AZM,RXMNT(JL)
       HEADER_ID(JL) = HEADER_ID(JL) + IDH(JL)
       IF (KNORM(JL) > 0) THEN
         IF (UNITS(JL) < 31 .OR. UNITS(JL) > 35) CALL WRITE_LOG_FILE (NLG,15,MXERR,2)
       END IF
       SVAZM(JL) = SV_AZM * D2R

       IF (IDH(JL) == 0) THEN                          ! Surface receiver processing
         !!!WRITE(NW,18)
         QD1 = MXQD1
         QD2 = MXQD2
         RXZ = MXRXZ
         DO JR = 1, NRX(JL)
           !!!READ (NR,*) QD1(JR,JL,1),QD2(JR,JL,1),RXZ(JR,JL)    ! RXED,RXND,RXZ
           !!!WRITE(NW,'(I4,3F12.1)') JR,QD1(JR,JL,1),QD2(JR,JL,1),RXZ(JR,JL)
         END DO
       ELSE                ! U-V-A processing
         WRITE(NW,19)
         DO JR = 1, NRX(JL)
           READ (NR,*) QD1(JR,JL,1),QD2(JR,JL,1),RXZ(JR,JL),BHDIP(JR,JL),BHAZM(JR,JL)    ! RXED,RXND,RXZ,BHDIP,BHAZM
           WRITE(NW,'(I4,3F12.1,2F9.1)') JR,QD1(JR,JL,1),QD2(JR,JL,1),RXZ(JR,JL),BHDIP(JR,JL),BHAZM(JR,JL)
         END DO
       END IF

     CASE (2)                                         ! Electric dipole receiver
       IDH(JL) = -1
       MQVR = 2
       MXRS = 5
       WRITE(NW,20)
       DO JR = 1, NRX(JL)
         READ (NR,*) QD1(JR,JL,1),QD2(JR,JL,1),QD1(JR,JL,2),QD2(JR,JL,2),RXZ(JR,JL)
         RXZ(JR,JL) = MIN (RXZ(JR,JL),-MIN_RXED)         ! Electrodes should be subsurface
         WRITE(NW,'(I4,5F13.2)') JR,QD1(JR,JL,1),QD2(JR,JL,1),QD1(JR,JL,2),QD2(JR,JL,2),RXZ(JR,JL)
       END DO

     CASE (3)                                         ! Point E field receiver
       READ(NR,*) CMP(JL),SV_AZM
       IDH(JL) = -1
       CALL VALIDATE_CMP (NLG,MXERR,JL,NLINES,CMP,NCMPL,ISYS)
       WRITE(NW,35) CMP(JL),SV_AZM
       SVAZM(JL) = SV_AZM * D2R
       DO JR = 1, NRX(JL)
         READ (NR,*) QD1(JR,JL,1),QD2(JR,JL,1),RXZ(JR,JL)
         RXZ(JR,JL) = MIN (RXZ(JR,JL),-MIN_RXED)         ! Electrodes should be subsurface
         WRITE(NW,'(I4,3F13.2)') JR,QD1(JR,JL,1),QD2(JR,JL,1),RXZ(JR,JL)
       END DO
     CASE DEFAULT
       CALL WRITE_LOG_FILE (NLG,9,MXERR,2)
     END SELECT
   END DO
   MRXTX = MAXVAL (NRXTX)

   ALLOCATE (RXED(MRXL,NLINES,MQVR),RXND(MRXL,NLINES,MQVR),RXE(MRXL,NLINES,MQVR),RXN(MRXL,NLINES,MQVR))

   RXED(1:MRXL,1:NLINES,1:MQVR) = QD1(1:MRXL,1:NLINES,1:MQVR)
   RXND(1:MRXL,1:NLINES,1:MQVR) = QD2(1:MRXL,1:NLINES,1:MQVR)

   DEALLOCATE (QD1, QD2)

   ALLOCATE (KHID(NLINES))
   NHID = 1
   KHID(1) = HEADER_ID(1)
   HEADER: DO JL = 2, NLINES
     DO J = 1,JL-1
       IF (HEADER_ID(JL) == HEADER_ID(J)) CYCLE HEADER
     END DO
     NHID = NHID + 1
     KHID(NHID) = HEADER_ID(JL)
   END DO HEADER
   IF (ISYS == 2) THEN
     HEADER_ID = 250
     MCMP = 1           ! Sampo output is single component (ABS (Bz/Bx))
   END IF

 CASE (2)              !  Moving Loop Tx - Fixed MD Rx offset

   NTX = 0
   NVRTX = 4
   NRXTX = MRXTX
   DO JS = 1,NTXL                 ! DSTAT(1,JS) was initialised to 0
     READ(NR,*) KTXP,TXLNGTH(JS),TXWDTH(JS),SV_AZM,SDE0(JS),SDN0(JS),SDZ0(JS)
     IF (KTXP > MRXL) THEN
       CALL WRITE_LOG_FILE (NLG,14,MXERR,1)
       KTXP = MRXL
       WRITE (NLG,34) JS,MRXL
     END IF

     READ(NR,*) DSTAT(2:KTXP,JS)
     WRITE(NW,27) JS,KTXP,TXLNGTH(JS),TXWDTH(JS),SDE0(JS),SDN0(JS),SDZ0(JS),SV_AZM,DSTAT(2:KTXP,JS)
     K1 = NTX + 1
     NTX = NTX + KTXP

     DO JR = 1,MRXTX
       JL = JR + (JS-1) * MRXTX       ! Line index
       SVAZM(JL) = SV_AZM * D2R
       NRX(JL) = KTXP
       READ(NR,*) LINE(JL),CMP(JL),XRXOF(JR,JS),YRXOF(JR,JS),ZRXOF(JR,JS),RXMNT(JL),UNITS(JL),KNORM(JL),IPLT(JL)
       CALL VALIDATE_CMP (NLG,MXERR,JL,NLINES,CMP,NCMPL, ISYS)
       WRITE(NW,30) LINE(JL),XRXOF(JR,JS),YRXOF(JR,JS),ZRXOF(JR,JS),CMP(JL),UNITS(JL),KNORM(JL),IPLT(JL),RXMNT(JL)

       LNTR(1,JL) = K1
       LNTR(2,JL) = NTX
       LNTR(3,JL) = JR             ! Rx index at start of Line JL
       LNTR(4,JL) = JR             ! Rx index at end of Line JL
     END DO
   END DO
   IF (ISYS == 2) THEN
     HEADER_ID = 250
     MCMP = 1           ! Sampo output is single component (ABS (Bz/Bx))
   END IF

 CASE (3)   !  Moving MD Tx   - Fixed MD Rx offset
   NVRTX = 1
   NRXTX = MRXTX
   NTX = 0
   DO JS = 1,NTXL                 ! DSTAT(1,JS) was initialised to 0
     READ(NR,*) KTXP,TXCLN(JS),TXAZM(JS),SV_AZM,SDE0(JS),SDN0(JS),SDZ0(JS)
     IF (KTXP > MRXL) THEN
       CALL WRITE_LOG_FILE (NLG,14,MXERR,1)
       KTXP = MRXL
       WRITE (NLG,34) JS,MRXL
     END IF
     READ(NR,*) DSTAT(2:KTXP,JS)
     WRITE(NW,28) JS,KTXP,TXCLN(JS),TXAZM(JS),SDE0(JS),SDN0(JS),SDZ0(JS),SV_AZM,DSTAT(2:KTXP,JS)
     K1 = NTX + 1
     NTX = NTX + KTXP
     SXDIP(K1:NTX) = TXCLN(JS) * D2R
     SXAZM(K1:NTX) = TXAZM(JS) * D2R

     DO JR = 1,MRXTX
       JL = JR + (JS-1) * MRXTX
       SVAZM(JL) = SV_AZM * D2R
       NRX(JL) = KTXP
       READ(NR,*) LINE(JL),CMP(JL),XRXOF(JR,JS),YRXOF(JR,JS),ZRXOF(JR,JS),RXMNT(JL),UNITS(JL),KNORM(JL),IPLT(JL)
       CALL VALIDATE_CMP (NLG,MXERR,JL,NLINES,CMP,NCMPL,ISYS)
       WRITE(NW,30) LINE(JL),XRXOF(JR,JS),YRXOF(JR,JS),ZRXOF(JR,JS),CMP(JL),UNITS(JL),KNORM(JL),IPLT(JL),RXMNT(JL)

       LNTR(1,JL) = K1             ! Tx index at start of Line JL
       LNTR(2,JL) = NTX            ! Tx index at end of Line JL
       LNTR(3,JL) = JR             ! Rx index at start of Line JL
       LNTR(4,JL) = JR             ! Rx index at end of Line JL
     END DO
   END DO
   IF (ISYS == 2) THEN
     HEADER_ID = 250
     MCMP = 1           ! Sampo output is single component (ABS (Bz/Bx))
   END IF

 CASE (4)              !  Coincident Loop
   NVRTX = 4
   RX_TYPE = 4
   NRXTX = 1
   MXRS = 100          ! maximum number of subnet receivers
   IPLT = 3
   NTX = 0
   MCMP = 1
   DO JL = 1,NLINES                 ! DSTAT(1,JL) was initialised to 0
     READ(NR,*) KTXP,TXLNGTH(JL),TXWDTH(JL),SV_AZM,SDE0(JL),SDN0(JL)
     IF (KTXP > MRXL) THEN
       CALL WRITE_LOG_FILE (NLG,14,MXERR,1)
       KTXP = MRXL
       WRITE (NLG,34) JL,MRXL
     END IF
     READ(NR,*) DSTAT(2:KTXP,JL)
     WRITE(NW,27) JL,KTXP,TXLNGTH(JL),TXWDTH(JL),SV_AZM,SDE0(JL),SDN0(JL),DSTAT(2:KTXP,JL)
     K1 = NTX + 1
     NTX = NTX + KTXP
     LNTR(1,JL) = K1
     LNTR(2,JL) = NTX

     SVAZM(JL) = SV_AZM * D2R
     NRX(JL) = KTXP
     READ(NR,*) LINE(JL), UNITS(JL)
     WRITE(NW,29) LINE(JL), UNITS(JL)
     LNTR(3,JL) = 1             ! Rx index at start of Line JL
     LNTR(4,JL) = 1             ! Rx index at end of Line JL

   END DO

 CASE (5)                   !  Downhole MD Tx_Rx probe
   NRXTX = 1
   NVRTX = 1
   ALLOCATE (QD1(1,MXTX,1),QD2(1,MXTX,1),BHDIP(MRXL,NLINES),BHAZM(MRXL,NLINES))
   NTX = 0
   DO JL = 1,NLINES
     READ(NR,*) LINE(JL),SV_AZM,NRX(JL),CMP(JL),RXOFF(JL),RXMNT(JL),UNITS(JL),KNORM(JL),IPLT(JL),IDH(JL)
     CALL VALIDATE_CMP (NLG,MXERR,JL,NLINES,CMP,NCMPL,ISYS)
     WRITE(NW,31) LINE(JL),SV_AZM,NRX(JL),CMP(JL),RXOFF(JL),UNITS(JL),KNORM(JL),IPLT(JL),IDH(JL),RXMNT(JL)
     SVAZM(JL) = SV_AZM * D2R
     K1 = NTX
     NTX = NTX + NRX(JL)
     LNTR(1,JL) = K1 + 1        ! Tx index at start of Line JL
     LNTR(2,JL) = NTX           ! Tx index at end of Line JL
     LNTR(3,JL) = 1             ! Rx index at start of Line JL
     LNTR(4,JL) = 1             ! Rx index at end of Line JL

     WRITE(NW,32)
     DO JR = 1,NRX(JL)
       JS = JR + K1
       READ(NR,*) QD2(1,JS,1),QD1(1,JS,1),TXZ(JS),BHDIP(JR,JL),BHAZM(JR,JL)
       WRITE(NW,'(I5,3F14.2,2F9.1,F10.1)') JR,QD2(1,JS,1),QD1(1,JS,1),TXZ(JS),BHDIP(JR,JL),BHAZM(JR,JL)
       SXZ(JS) = -TXZ(JS)
       SXDIP(JS) = (90. - BHDIP(JR,JL)) * D2R
       SXAZM(JS) = BHAZM(JR,JL) * D2R
       RXOTX(JS) = RXOFF(JL)
     END DO
   END DO
   SXND(1,1:NTX) = QD1(1,1:NTX,1)
   SXED(1,1:NTX) = QD2(1,1:NTX,1)
   DEALLOCATE (QD1,QD2)
 END SELECT
 DO JL = 1,NLINES
   IF (UNITS(JL) > 5) RXMNT(JL) = 1.0
 END DO

 IF (MAXVAL (KNORM) > 0 .AND. STEP == 0) THEN
   KNORM = 0
   CALL WRITE_LOG_FILE (NLG,13,MXERR,1)
 END IF

 MLINES = NLINES
 ALLOCATE (BFTL(MCHNL,MRXL,MCMP,MLINES),RDATA(MCHNL,MRXL,MCMP,MLINES),RWTS(MCHNL,MRXL,MCMP,MLINES),      &
           SXN(MXVRTX,NTX),SXE(MXVRTX,NTX),XRXTX(MRXTX,NTX,MQVR),YRXTX(MRXTX,NTX,MQVR),ZRXTX(MRXTX,NTX), &
           YXZPLT(3,MRXL,NLINES),NCTD(MRXTX,NTX),RXID(MRXTX,NTX),KNORM2(MRXTX,NTX))

 BFTL = 0.;  RDATA = 0.
 RWTS = 1
 SXN = 0.; SXE = 0.; XRXTX = 0.;  YRXTX = 0.;  ZRXTX = 0.
 YXZPLT = 0.D0;  RXID = 1;  KNORM2 = 0

 MD1 = 1; MD2 = 1
 IF (SURVEY_TYPE == 1 .OR. SURVEY_TYPE == 5) THEN
   MD1 = MRXL
   MD2 = NLINES
   ALLOCATE (RXAZM(MD1,MD2),RXDIP(MD1,MD2))
   DO JL = 1,NLINES
     DO JR = 1,NRX(JL)
       RXDIP(JR,JL) = (90. - BHDIP(JR,JL)) * D2R
       RXAZM(JR,JL) = BHAZM(JR,JL) * D2R
     END DO
   END DO
   DEALLOCATE (BHAZM,BHDIP)
 ELSE
   MD1 = 1
   MD2 = 1
   ALLOCATE (RXAZM(MD1,MD2),RXDIP(MD1,MD2))
   RXDIP = 0.
   RXAZM = 0.
 END IF

 IF (TDFD < 2) SWY = SWY * TXMNT
 IF (ISYS /= 6) CURNT = CURNT * TXMNT
 DEALLOCATE (TXCLN,TXAZM)

 1 FORMAT (T25,A/T25,'Develped by: Art Raiche'/T33,'for: AMIRA project P223F'///)
 2 FORMAT (T1,79('-'))
 3 FORMAT(/T3,'TDFD =',I2,';   DO3D =',I3,';   ISYS =',I2,';   PRFL =',I3,';   ISTOP =',I2)
 4 FORMAT(/10X,'+-----------------------------------------+' &
          /10X,'+  Time-Domain Ground System Information  +' &
          /10X,'+-----------------------------------------+' &
         // T3,'STEP =',I2,';   NSX =',I4,';   NCHNL =',I4,';   KRXW =',I2,';   REFTYM =',G12.4,';   OFFTYM =',G12.4)
 5 FORMAT(//T14,'TXON (ms)    Transmitter current (amps)' &
           /T14,'---------    --------------------------'/)
 6 FORMAT(//T10,'Receiver channel origin INPUT is shifted by', F9.3,' ms from signal origin.' &
          //T10,'Receiver Window Specifications (ms - referenced to signal origin)'/ &
            T10,'----------------------------------------------------------------'// &
            T62,'Referenced'/ &
            T8,'Window',T19,'Open',T30,'Close',T42,'Width',T52,'Centre',T64,'Centre'/ &
            T8,'------',T19,'----',T30,'-----',T42,'-----',T52,'------',T64,'------')
 7 FORMAT(/T3,'KHSQ =',I2,4X,'IPPD =',I2,4X,'MIN_FREQ =',G13.4,4X,'MAX_FREQ =',G13.4)
 8 FORMAT(/10X,'+----------------------------------------------+' &
          /10X,'+  Frequency-Domain Ground System Information  +' &
          /10X,'+----------------------------------------------+' &
          //T3,'NFRQ =',I3)
 9 FORMAT(/T12,'Frequency      Transmitter current in amps', &
          /T12,'---------      ---------------------------'/)
 10 FORMAT(//T3,'SURVEY_TYPE =',I2)
 11 FORMAT(/T3,'NLINES =',I3,';   MRXL =',I4,';   NTX =',I3,';   SOURCE_TYPE =',I2,';   MXVRTX =',I2,';   NTRN =',I3)

 12 FORMAT(/T3,'Vertex Locations for Loop Sources' &
           /T3,'---------------------------------')
 13 FORMAT(/T3,'Vertex Locations for Grounded Wire Sources' &
           /T3,'------------------------------------------')
 14 FORMAT(/T3,'Transmitter',I3,' has',I3,' vertices:' &
          //T13,'Easting      Northing      Elevation' &
           /T13,'-------      --------      ---------')
 15 FORMAT(/T3,'Magnetic Dipole Source Specification' &
          //T12,'Easting       Northing      Elevation   TXCLN   Azimuth' &
           /T12,'-------       --------      ---------   -----   -------')
 16 FORMAT(/T3,'WARNING:  Instead of using the default frequency range .001 Hz to 100 MHz' &
           /T3,'          to compute the HALFSPACE time-domain response, Leroi will now use' &
           /T3,'          the user-specified range.')
 17 FORMAT(//T3,'Line',I9,';   Tx Index',I3,';   Rx Type =',I2,';   NRX =',I3,';   Units =',I3)
 18 FORMAT(/T11,'Magnetic Dipole Receivers' &
           /T11,'-------------------------' &
          //T10,'Easting     Northing   Elevation' &
           /T10,'-------     --------   ---------')
 19 FORMAT(/T11,'Magnetic Dipole Receivers' &
           /T11,'-------------------------' &
          //T10,'Easting     Northing   Elevation    Dip    Azimuth' &
           /T10,'-------     --------   ---------    ---    -------')
 20 FORMAT(/T12,'Electric Dipole Receiver Electrodes' &
           /T12,'-----------------------------------' &
          //T12,'East 1      North 1       East 2      North 2       Depth' &
           /T12,'------      -------       ------      -------       -----')
 21 FORMAT(/T3,'NLINES =',I3,';   NTX =',I3,';   SOURCE_TYPE =',I2,';   MXVRTX =',I2,';   TXMMT =',G12.4)
 22 FORMAT(/T9,'CMP =',I4,';   KNORM =',I2,';   IPLT =',I2,';   IDH =',I2,';   SV_AZM =',F7.1,';   RXMNT =',G12.4)
 23 FORMAT( T3,'NTXL =',I3,';   MRXL =',I3,';   MRXTX =',I2,';   NTRN =',I3)
 24 FORMAT( T3,'NTXL =',I3,';   MRXL =',I3,';   MRXTX =',I2,';   TXMMT =',G12.4)
 25 FORMAT(/T3,'NLINES =',I3,';   MRXL =',I3,';   IDH =',I2,';   TXMMT =',G12.4)
 26 FORMAT(/T3,'NLINES =',I3,';   MRXL =',I3,';   NTRN =',I3)
 27 FORMAT(/T3,'Moving Loop Tx Line',I3,';   NTX =',I3,';   Tx_length =',F7.1,';   Tx_width =',F7.1, &
           /T3,'--------------------------------------------------------------------------' &
           /T3,'Initial position (E,N,Z) :',3F11.1 /T13,'Line_azimuth :',F6.1/T8,'Station_intervals :',60F8.1)
 28 FORMAT(/T3,'Moving Magnetic Dipole Tx Line',I3,';   NTX =',I3,';   TXCLN =',F6.1,';   TXAZM =',F6.1,&
          //T3,'Initial position (E,N,Z) :',3F11.2,/T13,'Line_azimuth :',F6.1/T8,'Station_intervals :',60F8.1)
 29 FORMAT(/T3,'Coincident loop survey - Line',I9,';   UNITS =',I3)
 30 FORMAT(/T3,'X,Y,Z Rx offsets for Line',I9,':',3F7.1 &
           /T5,'CMP =',I4,':  UNITS =',I3,';   KNORM =',I3,';   IPLT =',I2,';   RXMNT =',G12.4)
 31 FORMAT(/T3,'Line',I9,';   SV_AZM =',F6.1,';   NRX =',I3,';   CMP =',I4,';   OFFSET =',F6.1, &
           /T5,'UNITS =',I4,';   KNORM =',I3,';   IPLT =',I2,';   IDH =',I2,';   RXMNT =',G12.4)
 32 FORMAT(/T3,'Magnetic Dipole Transmitter Specification' &
          //T12,'Easting       Northing      Elevation     Dip   Azimuth' &
           /T12,'-------       --------      ---------     ---   -------')
 33 FORMAT(/T3,'NRX(',I2,') has been reduced to',I3)
 34 FORMAT(/T3,'KTXP for Transmitter Line',I3,' has been reduced to',I3)
 35 FORMAT(/T11,'Point E-field Receivers  CMP =',I4,';   SVAZM =',F7.1 &
           /T11,'-------------------------------------------------' &
          //T11,'Easting     Northing      Elevation' &
           /T11,'-------     --------      ---------')
 39 FORMAT(/T12,'Frequency' /T12,'---------'/)
 40 FORMAT(/T3,'UTEM base frequency =',F7.1,' Hz.   NCHNL = ',I3)
 50 FORMAT(/T3,'This version of Leroi does not include magnetotellurics.' &
           /T3,'It is for controlled sources only.')

    END SUBROUTINE READ_SYSTEM_AND_SURVEY_DATA

   SUBROUTINE READ_MODEL_DATA(MXNLYR, MXNPLT, MXNLITH, &
                              MXLYTH,                  &
                              MXLITHL,                 &
                              MXTHK)

!  --------------------------

!***  Called by MAIN
!***  Calls SET_CELLS

 IMPLICIT NONE
 REAL, PARAMETER ::  CELLW_MIN=0.01, CELLW_MAX=200.
 INTEGER KP,KP1,KP2
 INTEGER, ALLOCATABLE :: LITHL(:)
 REAL A1
 REAL, ALLOCATABLE, DIMENSION(:) ::  PLTOPD
 REAL(KIND=QL), ALLOCATABLE, DIMENSION(:) :: XCNTRD,YCNTRD

 ! Matlab / MEX input:
 REAL(KIND = 8), INTENT (IN)                 :: MXNLYR, MXNPLT, MXNLITH
 REAL(KIND = 8), DIMENSION(:,:), INTENT (IN) :: MXLYTH
 REAL(KIND = 8), DIMENSION(:), INTENT (IN)   :: MXLITHL
 REAL(KIND = 8), DIMENSION(:), INTENT (IN)   :: MXTHK

 ECNTRD = 0.D0; NCNTRD = 0.D0

!  Layered Model Specification
!  ---------------------------

 NLYR  = INT(MXNLYR)
 NPLT  = INT(MXNPLT)
 NLITH = INT(MXNLITH)
 !!!READ(NR,*) NLYR, NPLT, NLITH
 !!!WRITE(NW,1) NLYR, NPLT, NLITH
 IF (NLYR < 1) CALL WRITE_LOG_FILE (NLG,25,MXERR,2)
 IF (NLYR == 1) INTRUDE = .FALSE.
 IF (NPLT == 0) THEN
   DO3D = 0
   PRTSEC = .FALSE.
 END IF
 IF (DO3D == 0) THEN
   NPLT = 0
   IF (SURVEY_TYPE == 1) THEN
     ECNTRD = MAXVAL (RXED) - MINVAL (RXED)
     NCNTRD = MAXVAL (RXND) - MINVAL (RXND)
     IF (ABS (ECNTRD) < 9.D4) ECNTRD = 0.D0
     IF (ABS (NCNTRD) < 9.D4) NCNTRD = 0.D0
   END IF
 END IF
 !!!IF (DO3D == 0 .AND. NPLT > 0) WRITE(NW,10) NPLT
 NPAR = 9*NPLT + 2*NLYR-1

 ALLOCATE (LYTH(NLITH,NPROP),LITHL(NLYR),RES(NLYR),RMU(NLYR),RMUD(0:NLYR),REPS(NLYR),CHRG(NLYR), &
           CTAU(NLYR),CFREQ(NLYR),THK(NLYR),THKD(NLYR),MPAR(NPAR))

 THK=1.E5; RES=0; CHRG=0; CTAU=0; CFREQ=1; RMU=1; RMUD=1._QL
 REPS=1; LITHL=0; MPAR=0.

!  Initialise lithology list.

 LYTH(1:NLITH, 1) = -1.   !  blank resistivity indicator
 LYTH(1:NLITH, 2) = -1.   !  blank conductance (SIG_T) indicator
 LYTH(1:NLITH, 3) = 1.    !  Relative magnetic permeabilities
 LYTH(1:NLITH, 4) = 1.    !  Relative dielectric constants
 LYTH(1:NLITH, 5) = 0.    !  Chargeabilities
 LYTH(1:NLITH, 6) = 0.    !  CTAUs
 LYTH(1:NLITH, 7) = 1.    !  CFREQs

 !!!WRITE(NW,2)
 LYTH = MXLYTH
 DO J = 1,NLITH
   !!!READ (NR,*) LYTH(J,1:NPROP)
   !!!WRITE(NW,'(I4,T8,G12.4,T22,F7.1,F12.3,F11.3,F10.2,G12.3,F8.2)') J,LYTH(J,1:NPROP)
   IF (LYTH(J,1) < 0 .AND. LYTH(J,2) < 0) CALL WRITE_LOG_FILE (NLG,21,MXERR,2)

   IF (LYTH(J,3) < 0.01) LYTH(J,3) = 1.   ! Default RMU
   IF (LYTH(J,4) < 0.01) LYTH(J,4) = 1.   ! Default REPS


   IF (LYTH(J,5) < 1.E-3 .OR. LYTH(J,6) < 1.E-15 .OR. LYTH(J,7) < 1.E-6)  THEN
     LYTH(J,5) = 0   ! default CHRG
     LYTH(J,6) = 0   ! default CTAU
     LYTH(J,7) = 1   ! default CFRQ
   END IF

 END DO

 !!!WRITE(NW,3)
 LITHL = INT(MXLITHL)
 THK   = MXTHK
 IF (NLYR > 1) THEN
   DO J = 1, NLYR-1
     !!!READ (NR,*) LITHL(J), THK(J)
     !!!WRITE(NW,'(2I4,F7.1,T19,A)') J, LITHL(J), THK(J),'J, LITHL(J), THK(J)'
   END DO
 END IF
 !!!READ(NR,*) LITHL(NLYR)
 !!!WRITE(NW,'(2I4,T22,A)') NLYR,LITHL(NLYR),'Basement Lithology'

 DO JL = 1, NLYR
   J = LITHL(JL)

   !!!IF (J < 1 .OR. J > NLITH) THEN
   !!!  WRITE(NW,'(T3,A,I2,A,I4)') 'LITHL(',JL,') =',J
   !!!  CALL WRITE_LOG_FILE (NLG,22,MXERR,2)
   !!!END IF

   RES(JL)  =  LYTH(J,1)
   IF ( RES(JL) < 0) CALL WRITE_LOG_FILE (NLG,23,MXERR,2)

   RMU(JL)   = LYTH(J,3)
   RMUD(JL)   = REAL (RMU(JL),KIND=QL)
   REPS(JL)  = LYTH(J,4)
   CHRG(JL)  = LYTH(J,5)
   CTAU(JL)  = LYTH(J,6)
   CFREQ(JL) = LYTH(J,7)
 END DO
 THKD = REAL (THK,KIND=QL)


!*************************
!*************************

 IF (DO3D == 0) RETURN

!*************************
!*************************


!  Start reading plate variables if DO3D > 0

 ALLOCATE (LITHP(NPLT),SIG_T(NPLT),CHRGP(NPLT),CTAUP(NPLT),CFREQP(NPLT),PLTOP(NPLT),PLTOPD(NPLT), &
           XCNTR(NPLT),YCNTR(NPLT),XCNTRD(NPLT),YCNTRD(NPLT),PLNGTH(NPLT),PLWDTH(NPLT),DZM(NPLT), &
           PLAZM(NPLT),DIP(NPLT),PLDIP(NPLT),PLG(NPLT),PLUNJ(NPLT),PLYR(NPLT))

 LITHP=0; SIG_T=-1.; CHRGP=0; CTAUP=0; CFREQP=0; PLTOP=0; XCNTR=0; YCNTR=0;  PLNGTH=0;
 PLWDTH=0; DZM=0; DIP=0; PLG=0; PLYR = NLYR

 READ(NR,*) CELLW

 WRITE(NW,4) CELLW

 CELLW = MIN (CELLW_MAX, CELLW)
 CELLW = MAX (CELLW_MIN, CELLW)

 DO JP = 1, NPLT
   WRITE(NW,5) JP

! To maintain historical continuity, the input data description calls for the
! plate locator CNTR_East, CNTR_North read in as the east, north coordinates.
! Substitute YCNTR and XCNTR as the east, north coordinates in order to use
! a right-handed system with Z downwards.

   READ(NR,*) LITHP(JP),YCNTRD(JP),XCNTRD(JP),PLTOPD(JP)
   PLTOPD(JP) = -ABS (PLTOPD(JP))   ! No plates above ground
   WRITE(NW,'(2I4,2F13.2,F11.2,3X,A)') JP,LITHP(JP),YCNTRD(JP),XCNTRD(JP),PLTOPD(JP), &
                                      'JP, LITHP,  YCNTRD, XCNTRD,  PLTOP'
   J = LITHP(JP)
   SIG_T(JP) = LYTH(J,2)
   IF (SIG_T(JP) < 0) CALL WRITE_LOG_FILE (NLG,27,MXERR,2)

   READ(NR,*) PLNGTH(JP), PLWDTH(JP), DZM(JP), DIP(JP), PLG(JP)
   WRITE(NW,'(2F9.1,3F8.1,3X,A)') PLNGTH(JP),PLWDTH(JP),DZM(JP),DIP(JP),PLG(JP), &
                                 'PLNGTH, PLWDTH, DZM, DIP, PLG'
   IF (DZM(JP) < 0. .OR. DZM(JP) > 180.) THEN
     CALL WRITE_LOG_FILE (NLG,100,MXERR,2)
     WRITE(NLG,6) JP ;  WRITE(*,6) JP
   END IF
   IF (DIP(JP) < 0. .OR. DIP(JP) > 180.) THEN
     CALL WRITE_LOG_FILE (NLG,100,MXERR,2)
     WRITE(NLG,7) JP ;  WRITE(*,7) JP
   END IF
   IF (PLG(JP) < -90. .OR. PLG(JP) > 90.) THEN
     CALL WRITE_LOG_FILE (NLG,100,MXERR,2)
     WRITE(NLG,8) JP ;  WRITE(*,8) JP
   END IF

 END DO

 A1 = MAXVAL (ABS ( PLG))
 IF (INTRUDE .AND. A1 > 0.001) THEN
   CALL WRITE_LOG_FILE (NLG,33,MXERR,1)
   PLG = 0.
   WRITE(NW,12)
 END IF

 KP = 9*NPLT
 KP1 = KP + NLYR
 KP2 = KP1 + NLYR - 1
 MPAR(KP+1:KP1)  = RES(1:NLYR)
 MPAR(KP1+1:KP2) = THK(1:NLYR-1)

 IF (NPLT > 0) THEN
   ECNTRD = 0.5D0 * (MAXVAL (YCNTRD) + MINVAL (YCNTRD) )  ! Set up body centred origin
   NCNTRD = 0.5D0 * (MAXVAL (XCNTRD) + MINVAL (XCNTRD) )
   IF (ABS (ECNTRD) < 9.D4) ECNTRD = 0.D0
   IF (ABS (NCNTRD) < 9.D4) NCNTRD = 0.D0
 END IF
 XCNTR = REAL (XCNTRD - NCNTRD)   !  Set up old Leroi world
 YCNTR = REAL (YCNTRD - ECNTRD)
 PLTOP = -PLTOPD

 DO JP = 1, NPLT
   KP = 9* (JP-1)
   MPAR(KP+1) = SIG_T(JP)
   MPAR(KP+2) = PLNGTH(JP)
   MPAR(KP+3) = PLWDTH(JP)
   MPAR(KP+4) = PLTOPD(JP)
   MPAR(KP+5) = REAL (YCNTRD(JP))
   MPAR(KP+6) = REAL (XCNTRD(JP))
   MPAR(KP+7) = DZM(JP)
   MPAR(KP+8) = DIP(JP)
   MPAR(KP+9) = PLG(JP)

   J = LITHP(JP)
   IF (J < 1 .OR. J > NLITH) THEN
     WRITE(NW,'(T3,A,I2,A,I4)') 'LITHP(',JP,') =',J
     CALL WRITE_LOG_FILE (NLG,26,MXERR,2)
   END IF

   CHRGP(JP) =  LYTH(J,5)
   CTAUP(JP) =  LYTH(J,6)
   CFREQP(JP) = LYTH(J,7)
   IF ((DIP(JP) < 0.) .OR. (DIP(JP) > 179.99)) CALL WRITE_LOG_FILE (NLG,29,MXERR,2)
 END DO

 PLAZM = (DZM -90.) * D2R
 PLDIP = DIP * D2R
 PLUNJ = PLG * D2R

  1 FORMAT(//T3,'NLAYER =',I3,';   NPLATE =',I3,';   NLITH =',I3)
  2 FORMAT(//T27,'LITHOLOGY PROPERTIES'/T27,'--------------------' &
           //T35,'Relative   Relative     Cole-Cole Parameters'    &
            /T9,'Resistivity  Conductance     MU     Dielectric   CHRG    CTAU       CFREQ'/)
  3 FORMAT(//T3,'LAYERED EARTH INPUT DATA'/T3,'------------------------'/)
  4 FORMAT(//T3,'CELLW =',F7.2)
  5 FORMAT(/T3,'Input Data for Plate',I3/T3,'-----------------------')
  6 FORMAT(/T3,'Error in Plate',I3,'.  Dip azimuth must be between 0 and 180 degrees.')
  7 FORMAT(/T3,'Error in Plate',I3,'.  Dip must be between 0 and 180 degrees.')
  8 FORMAT(/T3,'Error in Plate',I3,'.  Plunge must be between -90 and 90 degrees.')
 10 FORMAT(/T3,'Although NPLATE =',I3,' DO3D = 0.  Thus only the layered half-space' &
           /T3,'response will be computed')
 12 FORMAT(/T3,'DO3D = 3 requires the plunge of all plates to be set to 0')

   END SUBROUTINE READ_MODEL_DATA

   SUBROUTINE READ_PARAMETER_CONTROL
!  ---------------------------------

!***  Called by: MAIN
!***      Calls: WRITE_LOG_FILE

 INTEGER NFIX,CTYPE,PLT_INDX,LYR_INDX,KPAR,J1
 REAL E1,E2,E3,A1,A2
 CHARACTER(LEN=12) PLATE_PRM(9),LYR_PRM(2)
 DATA LYR_PRM /  'Resistivity','Thickness'/
 DATA PLATE_PRM /'Conductance','Depth to top','Plate length','Dip width','CNTR_East', &
                 'CNTR_North','Dip azimuth','Dip angle','Plunge'/

!  Set degree of constraint on each parameter
!  CXPAR = 0 => parameter is completely free to vary as dictated by inversion step
!        = 1 => parameter is fixed
!        = 2 => parameter is constrained by elasticity.
!        = 3 => parameter bounds are buffered.

 READ(NR,*) NFIX,MAXITS,CNVRG,INVPRT
 IF (INVPRT < 0 .OR. INVPRT > 3) INVPRT = 1
 WRITE(NW,1) NFIX,MAXITS,CNVRG,INVPRT
 SELECT CASE (CNVRG)
 CASE (1)
   PCTCNV = 0.1
   NDSTP = 2
   ALLOCATE (KPCT(3))
   KPCT(1) = 5 ; KPCT(2) = 3
 CASE (2)
   READ(NR,*) PCTCNV
   NDSTP = 2
   ALLOCATE (KPCT(3))
   KPCT(1) = 5 ; KPCT(2) = 3
 CASE (10)
   PCTCNV = 0.1
   READ(NR,*) NDSTP
   ALLOCATE (KPCT(NDSTP))
   READ(NR,*) KPCT(1:NDSTP)
 CASE (20)
   READ(NR,*) PCTCNV
   READ(NR,*) NDSTP
   ALLOCATE (KPCT(NDSTP))
   READ(NR,*) KPCT(1:NDSTP)
 CASE DEFAULT
   CALL WRITE_LOG_FILE (NLG,63,MXERR,2)
 END SELECT
 WRITE(NW,2) PCTCNV,MAXITS ; WRITE(*,2) PCTCNV,MAXITS
 WRITE(NW,3) NDSTP,KPCT(1:NDSTP)

 ALLOCATE (ELAS(NPAR),LBND(NPAR),UBND(NPAR),CXPAR(NPAR))
 CXPAR = 0
 IF (NFIX > 0) THEN
   WRITE(NW,4)
   DO JP = 1, NFIX
     E1 = 0.
     E2 = -1.E10
     E3 =  1.E10
     READ(NR,*) CTYPE
     BACKSPACE NR
     SELECT CASE (CTYPE)            ! J1 is a dummy variable
     CASE(1)
       READ(NR,*) J1,PLT_INDX,KPAR
       E1 = 0.
     CASE(2)
       READ(NR,*) J1,PLT_INDX,KPAR,E1
     CASE(3)
       READ(NR,*) J1,PLT_INDX,KPAR,E1,E2,E3
     END SELECT

     IF (PLT_INDX > NPLT) THEN
       CALL WRITE_LOG_FILE (NLG,202,MXERR,1)
       WRITE(NLG,20) JP
       CYCLE
     ELSE IF (PLT_INDX > 0) THEN
       IF (KPAR < 1 .OR. KPAR > 9) THEN
         CALL WRITE_LOG_FILE (NLG,203,MXERR,1)
         WRITE(NLG,20) JP
         CYCLE
       END IF
     ELSE IF (PLT_INDX < -NLYR) THEN
       CALL WRITE_LOG_FILE (NLG,210,MXERR,1)
       WRITE(NLG,20) JP
       CYCLE
     ELSE IF (PLT_INDX < 0) THEN
       IF (KPAR < 1 .OR. KPAR > 2) THEN
         CALL WRITE_LOG_FILE (NLG,211,MXERR,1)
         WRITE(NLG,20) JP
         CYCLE
       END IF
     ELSE IF (PLT_INDX == 0) THEN
       CALL WRITE_LOG_FILE (NLG,212,MXERR,1)
       WRITE(NLG,20) JP
       CYCLE
     END IF

     E1 = ABS(E1)
     IF (E1 < 0.05) E1 = 0.      ! Hold for elasticities < 0.05
     IF (E1 > 0.95) E1 = 1.      ! Allow full freedom for elasticities > 0.95
     IF (E2 > E3) THEN           ! Switch if LB > UB
       A1 = E3
       E3 = E2
       E2 = A1
     END IF
     A1 = E3 - E2
     A2 = 0.005 * (ABS (E2) + ABS (E3))    ! If bound interval is infinitesimal,
     IF (A1  < A2) E1 = 0.                 ! set elastcicty to 0.

     IF (PLT_INDX < 0) THEN
       LYR_INDX = ABS (PLT_INDX)
       J1 = 9*NPLT + LYR_INDX             !  Resistivity of layer LYR_INDX
       IF (KPAR == 2) J1 = J1 + NLYR     !  Thickness of layer LYR_INDX
       CXPAR (J1) = CTYPE
       IF (E1 < 0.05) CXPAR(J1) = 1
       IF (CTYPE == 3) THEN
         WRITE(NW,5) J1,LYR_INDX,KPAR,LYR_PRM(KPAR),E1,E2,E3,CXPAR(J1)
       ELSE
         WRITE(NW,6) J1,LYR_INDX,KPAR,LYR_PRM(KPAR),E1,CXPAR(J1)
       END IF
     ELSE
       J1 = 9* (PLT_INDX - 1) + KPAR
       CXPAR (J1) = CTYPE
       IF (E1 < 0.05) CXPAR(J1) = 1
       IF (CTYPE == 3) THEN
         WRITE(NW,7) J1,PLT_INDX,KPAR,PLATE_PRM(KPAR),E1,E2,E3,CXPAR(J1)
       ELSE
         WRITE(NW,8) J1,PLT_INDX,KPAR,PLATE_PRM(KPAR),E1,CXPAR(J1)
       END IF
     END IF
     ELAS(J1) = E1
     LBND(J1) = E2
     UBND(J1) = E3
   END DO
   WRITE(NW,9)
 ELSE
   WRITE(NW,10)
 END IF

!  Stablise angles

 DO JP = 1,NPLT
   J1 = 9*JP-2
   IF (CXPAR(J1) == 0) THEN  ! Restrict DIP_AZIMUTH between 0 and PI
     CXPAR(J1) = 3
     ELAS(J1) = 0.9
     LBND(J1) = 0.
     UBND(J1) = 180.
   ELSE
     LBND(J1) = MAX (LBND(J1), 0.)
     UBND(J1) = MIN (UBND(J1), 180.)
   END IF

   J1 = 9*JP-1
   IF (CXPAR(J1) == 0) THEN  ! Restrict DIP between 0 and PI
     CXPAR(J1) = 3
     ELAS(J1) = 0.9
     LBND(J1) = 0.
     UBND(J1) = 180.
   ELSE
     LBND(J1) = MAX (LBND(J1), 0.)
     UBND(J1) = MIN (UBND(J1), 180.)
   END IF
   J1 = 9*JP
   IF (CXPAR(J1) == 0) THEN  ! Restrict PLUNGE between -PI/2 and PI/2
     CXPAR(J1) = 3
     ELAS(J1) = 0.9
     LBND(J1) = -90.
     UBND(J1) =  90.
   ELSE
     LBND(J1) = MAX (LBND(J1), -90.)
     UBND(J1) = MIN (UBND(J1),  90.)
   END IF
 END DO

  1 FORMAT(//T3,'---------------------------------------------------' &
            /T3,'Inversion Controls for Plate Parameters using Leroi' &
            /T3,'---------------------------------------------------' &
           //T3,'NFIX =',I3,3X,'MAXITS =',I3,3X,'CNVRG =',I2,3X,'INVPRT =',I2)
  2 FORMAT(/T3,'The inversion will finish if the RMS error is less than',F5.1,' percent' &
           /T3,'or for some other as yet undisclosed reason.' &
           /T3,'A maximum of',I3,' iterations will be allowed.')
  3 FORMAT(T3,'The inversion sequence will use',I2,' numerical derivative steps' &
          /T3,'Values in percent:',10I3)
  4 FORMAT(//T12,'Constrained Parameters'/T12,'----------------------' &
           //T5,'Global  Plate  Layer  Parameter'                                                        &
            /T5,'Index   Index  Index   Index      Parameter      Elasticity  Lower Bound   Upper Bound   CTYPE' &
            /T5,'------  -----  -----  ---------   ---------      ----------  -----------   -----------   -----')
  5 FORMAT(T7,I2,T21,I2,T30,I1,T39,A,T58,F4.2,T66,G12.4,T80,G12.4,T94,I3)
  6 FORMAT(T7,I2,T21,I2,T30,I1,T39,A,T58,F4.2,T94,I3)
  7 FORMAT(T7,I2,T14,I2,T30,I1,T39,A,T58,F4.2,T66,G12.4,T80,G12.4,T94,I3)
  8 FORMAT(T7,I2,T14,I2,T30,I1,T39,A,T58,F4.2,T94,I3)
  9 FORMAT(/T3,90('-'))
 10 FORMAT(/T3,'All plate and host parameters will be allowed to vary during inversion')
 20 FORMAT(T3,'Constraint',I2,' ignored')

 END SUBROUTINE READ_PARAMETER_CONTROL

 SUBROUTINE PREPARE_INVRT_DATA
!-----------------------------

 INTEGER MCHNL,LINE_CHK,FD_ORDER,NSTAT,MD,NDT,JC,JL,JR,JF,J0,J1,J2,JRD,NCL,KPC(4)
 INTEGER, DIMENSION(NLINES):: KMP,NKMP
 REAL, ALLOCATABLE,DIMENSION (:) :: DATA_FLOOR,QDATA,Q2DATA
 LOGICAL KMP1
 CHARACTER (LEN=1) TCHR
 CHARACTER(LEN=20) CTXT(3),QL0

! ----------------------------------------------------------------
! Set inversion dimensions:
!
!  NCHNL = number of time domain channels
!  MCHNL = NCHNL for Time domain or NFRQ for frequency domain
!  MCHNL = 2*NFRQ only if both inphase and quadrature data are included
!
!  RDATA & RWTS are data and weights in array form (MCHNL, NSTAT)
!  XDATA is the  data in column form  (MCHNL * NSTAT)
!  RWTS  is now restricted to integer values of 0 or 1 (reject or accept)
!  Note that data are normalised so that balance weighting is unnecessary.
!
!  XMODL contains model results in column form (MCHNL * NSTAT)
!  NRX = 1 for TD & NFRQ for FD
!
!  For TD, RDATA is ordered as vertical components for all delay times
!  followed by all in-line components, followed by all transverse
!  components for each station.
!
!  For FD data, in-phase data is followed by quadrature data
!
!  XDATA and XMODL are RDATA and BFTL, respectively, stacked into
!  1D array excluding all data weighted to zero.
! ----------------------------------------------------------------


!  Start reading from Leroi.inv on UNIT NRI = 13
!  First skip over alL comment lines

 ALLOCATE (SINGLE(NLINES))
 SINGLE = .FALSE.
 KMP1 = .FALSE.
 IF (SURVEY_TYPE == 4 .OR. ISYS == 2) THEN
   KMP1 = .TRUE.                             ! Coincident loop or Sampo
   SINGLE = .TRUE.                           ! Single component is put into X position in data
 END IF

 DO
   READ (NRI,'(A)') TCHR
   IF (TCHR /= '\' .AND. TCHR /= '/') EXIT
 END DO
 BACKSPACE (NRI)

 READ(NRI,*) FD_ORDER
 IF (TDFD == 2) THEN
   MCHNL = 2*NFRQ
   IF (FD_ORDER == 0) MCHNL = NFRQ
 ELSE
   FD_ORDER = 0
   MCHNL = NCHNL
 END IF
 WRITE(NW,7) FD_ORDER
 IF (FD_ORDER /= 0 .AND. FD_ORDER /= 1 .AND. FD_ORDER /= 2) THEN
   CALL WRITE_LOG_FILE(NLG,58,MXERR,2)
   WRITE(NLG,'(T3,A,I2)') 'FD_ORDER = ',FD_ORDER
 END IF

 ALLOCATE (QDATA(MCHNL*3),Q2DATA(MCHNL*3),DATA_FLOOR(MCHNL))

 DATA_FLOOR = 0.
 KMP = 1
 NKMP = 1
 DO JL = 1,NLINES
   WRITE(QL0,*) LINE(JL)
   READ(QL0,'(A)') CTXT(1)           ! Line number
   WRITE(NW,1) TRIM(ADJUSTL(CTXT(1)))

   IF (ISYS == 2) THEN
     READ(NRI,*) LINE_CHK,NSTAT
     WRITE(NW,2) KMP(JL), CMP(JL)
     CALL VALIDATE_LINE (NLG,MXERR,LINE_CHK,NSTAT,JL,NLINES,LINE,NRX)

   ELSE IF (RX_TYPE(JL) == 1 .OR. RX_TYPE(JL) == 3) THEN
     READ(NRI,*) LINE_CHK,NSTAT,KMP(JL)
     CALL VALIDATE_CMP (NLG,MXERR,JL,NLINES,CMP,NCMPL,ISYS)
     WRITE(NW,2) KMP(JL), CMP(JL)
     CALL VALIDATE_LINE (NLG,MXERR,LINE_CHK,NSTAT,JL,NLINES,LINE,NRX)
     CALL VALIDATE_KMP (NLG,MXERR,JL,NLINES,CMP,KMP)
   ELSE
     READ(NRI,*) LINE_CHK,NSTAT
     WRITE(NW,2) KMP(JL), CMP(JL)
     CALL VALIDATE_LINE (NLG,MXERR,LINE_CHK,NSTAT,JL,NLINES,LINE,NRX)
   END IF
   IF (KMP(JL) > 9) NKMP(JL) = 2
   IF (KMP(JL) > 99) NKMP(JL) = 3

   IF (NSTAT /= NRX(JL)) THEN
     CALL WRITE_LOG_FILE(NLG,100,MXERR,2)
     WRITE(NLG,10) TRIM (ADJUSTL (CTXT(1)))
     WRITE(NLG,13) JL,NRX(J),NSTAT
   END IF

   IF (TDFD < 2) THEN
     READ(NRI,*) DATA_FLOOR(1)
     WRITE(NW,3) DATA_FLOOR(1)
     DATA_FLOOR(2:MCHNL) = DATA_FLOOR(1)
   ELSE IF (TDFD == 2) THEN
     READ(NRI,*) DATA_FLOOR(1:MCHNL)

     IF (FD_ORDER /= 0) THEN
       WRITE(NW,4)
       DO JF = 1,NFRQ
         WRITE(NW,6) JF,FREQ(JF),DATA_FLOOR (JF),DATA_FLOOR (JF+NFRQ)
       END DO
     ELSE
       WRITE(NW,5)
       DO JF = 1,NFRQ
         WRITE(NW,6) JF,FREQ(JF),DATA_FLOOR (JF)
       END DO
     END IF
   END IF

!======================
!      DATA ENTRY
!======================

!  If there is more than one spatial component, arrange the data such that all
!  the data for one component is followed by all the data for the next.
!  When both inphase and quadrature data are present, arrange the data for each
!  component such that all the inphase data are followed by all the quadrature
!  data for that component.
!  For multi-component data where the data is presented as inphase and quadrature
!  duples, the data is temporarily arranged such that all the inphase data for all
!  components is followed by all the quadrature data for all components.

   IF (RX_TYPE(JL) == 2) SINGLE(JL) = .TRUE.
   WRITE(NW,17) TRIM(ADJUSTL(CTXT(1)))
   MD = NKMP(JL) * MCHNL
   QDATA = 0.;  Q2DATA = 0.
   DO JR = 1,NRX(JL)
     READ(NRI,*) JRD,QDATA(1:MD)
     IF (JRD /= JR) CALL WRITE_LOG_FILE (NLG,62,MXERR,2)
     IF (SINGLE(JL)) THEN
       KPC = 1
       IF (FD_ORDER == 1) THEN
         DO JF = 1,NFRQ
           Q2DATA(JF) = QDATA(2*JF-1)     ! arrange such that all inphase followed
           Q2DATA(JF+NFRQ) = QDATA(2*JF)  ! by all quadrature data
         END DO
         QDATA(1:MCHNL) = Q2DATA(1:MCHNL)
       END IF
       RDATA (1:MCHNL,JR,1,JL) = QDATA(1:MCHNL)
       IF (.NOT. KMP1) RWTS(1:MCHNL,JR,2:3,JL) = 0
       CYCLE
     END IF

!    Rearrange multi-component data if necessary.
!    If FD_ORDER = 1, arrange such that the inphase data for each spatial
!    component, is followed by all the quadrature data for that component.

     IF (FD_ORDER == 1) THEN
       DO JC = 1,NKMP(JL)
         J0 = NFRQ * (2*JC-2)
         DO JF = 1,NFRQ
           Q2DATA(J0+JF) = QDATA(J0+2*JF-1)
           Q2DATA(J0+JF+NFRQ) = QDATA(J0+2*JF)
         END DO
       END DO
       QDATA(1:MD) = Q2DATA(1:MD)
     END IF

     DO JC = 1, NKMP(JL)
       J1 = (JC -1) * MCHNL + 1
       J2 = JC * MCHNL
       RDATA(1:MCHNL,JR,JC,JL) = QDATA(J1:J2)
     END DO

!  Arrange the multi-component data such that the order is X, Y, Z
!  and that these are in the positions 1, 2, 3 respectively in RDATA

     SELECT CASE (KMP(JL))
     CASE (1)
       RDATA(1:MCHNL,JR,2:3,JL) = 0.
     CASE (2)
       RDATA(1:MCHNL,JR,2,JL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = 0.
       RDATA(1:MCHNL,JR,3,JL) = 0.
     CASE (3)
       RDATA(1:MCHNL,JR,3,JL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1:2,JL) = 0.
     CASE (12)
       RDATA(1:MCHNL,JR,3,JL) = 0.
     CASE (13)
       RDATA(1:MCHNL,JR,3,JL) = RDATA(1:MCHNL,JR,2,JL)
       RDATA(1:MCHNL,JR,2,JL) = 0.
     CASE (23)
       RDATA(1:MCHNL,JR,3,JL) = RDATA(1:MCHNL,JR,2,JL)
       RDATA(1:MCHNL,JR,2,JL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = 0.
     CASE (32)
       RDATA(1:MCHNL,JR,3,JL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = 0.
     CASE (31)
       RDATA(1:MCHNL,JR,3,JL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = RDATA(1:MCHNL,JR,2,JL)
       RDATA(1:MCHNL,JR,2,JL) = 0.
     CASE (21)
       QDATA(1:MCHNL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = RDATA(1:MCHNL,JR,2,JL)
       RDATA(1:MCHNL,JR,2,JL) = QDATA(1:MCHNL)
       RDATA(1:MCHNL,JR,3,JL) = 0.
     CASE (213)
       QDATA(1:MCHNL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = RDATA(1:MCHNL,JR,2,JL)
       RDATA(1:MCHNL,JR,2,JL) = QDATA(1:MCHNL)
     CASE(321)
       QDATA(1:MCHNL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = RDATA(1:MCHNL,JR,3,JL)
       RDATA(1:MCHNL,JR,3,JL) = QDATA(1:MCHNL)
     CASE (132)
       QDATA(1:MCHNL) = RDATA(1:MCHNL,JR,3,JL)
       RDATA(1:MCHNL,JR,3,JL) = RDATA(1:MCHNL,JR,2,JL)
       RDATA(1:MCHNL,JR,2,JL) = QDATA(1:MCHNL)
     CASE (231)
       QDATA(1:MCHNL) = RDATA(1:MCHNL,JR,3,JL)
       RDATA(1:MCHNL,JR,3,JL) = RDATA(1:MCHNL,JR,2,JL)
       RDATA(1:MCHNL,JR,2,JL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = QDATA(1:MCHNL)
     CASE(312)
       QDATA(1:MCHNL) = RDATA(1:MCHNL,JR,1,JL)
       RDATA(1:MCHNL,JR,1,JL) = RDATA(1:MCHNL,JR,2,JL)
       RDATA(1:MCHNL,JR,2,JL) = RDATA(1:MCHNL,JR,3,JL)
       RDATA(1:MCHNL,JR,3,JL) = QDATA(1:MCHNL)
     END SELECT

     KPC = -9
     SELECT CASE (CMP(JL))                ! Set RWTS = 0 for unused data components.
     CASE (1)                             ! when data includes all three components
       RWTS(1:MCHNL,JR,2:3,JL) = 0
       KPC(1) = 1
     CASE (2)
       RWTS(1:MCHNL,JR,1,JL) = 0
       RWTS(1:MCHNL,JR,3,JL) = 0
       KPC(1) = 2
     CASE (3)
       RWTS(1:MCHNL,JR,1:2,JL) = 0
       KPC(1) = 3
     CASE (12)
       RWTS(1:MCHNL,JR,3,JL) = 0
       KPC(1) = 1
       KPC(2) = 2
     CASE (13)
       RWTS(1:MCHNL,JR,2,JL) = 0
       KPC(1) = 1
       KPC(2) = 3
     CASE (23)
       RWTS(1:MCHNL,JR,1,JL) = 0
       KPC(1) = 2
       KPC(2) = 3
     CASE (123)
       KPC(1) = 1
       KPC(2) = 2
       KPC(3) = 3
     END SELECT
   END DO

   NCL = NCMPL(JL)
   DO JR = 1,NRX(JL)
     DO J1 = 1,NCL
       JC = KPC(J1)
       DO JF = 1,MCHNL
         IF (ABS (RDATA(JF,JR,JC,JL)) < DATA_FLOOR(JF)) RWTS(JF,JR,JC,JL) = 0
         RDATA(JF,JR,JC,JL) = RDATA(JF,JR,JC,JL) * RWTS(JF,JR,JC,JL)
       END DO
     END DO
   END DO

 END DO
 DEALLOCATE (QDATA,Q2DATA)

 KPRT = -1
 IF (TDFD == 2) THEN
   CALL WRITE_FD (NW,NW1,KPRT,NFRQ,MCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                  SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,FREQ,HEADER_ID,CMP,BFTL,RDATA,RWTS)
 ELSE
   TMS = 0.5 * (TOPN + TCLS)
   CALL WRITE_TD (NW,NW1,KPRT,NCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                  SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,TMS,HEADER_ID,CMP,BFTL,RDATA,RWTS)
 END IF

 NDT = MCHNL * MRXL * NLINES * 3
 ALLOCATE (QDATA(NDT))

 NDATA = 0                       ! Compact the data into a linear array.
 QDATA = 0                       ! Skip all data with weight 0
 DO JL = 1,NLINES
   DO JC = 1,MCMP
     DO JR = 1,NRX(JL)
       DO JF = 1,MCHNL
         IF (RWTS(JF,JR,JC,JL) > 0) THEN
           NDATA = NDATA + 1
           QDATA(NDATA) = RDATA(JF,JR,JC,JL)
         END IF
       END DO
     END DO
   END DO
 END DO
 WRITE(NW,20) NDATA
 IF (NDATA < NPAR) THEN
   CALL WRITE_LOG_FILE (NLG,100,MXERR,2)
   WRITE(NW,21) NPAR,NDATA
 END IF

 ALLOCATE (XDATA(NDATA),XMODL(NDATA))
 XDATA(1:NDATA) = QDATA(1:NDATA)
 DEALLOCATE (QDATA)

! Write the all the data not weighted to zero.


  1 FORMAT(/T2,'=======================================' &
           /T3,'Inversion controls for Line ',A)
  2 FORMAT(/T3,'KMP =',I4,4X,'CMP =',I4)
  3 FORMAT(/T3,'Time-Domain Data Floor =',G12.4,1X,A)
  4 FORMAT(/T8,'Frequency Domain Data Floors'/T8,'----------------------------' &
          //T8,'Freq     In-phase   Quadrature'/)
  5 FORMAT(/T8,'Frequency Domain Data Floors'/T8,'----------------------------' &
          //T8,'Freq     Floor'/)
  6 FORMAT(I4,F9.0,2G12.4)
  7 FORMAT(/T3,'FD_ORDER =',I2)
 10 FORMAT( T3,'Problem with Line ',A)
 13 FORMAT(T3,'NRX(',I2,') =',I2,4X,'NSTAT =',I2)
 17 FORMAT(/T3,'Weighted data for Line ',A/)
 20 FORMAT(//T3,'NDATA =',I5,' data points.' &
            /T3,'This excludes components not specified by CMP and data from the above set' &
            /T3,'that hasbeen weighted to zero using data floors.')
 21 FORMAT(/T3,'For Leroi, the number pf parameters cannot exceed the number of data points.' &
           /T3,'NPAR =',I3,4X,'NDATA =',I5)

   END SUBROUTINE PREPARE_INVRT_DATA


   SUBROUTINE SHOW_AND_TELL
!  ------------------------

! Prints out arrays and model in model-centred coordinates

!*** Called by MAIN

 IMPLICIT NONE
 REAL SXA,SXD,DEPTH(NLYR),DEL,XTNT

 WRITE (NW,1)
 IF (ABS (ECNTRD) + ABS (NCNTRD) > 1.D0) THEN
   WRITE (NW,2) ECNTRD,NCNTRD
 ELSE
   WRITE(NW,3)
 END IF

 SELECT CASE (SURVEY_TYPE)
 CASE (1)   !  Line with single transmitter and multiple receivers
   DO JL = 1,NLINES
     JS = LNTR(1,JL)
     WRITE(NW,4) LINE(JL),R2D*SVAZM(JL)

     SELECT CASE (SOURCE_TYPE)
     CASE(1:2)                               ! Open or closed loop Tx
       IF (SOURCE_TYPE == 1) WRITE(NW,5)
       IF (SOURCE_TYPE == 2) WRITE(NW,6)
       WRITE(NW,7) JS,NVRTX(JS)
       DO JV = 1, NVRTX(JS)
         WRITE(NW,'(I5,3F11.2)') JV,SXE(JV,JS),SXN(JV,JS),SXZ(JS)
       END DO

     CASE(3)                               ! Magnetic dipole Tx
       WRITE(NW,8)
       WRITE(NW,9)
       WRITE(NW,'(I4,3F11.2,2F10.2,G12.4)') JS,SXE(1,JS),SXN(1,JS),SXZ(JS),R2D*SXDIP(JS),R2D*SXAZM(JS),TXMNT
     END SELECT

     SELECT CASE (RX_TYPE(JL))
     CASE(1)                     !  Magnetic dipole receivers
       WRITE(NW,11) LINE(JL)
       IF (IDH(JL) < 3) THEN               !  Surface position only
         WRITE(NW,10)
         DO JR = 1,NRX(JL)
           WRITE(NW,'(I4,3F11.2,G12.4)') JR,RXE(JR,JL,1),RXN(JR,JL,1),RXZ(JR,JL),RXMNT(JL)
         END DO
       ELSE
         WRITE(NW,9)
         DO JR = 1,NRX(JL)
           WRITE(NW,'(I4,3F11.2,2F7.1,G12.4)') JR,RXE(JR,JL,1),RXN(JR,JL,1),RXZ(JR,JL),RXDIP(JR,JL),RXAZM(JR,JL),RXMNT(JL)
         END DO
       END IF

     CASE(2)                     !  Grounded wire receivers
       WRITE(NW,12) LINE(JL)
       DO JR = 1, NRX(JL)
         WRITE(NW,'(I4,6F11.2)') JR,RXE(JR,JL,1),RXN(JR,JL,1),RXE(JR,JL,2),RXN(JR,JL,2),RXZ(JR,JL)
       END DO

     CASE(3)                     !  Point E-field receivers
       WRITE(NW,61) LINE(JL)
       DO JR = 1,NRX(JL)
         WRITE(NW,'(I4,3F11.2)') JR,RXE(JR,JL,1),RXN(JR,JL,1),RXZ(JR,JL)
       END DO
     END SELECT
   END DO
 CASE (2)   !  Line with moving loop transmitters and one or more fixed offset receivers
   WRITE(NW,13)
   DO JL = 1,NLINES
     JR = 0
     DO JS = LNTR(1,JL), LNTR(2,JL)
       JR = JR + 1
       DO JV = 1,4
         IF (JV == 1) THEN
           IF (JR == 1) THEN
             WRITE(NW,15) LINE(JL),JS,JV,SXE(1,JS),SXN(1,JS),RXE(1,JL,1),RXN(1,JL,1),YXZPLT(1:2,JR,JL)
           ELSE
             WRITE(NW,16) JS,JV,SXE(1,JS),SXN(1,JS),RXE(JR,JL,1),RXN(JR,JL,1),YXZPLT(1:2,JR,JL)
           END IF
         ELSE
           WRITE(NW,17) JV,SXE(JV,JS),SXN(JV,JS)
         END IF
       END DO
     END DO
   END DO

 CASE (3)   !  Line with moving MD transmitters and one or more fixed offset receivers
   WRITE(NW,18)
   DO JL = 1,NLINES
     JR = 0
     DO JS = LNTR(1,JL), LNTR(2,JL)
       JR = JR + 1
       SXD = R2D*SXDIP(JS); SXA = R2D*SXAZM(JS)
       IF (JR == 1) THEN
         WRITE(NW,19) LINE(JL),JS,RXE(1,JL,1),RXN(1,JL,1),RXZ(1,JL),SXE(1,JS),SXN(1,JS),SXZ(JS),SXD,SXA
       ELSE
         WRITE(NW,20) JS,RXE(JR,JL,1),RXN(JR,JL,1),RXZ(JR,JL),SXE(1,JS),SXN(1,JS),SXZ(JS)
       END IF
     END DO
   END DO

 CASE (4)                                 !  Coincident loop lines
   WRITE(NW,14)

   DO JL = 1,NLINES
     JR = 0
     DO JS = LNTR(1,JL), LNTR(2,JL)
       JR = JR + 1
       DO JV = 1,4
         IF (JV == 1) THEN
           IF (JR == 1) THEN
             WRITE(NW,15) LINE(JL),JS,JV,SXE(1,JS),SXN(1,JS),YXZPLT(1:2,JR,JL)
           ELSE
             WRITE(NW,16) JS,JV,SXE(1,JS),SXN(1,JS),YXZPLT(1:2,JR,JL)
           END IF
         ELSE
           WRITE(NW,17) JV,SXE(JV,JS),SXN(JV,JS)
         END IF
       END DO
     END DO
   END DO

 CASE (5)   !  Downhole probe
   WRITE(NW,21)
   DO JL = 1,NLINES
     JR = 0
     DO JS = LNTR(1,JL), LNTR(2,JL)
       JR = JR + 1
       SXD = R2D*SXDIP(JS); SXA = R2D*SXAZM(JS)
       IF (JR == 1) THEN
         WRITE(NW,22) LINE(JL),JS,SXE(1,JS),SXN(1,JS),SXZ(JS),SXD,SXA,RXE(1,JL,1),RXN(1,JL,1),RXZ(1,JL)
       ELSE
         WRITE(NW,23) JS,SXE(1,JS),SXN(1,JS),SXZ(JS),SXD,SXA,RXE(JR,JL,1),RXN(JR,JL,1),RXZ(JR,JL)
       END IF
     END DO
   END DO

 END SELECT

 SELECT CASE (SURVEY_TYPE)
 CASE (1,3,5)
   DO JL = 1,NLINES     ! Plot coordinates
     WRITE(NW,24) LINE(JL)
     DO JR = 1,NRX(JL)
       WRITE(NW,'(I4,2F13.1,F9.1)') JR,YXZPLT(1:3,JR,JL)
     END DO
   END DO
 END SELECT

! Set up pretty output for layered earth.

 WRITE(NW,31)
 DEPTH = 0.
 DO JL = 2, NLYR
   DEPTH(JL) = DEPTH(JL-1) + THK(JL-1)
 END DO
 DO JL = 1, NLYR
   IF(JL == NLYR) THEN
     WRITE(NW,32) NLYR,DEPTH(JL),RES(NLYR),RMU(NLYR),REPS(NLYR),CHRG(NLYR),CFREQ(NLYR),CTAU(NLYR)
   ELSE
     WRITE(NW,33) JL,THK(JL),DEPTH(JL),RES(JL),RMU(JL),REPS(JL),CHRG(JL),CFREQ(JL),CTAU(JL)
   END IF
 END DO

 PLYR = NLYR
 DO JP = 1,NPLT
   IF (INTRUDE) THEN
     DO JL = 1,NLYR
       IF (PLTOP(JP) + 0.01 > DEPTH(JL)) PLYR(JP) = JL      ! Find layer containing Plate JP
     END DO
     IF (PLYR(JP) < NLYR) THEN
       XTNT = PLWDTH(JP) * SIN (PLDIP(JP))
       DEL = PLTOP(JP) + XTNT - DEPTH(PLYR(JP)+1)
       IF (DEL > 0.) THEN
         CALL WRITE_LOG_FILE (NLG,100,MXERR,1)
         XTNT = XTNT - DEL
         PLWDTH(JP) = XTNT / SIN (PLDIP(JP))
         WRITE(NW,46) JP; WRITE(NLG,46) JP; WRITE(*,46) JP
       END IF
     END IF
   ELSE
     IF (PLTOP(JP) < DEPTH(NLYR)) THEN
       DEL = DEPTH(NLYR) - PLTOP(JP)
       PLTOP(JP) = DEPTH(NLYR) + 0.001
       CALL WRITE_LOG_FILE (NLG,100,MXERR,1)
       WRITE(NW,44) JP,DEL; WRITE(NLG,44) JP,DEL; WRITE(*,44) JP,DEL
     END IF
   END IF
   IF (ABS (XCNTR(JP)) < 0.001) XCNTR(JP) = 0.
   IF (ABS (YCNTR(JP)) < 0.001) YCNTR(JP) = 0.
   IF (PLYR(JP) < NLYR) THEN
     WRITE(NW,40) JP,PLYR(JP)
   ELSE
     WRITE(NW,47) JP
   END IF
   WRITE(NW,41) LITHP(JP),SIG_T(JP),CHRGP(JP),CFREQP(JP),CTAUP(JP)
   WRITE(NW,42) PLTOP(JP),DZM(JP),PLNGTH(JP),XCNTR(JP),DIP(JP),PLWDTH(JP),YCNTR(JP),PLG(JP)
 END DO

 IF (TDFD < 2 .AND. DO3D /= 0) WRITE(NW,51)  ! Time-domain option
 IF (TDFD == 2 .AND. DO3D /= 0) WRITE(NW,52)  ! Frequency-domain option

 WRITE(NW,53)                 ! End of input data description


 1 FORMAT(//T3,'Before computation begins, Leroi may transform array and model coordinates' &
           /T3,'from GPS coordimnates where elevation increases positive upwards to a'      &
           /T3,'body-centred system where depth increases positive downwards.'              &
           /T3,'In this system, the dip of magnetic dipole transmitters and receivers'      &
           /T3,'= 0 for vertical dipoles and 90 for horizontal dipoles.')
 2 FORMAT( /T3,'=============================================================================='  &
           /T3,'The new computational horizontal origin is over the centre of the model region.' &
          //T3,'The transmitter, receiver and plate locations below are given with respect to '  &
           /T3,'the new computation origin at: EAST: ',F12.2,';    NORTH: ',F12.2                &
           /T3,'==============================================================================')
 3 FORMAT( /T3,'The computational horizontal origin remains unchanged.')
 4 FORMAT(/T3,'Transmitter and receiver locations for Line',I7 &
           /T3,'Survey aximuth =',F5.0,'degrees clockwise from North.')
 5 FORMAT(/T3,'Vertex Locations for Loop Sources' &
          /T3,'---------------------------------')
 6 FORMAT(/T3,'Vertex Locations for Grounded Wire Sources' &
          /T3,'------------------------------------------')
 7 FORMAT(/T7,'Transmitter',I3,' has',I3,' vertices.'&
        //T10,'Easting   Northing'/)
 8 FORMAT(/T3,'Dipole Source Specification')
 9 FORMAT(/T9,'Easting   Northing      Depth    Txcln    Azimuth   Moment' &
          /T9,'-------   --------      -----    -----    -------   ------')
 10 FORMAT(/T9,'Easting   Northing      Depth   Moment' &
           /T9,'-------   --------      -----   ------')
 11 FORMAT(//T3,'Locations for Magnetic Dipole Receivers in Line',I7 &
            /T3,'------------------------------------------------------')
 12 FORMAT(//T3,'Electrode Positions for Electric Dipoles in Line',I7    &
            /T3,'-------------------------------------------------------' &
           /T10,'East 1    North 1     East 2    North 2     Depth'/)
 13 FORMAT(/T9,'Receiver & Transmitter Vertex Coordinates' &
           /T9,'-----------------------------------------' &
          //T25,'Transmitter             Receiver               Plot Point' &
           /T5,'Line',T23,'East      North',T47,'East      North',T71,'East      North')
 14 FORMAT(/T9,'Coincident Loop Vertex Coordinates' &
           /T9,'----------------------------------' &
          //T25,'Transmitter',T49,'Plot Point'                  &
           /T5,'Line',T23,'East      North',T47,'East      North')
 15 FORMAT(/I8,2I4,2F11.1,2(2X,2F11.1))
 16 FORMAT(/8X,2I4,2F11.1,2(2X,2F11.1))
 17 FORMAT(12X,I4,2F11.1,2X,2F11.1)
 18 FORMAT(/T17,'Receiver & Transmitter Dipole Coordinates' &
           /T17,'-----------------------------------------' &
         //T23,'Receiver',T58,'Transmitter'/T23,'--------',T58,'-----------'   &
           /T5,'Line          East       North     Depth         East       North      Depth     Incl    Azm',&
           /T5,'----          ----       -----     -----         ----       -----      -----     ----    ---')
 19 FORMAT(/I8,I4,2F12.1,F9.1,2X,2F12.1,F9.1,2X,2F7.0)
 20 FORMAT(/8X,I4,2F12.1,F9.1,2X,2F12.1,F9.1,2X,2F7.0)
 21 FORMAT(/T16,'Downhole Probe Coordinates & Orientation'         &
           /T16,'----------------------------------------'         &
          //T26,'Transmitter',T51,'Borehole',T75,'Receiver'     &
           /T26,'-----------',T51,'--------',T75,'--------'                  &
           /T19,          'East      North     Depth      Dip     Azm       East      North     Depth '   &
           /T5,'Line',T19,'----      -----     -----      ---     ---       ----      -----     -----')
 22 FORMAT(I8,I4,2F11.1,F10.1,1X,2F8.1,2F11.1,F10.1)
 23 FORMAT(8X,I4,2F11.1,F10.1,1X,2F8.1,2F11.1,F10.1)
 24 FORMAT(//T3,'Plot points for receivers on Line',I7, &
           //T13,'East        North     Elev'          &
            /T13,'----        -----     ----' /I4,2F13.1,F9.1)
 31 FORMAT(//T11,'+----------------------------------+'  &
            /T11,'+  Layered Earth Model Parameters  +'  &
            /T11,'+----------------------------------+'  &
           //T2,'                   Depth' &
            /T2,'Layer  Thickness   to Top    Resistivity   MU-R   EPS-R   CHRG    CFREQ    CTAU' &
            /T2,'-----  ---------   ------    -----------   ----   -----   ----    -----    ----')
 32 FORMAT(I4,11X,F11.1,G15.4,2F7.2,2F8.2,G13.2)
 33 FORMAT(I4,   2F11.1,G15.4,2F7.2,2F8.2,G13.2)
 40 FORMAT(//T3,'Input Data for Plate',I3,' - contained in Layer',I3 &
            /T3,'-----------------------------------------------')
 41 FORMAT(/T3,'LITHP =',I3,':   SIGMA_T =',F7.1,';   CHRGP =',F7.3,';   CFREQP =',F7.3,';   CTAUP =', G11.2)
 42 FORMAT(/T3,'Depth to top =',F9.1,';    Dip Azimuth =',F6.0,';   Plate length =',F7.1 &
           /T3,'Centre North =',F9.1,';    Dip         =',F6.0,';   Plate width  =',F7.1 &
           /T3,'Centre East  =',F9.1,';    Plunge      =',F6.0)
 44 FORMAT (/T3,'All plates must be in basement unless DO3D = 3' &
            /T3,'Plate',I3,' has been shifted down by',F8.2,' metres.')
 46 FORMAT (/T3,'Plates are not allowed to cross layer boundaries.'&
            /T3,'The width of Plate',I3,' has been shortened')
 47 FORMAT(//T3,'Input Data for Plate',I3,' - contained in Basement' &
            /T3,'-----------------------------------------------')
 51 FORMAT(/T3,'Leroi will compute 3D responses for a time-domain system.'/)
 52 FORMAT(/T3,'Leroi will compute 3D responses for a frequency-domain system.'/)
 53 FORMAT(/75('-')/T24,'END OF INPUT DATA DESCRIPTION'/75('-'))
 61 FORMAT(//T3,'Locations for Point E-field Receivers in Line',I7     &
            /T3,'----------------------------------------------------' &
           //T9,'Easting   Northing      Depth' &
            /T9,'-------   --------      -----')

   END SUBROUTINE SHOW_AND_TELL

   SUBROUTINE SET_FRQ
!  ------------------

   REAL(KIND=QL), PARAMETER :: MID = 10._QL
   INTEGER J,PPDH,PPDL
   REAL(KIND=QL) QFRQ12,QFRQL,QFRQH,MIN_FREQD, MAX_FREQD
   REAL(KIND=QL), ALLOCATABLE :: FDUM(:)

   ALLOCATE (FDUM(1000))
   IF (TDFD == 1) THEN
     FDUM(1) = 0.1_QL
     PPDL = 3
     PPDH = 6
     MAX_FREQD = 1.0E5_QL
     T0 = MINVAL (TOPN) - SWX(NSX)
   ELSE IF (TDFD == 0) THEN
     QFRQ12 = EXP ( LOG (10.D0) / REAL (12._QL) )
     MIN_FREQD = 1.001_QL * REAL (MIN_FREQ,KIND=QL)
     MAX_FREQD = REAL (MAX_FREQ,KIND=QL)
     FDUM(1) = 1000._QL
     DO
       IF (FDUM(1) < MIN_FREQD) EXIT
       FDUM(1) = FDUM(1) / QFRQ12
     END DO

     IF (IPPD == 0) THEN
       PPDL = 3
       PPDH = 6
     ELSE
       PPDL = 3
       IF (IPPD > 3) PPDL = 6
       IF (IPPD > 6) PPDL = 12
       PPDH = PPDL
     END IF
   END IF

   QFRQL = EXP (LOG (10.D0) / REAL (PPDL) )
   QFRQH = EXP (LOG (10.D0) / REAL (PPDH) )
   NFRQ = 1
   DO J = 2,1000
     NFRQ = J
     IF (FDUM(J-1) < MID) THEN
        FDUM(J) = FDUM(J-1) * QFRQL
     ELSE
        FDUM(J) = FDUM(J-1) * QFRQH
     END IF
     IF (FDUM(J) > 0.999 * MAX_FREQD) EXIT
   END DO

   ALLOCATE (FREQ(NFRQ))
   FREQ(1:NFRQ) = REAL (FDUM(1:NFRQ))
   DEALLOCATE (FDUM)

   !!!WRITE(NW,1) FREQ(1),MID,PPDL,MID,FREQ(NFRQ),PPDH,FREQ(NFRQ)

 1 FORMAT(/T3,'The frequency-domain results are directly computed from' &
          /T3,G12.4,' Hz to',G12.4,' Hz, using',I3,' points per decade and from' &
          /T3,G12.4,' Hz to',G12.4,' Hz, using',I3,' points per decade.' &
         //T3,'These are used to construct the frequency-domain spectrum from DC to',G12.4,' Hz' &
          /T3,'before transformation to the time domain.')

   END SUBROUTINE SET_FRQ

   SUBROUTINE SET_RHO
!  ------------------

!  Sets up horizontal interpolation array (12 digit precision) for Hankel transforms
!  from 0.1 m to 10 km

!***  Called by READ_LEROI_DATA

   USE FILTER_COEFFICIENTS

   REAL(KIND=QL), ALLOCATABLE :: B(:)
   REAL(KIND=QL) QRHO, RBASE

!  Set the horizontal interpolation grid to conform to filter intervals.

   QRHO = LOG (10._QL) / REAL (NDEC_JN,KIND=QL)
   QRHO = EXP (QRHO)
   RBASE = EXP (REAL (-SHFTJN,KIND=QL))

   ALLOCATE (B(1000))
   B(1) = .1_QL

   DO JR = 1,1000                 !  Get starting point
     IF (RBASE < B(1)) EXIT
     RBASE = RBASE / QRHO
   END DO
   B(1) = RBASE

   DO JR = 2, 10000
     MXRHO = JR
     B(JR) = B(JR-1) * QRHO
     IF (B(JR) > 1.D4) EXIT
   END DO

   ALLOCATE (RHOTRP(MXRHO))
   RHOTRP(1:MXRHO) = REAL ( B(1:MXRHO))
   DEALLOCATE (B)

   END SUBROUTINE SET_RHO

   SUBROUTINE SET_TRP
!  ------------------

!  Sets up interpolation times for FD -> TD transform which use the
!  exact 6 points per decade frequency-domain data plus 6 per decade
!  interpolated values.  These are based on a 12 point per decade
!  cosine filter derived from the Niels Christensen routine FILCOA
!  with OMEGA = .3 PI and shift 0.

!***  Called by: MAIN program
!***       Uses:  MODULE FILTER_COEFFICIENTS

!             Output
!             ------

!        TRP - array of time values for FD -> TD transformations
!      NTYRP - number of values in TRP
!     EXTENT - the latest time for which time-domain output is required.
!      PULSE - time length of one signal pulse
!     NTYPLS - number of TRP values in 1 PULSE


 IMPLICIT NONE
 REAL, PARAMETER :: TWOPI=6.2831853, T0_MIN=0.1E-6
 INTEGER MXTYM,J1
 REAL EXTENT,T0
 REAL,ALLOCATABLE :: QQQ(:)
 REAL(KIND=QL) TBASE,QTYM, TQ

 MXTYM=200
 ALLOCATE (QQQ(MXTYM))
 QQQ = 0.

 QTYM = LOG (10.D0) /12.D0
 QTYM = EXP (QTYM)
 NPULS = 5
 EXTENT = 2.0 * NPULS * PULSE

 T0 = MINVAL (TOPN) - SWX(NSX)
 T0 = MAX (T0, T0_MIN)
 TBASE = 1.D0 / DBLE (TWOPI)
 DO J1 = 1,MXTYM
   IF (TBASE < T0) EXIT
   TBASE = TBASE / QTYM
 END DO

 TQ = TBASE
 QQQ(1) = REAL (TQ)
 DO J1 = 2, MXTYM
   NTYRP = J1
   TQ = TQ * QTYM
   QQQ(J1) = REAL(TQ)
   IF (QQQ(J1) < PULSE) NTYPLS = J1+2
   IF( QQQ(J1) > EXTENT) EXIT
 END DO

 ALLOCATE (TRP(NTYRP))
 TRP(1:NTYRP) = QQQ(1:NTYRP)
 DEALLOCATE (QQQ)

   END SUBROUTINE SET_TRP

   SUBROUTINE WRITE_NW1_INITIAL
!  ----------------------------
!
!***  Called by: MAIN
!
! Sets up the initial part of the output plotting file for inversion.

! HEADER_ID is used to set the appropriate LINE HEADER(s)
! HEADER_ID = 100 + 10 * I2 + I3 for time domain
!           = 200 + 10 * I2 + I3 for frequency domain
!
! I2 =1 (magnetic dipole RX);  =2 (electric dipole Rx);  =3 (point E-field Rx)
!    =4 (coincident loop);     =5 (Sampo)
!
! I1 = 0: X, Y, Z output (usually for surface receivers)
! I1 = 1: U, V, A output (for downhole receivers only)
! I1 = 2: W, N, S output (for UTEM downhole receivers only)
!
! In this version
!
!  HEADER_ID = 110 or 210 for SURVEY_TYPE = 2 or 3 (constant Rx offfset for loop or magnetic dipole Tx)
!            = 140 for SURVEY_TYPE = 4 (coincident loop )
!            = 110, 111, 112, 210, 211 or 212 for SURVEY_TYPE = 5 (drillhole probe)
!            = 250 for Sampo
!            = 120 or 220 for electric dipole receivers (SURVEY_TYPE = 1 only)
!            = 230 for point E-field receivers (SURVEY_TYPE = 1 only)
!
!  HEADER_ID is Line dependent for SURVEY_TYPE = 1


 INTEGER HID,CL,NCL
 CHARACTER(LEN=7) RES_MOD(9), THK_MOD(9)
 CHARACTER(LEN=20) SVTXT,SXTXT,QL,QL0
 CHARACTER(LEN=82) PLT_MOD(9)
 DATA RES_MOD /'  RES_1','  RES_2','  RES_3','  RES_4','  RES_5','  RES_6','  RES_7','  RES_8','  RES_9'/
 DATA THK_MOD /'  THK_1','  THK_2','  THK_3','  THK_4','  THK_5','  THK_6','  THK_7','  THK_8','  THK_9'/
 DATA PLT_MOD /'  SIG_T_1  PLNGTH_1  DIP_WDTH_1  DEPTH_1  EAST_1  NORTH_1  DIP_AZM_1  DIP_1  PLG_1', &
               '  SIG_T_2  PLNGTH_2  DIP_WDTH_2  DEPTH_2  EAST_2  NORTH_2  DIP_AZM_2  DIP_2  PLG_2', &
               '  SIG_T_3  PLNGTH_3  DIP_WDTH_3  DEPTH_3  EAST_3  NORTH_3  DIP_AZM_3  DIP_3  PLG_3', &
               '  SIG_T_4  PLNGTH_4  DIP_WDTH_4  DEPTH_4  EAST_4  NORTH_4  DIP_AZM_4  DIP_4  PLG_4', &
               '  SIG_T_5  PLNGTH_5  DIP_WDTH_5  DEPTH_5  EAST_5  NORTH_5  DIP_AZM_5  DIP_5  PLG_5', &
               '  SIG_T_6  PLNGTH_6  DIP_WDTH_6  DEPTH_6  EAST_6  NORTH_6  DIP_AZM_6  DIP_6  PLG_6', &
               '  SIG_T_7  PLNGTH_7  DIP_WDTH_7  DEPTH_7  EAST_7  NORTH_7  DIP_AZM_7  DIP_7  PLG_7', &
               '  SIG_T_8  PLNGTH_8  DIP_WDTH_8  DEPTH_8  EAST_8  NORTH_8  DIP_AZM_8  DIP_8  PLG_8', &
               '  SIG_T_9  PLNGTH_9  DIP_WDTH_9  DEPTH_9  EAST_9  NORTH_9  DIP_AZM_9  DIP_9  PLG_9'/

 WRITE(NW1,1) FVN,PVC,TRIM( ADJUSTL (TITLE))
 CALL GET_SURVEY_TEXT (SURVEY_TYPE,SVTXT)
 CALL GET_SOURCE_TEXT (SOURCE_TYPE,SXTXT)

 IF (TDFD < 2) THEN
   WRITE(NW1,2) TRIM (ADJUSTL (SVTXT)),TRIM (ADJUSTL (SXTXT))
   WRITE(NW1,3) NCHNL,REFTYM
   WRITE(NW1,4) TMS(1:NCHNL)
   WRITE(NW1,5) WTMS(1:NCHNL)
   IF (ISYS == 4) WRITE(NW1,24)
 ELSE
   WRITE(NW1,6) TRIM (ADJUSTL (SVTXT)),TRIM (ADJUSTL (SXTXT))
   WRITE(NW1,7) NFRQ
   WRITE(NW1,8) FREQ(1:NFRQ)
   IF (ISYS == 2) WRITE(NW1,22)
 END IF

 WRITE(NW1,9) NLINES

 HID = HEADER_ID(1)
 IF (HID > 240 .OR. HID == 140) THEN  ! Sampo, Coincident loop
   QL = '001'
   CL = 1
   NCL = 1
   CALL WRITE_LINE_HEADER (QL,HID,CL,NCL)
 ELSE
   DO JL = 1, NLINES
     NCL = NCMPL(JL)
     CL = CMP(JL)
     HID = HEADER_ID(JL)
     WRITE(QL0,*) LINE(JL)
     READ(QL0,'(A)') QL           ! Line number
     CALL WRITE_LINE_HEADER (QL,HID,CL,NCL)
   END DO
 END IF

 WRITE(NW1,10) NLYR,NPLT
 WRITE(NW1,11) PLT_MOD(1:NPLT),RES_MOD(1:NLYR),THK_MOD(1:NLYR-1)
 WRITE(NW1,12) MPAR(1:NPAR)

  1 FORMAT(T1,'/ ',I4.4,T15,'File version number'/T1,'/ PROGRAM_NAME: ',A/T1,'/ TITLE: ',A)
  2 FORMAT(T1,'/ Time-Domain Survey_Type = ',A,4X,'Source_Type = ',A)
  3 FORMAT(T1,'/ NCH=',I3.3,4X,'REFTYM(ms)=',G12.4)
  4 FORMAT(T1,'/ TIMES(ms)=',T17,100G13.4)
  5 FORMAT(T1,'/ CHNL_WDTH(ms)=',T17,100G13.4)
  6 FORMAT(T1,'/ Frequency-Domain Survey_Type = ',A,4X,'Source_Type = ',A)
  7 FORMAT(T1,'/ NFRQ=',I3.3)
  8 FORMAT(T1,'/ FREQS(Hz) =',100G13.4)
  9 FORMAT(T1,'/ NLINES =',I3)
 10 FORMAT(T1,'/ LAYERS=',I2.2/T1,'/ PLATES=',I2.2)
 11 FORMAT(T1,'/ MODEL_HEADER'/T1,'/ ',120A)
 12 FORMAT(T1,'/ MODEL_00',84F13.2)
 22 FORMAT(T1,'/ SYSTEM: SAMPO')
 24 FORMAT(T1,'/ SYSTEM: UTEM')

   END SUBROUTINE WRITE_NW1_INITIAL

   SUBROUTINE WRITE_LINE_HEADER (QL,HID,CL,NCL)
!  -------------------------------------------
!
!    QL  - Line number in character form
!    HID - Header ID defining survey & line character
!    CL  - component control

!  For inversion of magnetic dipole or point electric receiver data, all three components of the
!  data are put into the mv1 file, even if some are given null status.

 INTEGER HID,CL,NCL
 CHARACTER(LEN=20) QL
 CHARACTER(LEN=5) CHZ,CHX,CHY,CHU,CHV,CHA,CHS,CHN,CHW,CHE,CHC, &
                  RFZ,RFX,RFY,RFU,RFV,RFA,RFS,RFN,RFW,RFE, &
                  QFZ,QFX,QFY,QFU,QFV,QFA,QFS,QFN,QFW,QFE,SMP

 CHZ = '  CHZ' ;  RFZ = '  RFZ'  ;  QFZ = '  QFZ'    ! Z dipole component
 CHX = '  CHX' ;  RFX = '  RFX'  ;  QFX = '  QFX'    ! X dipole component
 CHY = '  CHY' ;  RFY = '  RFY'  ;  QFY = '  QFY'    ! Y dipole component
 CHU = '  CHU' ;  RFU = '  RFU'  ;  QFU = '  QFU'    ! U dipole component
 CHV = '  CHV' ;  RFV = '  RFV'  ;  QFV = '  QFV'    ! V dipole component
 CHA = '  CHA' ;  RFA = '  RFA'  ;  QFA = '  QFA'    ! A dipole component
 CHS = '  CHS' ;  RFS = '  RFS'  ;  QFS = '  QFS'    ! S dipole component - UTEM
 CHN = '  CHN' ;  RFN = '  RFN'  ;  QFN = '  QFN'    ! N dipole component - UTEM
 CHW = '  CHW' ;  RFW = '  RFW'  ;  QFW = '  QFW'    ! W dipole component - UTEM
 CHC = '  CHC' ;  SMP = '  SMP'                      ! coincident loop (TD); Sampo (FD)
 CHE = '  CHE' ;  RFE = '  RFE'  ;  QFE = '  QFE'    ! electric potential or field

 SELECT CASE (HID)
 CASE (110)            ! TD magnetic dipole receiver : X, Y, Z

   SELECT CASE (CL)
   CASE(1)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHX,JT,JT=1,NCHNL)
   CASE(2)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHY,JT,JT=1,NCHNL)
   CASE(3)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHZ,JT,JT=1,NCHNL)
   CASE(12)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHX,JT,JT=1,NCHNL), (CHY,JT,JT=1,NCHNL)
   CASE(13)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHX,JT,JT=1,NCHNL), (CHZ,JT,JT=1,NCHNL)
   CASE(23)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHY,JT,JT=1,NCHNL), (CHZ,JT,JT=1,NCHNL)
   CASE(123)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHX,JT,JT=1,NCHNL), (CHY,JT,JT=1,NCHNL), (CHZ,JT,JT=1,NCHNL)
   END SELECT

 CASE (210)           ! FD magnetic dipole receiver : X, Y, Z
   SELECT CASE (CL)
   CASE(1)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ)
   CASE(2)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ)
   CASE(3)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(12)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ),  &
                                              (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ)
   CASE(13)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ),  &
                                              (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(23)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ),  &
                                              (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(123)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ), (RFY,JF,JF=1,NFRQ), &
                                              (QFY,JF,JF=1,NFRQ), (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   END SELECT

 CASE (111)           ! Downhole TD magnetic dipole receiver : U, V, A
   SELECT CASE (CL)
   CASE(1)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHU,JT,JT=1,NCHNL)
   CASE(2)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHV,JT,JT=1,NCHNL)
   CASE(3)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHA,JT,JT=1,NCHNL)
   CASE(12)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHU,JT,JT=1,NCHNL), (CHV,JT,JT=1,NCHNL)
   CASE(13)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHU,JT,JT=1,NCHNL), (CHA,JT,JT=1,NCHNL)
   CASE(23)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHV,JT,JT=1,NCHNL), (CHA,JT,JT=1,NCHNL)
   CASE(123)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHU,JT,JT=1,NCHNL), (CHV,JT,JT=1,NCHNL), (CHA,JT,JT=1,NCHNL)
   END SELECT

 CASE (211)           ! Downhole FD magnetic dipole receiver : U, V, A
   SELECT CASE (CL)
   CASE(1)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFU,JF,JF=1,NFRQ), (QFU,JF,JF=1,NFRQ)
   CASE(2)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFV,JF,JF=1,NFRQ), (QFV,JF,JF=1,NFRQ)
   CASE(3)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFA,JF,JF=1,NFRQ), (QFA,JF,JF=1,NFRQ)
   CASE(12)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFU,JF,JF=1,NFRQ), (QFU,JF,JF=1,NFRQ),  &
                                              (RFV,JF,JF=1,NFRQ), (QFV,JF,JF=1,NFRQ)
   CASE(13)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFU,JF,JF=1,NFRQ), (QFU,JF,JF=1,NFRQ),  &
                                              (RFA,JF,JF=1,NFRQ), (QFA,JF,JF=1,NFRQ)
   CASE(23)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFV,JF,JF=1,NFRQ), (QFV,JF,JF=1,NFRQ),  &
                                              (RFA,JF,JF=1,NFRQ), (QFA,JF,JF=1,NFRQ)
   CASE(123)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFU,JF,JF=1,NFRQ), (QFU,JF,JF=1,NFRQ), (RFV,JF,JF=1,NFRQ), &
                                              (QFV,JF,JF=1,NFRQ), (RFA,JF,JF=1,NFRQ), (QFA,JF,JF=1,NFRQ)
   END SELECT

 CASE (112)           ! Downhole TD magnetic dipole receiver : N, S, W
   SELECT CASE (CL)
   CASE(1)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHN,JT,JT=1,NCHNL)
   CASE(2)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHS,JT,JT=1,NCHNL)
   CASE(3)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHW,JT,JT=1,NCHNL)
   CASE(12)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHN,JT,JT=1,NCHNL), (CHS,JT,JT=1,NCHNL)
   CASE(13)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHN,JT,JT=1,NCHNL), (CHW,JT,JT=1,NCHNL)
   CASE(23)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHS,JT,JT=1,NCHNL), (CHW,JT,JT=1,NCHNL)
   CASE(123)
     WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHN,JT,JT=1,NCHNL), (CHS,JT,JT=1,NCHNL), (CHW,JT,JT=1,NCHNL)
   END SELECT

 CASE (212)          ! Downhole FD magnetic dipole receiver : N, S, W
   SELECT CASE (CL)
   CASE(1)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFN,JF,JF=1,NFRQ), (QFN,JF,JF=1,NFRQ)
   CASE(2)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFS,JF,JF=1,NFRQ), (QFS,JF,JF=1,NFRQ)
   CASE(3)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFW,JF,JF=1,NFRQ), (QFW,JF,JF=1,NFRQ)
   CASE(12)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFN,JF,JF=1,NFRQ), (QFN,JF,JF=1,NFRQ),  &
                                              (RFS,JF,JF=1,NFRQ), (QFS,JF,JF=1,NFRQ)
   CASE(13)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFN,JF,JF=1,NFRQ), (QFN,JF,JF=1,NFRQ),  &
                                              (RFW,JF,JF=1,NFRQ), (QFW,JF,JF=1,NFRQ)
   CASE(23)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFS,JF,JF=1,NFRQ), (QFS,JF,JF=1,NFRQ),  &
                                              (RFW,JF,JF=1,NFRQ), (QFW,JF,JF=1,NFRQ)
   CASE(123)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFN,JF,JF=1,NFRQ), (QFN,JF,JF=1,NFRQ), (RFS,JF,JF=1,NFRQ), &
                                              (QFS,JF,JF=1,NFRQ), (RFW,JF,JF=1,NFRQ), (QFW,JF,JF=1,NFRQ)
   END SELECT

 CASE (140)           ! TD coincident loop
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHC,JT,JT=1,NCHNL)

 CASE (120)          ! TD electric dipole receiver
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (CHE,JT,JT=1,NCHNL)

 CASE (220)          ! FD electric dipole receiver
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFE,JF,JF=1,NFRQ), (QFE,JF,JF=1,NFRQ)

 CASE (230)          ! FD point E-field receiver
   SELECT CASE (CL)
   CASE(1)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ)
   CASE(2)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ)
   CASE(3)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(12)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ),  &
                                              (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ)
   CASE(13)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ),  &
                                              (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(23)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFY,JF,JF=1,NFRQ), (QFY,JF,JF=1,NFRQ),  &
                                              (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   CASE(123)
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (RFX,JF,JF=1,NFRQ), (QFX,JF,JF=1,NFRQ), (RFY,JF,JF=1,NFRQ), &
                                              (QFY,JF,JF=1,NFRQ), (RFZ,JF,JF=1,NFRQ), (QFZ,JF,JF=1,NFRQ)
   END SELECT

 CASE (250)            ! FD Sampo
   WRITE(NW1,1) TRIM (ADJUSTL(QL)), HID, NCL, (SMP,JF,JF=1,NFRQ)

 CASE DEFAULT
   CALL WRITE_LOG_FILE (NLG,35,MXERR,2)   !  Write fatal error message
 END SELECT


 1 FORMAT(T1,'/ LINE_HEADER    LINE_ID ',A,4X,'HID:',I4,4X,'NCTD:',I2 /T1,'/ StatIndx  XLOC  YLOC  ZLOC  ',250(A,I3.3))

   END SUBROUTINE WRITE_LINE_HEADER
!==================================================================================================

 END MODULE INPUT_DATA_FOR_LEROI


MODULE MAIN_CODE
CONTAINS
SUBROUTINE LEROI_TEM(MXTDFD, MXDO3D, MXISYS, MXPRFL, MXISTOP,                &
                     MXSTEP, MXNSX, MXNCHNL, MXKRXW, MXREFTYM, MXOFFTYM,     &
                     MXTXON, MXWAVEFORM,                                     &
                     MXTOPN, MXTCLS,                                         &
                     MXSURVEY_TYPE,                                          &
                     MXNLINES, MXMRXL, MXNTX, MXSOURCE_TYPE, MXMXVRTX, MXA1, &
                     MXNVRTX, MXTXZ,                                         &
                     MXSXED, MXSXND,                                         &
                     MXLINE, MXLNTR, MXRX_TYPE, MXNRX, MXUNITS,              &
                     MXCMP, MXSV_AZM, MXKNORM, MXIPLT, MXIDH, MXRXMNT,       &
                     MXQD1, MXQD2, MXRXZ,                                    &
                     MXNLYR, MXNPLT, MXNLITH,                                &
                     MXLYTH,                                                 &
                     MXLITHL,                                                &
                     MXTHK,                                                  &
                     FORWARD_MODEL)
!------------

!*** Calls FDREAD, HSBOSS, HSBOSS_FRQ, LEROI_3D, SET_SWYTD, TDEM_3D
!          TDEM_3D, WRITE_FD, WRITE_TD, WRITE_LOG_FILE

!*** Calls from INPUT_DATA_FOR_LEROI:
!          READ_READ_SYSTEMY_DATA, READ_MODEL_DATA, SET_TRP, SET_FRQ,

 USE INPUT_DATA_FOR_LEROI
 USE FREQUENCY_SELECT

 IMPLICIT NONE
 INTEGER QQDT(8),QQHMS(2),NFRQHS,IPR
 COMPLEX, PARAMETER :: ZERO=(0.,0.)
 REAL, ALLOCATABLE :: FRQHS(:), BPRM(:,:)
 REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: BTD,BTD_SCAT
 COMPLEX, ALLOCATABLE, DIMENSION(:,:,:,:) :: BFD_SCAT,BFD
 REAL CMP_START, CMP_END, ELAPSED
 REAL (KIND = 8) :: FORWARD_MODEL(:)

 ! Matlab / MEX input.
 ! READ_SYSTEM_AND_SURVEY inputs:
 REAL(KIND = 8), INTENT (IN)                  :: MXTDFD, MXDO3D, MXISYS, MXPRFL, MXISTOP
 !
 REAL(KIND = 8), INTENT (IN)                  :: MXSTEP, MXNSX, MXNCHNL, MXKRXW, MXREFTYM, MXOFFTYM
 !
 REAL(KIND = 8), DIMENSION(:), INTENT (IN)    :: MXTXON, MXWAVEFORM
 !
 REAL(KIND = 8), DIMENSION(:), INTENT (IN)    :: MXTOPN, MXTCLS
 !
 REAL(KIND = 8), INTENT (IN)                  :: MXSURVEY_TYPE
 !
 REAL(KIND = 8), INTENT (IN)                  :: MXNLINES, MXMRXL, MXNTX, MXSOURCE_TYPE, MXMXVRTX, MXA1
 !
 REAL(KIND = 8), DIMENSION(:), INTENT (IN)    :: MXNVRTX, MXTXZ
 !
 REAL(KIND = 8), DIMENSION(:,:), INTENT(IN)   :: MXSXED, MXSXND
 !
 REAL(KIND = 8), DIMENSION(:), INTENT(IN)     :: MXLINE, MXRX_TYPE, MXNRX, MXUNITS
 REAL(KIND = 8), DIMENSION(:,:), INTENT(IN)   :: MXLNTR
 !
 REAL(KIND = 8), DIMENSION(:), INTENT(IN)     :: MXCMP, MXKNORM, MXIPLT, MXIDH, MXRXMNT
 REAL(KIND = 8), INTENT(IN)                   :: MXSV_AZM
 !
 REAL(KIND = 8), DIMENSION(:,:,:), INTENT(IN) :: MXQD1, MXQD2
 REAL(KIND = 8), DIMENSION(:,:), INTENT(IN)   :: MXRXZ
 ! READ_MODEL_DATA inputs:
 REAL(KIND = 8), INTENT (IN)                  :: MXNLYR, MXNPLT, MXNLITH
 REAL(KIND = 8), DIMENSION(:,:), INTENT (IN)  :: MXLYTH
 REAL(KIND = 8), DIMENSION(:), INTENT (IN)    :: MXLITHL
 REAL(KIND = 8), DIMENSION(:), INTENT (IN)    :: MXTHK

 !!!OPEN(NR,FILE = 'Leroi.cfl',STATUS = 'OLD')
 !!!OPEN(NW,FILE = 'Leroi.out',STATUS = 'REPLACE')

 CALL DATE_AND_TIME (DATE, TIME, ZONE, QQDT)
 QQHMS(1:2) = QQDT(5:6)
 !WRITE(*,97) QQHMS(1:2),QQDT(3),MONTH(QQDT(2)),QQDT(1)
 !!!WRITE(NW,97) QQHMS(1:2),QQDT(3),MONTH(QQDT(2)),QQDT(1)

 CALL CPU_TIME (CMP_START)

 ! Set up system & survey variables:
 CALL READ_SYSTEM_AND_SURVEY_DATA(MXTDFD, MXDO3D, MXISYS, MXPRFL, MXISTOP,                &
                                  MXSTEP, MXNSX, MXNCHNL, MXKRXW, MXREFTYM, MXOFFTYM,     &
                                  MXTXON, MXWAVEFORM,                                     &
                                  MXSURVEY_TYPE,                                          &
                                  MXNLINES, MXMRXL, MXNTX, MXSOURCE_TYPE, MXMXVRTX, MXA1, &
                                  MXNVRTX, MXTXZ,                                         &
                                  MXSXED, MXSXND,                                         &
                                  MXLINE, MXLNTR, MXRX_TYPE, MXNRX, MXUNITS,              &
                                  MXCMP, MXSV_AZM, MXKNORM, MXIPLT, MXIDH, MXRXMNT,       &
                                  MXQD1, MXQD2, MXRXZ,                                    &
                                  MXTOPN, MXTCLS)
 ALLOCATE (BPRM(MRXTX,NTX))        ! Total primary (air) B fields
 BPRM = 1.                           ! B field normalisation

 CALL READ_MODEL_DATA(MXNLYR, MXNPLT, MXNLITH, &
                      MXLYTH,                  &
                      MXLITHL,                 &
                      MXTHK)

 CALL SET_RHO              ! Horizontal interpolation array

! Put arrays in body centred system.

 SELECT CASE (SURVEY_TYPE)

 CASE(1)
   CALL SET_SURVEY_1 (NLINES,MRXL,NTX,MXVRTX,NVRTX,MRXTX,NRX,MQVR,RX_TYPE,IPLT,NCNTRD, &
                      ECNTRD,SXND,SXED,SXN,SXE,SXZ,RXND,RXED,RXN,RXE,RXZ,LNTR,XRXTX,   &
                      YRXTX,ZRXTX,RXID,NCTD,YXZPLT,KNORM,KNORM2)
   DEALLOCATE (SXND,SXED,RXND,RXED)

 END SELECT
 DEALLOCATE (DSTAT,SDN0,SDE0,XRXOF,YRXOF)
 IF (SOURCE_TYPE == 1) CALL SET_VERTEX_ORDER (NTX,MXVRTX,NVRTX,SXN,SXE)

 IF (MAXVAL (KNORM) > 0) THEN     !  Set up DC primary fields
   IF (SOURCE_TYPE == 1) THEN
      CALL PRMDC_LP (NTX,MXVRTX,NVRTX,SXN,SXE,MRXTX,NRXTX,MQVR,RXID,XRXTX,YRXTX,ZRXTX,KNORM2,BPRM)
   END IF
 END IF

 !CALL SHOW_AND_TELL        ! Set & Print array & model coordinates in body-centred system

 ! OPEN(NW1,FILE = 'Leroi.mf1',STATUS = 'REPLACE')
! CALL WRITE_NW1_INITIAL

! SELECT CASE (MXERR)
! CASE (0)
!   WRITE(*,'(/T3,A//T3,A)') 'Control file passed initial tests.', 'Computation begins.'
! CASE (1)
!   WRITE(*,'(/T3,A//T3,A)') 'Computation begins.','Look at comments in LEROI.LOG'
! CASE(2)
!   WRITE(*,'(/T3,A//T3,A//T3,A)') 'FATAL INPUT DATA ERRORS IN LEROI.CFL OR LEROI.INV', &
!                                  'Refer to messages in LEROI.LOG', &
!                                  'Execution will not occur until these are corrected.'
!   STOP
! END SELECT
 IF (ISTOP == 1) STOP

!  For time-domain, set up frequencies, interpolation times

 IF (TDFD < 2) THEN   ! Time-Domain
   CALL SET_SWYTD (NSX,SWX,SWY,T0SX)  ! Compute dI/dt at the receiver
   CALL SET_TRP
   CALL SET_FRQ
 END IF

 IF (KHSQ == 1) THEN
   NFRQHS = NFRQ
   ALLOCATE (FRQHS(NFRQ))
   FRQHS(1:NFRQ) = FREQ(1:NFRQ)
 ELSE
   NFRQHS = NF_6PDE
   ALLOCATE (FRQHS(NFRQHS))
   FRQHS(1:NFRQHS) = FRQ_6PDE(1:NFRQHS)
 END IF

   KPRT = 0                   ! Print model data only


   IF (TDFD < 2) THEN   ! Time-Domain.  Compute BTD_SCAT, the Scattering Response

     ALLOCATE ( BTD(NCHNL,MRXTX,NTX,3), BTD_SCAT(NCHNL,MRXTX,NTX,3))
     BTD = 0.
     BTD_SCAT = 0.

!  Compute BTD, the layered earth response convolved with the excitation waveform

     CALL HSBOSS_TD (NFRQHS,FRQHS,STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL, &
                     TOPN,TCLS,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM,   &
                     NRXTX,RXID,MRXTX,MQVR,XRXTX,YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD,   &
                     RES,RMUD,REPS,CHRG,CTAU,CFREQ,NCTD,BTD)
!  Redefine BTD as the total response.
!  Reconfigure output from Tx to Line basis.
!  Perform additional output processing where required
!  Write out the results.

     CALL SET_OUTPUT_LINES_TD (NCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE,RXMNT, &
                               UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BTD,BFTL)

!  Write out the results.

!     CALL WRITE_TD (NW,NW1,KPRT,NCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
!                    SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,TMS,HEADER_ID,CMP,BFTL,RDATA,RWTS)

!     IF (PRTSEC) THEN
!       WRITE(NW,11)
!       WRITE(NW1,12)
!       CALL SET_OUTPUT_LINES_TD (NCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE,RXMNT, &
!                                 UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BTD_SCAT,BFTL)
!
!       LINE = LINE + 900000000
!       CALL WRITE_TD (NW,NW1,KPRT,NCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
!                      SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,TMS,HEADER_ID,CMP,BFTL,RDATA,RWTS)
!     END IF
 END IF
 !PRINT*, BFTL(1:NCHNL,1,3,1)
 FORWARD_MODEL = BFTL(1:NCHNL,1,3,1)

 CALL CPU_TIME (CMP_END)
 ELAPSED = CMP_END - CMP_START
 CALL DATE_AND_TIME (DATE, TIME, ZONE, QQDT)
 QQHMS(1:2) = QQDT(5:6)
 !WRITE(*,98)   QQHMS(1:2),QQDT(3),MONTH(QQDT(2)),QQDT(1),ELAPSED
 !!!WRITE(NW,98)  QQHMS(1:2),QQDT(3),MONTH(QQDT(2)),QQDT(1),ELAPSED
 !WRITE(NW1,99) QQHMS(1:2),QQDT(3),MONTH(QQDT(2)),QQDT(1),ELAPSED

 CLOSE (NW)
 CLOSE (NLG)
 CALL CLEAN_UP()
! STOP
 11 FORMAT(//T3,'*************************************' &
           //T3,'      SCATTERED FIELD OUTPUT'          &
           //T3,'*************************************')
 12 FORMAT(T1,'/'/T1,'/ SCATTERED FIELDS'/'/')
 97 FORMAT(/T13,'Leroi task started at ',I2.2,':',I2.2,' on',I3.2,1X,A,I5)
 98 FORMAT(//T3,'Leroi task completed at ',I2.2,':',I2.2,' on',I3.2,1X,A,I5 &
           //T3,'Computation time = ',F10.2,' seconds.'//)
 99 FORMAT(T1,'/'/T1,'/ Leroi task completed at ',I2.2,':',I2.2,' on',I3.2,1X,A,I5 &
                 /T1,'/ Computation time = ',F10.2,' seconds.')
 END SUBROUTINE LEROI_TEM

 SUBROUTINE CLEAN_UP
 USE INPUT_DATA_FOR_LEROI
! Deallocates all modular allocatable variables that have been allocated
! (discovered using a trial-and-error approach)
IMPLICIT NONE
DEALLOCATE(LINE,IPLT,IDH,NVRTX,UNITS,KNORM,NRX,RX_TYPE,CMP,NRXTX, &
                                       HEADER_ID,KHID)
DEALLOCATE( RXID,KNORM2,NCTD,LNTR)
DEALLOCATE( &
TXON,WAVEFORM,CURNT,TRP,TMS,WTMS,TOPN,TCLS,FREQ,SWX,TXZ,SXZ,SVAZM,SDZ0, &
                                    TXLNGTH,TXWDTH,SXDIP,SXAZM,RHOTRP,RXMNT,RXOFF,RXOTX)
DEALLOCATE(&
SWY,LYTH,SXE,SXN,ZRXTX,RXDIP,RXAZM,RXZ,ZRXOF)
DEALLOCATE( XRXTX,YRXTX,RXE,RXN, BFTL)
DEALLOCATE( YXZPLT )
DEALLOCATE( RES,THK,RMU,REPS,CHRG,CTAU,CFREQ, &
 MPAR )
DEALLOCATE(RMUD,THKD)
DEALLOCATE( RWTS, NCMPL,RDATA)
END SUBROUTINE CLEAN_UP

END MODULE MAIN_CODE


 REAL FUNCTION COSTRN (WF,YFRQ,NFRQ,T)
!-------------------------------------

!***  Calls CUBVAL
!***  Called by TDEM_3D

! LAST MODIFICATION DATE: October, 2001

! Produces time-domain value at time T by cosine transformation of NFRQ
! frequency-domain values contained in cubic spline array YFRQ.
! Array WF contains the LOG (base e) of the angular frequency values.

! The routine uses filter coefficients derived from the Niels Christensen
! fast Hankel transform routine FILCOA at a spacing of 12 points per decade
! and omega = 0.3.  Various filters were tested using a vertical magnetic
! dipole receiver in a very large circular for which accurate frequency
! and time-domain solutions were programmed.  This particular filter gave
! the overall best accuracy for 1/2 spaces ranging in resistivity from
! .1 to 10,000 ohm-m for times ranging from .01 to 50 msec.


!  K(W,T) = (2/PI) * F(W) * COS(WT) dW

! Letting X = WT, the above becomes
!
!  K(W,T) = (2/PI*T) * F(X/T) * COS(X) dX
!
! From Abramowitz and Stegun, COS(X) = SQRT(X*PI/2) * J(-1/2:X).
! Filter Coefficients are used to represent X**(1/2) * J(-1/2:X)
!
!  COSTRN = SQRT (2/PI) * SUM(i) { WCOS(i) * F [X(i) /T] }

! The accumulation is done using 12 digit precision


 USE FILTER_COEFFICIENTS

 IMPLICIT NONE
 INTEGER, PARAMETER :: NDEC_COS=12, KFLOW=-200, KFHIGH=99
 REAL, PARAMETER :: FAC=.7978846, TOL=1.0E-6
 INTEGER J1,NFRQ
 REAL WF(NFRQ),YFRQ(4,NFRQ),T,YS,CUBVAL,V1
 REAL(KIND=QL) DELTA,Y1,Y,TD,YTYM,VAL

 INTENT (IN) WF,YFRQ,NFRQ,T


 DELTA = LOG (10._QL)/ REAL (NDEC_COS, KIND=QL)
 TD = REAL (T, KIND=QL)
 YTYM = 0.
 Y1 = -LOG (TD) -DELCOS

! Begin right side convolution at weight 0.
! Stop when frequency domain array is exhausted.

 MOVE_HIGH: DO J1 = 0, KFHIGH

   Y = Y1 + J1 * DELTA
   YS = REAL(Y)
   IF (YS > WF(NFRQ)) EXIT MOVE_HIGH
   IF (YS < WF(1)) YS = WF(1)
   V1 = CUBVAL (WF,YFRQ,NFRQ,YS)
   VAL = WCOS(J1) * REAL (V1,KIND=QL)
   YTYM = YTYM + VAL
 END DO MOVE_HIGH

 Y = Y1

! Begin left side convolution at weight -1.
! When log angular frequency is less than WF(3), check convergence.
! Continue left using the fact that impulse B is inversely proportional to
! frequency as freq -> 0; i.e., step response B is constant.

 MOVE_LOW: DO J1 = -1, KFLOW, -1

   Y = Y1 + J1 * DELTA
   YS = REAL(Y)
   IF (YS > WF(NFRQ)) CYCLE
   IF (YS < WF(1)) YS = WF(1)
   V1 = CUBVAL (WF,YFRQ,NFRQ,YS)
   VAL = WCOS(J1) * REAL (V1,KIND=QL)
   YTYM = YTYM + VAL
   IF ((Y < WF(3))) THEN
     IF (ABS (VAL) < TOL * ABS (YTYM)) EXIT MOVE_LOW
   END IF
 END DO MOVE_LOW

 COSTRN = FAC * REAL (YTYM) / T

 END FUNCTION COSTRN

  REAL FUNCTION CUBINT (X_ARRAY, Y_VAL, NVAL, X1, X2)
! ---------------------------------------------------

!  Integrates a function from X1 to X2 using its cubic spline representation.

!***  Called by  TXCNVD, TXCNVL
!***  Calls INTERV.  On exit from INTERV
!
!       MFLAG = -1  : X is to the left of interpolated range
!             =  1  : X is to the right of interpolated range
!             =  0  : X is in the interpolated range

!      NVAL - total number of knots including endpoints.
!
!     X_ARRAY(I), I = 1,NVAL - Location of the X coordinate for each known Y value.
!                              The rightmost data point used to calculate coefficients
!                              is not included.
!
!     Y_VAL(J,I), J = 1,4; I = 1,NVAL
!
!              The coefficients of the cubic spline represent the
!              indefinite integral of F, on the I'th interval, as:
!
!       INTGR [ F(X) ] = Y_VAL(4,I)/24 * H**4  +  Y_VAL(3,I)/6 * H**3  +
!                        Y_VAL(2,I)/2 * H**2  +  Y_VAL(1,I) * H
!
!                          WITH  H = X - X_ARRAY(K)
!
!  This is a modification of the FUNCTION PPVALU in the book
!  "A PRACTICAL GUIDE TO SPLINES"  by C. DE BOOR

!*********************************************************************

  IMPLICIT NONE
  INTEGER I,I1,I2,MFLAG,NVAL
  REAL H,H1,H2,X1,X2,X_ARRAY(NVAL), Y_VAL(4,NVAL)

!  Find the indices I1 and I2 of largest breakpoints to the left of X1
!  and X2 respectively.
!
  CALL INTERV ( X_ARRAY, NVAL-1, X1, I1, MFLAG )
  CALL INTERV ( X_ARRAY, NVAL-1, X2, I2, MFLAG )
  H1 = X1 - X_ARRAY(I1)
  IF (MFLAG == -1) H1 = 0.

  H2 = X2 - X_ARRAY(I2)
  CUBINT = (((Y_VAL(4,I2)*H2/4.0 + Y_VAL(3,I2) )*H2/3.0 + &
              Y_VAL(2,I2) )*H2/2.0 + Y_VAL(1,I2) )*H2 &
         - (((Y_VAL(4,I1)*H1/4.0 + Y_VAL(3,I1) )*H1/3.0 + &
              Y_VAL(2,I1) )*H1/2.0 + Y_VAL(1,I1) )*H1

!  Include integrals over intervening intervals.

  IF (I2 > I1) THEN
    DO I = I1, I2-1
      H = X_ARRAY(I+1) - X_ARRAY(I)
      CUBINT = CUBINT + (((Y_VAL(4,I)*H/4.0 + Y_VAL(3,I) )*H/3.0 + &
                           Y_VAL(2,I) )*H/2.0 + Y_VAL(1,I) )*H
    END DO
  END IF

 END FUNCTION CUBINT

  SUBROUTINE CUBSPL (XVAL, F, N)
! ------------------------------

!***  Called by FOLD_AND_CONVOLVE, READ_SYSTEM_AND_LAYER_DATA, TXCNVD

!  Calculates coefficients for cubic spline interpolation.
!  Call function CUBVAL to evaluate function values after interpolation.
!  Adapted from "A Practical Guide to Splines"  by Carl de Boor.

!             INPUT
!             -----
!
!     N = number of data points. assumed to be at least 4
!
!  (XVAL(I), F(1,I), I=1,...,N) = abscissae and ordinates of the data points.
!                                 XVAL is assumed to be strictly increasing.
!
!          OUTPUT
!          ------
!
!     F(J,I), J=1,...,4; I=1,. N-1 = the polynomial coefficients
!         of the cubic interpolating spline with interior knots (or joints)
!         XVAL(2), ..., XVAL(N-1).
!
!        In the interval: (XVAL(I) - XVAL(I+1)), the spline F is given by:
!
!        F(X) = F(1,I) + H* (F(2,I) + H* (F(3,I) + H* F(4,I)/3.) /2.)
!
!     where H = X - XVAL(I).  FUNCTION  CUBVAL of it s variations may be
!     used to evaluate F or its derivatives from XVAL,C, L = N-1, & K=4.
!------------------------------------------------------------------------

  IMPLICIT NONE
  INTEGER N,I,J,M
  REAL F(4,N),XVAL(N),DIVDF1,DIVDF3,DXVAL,G

!  A tridiagonal linear system for the unknown slopes S(I) of F at
!  XVAL(I), I=1,...,N, is generated and then solved by Gauss elimination,
!  with S(I) ending up in F(2,I), ALL I.
!  F(3,.) AND F(4,.) are used initially for temporary storage.

!  Compute first differences of XVAL sequence and store in F(3,.).
!  Also, compute first divided difference of data and store in F(4,.).

  DO M = 2,N
    F(3,M) = XVAL(M) - XVAL(M-1)
    F(4,M) = (F(1,M) - F(1,M-1)) /F(3,M)
  END DO

!  Not-a-knot condition at left

 F(4,1) = F(3,3)
 F(3,1) = F(3,2) + F(3,3)
 F(2,1) = ((F(3,2) + 2.* F(3,1)) * F(4,2)*F(3,3) + F(3,2)**2 * F(4,3)) /F(3,1)

!  Generate the corresponding equations and
!  perform the forward pass of Gauss elimination, after which the M-TH
!  equation reads    F(4,M)*S(M) + F(3,M)*S(M+1) = F(2,M).

 DO M = 2, N-1
   G = -F(3,M+1) / F(4,M-1)
   F(2,M) = G*F(2,M-1) + 3.* (F(3,M)*F(4,M+1) + F(3,M+1)*F(4,M))
   F(4,M) = G* F(3,M-1) + 2.* (F(3,M) + F(3,M+1))
 END DO

 G = F(3,N-1) + F(3,N)
 F(2,N) = ((F(3,N) + 2.*G) *F(4,N)*F(3,N-1) + F(3,N)**2 *(F(1,N-1) - F(1,N-2)) /F(3,N-1))/G
 G = -G / F(4,N-1)
 F(4,N) = F(3,N-1)


 F(4,N) = G*F(3,N-1) + F(4,N)
 F(2,N) = (G*F(2,N-1) + F(2,N)) /F(4,N)

!  Perform back substitution.

 DO J = N-1, 1, -1
   F(2,J) = (F(2,J) - F(3,J) *F(2,J+1)) /F(4,J)
 END DO

!  Generate cubic coefficients in each interval, i.e., the derivatives at its
!  left endpoint, from value and slope at its endpoints.

 DO I = 2,N
   DXVAL = F(3,I)
   DIVDF1 = (F(1,I) - F(1,I-1)) /DXVAL
   DIVDF3 = F(2,I - 1) + F(2,I) - 2.*DIVDF1
   F(3,I-1) = 2.* (DIVDF1 - F(2,I-1) - DIVDF3) /DXVAL
   F(4,I-1) = (DIVDF3/DXVAL) * (6./DXVAL)
 END DO
 END SUBROUTINE CUBSPL

 REAL FUNCTION CUBVAL (X_ARRAY, Y_VAL, NXVAL, X1)
!------------------------------------------------

!  Evaluates a function at X1 from from its cubic spline representation.

!***  Called by COSTRN, FOLD_AND_CONVOLVE, TXCNVD, TXCNVL
!***  Calls INTERV.  On exit from INTERV
!
!       MFLAG = -1  : X is to the left of interpolated range
!             =  1  : X is to the right of interpolated range
!             =  0  : X is in the interpolated range

!      NXVAL - total number of knots including endpoints.
!
!     X_ARRAY(I), I = 1,NXVAL - location of the knots.  The rightmost data
!                            point used to calculate coefficients is not
!                            included.
!
!     Y_VAL(J,I), J = 1,4; I = 1,NXVAL
!
! The coefficients of the cubic spline on the I'th interval represent F as:
!
!                F(X) = Y_VAL(4,I)/6 * H**3  +  Y_VAL(3,I)/2 * H**2  +
!                       Y_VAL(2,I) * H  +  Y_VAL(1,I)
!
!                          with  H = X - X_ARRAY(I)
!
!  This is a modification of the FUNCTION PPVALU in the book
!  "A PRACTICAL GUIDE TO SPLINES"  by C. DE Boor
!
!             METHOD
!             ------
!
!  The interval index I, appropriate for X, is found through a call to INTERV.
!  The formula for F is evaluated using nested multiplication.
!******************************************************************************

 IMPLICIT NONE
 INTEGER I,MFLAG,NXVAL
 REAL X_ARRAY(NXVAL),Y_VAL(4,NXVAL),X1,H

 INTENT (IN) X_ARRAY, Y_VAL, NXVAL, X1

!  Find index I of largest breakpoint to the left of X1.

 CALL INTERV ( X_ARRAY, NXVAL-1, X1, I, MFLAG )
 H = X1 - X_ARRAY(I)
 IF (MFLAG == -1) H = 0.
 CUBVAL = ((Y_VAL(4,I)*H/3.0 + Y_VAL(3,I) )*0.5*H + Y_VAL(2,I) )*H + Y_VAL(1,I)

 END FUNCTION CUBVAL

 COMPLEX FUNCTION C2DINTRP (XV,NX,ZV,NZ,FR,FI,X1,Z1)
!---------------------------------------------------

!  2 dimensional interpolation of a complex function (FR,FI) on point (X1,Z1)
!  For each value of Z in ZV, a cubic spline repesents the functions in the X direction.
!  The cubic splines are evaluated at X1 for 4 different Z values, above and below Z1
!  A four point non-uniform Lagrange interpolation is used inhe Z direction.
!
!***  Called by
!***  Calls INTERV, INTERV_Z
!
!  XV(NX) - increasing array of X values
!  ZV(NZ) - increasing array of Z values
!  FR(1:4,KX,KZ) - real function values at XV(KX), ZV(KZ)
!  FI(1:4,KX,KZ) - imaginary function values at XV(KX), ZV(KZ)

 IMPLICIT NONE
 INTEGER KX,KZ,MFL,NFL,NX,NZ,J1,JZ
 REAL X1,Z1,XV(NX),ZV(NZ),FR(4,NX,NZ),FI(4,NX,NZ),H,HZ,D1,D4,P,P2,VR,VI
 REAL,DIMENSION(4) :: CR,CI,A

!  Find index KX of largest breakpoint to the left of X1.

 CALL INTERV (XV, NX-1, X1, KX, MFL)
 IF (MFL == -1) H = 0.
 H = X1 - XV(KX)

!  Find index KZ of largest ZV above Z1.

 CALL INTERV_Z (ZV, NZ-1, Z1, KZ, NFL)
 HZ = ZV(KZ+1) - ZV(KZ)
 D1 = (ZV(KZ) - ZV(KZ-1)) / HZ
 D4 = (ZV(KZ+2) - ZV(KZ)) / HZ
 P = (Z1 - ZV(KZ)) / HZ
 P2 = P*P

 A(1) = (P2-P) * (D4-P) / (D1 * (1.+D1) * (D1+D4))
 A(2) = (D1+P) * (1.-P) * (D4-P) / (D1 *D4)
 A(3) = (D1*P+P2) * (D4-P) / ((1.+D1) * (D4-1.))
 A(4) = (D1+P) * (P2-P) / (D4 * (D1+D4) * (D4-1.))

 DO J1 = 1,4
   JZ = KZ - 2 + J1
   CR(J1) = ((FR(4,KX,JZ)*H/3.0 + FR(3,KX,JZ) )*0.5*H + FR(2,KX,JZ) )*H + FR(1,KX,JZ)
   CI(J1) = ((FI(4,KX,JZ)*H/3.0 + FI(3,KX,JZ) )*0.5*H + FI(2,KX,JZ) )*H + FI(1,KX,JZ)
 END DO
 VR = DOT_PRODUCT (A,CR)
 VI = DOT_PRODUCT (A,CI)

 C2DINTRP = CMPLX (VR,VI)

 END FUNCTION C2DINTRP

 SUBROUTINE CUBVALRZ (X_ARRAY, NRVAL, NZVAL, FUN_R, FUN_I, X1, JZ, C2)
!-------------------------------------------------------------------

!  Uses method of CUBSPL to create complex C2 at X1 from two
!  real splined functions of depth index JZ.

!***  Called by
!***  Calls INTERV.  On exit from INTERV
!
!******************************************************************************

  IMPLICIT NONE
  INTEGER I,MFLAG,NRVAL,NZVAL,JZ
  REAL X_ARRAY(NRVAL),FUN_R(4,NRVAL,NZVAL),FUN_I(4,NRVAL,NZVAL),X1R
  REAL X1,H,CR,CI,A(4),B(4)
  COMPLEX C2

!  Find index I of largest breakpoint to the left of X1.

  X1R = REAL (X1)
  CALL INTERV ( X_ARRAY, NRVAL-1, X1R, I, MFLAG )
  H = X1 - X_ARRAY(I)
  IF (MFLAG == -1) H = 0.
  A(1:4) = FUN_R(1:4,I,JZ)
  B(1:4) = FUN_I(1:4,I,JZ)
  CR = ((A(4)*H/3. + A(3))*0.5*H + A(2))*H + A(1)
  CI = ((B(4)*H/3. + B(3))*0.5*H + B(2))*H + B(1)
  C2 = CMPLX (CR,CI)

 END SUBROUTINE CUBVALRZ

 SUBROUTINE CCUBVAL (X_ARRAY, NVAL, FUN_R, FUN_I, X1, C2)
!-----------------------------------------------------------

!  Uses method of CUBSPL to create complex CD2 at X1 from two
!  real splined functions

!***  Called by
!***  Calls INTERV.  On exit from INTERV
!
!******************************************************************************

  IMPLICIT NONE
  INTEGER I,MFLAG,NVAL
  REAL X_ARRAY(NVAL),FUN_R(4,NVAL),FUN_I(4,NVAL),X1R
  REAL X1,H,CR,CI,A(4),B(4)
  COMPLEX C2

!  Find index I of largest breakpoint to the left of X1.

  X1R = REAL (X1)
  CALL INTERV ( X_ARRAY, NVAL-1, X1R, I, MFLAG )
  H = X1 - X_ARRAY(I)
  IF (MFLAG == -1) H = 0.
  A(1:4) = FUN_R(1:4,I)
  B(1:4) = FUN_I(1:4,I)
  CR = ((A(4)*H/3. + A(3))*0.5*H + A(2))*H + A(1)
  CI = ((B(4)*H/3. + B(3))*0.5*H + B(2))*H + B(1)
  C2 = CMPLX (CR,CI)

 END SUBROUTINE CCUBVAL

 SUBROUTINE CDCUBVAL (X_ARRAY, FUN_R, FUN_I, NVAL, X1, CD2)
!-----------------------------------------------------------

!  Uses method of CUBSPL to create complex double precision CD2 at X1 from two
!  real splined functions

!***  Called by
!***  Calls INTERV.  On exit from INTERV
!
!******************************************************************************

  IMPLICIT NONE
  INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
  INTEGER I,MFLAG,NVAL
  REAL X_ARRAY(NVAL),FUN_R(4,NVAL),FUN_I(4,NVAL),X1R
  REAL(KIND=QL) X1,H,CR,CI,A(4),B(4)
  COMPLEX(KIND=QL) CD2

!  Find index I of largest breakpoint to the left of X1.

  X1R = REAL (X1)
  CALL INTERV ( X_ARRAY, NVAL-1, X1R, I, MFLAG )
  H = X1 - REAL (X_ARRAY(I),KIND=QL)
  IF (MFLAG == -1) H = 0.D0
  A(1:4) = REAL (FUN_R(1:4,I),KIND=QL)
  B(1:4) = REAL (FUN_I(1:4,I),KIND=QL)
  CR = ((A(4)*H/3.D0 + A(3))*0.5D0*H + A(2))*H + A(1)
  CI = ((B(4)*H/3.D0 + B(3))*0.5D0*H + B(2))*H + B(1)
  CD2 = CMPLX (CR,CI,KIND=QL)

 END SUBROUTINE CDCUBVAL

 REAL FUNCTION DIST2D (X1,Y1,X2,Y2)
!----------------------------------

! Computes distance RHO between points (X1, Y1) & (X2, Y2)

 REAL X1,Y1,X2,Y2

 DIST2D = SQRT ((X1-X2)**2 + (Y1-Y2)**2)

 END FUNCTION DIST2D

 SUBROUTINE FDREAD (ND,NFRQ,NTX,MRXTX,NRXTX,NCTD,BFD_SCAT)
!--------------------------------------------------------

!  Reads frequency-domain scattered impulse magnetic field data
!  (real & imaginary components) from logical UNIT ND into
!  array BFD_SCAT for conversion to time-domain by SUBROUTINE TDEM_OUT.

!*** Called by MAIN

!            NFRQ - number of frequencies
!             NTX - number of transmitter positions
!           MRXTX - maximum number of receivers for any transmitter
!           NRXTX - number of receivers for each transmitter position
!            NCTD - number of components for each receiver
!   BFD1(I,J,K,L) - Lth component of the complex frequency-domain response at
!                   receiver J, of transmitter K for frequency I.  If NCTD = 1
!                   as is the case for coincident loop, or electric dipole,
!                   then the relevant component is stored in L=1 position.

  IMPLICIT NONE
  INTEGER ND,NFRQ,NTX,MRXTX,NRXTX(NTX),NCTD(MRXTX,NTX),JF,JS,JR,JC,NC,NC2
  REAL A(6)
  COMPLEX BFD_SCAT(NFRQ,MRXTX,NTX,3)

  DO JF = 1,NFRQ
    DO JS = 1, NTX
      DO JR = 1, NRXTX(JS)
        NC = NCTD(JR,JS)
        NC2 = 2*NC
        READ(ND,*) A(1:NC2)
        DO JC = 1,NC
          BFD_SCAT(JF,JR,JS,JC) = CMPLX (A(2*JC-1), A(2*JC))
        END DO
      END DO
    END DO
  END DO
 END SUBROUTINE FDREAD

 SUBROUTINE GET_SOURCE_TEXT (J,SXTXT)
!-----------------------------------

 INTEGER J
 CHARACTER(LEN=20) SXTXT

 SELECT CASE (J)
 CASE (1)
   SXTXT = 'CLOSED_LOOP'
 CASE (2)
   SXTXT = 'GROUNDED_OPEN_LOOP'
 CASE (3)
   SXTXT = 'MAGNETIC_DIPOLE'
 CASE (4)
   SXTXT = 'RECTANGULAR_LOOP'
 CASE (5)
   SXTXT = 'PLANE_WAVE'
 END SELECT
 END SUBROUTINE GET_SOURCE_TEXT

 SUBROUTINE GET_SURVEY_TEXT (J,SVTXT)
!-----------------------------------

 INTEGER J
 CHARACTER(LEN=20) SVTXT

 SELECT CASE (J)
 CASE (1)
   SVTXT = 'FIXED_SOURCE'
 CASE (2)
   SVTXT = 'MOVING_LOOP'
 CASE (3)
   SVTXT = 'MOVING_DIPOLE'
 CASE (4)
   SVTXT = 'COINCIDENT_LOOP'
 CASE (5)
   SVTXT = 'DRILL_HOLE_PROBE'
 END SELECT
 END SUBROUTINE GET_SURVEY_TEXT

 SUBROUTINE GET_UNITS_TEXT (J,UTXT)
!----------------------------------

 INTEGER J
 CHARACTER(LEN=20) UTXT(2)

 SELECT CASE (J)
 CASE (1)
   UTXT(1) = 'volts'
   UTXT(2)  = 'V'
 CASE (2)
   UTXT(1) = 'millivolts'
   UTXT(2) = 'mV'
 CASE (3)
   UTXT(1) = 'microvolts'
   UTXT(2) = 'mu-V'
 CASE (4)
   UTXT(1) = 'nanovolts'
   UTXT(2) = 'nV'
 CASE (11)
   UTXT(1) = 'nanoteslas / sec'
   UTXT(2) = 'nT/s'
 CASE (12)
   UTXT(1) = 'picoteslas / sec'
   UTXT(2) = 'pT/s'
 CASE (21)
   UTXT(1) = 'nanoteslas'
   UTXT(2) = 'nT'
 CASE (22)
   UTXT(1) = 'picoteslas'
   UTXT(2) = 'pT'
 CASE (31)
   UTXT(1) = 'ratio'
   UTXT(2) = 'ratio'
 CASE (32)
   UTXT(1) = 'percent'
   UTXT(2) = 'percent'
 CASE (33)
   UTXT(1) = 'parts per thousand'
   UTXT(2) = 'PPT'
 CASE (34)
   UTXT(1) = 'parts per million'
   UTXT(2) = 'PPM'
 CASE (35)
   UTXT(1) = 'parts per billion'
   UTXT(2) = 'PPB'
 CASE (41)
   UTXT(1) = 'volts per metre'
   UTXT(2) = 'V/m'
 CASE (42)
   UTXT(1) = 'millivolts per metre'
   UTXT(2) = 'mV/m'
 CASE (43)
   UTXT(1) = 'microvolts per metre'
   UTXT(2) = 'mV/m'
 CASE (44)
   UTXT(1) = 'nanovolts per metre'
   UTXT(2) = 'nV/m'
 END SELECT
 END SUBROUTINE GET_UNITS_TEXT

   SUBROUTINE INTERV (XT, LXT, X, LEFT, MFLAG)
!-------------------------------------------

!***   Called by CUBVAL, CUBINT
!       INPUT: XT, LXT, X
!      OUTPUT: LEFT, MFLAG
!
!  XT is an ascending sequence of length LXT.
!  INTERV finds the interval in XT containing the input variable X.
!
!  If XT(1)  <=  X  <=  XT(LXT) then MFLAG = 0 and
!
!  XT (LEFT)  <=  X  <=  XT (LEFT+1)
!
!  --------------------------------------------
!  If  X <  XT(1) then MFLAG = - 1 & LEFT = 1
!  If  X >  XT(lxt) then MFLAG = 1 & LEFT = LXT
!  --------------------------------------------
!
!  from  * A PRACTICAL GUIDE TO SPLINES *  by C. DE BOOR
!
!             METHOD
!             ------
!
!  The program is designed to be efficient in the common situation that
!  it is called repeatedly, with  X  taken from an increasing or decreasing
!  sequence. This will happen, e.g., when a pp function is to be grapged.
!  The first guess for  LEFT  is therefore taken to be the value returned at
!  the previous call and stored in the  L O C A L  variable ILO. A first
!  check ascertains that  ILO < LXT (This is necessary since the present
!  call may have nothing to do with the previous call).
!  Then, if XT(ILO) <= XT(ILO+1),
!  we set  LEFT = ILO  and are done after just three comparisons.
!  Otherwise, we repeatedly double the difference  ISTEP = IHI - ILO
!  while also moving  ILO  AND  IHI  in the direction of  X , until
!                      XT(ILO) <= X < XT(IHI) ,
!  after which we use bisection to get, in addition, ILO+1 = IHI .
!  LEFT = ILO  is then returned.
!******************************************************************************
!******************************************************************************

  IMPLICIT NONE
  INTEGER LEFT,LXT,MFLAG,IHI,ILO,ISTEP,MIDDLE,J1
  REAL X,XT(LXT)
  SAVE ILO

  DATA ILO /1/

!***********************************************************
!  Trivial returns when X is not in the range.

  IF ( (X <= XT(1)) .OR. (LXT <= 1) ) THEN
    MFLAG = -1
    LEFT = 1
    RETURN
  END IF

  IF (X >= XT(LXT)) THEN
    MFLAG = 1
    LEFT = LXT
    RETURN
  END IF

  MFLAG = 0
  IF (ILO >= LXT) ILO = LXT-1
  IHI = ILO + 1

!  Trivial return when X is already in the interval.

  IF ( (X <= XT(IHI)) .AND. (X >= XT(ILO)) ) THEN
    LEFT = ILO
    RETURN
  END IF
!***********************************************************

  IF (X <= XT(ILO)) THEN  ! decrease ILO  to capture X.
    ISTEP = 1
    DO J1 = 1,LXT
      IHI = ILO
      ILO = IHI - ISTEP
      ILO = MAX(1, ILO)
      IF ( (X >= XT(ILO)) .OR. (ILO == 1) ) EXIT
      ISTEP = ISTEP*2
    END DO

  ELSE IF ( X >= XT(IHI)) THEN  ! increase IHI to capture X

    ISTEP = 1
    DO J1 = 1,LXT
      ILO = IHI
      IHI = ILO + ISTEP
      IHI = MIN (IHI,LXT)
      IF ( (X <= XT(IHI)) .OR. (IHI == LXT) ) EXIT
      ISTEP = ISTEP*2
    END DO

  END IF

!  Now XT(ILO) <= X < XT(IHI) . Narrow the interval.

  DO J1 = 1,LXT
    MIDDLE = (ILO + IHI)/2
    IF (MIDDLE == ILO) EXIT
    IF (X < XT(MIDDLE)) THEN
      IHI = MIDDLE
    ELSE
      ILO = MIDDLE
    END IF
  END DO

! Task complete

  LEFT = ILO
  RETURN

 END SUBROUTINE INTERV

 SUBROUTINE INTERV_Z (XT, LXT, X, LEFT, MFLAG)
!---------------------------------------------

!  Copy of INTERV to allow efficient searches in two directions.

!***   Called by
!       INPUT: XT, LXT, X
!      OUTPUT: LEFT, MFLAG
!
!  XT is an ascending sequence of length LXT.
!  INTERV finds the interval in XT containing the input variable X.
!
!  If XT(1)  <=  X  <=  XT(LXT) then MFLAG = 0 and
!
!  XT (LEFT)  <=  X  <=  XT (LEFT+1)
!
!  --------------------------------------------
!  If  X <  XT(1) then MFLAG = - 1 & LEFT = 1
!  If  X >  XT(lxt) then MFLAG = 1 & LEFT = LXT
!  --------------------------------------------
!
!  from  * A PRACTICAL GUIDE TO SPLINES *  by C. DE BOOR
!
!             METHOD
!             ------
!
!  The program is designed to be efficient in the common situation that
!  it is called repeatedly, with  X  taken from an increasing or decreasing
!  sequence. This will happen, e.g., when a pp function is to be grapged.
!  The first guess for  LEFT  is therefore taken to be the value returned at
!  the previous call and stored in the  L O C A L  variable ILO. A first
!  check ascertains that  ILO < LXT (This is necessary since the present
!  call may have nothing to do with the previous call).
!  Then, if XT(ILO) <= XT(ILO+1),
!  we set  LEFT = ILO  and are done after just three comparisons.
!  Otherwise, we repeatedly double the difference  ISTEP = IHI - ILO
!  while also moving  ILO  AND  IHI  in the direction of  X , until
!                      XT(ILO) <= X < XT(IHI) ,
!  after which we use bisection to get, in addition, ILO+1 = IHI .
!  LEFT = ILO  is then returned.
!******************************************************************************
!******************************************************************************

  IMPLICIT NONE
  INTEGER LEFT,LXT,MFLAG,IHI,ILO,ISTEP,MIDDLE,J1
  REAL X,XT(LXT)
  SAVE ILO

  DATA ILO /1/

!***********************************************************
!  Trivial returns when X is not in the range.

  IF ( (X <= XT(1)) .OR. (LXT <= 1) ) THEN
    MFLAG = -1
    LEFT = 1
    RETURN
  END IF

  IF (X >= XT(LXT)) THEN
    MFLAG = 1
    LEFT = LXT
    RETURN
  END IF

  MFLAG = 0
  IF (ILO >= LXT) ILO = LXT-1
  IHI = ILO + 1

!  Trivial return when X is already in the interval.

  IF ( (X <= XT(IHI)) .AND. (X >= XT(ILO)) ) THEN
    LEFT = ILO
    RETURN
  END IF
!***********************************************************

  IF (X <= XT(ILO)) THEN  ! decrease ILO  to capture X.
    ISTEP = 1
    DO J1 = 1,LXT
      IHI = ILO
      ILO = IHI - ISTEP
      ILO = MAX(1, ILO)
      IF ( (X >= XT(ILO)) .OR. (ILO == 1) ) EXIT
      ISTEP = ISTEP*2
    END DO

  ELSE IF ( X >= XT(IHI)) THEN  ! increase IHI to capture X

    ISTEP = 1
    DO J1 = 1,LXT
      ILO = IHI
      IHI = ILO + ISTEP
      IHI = MIN (IHI,LXT)
      IF ( (X <= XT(IHI)) .OR. (IHI == LXT) ) EXIT
      ISTEP = ISTEP*2
    END DO

  END IF

!  Now XT(ILO) <= X < XT(IHI) . Narrow the interval.

  DO J1 = 1,LXT
    MIDDLE = (ILO + IHI)/2
    IF (MIDDLE == ILO) EXIT
    IF (X < XT(MIDDLE)) THEN
      IHI = MIDDLE
    ELSE
      ILO = MIDDLE
    END IF
  END DO

! Task complete

  LEFT = ILO
  RETURN

 END SUBROUTINE INTERV_Z

 SUBROUTINE PRMDC_LP (NTX,MXVRTX,NVRTX,SXN,SXE,MRXTX,NRXTX,MQVR,RXID,XRXTX,YRXTX,ZRXTX,KNORM2,BPRM)
!--------------------------------------------------------------------------------------------------

! Returns the primary DC magnetic field in Teslas due to polygonal loop lying on the
! surface of a flat earth at z = 0.  It accumulates BPRM segment by segment
! by rotating each segment in turn to lie along the transformed X axis.

!               Input
!               -----
!
!  NTX                    = number of transmitter loops
!  MXVRTX                 = maximum number of vertices
!  NVRTX(JS)              = number of vertices for transmitter JS
!  SXN, SXE (JV,JS)       : northing and easting of vertex JV of Tx JS
!  MRXTX                  = maximum number of receivers per transmitter
!  NRXTX(JS)              = number of receivers for transmitter JS
!  MQVR - maximum number of vertices for all receivers
!  XRXTX, YRXTX (JR,JS,1) : northing and easting of Rx JR of Tx JS
!  ZRXTX(JR,JS)           : depth of Rx JR of Tx JS (positive down)
!  KNORM2(MRXTX,NTX)      : normalisation indicator for receiver JR of transmitter JS
!
!               Output
!               ------
!
!  KNORM = 0 :  BPRM (JR,JS) = 1 Tesla
!  KNORM = 1 :  BPRM (JR,JS) = Total DC Field
!  KNORM = 2 :  BPRM (JR,JS) = Vertical DC Field


 IMPLICIT NONE
 REAL, PARAMETER ::  FAC = 1.0E-7       !  MU0 / 4 PI
 INTEGER NTX,MRXTX,NRXTX(NTX),MQVR,RXID(MRXTX,NTX),MXVRTX,NVRTX(NTX),KNORM2(MRXTX,NTX),JS,JR,JV
 REAL X0,Y0,Z0,X1,Y1,X2,Y2,LNGTH,CJ,SJ,XI,ETA,RHOSQ,LXI,B1, &
      ZRXTX(MRXTX,NTX),BP(4),BPRM(MRXTX,NTX)
 REAL, DIMENSION (MXVRTX,NTX) :: SXN,SXE
 REAL, DIMENSION (MRXTX,NTX,MQVR) :: XRXTX,YRXTX

 BPRM = 1.
 DO JS = 1,NTX
   DO JR = 1,NRXTX(JS)
     IF (RXID(JR,JS) > 1) CYCLE
     BP = 0.
     X0 = XRXTX(JR,JS,1)
     Y0 = YRXTX(JR,JS,1)
     Z0 = ZRXTX(JR,JS)
     DO JV = 1,NVRTX(JS)
       X1 = SXN(JV,JS);  Y1 = SXE(JV,JS)
       IF (JV == NVRTX(JS)) THEN
         X2 = SXN(1,JS);  Y2 = SXE(1,JS)
       ELSE
         X2 = SXN(JV+1,JS);  Y2 = SXE(JV+1,JS)
       END IF

       LNGTH = SQRT ((X2-X1)**2 + (Y2-Y1)**2)
       CJ = (X2 - X1) / LNGTH
       SJ = (Y2 - Y1) / LNGTH
       XI =   (X0 - X1) * CJ + (Y0 - Y1) * SJ   ! Transformed Rx position
       ETA = -(X0 - X1) * SJ + (Y0 - Y1) * CJ
       RHOSQ = ETA**2 + Z0 **2
       LXI = LNGTH - XI
       B1 = (LXI / SQRT (LXI**2 + RHOSQ)) + (XI / SQRT (XI**2 + RHOSQ))
       B1 = FAC * B1 / RHOSQ
       BP(1) = BP(1) + B1 * Z0 * SJ
       BP(2) = BP(2) - B1 * Z0 * CJ
       BP(3) = BP(3) + B1 * ETA
       BP(4) = B1 * SQRT (ETA**2 + Z0**2)
     END DO
     IF (KNORM2(JR,JS) == 1) THEN
       BPRM(JR,JS) = BP(4)                  ! Normalise to TOTAL field
     ELSE IF (KNORM2(JR,JS) == 2) THEN
       BPRM(JR,JS) = ABS (BP(3))            ! Normalise to VERTICAL field
     END IF
   END DO
 END DO
 END  SUBROUTINE PRMDC_LP

 SUBROUTINE PRMDC_MD (NTX,SXN,SXE,SXZ,SXDIP,SXAZM,MRXTX,NRXTX,RXID,XRXTX,YRXTX,ZRXTX,KNORM2,BPRM)
!------------------------------------------------------------------------------------------------

!***  Called by MAIN

! Returns the primary DC magnetic field in Teslas due to magnetic dipole transmitter of
! unit moment at each receiver position.
!
!               Input
!               -----
!
!  NTX                    = number of transmitter loops
!  SXN, SXE, SXZ          : Tx northing, easting and depth
!  SXDIP, SXAZM           : Tx dip and azimuth
!  MRXTX                  = maximum number of receivers per transmitter
!  NRXTX(JS)              = number of receivers for transmitter JS
!  XRXTX, YRXTX (JR,JS,1) : northing and easting of Rx JR of Tx JS
!  ZRXTX(JR,JS)           : depth of Rx JR of Tx JS (positive down)
!  KNORM2(MRXTX,NTX)      : normalisation indicator for receiver JR of transmitter JS
!
!               Output
!               ------
!
!  KNORM = 0 :  BPRM (JR,JS) = 1 Tesla
!  KNORM = 1 :  BPRM (JR,JS) = Total DC Field in Teslas (absolute value)
!  KNORM = 2 :  BPRM (JR,JS) = Axial DC Field in Teslas (absolute value)

 IMPLICIT NONE
 REAL, PARAMETER ::  BFAC = 1.0E-7       !  MU0 / 4 PI
 INTEGER NTX,MRXTX,RXID(MRXTX,NTX),NRXTX(NTX),KNORM2(MRXTX,NTX),JS,JR
 REAL ZRXTX(MRXTX,NTX),BPRM(MRXTX,NTX),SNDP,CSDP,SNAZ,CSAZ,X(3),RSQ,R,ZBAR3
 REAL, DIMENSION (NTX) :: SXZ,SXAZM,SXDIP
 REAL, DIMENSION (1,NTX) :: SXN,SXE
 REAL, DIMENSION (MRXTX,NTX,1) :: XRXTX,YRXTX

 BPRM = 1.
 DO JS = 1,NTX
   SNDP = SIN (SXDIP(JS))
   CSDP = COS (SXDIP(JS))
   SNAZ = SIN (SXAZM(JS))
   CSAZ = COS (SXAZM(JS))

   DO JR = 1,NRXTX(JS)
     IF (RXID(JR,JS) > 1) CYCLE
     X(1) = XRXTX(JR,JS,1) - SXN (1,JS)
     X(2) = YRXTX(JR,JS,1) - SXE (1,JS)
     X(3) = ZRXTX(JR,JS)   - SXZ (JS)

     RSQ = X(1)**2 + X(2)**2 + X(3)**2
     R = SQRT (RSQ)
!  Rotate to coordinate system where new Z axis lies along dipole axis.

     X(1) = X(1) * CSAZ + X(2) * SNAZ
     X(3) = X(3) * CSDP + X(1) * SNDP
     ZBAR3 = 3. * X(3)**2 / RSQ
     IF (KNORM2(JR,JS) == 1) THEN
       BPRM(JR,JS) = SQRT (ZBAR3 + 1.)       ! Normalise to TOTAL field
     ELSE IF (KNORM2(JR,JS) == 2) THEN
       BPRM(JR,JS) = ZBAR3 - 1.               ! Normalise to VERTICAL field
     END IF
     BPRM(JR,JS) = BFAC * BPRM(JR,JS) / R**3
   END DO
 END DO

 END SUBROUTINE PRMDC_MD

 SUBROUTINE SET_TIME_ORDER (NCHNL,TMS,WTMS)
!------------------------------------------

 IMPLICIT NONE
 INTEGER NCHNL,J1,J2
 REAL TMS(NCHNL),WTMS(NCHNL),A1

 DO J1 = 1,NCHNL-1
   DO J2 = J1+1, NCHNL
     IF (TMS(J1) > TMS(J2)) THEN
       A1 = TMS(J1)
       TMS(J1) = TMS(J2)
       TMS(J2) = A1
       A1 = WTMS(J1)
       WTMS(J1) = WTMS(J2)
       WTMS(J2) = A1
     END IF
   END DO
 END DO
 END SUBROUTINE SET_TIME_ORDER

 SUBROUTINE SET_TMSR (NCHNL,TMS)
!-------------------------------

!  Reverses TMS for UTEM output

 IMPLICIT NONE
 INTEGER NCHNL,J,JR
 REAL TMS(NCHNL), TMS1(NCHNL)

 TMS1 = TMS
 DO J = 1,NCHNL
   JR = NCHNL + 1 - J
   TMS(J) = TMS1(JR)
 END DO
 END SUBROUTINE SET_TMSR

 SUBROUTINE SET_VERTEX_ORDER (NTX,MXVRTX,NVRTX,SXN,SXE)
!------------------------------------------------------

! Assumes that the vertices have been entered in order, either clockwise or
! counter-clockwise.  If counter-clockwise, it reorders them.

!           NTX - number of loop transmitters
!        MXVRTX - maximum number of vertices for any loop
!      NVRTX(J) - number of vertices for transmitter J
!      SXE(I,J) = local east coordinate of vertex I for loop position J
!      SXN(I,J) = local coordinate of vertex I for loop position J

 IMPLICIT NONE
 INTEGER NTX,MXVRTX,NVRTX(NTX),NV,KS,KS1,JV,J1,JS
 REAL, DIMENSION (MXVRTX,NTX) :: SXN,SXE
 REAL, DIMENSION (MXVRTX) :: TEMPN,TEMPE
 REAL R,RMAX,XN_CNTR,YE_CNTR,CTH,STH,EMID,XN(MXVRTX),YE(MXVRTX)

 DO JS = 1,NTX
   NV = NVRTX(JS)
   KS = 0
   RMAX = 0.
   DO JV = 1,NV
     J1 = JV + 1
     IF (JV == NV) J1 = 1
     R = (SXN(J1,JS) - SXN(JV,JS))**2 + (SXE(J1,JS) - SXE(JV,JS))**2
     IF (R > RMAX) THEN
       RMAX = R               ! Find longest segment
       KS = JV
     END IF
   END DO

   RMAX = SQRT (RMAX)
   KS1 = KS + 1
   IF (KS1 > NV) KS1 = 1

!  Renumber loop vertices such that order is maintained but the longest segment
!  goes from new verex index NV to 1.  Rotate & translate loop such that vertex
!  NV is at the origin and vetex 1 is north on the axis.

   XN_CNTR = SXN(KS,JS)
   YE_CNTR = SXE(KS,JS)
   CTH = (SXN(KS1,JS) - XN_CNTR) / RMAX
   STH = (SXE(KS1,JS) - YE_CNTR) / RMAX

   XN(NV) = 0.;  YE(NV) = 0.;  XN(1) = RMAX;  YE(1) = 0.
   DO JV = 2, NV - 1
     J1 = JV + KS
     IF (J1 > NV) J1 = J1 - NV
     XN(JV) = CTH * (SXN(J1,JS) - XN_CNTR) + STH * (SXE(J1,JS) - YE_CNTR)
     YE(JV) = CTH * (SXE(J1,JS) - YE_CNTR) - STH * (SXN(J1,JS) - XN_CNTR)
   END DO


   RMAX = 0
   DO JV = 2, NV-1
     R = XN(JV) - XN(JV+1)
     IF (R > RMAX) THEN      ! Find segment with longest north to south extent
       RMAX = R
       EMID = (YE(JV) + YE(JV+1)) /2.
     END IF
   END DO

   IF (EMID < 0 ) THEN
     TEMPN(1:NV) = SXN(1:NV,JS)
     TEMPE(1:NV) = SXE(1:NV,JS)
     DO JV = 1,NV
       SXN(JV,JS) = TEMPN(NV+1-JV)
       SXE(JV,JS) = TEMPE(NV+1-JV)
     END DO
   END IF
 END DO

 END SUBROUTINE SET_VERTEX_ORDER

 SUBROUTINE SET_OUTPUT_LINES_FD (NFRQ,MCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE,RXMNT, &
                                 UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BFD,BFTL)
!---------------------------------------------------------------------------------------------------------

!** Called by MAIN

!   INPUT:  BFD (JF,JRS,JS,JC) : component JC of frequency-domain output for frequency JF
!                                of receiver JRS belonging to transmitter JS
!                                The units are in teslas or teslas / sec.
!                                Components are: north, east, vertical


!  OUTPUT:  BFTL (JF,JRL,JCL,JL) : frequency-domain output for receiver JRL belonging to Line JL.
!                                  The components,JCL, are either X,Y,Z as defined by a surface
!                                  survey or U,V,A, or modified U,V,A for downhole receivers.
!                                  JF = 1:NFRQ => REAL (BFD)
!                                  JF = NFRQ+1 : 2*NFRQ => AIMAG (BFD)

!          BFTL is normalised or not as determined by KNORM2 and BPRM.
!          BFTL units are determined for each line by UNITS.

!          Sampo:  When ISYS = 2, BFTL (*,*,1,*) = the normalised Sampo response

!
!               SURVEY_TYPE = 1
!               ---------------
!   LNTR(1,L) : Tx index for Line L.   LNTR(2,L) = LNTR(1,L)
!   LNTR(3,L) : Rx index  for start of Line L
!   LNTR(4,L) : Rx index for finish of Line L
!
!               SURVEY_TYPE > 1
!               ---------------
!   LNTR(1,L) : Tx index for start of Line L
!   LNTR(2,L) : Tx index for finish of Line L
!   LNTR(3,L) : Rx index for Line L.   LNTR(4,L) = LNTR(3,L)
!
!   The Rx index for variable LNTR is defined with reference to specific
!   transmitters rather than the global number.  Tx indices are global.
!
!   NFRQ         : number of frequencies
!   MCHNL        = NFRQ for Sampo;  = 2*NFRQ otherwise
!   NTX          : total number of transmitters
!   MRXTX        : maximum number of receivers per transmitter position
!   NRXTX        : number of receivers for transmitter position JS
!   NLINES       : number of receiver lines
!   MRXL         : maximum number of receiver positions for any line
!   MCMP         = 1 for Sampo;  = 3 otherwise
!   NRX          : number of receiver positions for Line L
!   KNORM2       : dc B field normalisation for Rx JR of Tx JS
!                : 0 (unnormalised), 1 (by components), 2 (by total field)
!   UNITS        : output units for line L
!   SVAZM        : survey azimuth for Line L
!   ISYS         : If ISYS = 2, use Sampo convention
!   IDH          : (0) surface survey; (1) conventional U,V,A; (2) UTEM S,N,W
!   (MD1,MD2))   : (MRXL, NLINES) for DH receivers; (1,1) otherwise
!   RXAZM,RXDIP  : azimuth & dip of receiver JR of line JL
!   CURNT        : maximum waveform current
!   BPRM         : Axial or total primary field (Teslas/amp) at receiver JR transmitter (JS)

!  YXZPLT(1:3,I,L) - GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L


 IMPLICIT NONE
 INTEGER MCHNL,MCMP,NFRQ,NTX,MRXTX,NRXTX(NTX),NLINES,MRXL,MD1,MD2,LNTR(4,NLINES), &
         KNORM2(MRXTX,NTX),ISYS,JS,JR,JL,JRL,JC,JF1,JF2
 INTEGER, DIMENSION(NLINES) :: NRX,UNITS,RX_TYPE,IDH
 REAL, DIMENSION(NLINES) :: SVAZM,RXMNT
 REAL, DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL CURNT(NFRQ),BPRM(MRXTX,NTX),BFTL(MCHNL,MRXL,MCMP,NLINES),QR(3),QI(3), &
      QXR,QXI,QYR,QYI,CAZ,SAZ,CAZ0,SAZ0,SDP,CDP,A1
 COMPLEX BFD(NFRQ,MRXTX,NTX,3)

!================================================================

 IF (ISYS == 2) THEN  !  Sampo based on absoluute ratio
   UNITS = 31
   DO JL = 1,NLINES
     CAZ0 = COS (SVAZM (JL))
     SAZ0 = SIN (SVAZM (JL))
     IF (ABS (CAZ0) < 1.E-4) CAZ0 = 0.
     IF (ABS (SAZ0) < 1.E-4) SAZ0 = 0.
     JRL = 0
     DO JS = LNTR(1,JL), LNTR(2,JL)
       DO JR = LNTR(3,JL), LNTR(4,JL)
         BFD(1:NFRQ,JR,JS,1) = CAZ0 * BFD(1:NFRQ,JR,JS,1) + SAZ0 * BFD(1:NFRQ,JR,JS,2)
         JRL = JRL + 1
         BFTL(1:MCHNL,JRL,1,JL) = ABS (BFD(1:NFRQ,JR,JS,3) / BFD(1:NFRQ,JR,JS,1))
       END DO
     END DO
   END DO
!**************
   RETURN
!**************
 END IF
!================================================================

 DO JS = 1,NTX
   DO JR = 1,NRXTX(JS)
     DO JC = 1,3
       IF (KNORM2(JR,JS) == 0) THEN
         BFD(1:NFRQ,JR,JS,JC) = BFD(1:NFRQ,JR,JS,JC) * CURNT(1:NFRQ)
       ELSE
         BFD(1:NFRQ,JR,JS,JC) = BFD(1:NFRQ,JR,JS,JC) / BPRM(JR,JS)
       END IF
     END DO
   END DO
 END DO

!  Convert to LINE-based output and apply units

 DO JL = 1,NLINES
   A1 = 1.
   SELECT CASE (UNITS(JL))
   CASE (32)
     A1 = 100.
   CASE (2,33,42)
     A1 = 1000.
   CASE (3,34,43)
     A1 = 1.E6
   CASE (4,11,21,35,44)
     A1 = 1.E9
   CASE (12,22)
     A1 = 1.E12
   END SELECT
   A1 = A1 * RXMNT(JL)

   JRL = 0
   DO JS = LNTR(1,JL), LNTR(2,JL)
     DO JR = LNTR(3,JL), LNTR(4,JL)
       JRL = JRL + 1
       DO JC = 1,MCMP
         BFTL(1:NFRQ,JRL,JC,JL) = A1 * REAL (BFD(1:NFRQ,JR,JS,JC))
         BFTL(NFRQ+1:2*NFRQ,JRL,JC,JL) = A1 * AIMAG (BFD(1:NFRQ,JR,JS,JC))
       END DO
     END DO
   END DO

!  For surface dipole surveys, reorient component 1 from north to lie along
!  the Survey X axis and component 2 from east to lie along the Survey Y axis.
!  For downhole surveys apply u,V,A transformation.

   IF (RX_TYPE(JL) > 1) CYCLE  ! Skip if not magnetic dipole receiver

   CAZ0 = COS (SVAZM (JL))
   SAZ0 = SIN (SVAZM (JL))
   IF (ABS (CAZ0) < 1.E-4) CAZ0 = 0.
   IF (ABS (SAZ0) < 1.E-4) SAZ0 = 0.
   DO JR = 1,NRX(JL)
     DO JF1 = 1,NFRQ
       JF2 = JF1 + NFRQ
       QR(1:3) = BFTL(JF1,JR,1:3,JL)
       QI(1:3) = BFTL(JF2,JR,1:3,JL)
       IF (IDH(JL) == 0) THEN    !  Surface survey
         BFTL(JF1,JR,1,JL) =  CAZ0 * QR(1) + SAZ0 * QR(2)    ! X component parallel to survey direction
         BFTL(JF1,JR,2,JL) = -SAZ0 * QR(1) + CAZ0 * QR(2)    ! Y component transverse to survey direction
         BFTL(JF2,JR,1,JL) =  CAZ0 * QI(1) + SAZ0 * QI(2)    ! X component parallel to survey direction
         BFTL(JF2,JR,2,JL) = -SAZ0 * QI(1) + CAZ0 * QI(2)    ! Y component transverse to survey direction
       ELSE
         CAZ = COS (RXAZM(JR,JL))
         SAZ = SIN (RXAZM(JR,JL))
         CDP = COS (RXDIP(JR,JL))
         SDP = SIN (RXDIP(JR,JL))
         IF (ABS (CAZ) < 1.E-4) CAZ = 0.
         IF (ABS (SAZ) < 1.E-4) CAZ = 0.
         IF (ABS (CDP) < 1.E-4) CDP = 0.
         IF (ABS (SDP) < 1.E-4) CDP = 0.
         QXR =  CAZ * QR(1) + SAZ * QR(2)                  ! local horizontal radial component
         QXI =  CAZ * QI(1) + SAZ * QI(2)                  ! local horizontal radial component
         QYR = -SAZ * QR(1) + CAZ * QR(2)                  ! local horizontal transverse component
         QYI = -SAZ * QI(1) + CAZ * QI(2)                  ! local horizontal transverse component
         BFTL(JF1,JR,3,JL) =  CDP * QR(3) + SDP * QXR       ! Axial component
         BFTL(JF2,JR,3,JL) =  CDP * QI(3) + SDP * QXI       ! Axial component

         IF (IDH(JL) == 1) THEN                              ! Conventional U,V,A processing
           BFTL(JF1,JR,1,JL) = -SDP * QR(3) + CDP * QXR        ! local u component
           BFTL(JF2,JR,1,JL) = -SDP * QI(3) + CDP * QXI        ! local u component
           BFTL(JF1,JR,1,JL) = -BFTL(JF1,JR,1,JL)                ! local u component sign change to amuse EMIT
           BFTL(JF2,JR,1,JL) = -BFTL(JF2,JR,1,JL)                ! local u component sign change to amuse EMIT
           BFTL(JF1,JR,2,JL) = QYR                             ! local v component
           BFTL(JF2,JR,2,JL) = QYI                             ! local v component

         ELSE IF (IDH(JL) == 2) THEN                         ! Utem style U,V,A processing
           QXR =  CAZ0 * QR(1) + SAZ0 * QR(2)                 !   Line-referenced horizontal radial component
           QXI =  CAZ0 * QI(1) + SAZ0 * QI(2)                 !   Line-referenced horizontal radial component
           BFTL(JF1,JR,1,JL) = -SDP * QR(3) + CDP * QXR        !   Line-referenced U component
           BFTL(JF2,JR,1,JL) = -SDP * QI(3) + CDP * QXI        !   Line-referenced U component

           BFTL(JF1,JR,2,JL) = SQRT (SUM (QR(1:3)**2) - BFTL(JF1,JR,1,JL)**2 - BFTL(JF1,JR,3,JL)**2)
           BFTL(JF2,JR,2,JL) = SQRT (SUM (QI(1:3)**2) - BFTL(JF2,JR,1,JL)**2 - BFTL(JF2,JR,3,JL)**2)
         END IF
       END IF
     END DO
   END DO
 END DO

 END SUBROUTINE SET_OUTPUT_LINES_FD

 SUBROUTINE SET_OUTPUT_LINES_TD (NCHNL,NTX,MRXTX,NRXTX,NLINES,MRXL,MCMP,NRX,LNTR,KNORM2,RX_TYPE,RXMNT, &
                                 UNITS,ISYS,IDH,SVAZM,MD1,MD2,RXAZM,RXDIP,CURNT,BPRM,BTD,BFTL)
!----------------------------------------------------------------------------------------------------

!** Called by MAIN

!   INPUT:  BTD (JT,JRS,JS,JC) : component JC of time-domain output for channel JT
!                                of receiver JRS belonging to transmitter JS
!                                The units are in teslas or teslas / sec.
!                                Components are: north, east, vertical


!  OUTPUT:  BFTL (JT,JRL,JL,JCL) : time domain output for receiver JRL belonging to Line JL.
!                                  The components,JCL, are either X,Y,Z as defined by a surface
!                                  survey or U,V,A, or modified U,V,A for downhole receivers.

!          BFTL is normalised or not as determined by KNORM2 and BPRM.
!          BFTL units are determined for each line by UNITS.
!
!          UTEM: When ISYS = 1, BFTL(JT,*,*,*) is presented in reverse order from the latest
!                (JT = 1) to the earliest time (JT = NCHNL).  In this case the response from
!                JT = 1 is subtracted from all the other channels.
!
!               SURVEY_TYPE = 1
!               ---------------
!   LNTR(1,L) : Tx index for Line L.   LNTR(2,L) = LNTR(1,L)
!   LNTR(3,L) : Rx index  for start of Line L
!   LNTR(4,L) : Rx index for finish of Line L
!
!               SURVEY_TYPE > 1
!               ---------------
!   LNTR(1,L) : Tx index for start of Line L
!   LNTR(2,L) : Tx index for finish of Line L
!   LNTR(3,L) : Rx index for Line L.   LNTR(4,L) = LNTR(3,L)
!
!   The Rx index for variable LNTR is defined with reference to specific
!   transmitters rather than the global number.  Tx indices are global.
!
!   NCHNL        : number of channels
!   NTX          : total number of transmitters
!   MRXTX        : maximum number of receivers per transmitter position
!   NRXTX        : number of receivers for transmitter position JS
!   NLINES       : number of receiver lines
!   MRXL         : maximum number of receiver positions for any line
!   MCMP         = 1 for coincident loop;  = 3 otherwise
!   NRX          : number of receiver positions for Line L
!   KNORM2       : dc B field normalisation for Rx JR of Tx JS
!                : 0 (unnormalised), 1 (by components), 2 (by total field)
!   UNITS        : output units for line L
!   SVAZM        : survey azimuth for Line L
!   ISYS         : If ISYS = 1,use Utem convention
!   IDH          : (0) surface survey; (1) conventional U,V,A; (2)  UTEM S,N,W
!   (MD1,MD2))   : (MRXL, NLINES) for DH receivers; (1,1) otherwise
!   RXAZM,RXDIP  : azimuth & dip of receiver JR of line JL
!   CURNT        : maximum waveform current
!   BPRM         : Axial or total primary field (Teslas/amp) at receiver JR transmitter (JS)

!   YXZPLT(1:3,I,L) - GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L


 INTEGER NCHNL,NTX,MRXTX,NRXTX(NTX),NLINES,MRXL,MCMP,MD1,MD2,LNTR(4,NLINES),KNORM2(MRXTX,NTX), &
         ISYS,JS,JR,JL,JRL,JC,JT,JTR
 INTEGER, DIMENSION(NLINES) :: NRX,UNITS,RX_TYPE,IDH
 REAL, DIMENSION(NLINES) :: SVAZM,RXMNT
 REAL, DIMENSION(MD1,MD2) :: RXAZM,RXDIP
 REAL CURNT(1),BPRM(MRXTX,NTX),BTD(NCHNL,MRXTX,NTX,3),BFTL(NCHNL,MRXL,MCMP,NLINES), &
      Q1(1:3),QX,QY,QZ,CAZ,SAZ,CAZ0,SAZ0,SDP,CDP,PHI,QT(NCHNL),ALF(3,3),RHO,A1

 ALF = 0.
 DO JS = 1,NTX
   DO JR = 1,NRXTX(JS)
     IF (KNORM2(JR,JS) == 0) CYCLE
     DO JC = 1,3
       BTD(1:NCHNL,JR,JS,JC) = BTD(1:NCHNL,JR,JS,JC) / (BPRM(JR,JS) * CURNT(1))
     END DO
   END DO
 END DO

!  Convert to LINE-based output and apply units

 BFTL = 0.
 DO JL = 1,NLINES
   A1 = 1.
   SELECT CASE (UNITS(JL))
   CASE (32)
     A1 = 100.
   CASE (2,33)
     A1 = 1000.
   CASE (3,34)
     A1 = 1.E6
   CASE (4,11,21,35)
     A1 = 1.E9
   CASE (12,22)
     A1 = 1.E12
   END SELECT
   A1 = A1 * RXMNT(JL)

   JRL = 0
   DO JS = LNTR(1,JL), LNTR(2,JL)
     DO JR = LNTR(3,JL), LNTR(4,JL)
       JRL = JRL + 1
       DO JC = 1,MCMP
         BFTL(1:NCHNL,JRL,JC,JL) = A1 * BTD(1:NCHNL,JR,JS,JC)
       END DO
     END DO
   END DO

!  For surface dipole surveys only, reorient component 1 from north to lie along
!  the Survey X axis and component 2 from east to lie along the Survey Y axis.

   IF (RX_TYPE(JL) > 1) CYCLE  ! Skip if not magnetic dipole receiver

   CAZ0 = COS (SVAZM (JL))
   SAZ0 = SIN (SVAZM (JL))
   IF (ABS (CAZ0) < 1.E-4) CAZ0 = 0.
   IF (ABS (SAZ0) < 1.E-4) SAZ0 = 0.
   DO JR = 1,NRX(JL)
     IF (IDH(JL) == 0) THEN    !  Surface survey
       DO JT = 1,NCHNL
         Q1(1:2) = BFTL(JT,JR,1:2,JL)
         BFTL(JT,JR,1,JL) =  CAZ0 * Q1(1) + SAZ0 * Q1(2)
         BFTL(JT,JR,2,JL) = -SAZ0 * Q1(1) + CAZ0 * Q1(2)
       END DO
     ELSE
       CAZ = COS (RXAZM(JR,JL))
       SAZ = SIN (RXAZM(JR,JL))
       CDP = COS (RXDIP(JR,JL))
       SDP = SIN (RXDIP(JR,JL))
       DO JT = 1,NCHNL
         Q1(1:3) = BFTL(JT,JR,1:3,JL)
         QZ = Q1(3)
         IF (IDH(JL) == 1) THEN                           ! Express BFTL in U,V,A
           QX =  CAZ * Q1(1) + SAZ * Q1(2)                 ! local horizontal radial component
           QY = -SAZ * Q1(1) + CAZ * Q1(2)                 ! local horizontal transverse component
           BFTL(JT,JR,3,JL) =  CDP * QZ + SDP * QX         ! Axial component
           BFTL(JT,JR,1,JL) = -SDP * QZ + CDP * QX          ! local U component
           BFTL(JT,JR,1,JL) = -BFTL(JT,JR,1,JL)             ! local u component sign change to amuse EMIT
           BFTL(JT,JR,2,JL) = QY                            ! local V component

         ELSE IF (IDH(JL) == 2) THEN                 ! Express BFTL in S, N, W
           QX =  CAZ0 * Q1(1) + SAZ0 * Q1(2)         !   X radial component in survey system
           QY = -SAZ0 * Q1(1) + CAZ0 * Q1(2)         !   Y transverse component in survey system
           PHI = RXAZM(JR,JL) - SVAZM(JL)            !   local - survey aximuth

           ALF(3,1) = SDP * COS (PHI)               ! direction cosines for W (axial) component
           ALF(3,2) = SDP * SIN (PHI)
           ALF(3,3) = CDP

           RHO = SQRT (ALF(3,2)**2 + ALF(3,3)**2)
           ALF(2,1) = 0.                             ! direction cosines for N (out-of-section component)
           ALF(2,2) =  ALF(3,3) / RHO
           ALF(2,3) = -ALF(3,2) /RHO

           ALF(1,1) = ALF(2,2) * ALF(3,3) - ALF(2,3) * ALF(3,2)   ! direction cosines for S (in-section component)
           ALF(1,2) = ALF(2,3) * ALF(3,1)
           ALF(1,3) = -ALF(2,2) * ALF(3,1)

           BFTL(JT,JR,1,JL) = ALF(1,1) * QX + ALF(1,2) * QY + ALF(1,3) * QZ  ! S
           BFTL(JT,JR,2,JL) =                 ALF(2,2) * QY + ALF(2,3) * QZ  ! N
           BFTL(JT,JR,3,JL) = ALF(3,1) * QX + ALF(3,2) * QY + ALF(3,3) * QZ  ! W
         END IF
       END DO
     END IF
     IF (ISYS == 4) THEN        !  reverse channels for UTEM & subtract channel 1 response
       DO JC = 1,3
         DO JT = 1,NCHNL
           JTR = NCHNL - JT + 1
           QT(JT) = BFTL(JTR,JR,JC,JL)
         END DO
         DO JT = 1,NCHNL
           BFTL(JT,JR,JC,JL) = QT(JT)
           IF (JT > 1) BFTL(JT,JR,JC,JL) = BFTL(JT,JR,JC,JL) - BFTL(1,JR,JC,JL)
         END DO
       END DO
     END IF
   END DO
 END DO

 END SUBROUTINE SET_OUTPUT_LINES_TD

 SUBROUTINE SET_SWYTD (NSX,SWX,SWY,T0SX)
!----------------------------------------

! For time-domain, SET_SWYTD computes dI/dt at the transmitter using
!
!  Computes SWY to be dI(t)/dt
!  The units of SWY are amps / s

!*** Called by MAIN

!             INPUT
!             -----
!
!          NSX - number of source points in waveform
!          SWX - time abscissae for input waveform in seconds
!     SWY(*,3) - waveform in amps * NTRN * transmitter moment
!   SWY(*,1:2) = 0
!
!             OUTPUT
!             ------
!
!     SWY(*,3) = transmitter moment * waveform in amps
!     SWY(*,2) = transmitter moment * delta I(t) in amps
!     SWY(*,1) = transmitter moment * dI(t)/dt   in amps/s
!         T0SX = end of dI/dt on time or start of off-time

 IMPLICIT NONE
 REAL, PARAMETER :: T0_MIN=1.E-7
 INTEGER NSX,JT
 REAL SWX(NSX),SWY(NSX,3),DELT,T0SX

 INTENT (IN) NSX, SWX
 INTENT (INOUT) SWY


! Compute delta I in SWY(*,2) and dI/dt if it exists in SW(*,1).
! Compute the negative derivative so that dI/dt will be positive
! just before signal turn-off.  Store on right node (27.01.00)

 DO JT = 2, NSX
   SWY(JT,2) = SWY(JT-1,3) - SWY(JT,3)
   DELT = SWX(JT) - SWX(JT-1)
   IF (DELT > T0_MIN) THEN
     SWY(JT,1) = SWY(JT,2) / DELT
   END IF
 END DO


 T0SX = SWX(NSX)
 DO JT = NSX-1, 1, -1
   IF ( ABS (SWY(JT,1)) > 1.E-3) EXIT
   T0SX = SWX(JT)
 END DO
 END SUBROUTINE SET_SWYTD

 SUBROUTINE SET_SURVEY_1 (NLINES,MRXL,NTX,MXVRTX,NVRTX,MRXTX,NRX,MQVR,RX_TYPE,IPLT,NCNTRD, &
                          ECNTRD,SXND,SXED,SXN,SXE,SXZ,RXND,RXED,RXN,RXE,RXZ,LNTR,XRXTX,   &
                          YRXTX,ZRXTX,RXID,NCTD,YXZPLT,KNORM,KNORM2)
!----------------------------------------------------------------------------------------
!
!**  Called by MAIN
!
!  FOR SURVEY_TYPE = 1, SET_SURVEY_1
!  transforms transmitter coordinates from a global to local system if necessary.
!  Specifying transmitter and receiver arrays in GPS coordinates can
!  require double precision.  Rather than carry unnecessary higher precision
!  into the computation, arrays are adjusted by ECNTRD, NCNTRD
!
!  Although the user finds it more efficient to specify receivers by line,
!  computational efficiency requires these receivers to be referenced by their
!  transmitters.  Thus SET_SURVEY takes receiver positions and properties
!  associated with a line and references these to the transmitters.
!
!  Line-based arrays are constructed containing  the the global plot coordinates
!  of every Rx-Tx combination used in the survey.
!
!                    Input
!                    -----
!   NLINES      - number of lines
!   NTX         - total number of transmitter positions
!   MXVRTX      - maximum number of transmitter vertices
!   NVRTX(J)    - number of vertices for transmitter J
!   MRXTX       - maximum number of receivers per transmitter
!   MRXL        - maximum number of receivers per line = MRXTX
!   NRX(L)      - the number of receivers in Line L.
!   MQVR        - maximum number of vertices for all receivers
!               = 2 if any receiver is electric dipole, = 1 otherwise
!   RX_TYPE(L)  - Receiver type for Line L
!   IPLT        = 1 : plot response at receiver location
!               = 2 : plot response at transmitter-receiver midpoint
!   KNORM(L)    - norm indicator for line L
!   LNTR(1,L)   - transmitter index associated with Line L.  LNTR(2,L) = LNTR(1,L)
!
!   SXND(K,J)   - global north coordinate of vertex K of Tx J
!   SXED(K,J)   - global east coordinate of vertex K of Tx J
!   SXZ(J)      - depth of Tx J
!   RXND(I,J,K) - global north coordinate of vertex K of Rx I of Line J
!   RXED(I,J,K) - global east coordinate of vertex K of Rx I of Line J
!   RXZ(I,J)    - ground clearance (not depth) of dipole Rx I of Line J
!                 (electric dipole receivers have been rendered horizontal)
!
!      SXND, SXED, RXND, RXED are deallocated before return
!
!      NCNTRD   - North offset of local coordinate system       LOCAL = GLOBAL - CNTRD
!      ECNTRD   - East offset of local coordinate system
!
!                    Output
!                    ------
!  SXN(K,J)     - local north coordinate of vertex K of Tx J
!  SXE(K,J)     - local east coordinate of vertex K of Tx J
!  RXZ(I,J)     - depth of magnetic dipole Rx I of Line J
!  NCTD(I,J)    = 3 for MD Rx; = 1 for electric dipole Rx or coincident loop
!  LNTR(3,L)    - first receiver of Tx LNTR(1,JL) used for Line JL
!  LNTR(4,L)    - last receiver of Tx LNTR(1,JL) used for Line JL
!  XRXTX(I,J,K) - north coordinate of the Kth vertex of the Ith receiver of Tx J
!  YRXTX(I,J,K) - east coordinate of the Kth vertex of the Ith receiver of Tx J
!  ZRXTX(I,J)   - depth of the Ith receiver of transmitter J
!  RXID(I,J)    - receiver type for Rx I for Tx J
!  KNORM2(I,J)  - norm indicator for Rx I for Tx J
!  YXZPLT(1:3,I,L) - GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 INTEGER NLINES,MRXL,NTX,MXVRTX,NVRTX(NTX),MRXTX,MQVR,JL,JS,JR,JRP, &
         JV,KVR,NV,KRXTX(NTX),LNTR(4,NLINES)
 INTEGER, DIMENSION (NLINES) :: RX_TYPE,IPLT,KNORM,NRX
 INTEGER, DIMENSION (MRXTX,NTX) :: KNORM2,NCTD,RXID
 REAL SXZ(NTX),ZRXTX(MRXTX,NTX),A1
 REAL, DIMENSION(MXVRTX,NTX) :: SXN ,SXE
 REAL, DIMENSION(MRXL,NLINES) :: RXZ
 REAL, DIMENSION(MRXL,NLINES,MQVR) :: RXN, RXE
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL(KIND=QL) ECNTRD,NCNTRD,XSC,YSC,XRC,YRC
 REAL(KIND=QL), DIMENSION(MXVRTX,NTX) :: SXND,SXED
 REAL(KIND=QL), DIMENSION(MRXL,NLINES,MQVR) :: RXND,RXED
 REAL(KIND=QL), DIMENSION(3,MRXL,NLINES) :: YXZPLT

!  MRXL = MRXTX

 NCTD = 1
 SXE = REAL (SXED - ECNTRD)
 SXN = REAL (SXND - NCNTRD)

 XRXTX = 0.;  YRXTX = 0.; ZRXTX = 0.
 YXZPLT = 0.D0

 KRXTX = 0
 RXN = REAL (RXND - NCNTRD)
 RXE = REAL (RXED - ECNTRD)
 RXZ = -RXZ                 ! Convert elevation to depth
 DO JL = 1,NLINES
   KVR = RX_TYPE(JL)                  ! KVR = number of vertices for receiver type RX_TYPE(JL)
   IF (RX_TYPE(JL) == 3) KVR = 1

   JS = LNTR(1,JL)                    ! Identify transmitter for Line JL
   LNTR(3,JL) =  KRXTX(JS) + 1        ! First receiver of Tx JS used for Line JL
   KRXTX(JS) = KRXTX(JS) + NRX(JL)    ! Tx indices of receivers of Line L
   LNTR(4,JL) =  KRXTX(JS)            ! Last receiver of Tx JS used for Line JL
   NV = NVRTX(JS)
   XSC = SUM (SXND(1:NV,JS)) / REAL (NV)
   YSC = SUM (SXED(1:NV,JS)) / REAL (NV)

   DO JR = 1, NRX(JL)
     JRP = JR + LNTR(3,JL) - 1
     IF (RX_TYPE(JL) == 1) THEN
       KNORM2(JRP,JS) = KNORM(JL)
       NCTD(JRP,JS) = 3
     END IF
     RXID(JRP,JS) = RX_TYPE(JL)
     ZRXTX(JRP,JS) = RXZ(JR,JL)
     DO JV = 1,KVR
       XRXTX(JRP,JS,JV) = RXN(JR,JL,JV)
       YRXTX(JRP,JS,JV) = RXE(JR,JL,JV)
       IF (ABS (XRXTX(JRP,JS,JV)) < 1.E-4) XRXTX(JRP,JS,JV) = 0.
       IF (ABS (YRXTX(JRP,JS,JV)) < 1.E-4) YRXTX(JRP,JS,JV) = 0.
     END DO
     XRC = SUM (RXND(JR,JL,1:KVR)) / REAL (KVR)
     YRC = SUM (RXED(JR,JL,1:KVR)) / REAL (KVR)
     IF (IPLT(JL) == 1) THEN
       YXZPLT(1,JR,JL) = YRC
       YXZPLT(2,JR,JL) = XRC
       YXZPLT(3,JR,JL) = -REAL (RXZ(JR,JL),QL)
     ELSE
       YXZPLT(1,JR,JL) = 0.5 * (YRC + YSC)
       YXZPLT(2,JR,JL) = 0.5 * (XRC + XSC)
       A1 = 0.5 * (RXZ(JR,JL) + SXZ(JS))
       YXZPLT(3,JR,JL) = -REAL (A1,QL)           ! Make depth display negative.
     END IF
   END DO
 END DO

 END SUBROUTINE SET_SURVEY_1

 SUBROUTINE SET_Z (IACC,NPPD,ZMIN,ZMAX,SPAN,DEL0,NQS,QSTORE,NZ1)
!---------------------------------------------------------------

!  Sets up 4 point vertical interpolation array QSTORE(NZ1) between ZMIN and
!  ZMAX such that two points precede ZMIN and that 2 points follow ZMAX.
!  Intially it covers the interval SPAN with NPPD logarithmically points
!  until the interval between points, DEL1, exceeds DELO.
!  From then on a uniform interval of DEL1 is used.
!  Numerical 4 point cubic interpolation experiments with Green's function integrals
!  suggested that SPAN = MIN (skin depth, 10.) and DEL0 = skin depth / 5
!  If IACC > 1, then halve the intervals logarithmically.

 IMPLICIT NONE
 INTEGER NQS,NPPD,NZ1,JZ,NZ,IACC
 REAL QZ,ZMIN,ZMAX,SPAN,DEL0,DEL1,QSTORE(NQS)
 LOGICAL ADDPOINTS

 ADDPOINTS = .TRUE.
 QZ = LOG (SPAN) / REAL (NPPD)
 QZ = EXP (QZ)
 QSTORE(3) = ZMIN
 QSTORE(2) = ZMIN / QZ
 QSTORE(1) = QSTORE(2) / QZ

 DO JZ = 4,NQS
   NZ1=JZ
   QSTORE(JZ) = QSTORE(JZ-1) * QZ
!---------------------------
   IF (QSTORE(JZ-1) > ZMAX) THEN
     ADDPOINTS = .FALSE.
     EXIT
   END IF
!---------------------------

   DEL1 = QSTORE(JZ) - QSTORE(JZ-1)
   IF (DEL1 > DEL0) EXIT
 END DO

 IF (ADDPOINTS) THEN
   NZ = NZ1+1
   DO JZ = NZ, NQS
     NZ1 = JZ
     QSTORE(JZ) = QSTORE(JZ-1) + DEL1
     IF (QSTORE(JZ-1) > ZMAX) EXIT
   END DO
 END IF

 IF (IACC == 2) THEN   ! Halve intervals for improved interpolation accuracy
   NZ = NZ1
   NZ1 = 2*NZ1 - 1
   DO JZ = NZ,2,-1
     QSTORE (2*JZ-1) = QSTORE(JZ)
   END DO
   DO JZ = 2,NZ1-1,2
     QSTORE (JZ) = SQRT (QSTORE(JZ-1) * QSTORE(JZ+1))
   END DO
 END IF
 END SUBROUTINE SET_Z

 SUBROUTINE SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)
!--------------------------------------------

!  Determines the value of KFG needed for layer coefficient computation
!  I1 = 1 for magnetic source or 2 for electric source.

 IMPLICIT NONE
 INTEGER I1,I2,I3,SXLYR,RXLYR,NLYR,KFG

 I2 = 1; I3 = 1
 IF (SXLYR == 0) I2 = 0
 IF (RXLYR == 0) I3 = 0
 IF (I2 == 1 .AND. I3 == 1) THEN
   IF (RXLYR > SXLYR) I3 = 2
   IF (RXLYR < SXLYR) I2 = 2
 END IF
 IF (SXLYR == NLYR) I2 = 3
 IF (RXLYR == NLYR) I3 = 3
 KFG = 100*I1 + 10*I2 + I3

 END SUBROUTINE SET_KFG

 SUBROUTINE TDEM_3D (STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,TOPN, &
                     TCLS,FREQ,NFRQ,NTX,MRXTX,NRXTX,RXID,NCTD,BFD1,BTD1)
!----------------------------------------------------------------------------

!***  Called by MAIN
!***  Calls CUBSPL, COSTRN, FOLD_CONVOLVE

!  Computes BTD1, the time domain response for the input frequency_domain data.
!  It computes voltage (volts) or dB/dt (teslas/sec) if STEP = 0) or magnetic
!  field B (in teslas) if STEP = 1)  by convolving the step response of the
!  earth with the negative time-derivative of the current waveform.
!  For magnetic field, this is averaged across the receiver window.  For db/dt,
!  or voltage, this is differenced across the  receiver window.  The negative
!  dI/dt is used so that current switch off corresponds to positive response.
!
!  On entry, the imaginary component of the frequency-domain data in array BFD1
!  is divided by frequency and then cosine transformed into time-domain step
!  response data out to NPULS bipolar cycles.  For each receiver position for
!  for each transmitter, FOLD_AND_CONVOLVE is called to fold the positive &
!  negative parts of the bipolar current pulse into a half-cycle (length PULSE)
!  decay curve.  This result is convolved with the dI/dt waveform.
!
!        STEP = 1 for normal step response is to be computed
!             = 4 for UTEM pure step response
!         NSX - number of points used to discretise transmitter signal
!         SWX - abscissae (seconds) of current waveform
!         SWY - dI/dt at times SWX
!       NPULS - number of bipolar pulses of length PULSE
!       PULSE - length single on pulse plus off-time
!       NTYRP - number of values in TRP for total signal length: 2 * NPULS *PULSE
!         TRP - array of time values for FD -> TD transformations
!      NTYPLS - number of TRP values in 1 PULSE
!       NCHNL - number of channels
!        TOPN - time at which receiver channel I opens.
!        TCLS - time at which receiver channel I closes.
!        FREQ - array of NFRQ frequencies.
!         NTX - number of transmitter positions.
!       MRXTX - maximum number of receiver positions for any transmitter position
!    NRXTX(J) = number of receivers FOR transmitter J
!   RXID(I,J) = RX_TYPE of receiver I for transmitter J
!   NCTD(I,J) = number of components for receiver I for transmitter J
!             = number of components, = 3 or 1 usually
!   BFD1(I,J,K,L) - Lth component of the complex frequency-domain response at
!                   receiver J, of transmitter K for frequency I.  If NCTD = 1
!                   as is the case for coincident loop, or electric dipole,
!                   then the relevant component is stored in L=1 position

 IMPLICIT NONE
 REAL, PARAMETER :: TWOPI=6.283185
 INTEGER STEP,NSX,NPULS,NTYPLS,NTYRP,NCHNL,NFRQ,NTX,MRXTX,NRXTX(NTX), &
         NCTD(MRXTX,NTX),STEPC,RXID(MRXTX,NTX),JR,JS,JF,JC,JT
 REAL, DIMENSION(:,:), ALLOCATABLE ::  YSCAT,YFRQ
 REAL PULSE,FREQ(NFRQ),WF(NFRQ),SWX(NSX),SWY(NSX,3), &
      COSTRN,T,YCUM(NCHNL),TOPN(NCHNL),TCLS(NCHNL),TRP(NTYRP), &
      BTD1(NCHNL,MRXTX,NTX,3)
 COMPLEX BFD1(NFRQ,MRXTX,NTX,3)

 BTD1 = 0.

 ALLOCATE (YSCAT(4,NTYRP), YFRQ(4,NFRQ) )
 YSCAT=0; YFRQ=0

!  The - sign below is a consequence of using the sine transform for a
!  +iwt sign convention

 WF(1:NFRQ) = ALOG (TWOPI * FREQ(1:NFRQ))

!  For each component at each receiver, compute the time-domain step response
!  by splining the imaginary part of the frequency-domain response, converting
!  it to time-domain step function response and folding the NPULS bipolar decay
!  curve into a combined pulse decay curve of length PULSE.  Convolve this with
!  the TX waveform to produce BTD1, the 'observable" stripped response for the
!  system.

 DO JS = 1,NTX
   DO JR = 1, NRXTX(JS)
     STEPC = STEP
     IF (RXID(JR,JS) == 2) THEN
       STEPC = 3                      ! Electrode E field Transform INTEGRAL { E dl } to TD
     END IF
     DO JC = 1,NCTD(JR,JS)
       DO JF = 1,NFRQ     ! Divide by -iw to set up step response
         IF (STEPC == 3) THEN
           YFRQ(1,JF) = REAL ( BFD1(JF,JR,JS,JC) )
         ELSE
           YFRQ(1,JF) = -AIMAG ( BFD1(JF,JR,JS,JC) ) / (TWOPI * FREQ(JF))
         END IF
       END DO
       CALL CUBSPL (WF,YFRQ,NFRQ)

       YSCAT = 0.

       DO JT = 1, NTYRP   !  Convert to step-function time-domain.
         T = TRP(JT)
         YSCAT(1,JT) = COSTRN (WF,YFRQ,NFRQ,T)
       END DO

       CALL FOLD_AND_CONVOLVE (STEPC,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYRP,NTYPLS, &
                               NCHNL,TOPN,TCLS,YSCAT,YCUM)

       BTD1(1:NCHNL,JR,JS,JC) = YCUM(1:NCHNL)
     END DO
   END DO
 END DO

 DEALLOCATE (YSCAT, YFRQ)

 END SUBROUTINE TDEM_3D

 SUBROUTINE FOLD_AND_CONVOLVE (STEPC,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYRP,NTYPLS, &
                               NCHNL,TOPN,TCLS,YPLS,YCUM)
!----------------------------------------------------------------------------

!  Computes the "observed" response YCUM by convolving the splined earth
!  response function, YPLS, with the TX waveform.

!***  Called by HSBOSS and TDEM_3D
!***  Calls: CUBVAL, CUBSPL, TXCNVL

!    STEPC = 1 => average convolved response over receiver windows.  This will
!                 be the case when input and desired output are both either
!                 step response or impulse response.
!          = 0 => difference convolved response over receiver windows. This will
!                 be the case when input is step and desired output is impulse.
!          = 3 => electrode voltage convolution with current
!          = 4 for UTEM pure step response
!      NSX - number of points used to discretise transmitter signal
!      SWX - abscissae (seconds) of current waveform
!      SWY - dI/dt at times SWX
!    NPULS - number of bipolar pulses of length PULSE
!    PULSE - length single on pulse plus off-time
!    NTYRP - number of values in TRP for total signal length: 2 * NPULS *PULSE
!      TRP - array of time values for FD -> TD transformations
!   NTYPLS - number of TRP values in 1 PULSE
!    NCHNL - number of channels
!     TOPN - time at which receiver channel I opens.
!     TCLS - time at which receiver channel I closes.
!

 IMPLICIT NONE
 INTEGER JT,NTYRP,NTYPLS,NPULS,NSX,NCHNL,JGL,JP,STEPC,MXCNV
 REAL PULSE,TRP(NTYRP),SWX(NSX),SWY(NSX,3),TOPN(NCHNL),TCLS(NCHNL),T1,T2,WIDTH, &
      HWIDTH,YC1,YC2,TC(3),GLX(3),GLW(3),YPLS(4,NTYRP),X,XP,YCUM(NCHNL),CUBVAL, &
      TXCNVL,TXCNVD,WT,FOLD(NTYRP)
 DATA GLW(1:3) /.5555556, .8888889, .5555556/, GLX(1:3) /-.7745967, 0., .7745967/

!  Accumulate the results of NPULS bipolar cycles by splining the instantaneous
!  response and folding the positive and negative parts of each cycle back
!  into a single pulse.  For on-time systems, compute a correction factor.
!  which will be based on the negative instantaneous response at .1 microsecond.

 CALL CUBSPL (TRP,YPLS,NTYRP)
 FOLD = 0.
 DO JT = 1,NTYPLS
   X = TRP(JT)
   XP = X + PULSE
   FOLD(JT) = CUBVAL (TRP,YPLS,NTYRP,X) - CUBVAL (TRP,YPLS,NTYRP,XP)
   FOLD(JT) = CUBVAL (TRP,YPLS,NTYRP,X) - CUBVAL (TRP,YPLS,NTYRP,XP)

   DO JP = 2, NPULS
     X = XP + PULSE
     XP = X + PULSE
     FOLD(JT) = CUBVAL (TRP,YPLS,NTYRP,X) - CUBVAL (TRP,YPLS,NTYRP,XP) &
              + FOLD(JT)
   END DO
 END DO

 YPLS = 0.
 YPLS(1,1:NTYPLS) = FOLD(1:NTYPLS)
 CALL CUBSPL (TRP,YPLS,NTYPLS)
 YCUM = 0.

 MXCNV = NTYPLS + NSX
 DO JT = 1, NCHNL
   T1 = TOPN(JT)
   T2 = TCLS(JT)
   WIDTH = T2 - T1
   HWIDTH = WIDTH /2.

   IF (STEPC == 0) THEN  ! Differentiate to compute impulse response for step input
     YC1 = TXCNVL (T1,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
     YC2 = TXCNVL (T2,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
     YCUM(JT) = (YC1 - YC2) / WIDTH

   ELSE

! Step response for step input or impulse response for impulse input
! Average the response over receiver windows using 3 point Gaussian integration.

     TC(2) = (TCLS(JT) + TOPN(JT)) /2.
     TC(1) = TC(2) + HWIDTH * GLX(1)
     TC(3) = TC(2) + HWIDTH * GLX(3)

     DO JGL = 1, 3
       T1 = TC(JGL)
       WT = GLW(JGL) / 2.
       IF (STEPC == 1) THEN                                     ! Magnetic dipole or loop receiver
         YC1 = TXCNVL (T1,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
       ELSE IF (STEPC == 4) THEN                                ! pure on-time step
         YC1 = SWY(1,1) * CUBVAL (TRP,YPLS,NTYPLS,T1)
       ELSE IF (STEPC == 3) THEN                                ! Electrode receiver
         YC1 = TXCNVD (MXCNV,T1,NTYPLS,TRP,YPLS,NSX,SWX,SWY,3)  ! Current is in SWY(*,3)
       END IF
       YCUM(JT) = YCUM(JT) + (WT * YC1)
     END DO
   END IF
 END DO

 END SUBROUTINE FOLD_AND_CONVOLVE

 REAL FUNCTION TXCNVD (MXCNV,T,NTYPLS,TRP,YPLS,NSX,SWX,SWY,K1)
!-------------------------------------------------------------
!
!***  Called by FOLD_AND_CONVOLVE
!***  Calls CUBINT, CUBSPL, CUBVAL, LINVAL, TXCMRG

!  Convolves earth response function (ERF) with the source waveform
!  contained in SWY(*,K1) at NSX points to produce the system response
!  of the earth.  In Leroi, it is used for electric field computations.
!  The source current function is contained in component K1 = 3
!
!       MXCNV = NTYPLS + NSX
!           T - convolution time in sec measured from the beginning
!               of the source waveform.
!   TRP, YPLS - abscissa & ordinate values of earth response function to
!               be convolved.
!      NTYPLS - number of values in TRP and YPLS
!         SWX - abscissa of time values of source waveform in sec.
!    SWY(*,K1) - source current values
!         NSX - number of points in SWX & in each waveform stored in SWY
!
!  Defining  T1 = MIN {T, signal length,}, the convolution is formally
!  computed as
!
!   TXCNVD (T) = INT (T0 -> T) { YPLS (tau) * SWY (T-tau)  d tau }

!  where T0 = MAX { TRP(1), T - SWX (NSX)}
!
!       ONTIME RESPONSE
!       ---------------
!  For response in the on-time period, ( T < signal length) a correction to
!  account for the response from 0 -> T0 is needed.  Analysis and subsequent
!  numerical experiments confirm that as T -> 0, step dB/dt -> A * T**(-1/2).
!  Thus ERFINT, the integral of YPLS from 0 to TRP(1), is simply
!  2 * TRP(1) * YPLS (TRP(1)) if TRP(1) is chosen sufficiently early.
!  The convolution correction factor is SWY(T) * ERFINT.

!  Alternatively, we can difference the step B field from 0 to TRP(1) which
!  is a lot easier since the step B field at T = 0 is simply the DC field due
!  to a transmitter image buried at z = ALT; i.e., the z+z' term.  In this case,
!  the bigger TRP(1) is, the more accurate the difference in B but this must be
!  sufficiently small so that the change in dI/dt is negligable.  Thus, TRP(1)
!  is chosen to be .1 microsecond.

 IMPLICIT NONE
 INTEGER K1,MXCNV,NTYPLS,NSX,N1,J1,N2,J2,NCNV
 REAL T,TC,T0,TRP(NTYPLS),YPLS(4,NTYPLS),SWX(NSX),SWY(NSX,3),YCNV(4,MXCNV), &
      XCNV(MXCNV),X1(MXCNV),Y1(MXCNV),X2(MXCNV),Y2(MXCNV),CUBVAL,CUBINT,LINVAL

 INTENT (IN) MXCNV,T,NTYPLS,TRP,YPLS,NSX,SWX,SWY

!  Set up X1,Y1, the N1 values of SWX, SWY * YPLS for signal ontime < T.
!  where X1, the conjugate signal time, contains T-SWX values.
!  Set up X2,Y2, the N2 values of TRP, YPLS * SWY for ERF points  <= T.

 TXCNVD = 0.0
 N1 = 0
 DO J1 = NSX, 1, -1
   TC = T - SWX(J1)
   IF (TC < 0.) CYCLE
   N1 = N1 + 1
   X1(N1) = TC
   Y1(N1) = SWY(J1,K1) * CUBVAL (TRP,YPLS,NTYPLS,TC)
 END DO

 T0 = T - SWX(NSX)
 T0 = MAX (T0, TRP(1))/ 1.0001
 N2 = 0
 DO J2 = 1,NTYPLS
   IF ((TRP(J2) > T0) .AND. (TRP(J2) < T)) THEN
     N2 = N2 + 1
     X2(N2) = TRP(J2)
     TC = T - TRP(J2)
     Y2(N2) = YPLS(1,J2) * LINVAL(NSX,SWX,SWY,K1,TC)
   END IF
 END DO

!  Merge the two lists into XCNV, YCNV of length NCNV.
!  Then spline and integrate

!+++++++++++++++++++++++++++++++++
 IF (N1 + N2 < 4) RETURN
!+++++++++++++++++++++++++++++++++

 CALL TXCMRG (MXCNV,X1,Y1,N1,X2,Y2,N2,XCNV,YCNV,NCNV)

!+++++++++++++++++++++++++++++++++
 IF (NCNV < 4) RETURN
!+++++++++++++++++++++++++++++++++

 CALL CUBSPL (XCNV,YCNV,NCNV)
 TXCNVD = CUBINT (XCNV,YCNV,NCNV,T0,T)

 END FUNCTION TXCNVD

 REAL FUNCTION TXCNVL (T,NTYPLS,TRP,YPLS,NSX,SWX,SWY)
!----------------------------------------------------

!***  Called by FOLD_AND_CONVOLVE
!***  Calls CUBINT, CUBVAL

!  Computes the system dB/dt response by convolving the computed dI/dt with
!  the impulse B response of the earth.  For step current drops, system dB/dt
!  is computed asthe product of instantaneous current drop times the
!  earth step dB/dt.

!  This routine assumes that the source waveform is composed of NSX linear
!  segments.  Thus NSX-1 constant dI/dt values are contained in SWY(*,1).

!  The input earth response function (step dB/dt or equivalently, impulse B)
!  must be contained in a splined array of NTYPLS values of time (abscissa) TRP
!  and ordinate YPLS.  System dB/dt is computed by integrating YPLS between
!  the SWX points of constant dI/dt segments.

!              T - convolution time in sec measured from the beginning
!                  of the source waveform.
!      TRP, YPLS - abscissa & ordinate values of earth response function to
!                  be convolved.
!         NTYPLS - number of values in TRP and YPLS
!            SWX - abscissa of time values of source waveform in sec.
!       SWY(*,1) - dI/dt if it exists (0 otherwise)
!       SWY(*,2) - first difference values of source waveform
!                  (-delta I) in amps.
!            NSX - number of points in SWX & 1

 IMPLICIT NONE
 REAL, PARAMETER :: T0_MIN=1.E-7
 INTEGER NTYPLS,NSX,JT
 REAL T,TF,CNV,TB,DELT,SEG,TRP(NTYPLS),YPLS(4,NTYPLS),SWX(NSX),SWY(NSX,3),TEND, &
      CUBINT,CUBVAL
 LOGICAL DER

 TF = T - TRP(1)
 CNV = 0.
 DO JT = 2, NSX
   IF (SWX(JT) < T0_MIN) CYCLE
   IF (SWX(JT-1) > TF) EXIT
   TB = T - MIN (TF, SWX(JT))
   DELT = SWX(JT) - SWX(JT-1)
   DER = .FALSE.
   IF (DELT > T0_MIN) THEN
     TEND = T - SWX(JT-1)
     DER = .TRUE.
   END IF

!  For an instantaneous step drop in current, SEG is YPLS times SWY(*,2),
!  since YPLS is already the dB/dt step response.  Otherwise SEG is the
!  integral of YPLS * constant dI/dt SWY(*,1) since YPLS is also impulse B.

   IF (DER) THEN
     SEG = SWY(JT,1) * CUBINT (TRP,YPLS,NTYPLS,TB,TEND)
   ELSE
     SEG = SWY(JT,2) * CUBVAL (TRP,YPLS,NTYPLS,TB)
   END IF
   CNV = CNV + SEG
 END DO
 TXCNVL = CNV

 END FUNCTION TXCNVL

 REAL FUNCTION LINVAL (NX,XVAL,YVAL,K1,X1)
!-----------------------------------------

!  Evaluates a function at X1 from from its linear representation.

!***  Called by TXCNVD
!
!***  Calls INTERV.  On exit from INTERV
!
!       MFLAG = -1  => X is to the left of interpolated range
!             =  1  => X is to the right of interpolated range
!             =  0  => X is in the interpolated range

!
!     XVAL(1:NX) - location of the abscissa knots.  The rightmost data point
!                  used to calculate coefficients is not included.
!
!  YVAL(1:NX,K1) = function values.
!
!
!     The value is a linear interpolation between the knots.
!
!             METHOD
!             ------
!
!  The interval index I, appropriate for X, is found through a call to INTERV.
!  The formula for F is evaluated using nested multiplication.
!******************************************************************************

 IMPLICIT NONE
 INTEGER K1,I,MFLAG,NX
 REAL XVAL(NX),YVAL(NX,3),X1,H

 INTENT (IN) NX,XVAL,YVAL,X1
!
!  Find index I of largest breakpoint to the left of X1.
!
 CALL INTERV ( XVAL, NX-1, X1, I, MFLAG )

 H = X1 - XVAL(I)
 IF (MFLAG == -1) H = 0.
 LINVAL = YVAL(I,K1) + H * (YVAL(I+1,K1) - YVAL(I,K1)) / (XVAL(I+1) - XVAL(I))

 END FUNCTION LINVAL


 SUBROUTINE TXCMRG (MXCNV,X1,Y1,N1,X2,Y2,N2,XCNV,YCNV,NCNV)
!----------------------------------------------------------

!  Merges two previously sorted list pairs X1, Y1 of length N1 and X2, Y2 of
!  length N2 into list pair XCNV, YCNV of length NCNV into ascending values of
!  XCNV.

!***  Called by TXCNVD

 IMPLICIT NONE
 REAL, PARAMETER :: TOL=1.E-3
 INTEGER MXCNV,N1,N2,NCNV,K1,K2,N,J1
 REAL DELT,TL1,XCNV(MXCNV),X1(MXCNV),Y1(MXCNV),X2(MXCNV),Y2(MXCNV),YCNV(4,MXCNV)
 LOGICAL LIST1, LIST2

 INTENT (IN) MXCNV,X1,Y1,N1,X2,Y2,N2
 INTENT (OUT) XCNV,YCNV,NCNV

 LIST1 = .TRUE.
 LIST2 = .TRUE.
 K1 = 1
 K2 = 1
 N = N1 + N2

 DO J1 = 1, N
   IF (LIST1 .AND. LIST2) THEN
     IF (X1(K1) < X2(K2)) THEN
       XCNV(J1) = X1(K1)
       YCNV(1,J1) = Y1(K1)
       K1 = K1 + 1
       IF (K1 > N1) LIST1 = .FALSE.
     ELSE
       XCNV(J1) = X2(K2)
       YCNV(1,J1) = Y2(K2)
       K2 = K2 + 1
       IF (K2 > N2) LIST2 = .FALSE.
     END IF
   ELSE IF (LIST1) THEN
     XCNV(J1) = X1(K1)
     YCNV(1,J1) = Y1(K1)
     K1 = K1 + 1
     IF (K1 > N1) LIST1 = .FALSE.
   ELSE IF (LIST2) THEN
     XCNV(J1) = X2(K2)
     YCNV(1,J1) = Y2(K2)
     K2 = K2 + 1
     IF (K2 > N2) LIST2 = .FALSE.
   END IF
 END DO

 NCNV = 1      !  Clean up list
 DO J1 = 2, N
   DELT = XCNV(J1) - XCNV(NCNV)
   TL1 = TOL * XCNV(J1)
   IF (DELT > TL1) THEN
     NCNV = NCNV + 1
     XCNV(NCNV) = XCNV(J1)
     YCNV(1,NCNV) = YCNV(1,J1)
   END IF
 END DO

 END SUBROUTINE TXCMRG

 SUBROUTINE VALIDATE_CMP (NLG,MXERR,JL,NLINES,CMP,NCMPL,ISYS)
!------------------------------------------------------------

 IMPLICIT NONE
 INTEGER NLG,MXERR,JL,NLINES,QCMP,ISYS
 INTEGER, DIMENSION(NLINES) :: CMP,NCMPL

 SELECT CASE (CMP(JL))
 CASE (1,2,3)
   QCMP = CMP(JL)
 CASE (12,21)
   QCMP = 12
 CASE (13,31)
   QCMP = 13
 CASE (23,32)
   QCMP = 23
 CASE (123,132,231,213,321,312)
   QCMP = 123
 CASE DEFAULT
   CALL WRITE_LOG_FILE (NLG,24,MXERR,2)
 END SELECT
 CMP(JL) = QCMP
 NCMPL(JL) = 1
 IF (CMP(JL) > 9) NCMPL(JL) = 2
 IF (CMP(JL) > 99) NCMPL(JL) = 3
 IF (ISYS == 2) THEN
   CMP(JL) = 1
   NCMPL(JL) = 1
 END IF

 END SUBROUTINE VALIDATE_CMP

 SUBROUTINE VALIDATE_KMP (NLG,MXERR,J,NLINES,CMP,KMP)
!-----------------------------------------------------

 IMPLICIT NONE
 INTEGER NLG,MXERR,J,NLINES,QCMP
 INTEGER, DIMENSION(NLINES) :: CMP,KMP

 SELECT CASE (KMP(J))
 CASE (1)
   QCMP = 1
 CASE(2)
   QCMP = 2
 CASE(3)
   QCMP = 3
 CASE (12,21)
   QCMP = 12
 CASE (13,31)
   QCMP = 13
 CASE (23,32)
   QCMP = 23
 CASE (123,132,231,213,321,312)
   QCMP = 123
 CASE DEFAULT
   CALL WRITE_LOG_FILE (NLG,52,MXERR,2)
 END SELECT

 SELECT CASE (CMP(J))
 CASE(1)
   IF (QCMP /= 1 .AND. QCMP /= 12 .AND. QCMP /= 13 .AND. QCMP /= 123) THEN
     CALL WRITE_LOG_FILE (NLG,54,MXERR,2)
     WRITE(NLG,1) J,J
   END IF
 CASE(2)
   IF (QCMP /= 2 .AND. QCMP /= 12 .AND. QCMP /= 23 .AND. QCMP /= 123) THEN
     CALL WRITE_LOG_FILE (NLG,54,MXERR,2)
     WRITE(NLG,2) J,J
   END IF
 CASE(3)
   IF (QCMP /= 3 .AND. QCMP /= 13 .AND. QCMP /= 23 .AND. QCMP /= 123) THEN
     CALL WRITE_LOG_FILE (NLG,54,MXERR,2)
     WRITE(NLG,3) J,J
   END IF
 CASE(12)
   IF (QCMP /= 12 .AND. QCMP /= 123) THEN
     CALL WRITE_LOG_FILE (NLG,54,MXERR,2)
     WRITE(NLG,4) J,J
   END IF
 CASE(13)
   IF (QCMP /= 13 .AND. QCMP /= 123) THEN
     CALL WRITE_LOG_FILE (NLG,54,MXERR,2)
     WRITE(NLG,5) J,J
   END IF
 CASE(23)
   IF (QCMP /= 23 .AND. QCMP /= 123) THEN
     CALL WRITE_LOG_FILE (NLG,54,MXERR,2)
     WRITE(NLG,6) J,J
   END IF
 CASE(123)
   IF (QCMP /= 123) THEN
     CALL WRITE_LOG_FILE (NLG,54,MXERR,2)
     WRITE(NLG,7) J,J
   END IF
 END SELECT

 1 FORMAT(T3,'If CMP(',I2,') = 1, KMP(',I2,') must be 1, 12, 21, 13, 31, or a permutation of 123')
 2 FORMAT(T3,'If CMP(',I2,') = 2, KMP(',I2,') must be 2, 12, 21, 23, 32, or a permutation of 123')
 3 FORMAT(T3,'If CMP(',I2,') = 3, KMP(',I2,') must be 3, 13, 31, 23, 32, or a permutation of 123')
 4 FORMAT(T3,'If CMP(',I2,') = 12, KMP(',I2,') must be 12, 21 or a permutation of 123')
 5 FORMAT(T3,'If CMP(',I2,') = 13, KMP(',I2,') must be 13, 31 or a permutation of 123')
 6 FORMAT(T3,'If CMP(',I2,') = 23, KMP(',I2,') must be 23, 32 or a permutation of 123')
 7 FORMAT(T3,'If CMP(',I2,') = 123, KMP(',I2,') must be a permutation of 123')

 END SUBROUTINE VALIDATE_KMP

 SUBROUTINE VALIDATE_LINE (NLG,MXERR,LINE_CHK,NSTAT,JL,NLINES,LINE,NRX)
!----------------------------------------------------------------------

!*** Call WRITE_LOG_FILE
!*** Called by PREPARE_INVRT_DATA

! Validates Line number LINE_CHK aagainst Line(J).
! Validates NSTAT against NRX, the number of stations on Line J

 IMPLICIT NONE
 INTEGER NLG,NLINES,LINE_CHK,NSTAT,JL,LINE(NLINES),NRX(NLINES),MXERR
 CHARACTER(LEN=20) CTXT(2),QL0

 IF (LINE_CHK /= LINE(JL)) THEN
   CALL WRITE_LOG_FILE (NLG,50,MXERR,2)
   WRITE(QL0,*) LINE(JL)
   READ(QL0,'(A)') CTXT(1)
   WRITE(QL0,*) LINE_CHK
   READ(QL0,'(A)') CTXT(2)
   WRITE(NLG,90) JL,CTXT(1:2)
 END IF

 IF (NSTAT /= NRX(JL)) THEN
   CALL WRITE_LOG_FILE (NLG,51,MXERR,2)
   WRITE(NLG,91) JL,NRX(JL),NSTAT
 END IF

 90 FORMAT(T3,'LINE(',I2,') =',A,4X,'LINE_CHK =',A)
 91 FORMAT(T3,'NRX(',I2,') =',I4,4X,'NSTAT =',I4)

 END SUBROUTINE VALIDATE_LINE

 SUBROUTINE WRITE_TD (NW,NW1,KPRT,NCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                      SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,TMS,HEADER_ID,CMP,BFTL,RDATA,RWTS)
!---------------------------------------------------------------------------------------------

!  Writes time domain output to unit NW
!  If KPRT = 1, write error structure.
!
!*** Called by: MAIN
!***     Calls:
!
!   NW,NW1          : output unit numbers
!   KPRT            = 0 => write model data only
!                   = 1 write model data plus error structurete
!                   = -1 write inversion data only
!   NLINES          : number of lines
!   MRXL            : maximum number of receivers per line
!   MCMP            = 1 for coincident loop;  = 3 otherwise
!   NRX             : number of receivers per line
!   NCHNL           : number of channels
!   SURVEY_TYPE     = 1: general transmitter with independent receiver arrays
!                   = 2: moving rectangular loop with one or more fixed offset MD receivers
!                   = 3: fixed offset MD - MD surface surface
!                   = 4: coincident Loop
!                   = 5: downhole MD-MD probe
!   LINE(L)         : line number of Line L
!   UNITS(L)        : unit designater of Line L
!   IDH(L)          : surface (0);  local U,V,A (1);  modified U,V,A (2)
!   RXID(L)         : receiver type Line L - mag dipole (1); electric dipole (2)
!   UNITS(L)        : integer uniot designator = see SUBROUTINE GET_UNITS_TEXT
!   SVAZM(L)        : survey aximuth for Line L (radians)
!   PRFL            : 1 for profile output; = 0 for temporal output
!   SVAZM           : survey azimuth (radians) of Line L
!   ISYS            : UTEM output if ISYS = 4
!   IPLT(J,L)       : the response of Rx J of Line L is plotted at 1:Rx;  2:Tx-Rx midpoint;  3:Tx midpoint
!   YXZPLT(1:3,I,L) : GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L
!   TMS             : time midpoints (ms) for NCHNL windows
!   HEADER_ID       : used to relate header to RX_TYPE and plot orientation
!   CMP             : component selection
!   BFTL(I,J,K,L)   : the Kth component measured response at frequency I from receiver J,
!                     of Line L, transmitter K (modelled)
!   RDATA(I,J,K,L)  : the Kth component measured response at frequency I from receiver J,
!                     of Line L, transmitter K (inversion data)
!   RWTS(I,J,K,L)   : inversion weights corresponding to RDATA

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 REAL, PARAMETER :: R2D= 180./3.14159
 INTEGER NW,NW1,KPRT,IDT,NLINES,MRXL,MCMP,NCHNL,PRFL,SURVEY_TYPE,ISYS,KDEG,NRX1,JL,JR,LC,JT
 INTEGER, DIMENSION(NLINES) :: NRX,CMP,LINE,UNITS,RX_TYPE,HEADER_ID,IDH,IPLT
 INTEGER RWTS(NCHNL,MRXL,MCMP,NLINES)
 REAL, DIMENSION(NCHNL,MRXL,MCMP,NLINES) :: BFTL,RDATA
 REAL SVAZM(NLINES),TMS(NCHNL),YTR(NCHNL,MRXL)
 REAL(KIND=QL) YXZPLT(3,MRXL,NLINES),RXPLT(3,MRXL)
 CHARACTER(LEN=120) TITLE
 CHARACTER(LEN=20) UTXT(2),CTXT(2),PLT_PT(3),QL0,DTYPE(4)
 DATA PLT_PT /'Rx','Tx-Rx_MID','Tx'/
 DATA DTYPE /'model output','data','Model Output','Data'/

 IDT = 1
 IF (KPRT == -1) THEN
   IDT = 2
   BFTL = RDATA
 END IF
 IF (KPRT == 0) THEN             !  No inversion
   WRITE(NW,'(/T3,A)') TRIM( ADJUSTL (TITLE))
   IF (ISYS == 4) THEN
     WRITE(NW,7)
   ELSE
     IF (SURVEY_TYPE == 1) WRITE(NW,1) TRIM (ADJUSTL (DTYPE(IDT)))
     IF (SURVEY_TYPE == 2) WRITE(NW,2) TRIM (ADJUSTL (DTYPE(IDT)))
     IF (SURVEY_TYPE == 3) WRITE(NW,3) TRIM (ADJUSTL (DTYPE(IDT)))
     IF (SURVEY_TYPE == 4) WRITE(NW,4) TRIM (ADJUSTL (DTYPE(IDT)))
     IF (SURVEY_TYPE == 5) WRITE(NW,5) TRIM (ADJUSTL (DTYPE(IDT)))
     WRITE(NW,6)
   END IF
 END IF

 IDT = 3
 IF (KPRT == -1) IDT = 4
 DO JL = 1,NLINES
   LC = CMP(JL)

   CALL GET_UNITS_TEXT (UNITS(JL),UTXT)
   WRITE(QL0,*) LINE(JL)
   READ(QL0,'(A)') CTXT(1)           ! Line number
   CTXT(2) = PLT_PT(IPLT(JL))
   KDEG = INT (R2D * SVAZM(JL))
   WRITE(NW1,16) TRIM (ADJUSTL (CTXT(1))),HEADER_ID(JL),KDEG,TRIM(UTXT(2)),TRIM (ADJUSTL (CTXT(2)))
   IF (RX_TYPE(JL) == 2) THEN
     WRITE(NW,12) TRIM (ADJUSTL (CTXT(1))),TRIM(UTXT(1)),TRIM (ADJUSTL (CTXT(2)))
   ELSE
     IF (SURVEY_TYPE == 1) THEN
       WRITE(NW,11) TRIM (ADJUSTL (CTXT(1))),KDEG,TRIM(UTXT(1)),TRIM (ADJUSTL (CTXT(2)))
     ELSE
       WRITE(NW,10) TRIM (ADJUSTL (CTXT(1))),KDEG,TRIM(UTXT(1)),TRIM (ADJUSTL (CTXT(2)))
     END IF
   END IF

   NRX1 = NRX(JL)
   DO JR = 1,NRX1
     RXPLT(1:3,JR) = YXZPLT(1:3,JR,JL)
   END DO

   IF (RX_TYPE(JL) == 1) THEN            ! magnetic dipole output
     DO JR = 1,NRX1
       SELECT CASE (CMP(JL))
       CASE(1)
         WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NCHNL,JR,1,JL)
       CASE(2)
         WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NCHNL,JR,2,JL)
       CASE(3)
         WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NCHNL,JR,3,JL)
       CASE(12)
         WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NCHNL,JR,1,JL), BFTL(1:NCHNL,JR,2,JL)
       CASE(13)
         WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NCHNL,JR,1,JL), BFTL(1:NCHNL,JR,3,JL)
       CASE(23)
         WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NCHNL,JR,2,JL), BFTL(1:NCHNL,JR,3,JL)
       CASE(123)
         WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NCHNL,JR,1,JL), BFTL(1:NCHNL,JR,2,JL), BFTL(1:NCHNL,JR,3,JL)
       END SELECT
     END DO

     IF (LC == 3 .OR. LC == 13 .OR. LC == 23 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! Z, A or W component
       CASE (0)
         WRITE(NW,23) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,26) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,29) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NCHNL,JR) = BFTL(1:NCHNL,JR,3,JL)
       END DO
       CALL WRTDP (NW,PRFL,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)
       IF (KPRT == 1) CALL WRITE_TD_MISFIT (3)
     END IF

     IF (LC == 1 .OR. LC == 13 .OR. LC == 12 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! X, U or S component
       CASE (0)
         WRITE(NW,21) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,24) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,27) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NCHNL,JR) = BFTL(1:NCHNL,JR,1,JL)
       END DO
       CALL WRTDP (NW,PRFL,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)
       IF (KPRT == 1) CALL WRITE_TD_MISFIT (1)
     END IF

     IF (LC == 2 .OR. LC == 12 .OR. LC == 23 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! Y, V or N component
       CASE (0)
         WRITE(NW,22) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,25) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,28) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NCHNL,JR) = BFTL(1:NCHNL,JR,2,JL)
       END DO
       CALL WRTDP (NW,PRFL,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)
       IF (KPRT == 1) CALL WRITE_TD_MISFIT (2)
     END IF

   ELSE                                  ! Coincident loop or electric dipole output
     IF (SURVEY_TYPE == 4) WRITE(NW,30) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
     IF (RX_TYPE(JL) == 2)  WRITE(NW,31) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
     DO JR = 1,NRX1
       YTR(1:NCHNL,JR) = BFTL(1:NCHNL,JR,1,JL)
       WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), YTR(1:NCHNL,JR)
     END DO
     CALL WRTDP (NW,PRFL,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)
     IF (KPRT == 1) CALL WRITE_TD_MISFIT (1)
   END IF
 END DO

  1 FORMAT(/T3,'Time-domain ',A,' for transmitter(s) with independent receiver lines.')
  2 FORMAT(/T3,'Time-domain ',A,' for moving rectangular loop Tx with fixed offset receiver(s).')
  3 FORMAT(/T3,'Time-domain ',A,' for moving dipole Tx with fixed offset receiver(s).')
  4 FORMAT(/T3,'Time-domain ',A,' for rectangular coincident loop survey.')
  5 FORMAT(/T3,'Time-domain ',A,' for downhole Tx-Rx probe.')
  6 FORMAT( T3,'Channels are ordered from earliest time to latest time.')
  7 FORMAT(/T3,'UTEM Survey:  Channels are ordered from latest time to earliest time.' &
           /T3,'Channel 1 response is subtracted from the other channels.')
 10 FORMAT(/T3,'Line ',A,4X,'Survey azimuth =',I4,' degrees',4X,'Units = ',A,4X,'Plot point: ',A)
 11 FORMAT(/T3,'Line ',A,4X,'Magnetic dipole Rx    Survey azimuth =',I4,' degrees',4X,'Units = ',A,4X,'Plot point: ',A)
 12 FORMAT(/T3,'Line ',A,4X,'Electric dipole Rx',4X,'Units = ',A,4X,'Plot point: ',A)
 15 FORMAT(I5,2F12.1,F9.1,150G13.4)
 16 FORMAT(/T3,'Line ',A,4X,'HID:',I4,4X,'SVAZ:',I4,4X,'Units: ',A,4X,'PP: ',A)
 21 FORMAT(/T10,'X : Radial Component ',A,' for Line ',A &
           /T10,'--------------------------------------------')

 22 FORMAT(/T10,'Y : Tangential Component ',A,' for Line ',A &
           /T10,'------------------------------------------------')
 23 FORMAT(/T10,'Z : Vertical Component ',A,' for Line ',A &
           /T10,'----------------------------------------------')
 24 FORMAT(/T10,'U : Slope Component ',A,' for Line ',A &
           /T10,'-----------------------------------------------')
 25 FORMAT(/T10,'V : Horizontal Component ',A,' for Line ',A &
           /T10,'------------------------------------------------')
 26 FORMAT(/T10,'A : Axial Component ',A,' for Line ',A &
           /T10,'-------------------------------------------')
 27 FORMAT(/T10,'S : In-section Component ',A,' for Line ',A &
           /T10,'------------------------------------------------')
 28 FORMAT(/T10,'N : Out-section Component ',A,' for Line ',A &
           /T10,'-------------------------------------------------')
 29 FORMAT(/T10,'W : Axial Component ',A,' for Line ',A &
           /T10,'------------------------------------------')
 30 FORMAT(/T10,'Coincident Loop ',A,' for Line ',A &
           /T10,'--------------------------------------')
 31 FORMAT(/T10,'Electric Dipole ',A,' for Line ',A &
           /T10,'--------------------------------------')

 CONTAINS

   SUBROUTINE WRITE_TD_MISFIT (JC)
!  ------------------------------

   IMPLICIT NONE
   INTEGER JC
   REAL DENOM,VM,VD

   DO JR = 1,NRX1
     DO JT = 1,NCHNL
       YTR(JT,JR) = 0.
       VD = RDATA(JT,JR,JC,JL)
       VM = BFTL(JT,JR,JC,JL)
       DENOM = SQRT( (VM*VM + VD*VD)/2.0)
       IF (DENOM > 0 .AND. RWTS(JT,JR,JC,JL) > 0 ) YTR(JT,JR) = 100. * (VD - VM) / DENOM
     END DO
   END DO

   WRITE(NW,1) JC
   CALL WRTDP (NW,PRFL,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)

 1 FORMAT(/T10,'Percent Misfit for Component',I2/T10,'----------------------')

   END SUBROUTINE WRITE_TD_MISFIT

 END SUBROUTINE WRITE_TD

 SUBROUTINE WRTDP (NW,PRFL,NCHNL,TMS,MRXL,NRX1,RXPLT,YTR)
!--------------------------------------------------------

!***  Called by: WRITE_TD
!***      CallS: nil

!  Writes time-domain output in profile form.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 INTEGER NW,PRFL,NCHNL,MRXL,NRX1,JR,JT
 REAL TMS(NCHNL),YTR(NCHNL,MRXL)
 REAL(KIND=QL) RXPLT(3,MRXL)

 CHARACTER(LEN=8) CHN(150)
 DATA CHN(1:150) &
   /' CHNL 1 ',' CHNL 2 ',' CHNL 3 ',' CHNL 4 ',' CHNL 5 ',' CHNL 6 ', &
    ' CHNL 7 ',' CHNL 8 ',' CHNL 9 ','CHNL 10 ','CHNL 11 ','CHNL 12 ', &
    'CHNL 13 ','CHNL 14 ','CHNL 15 ','CHNL 16 ','CHNL 17 ','CHNL 18 ', &
    'CHNL 19 ','CHNL 20 ','CHNL 21 ','CHNL 22 ','CHNL 23 ','CHNL 24 ', &
    'CHNL 25 ','CHNL 26 ','CHNL 27 ','CHNL 28 ','CHNL 29 ','CHNL 30 ', &
    'CHNL 31 ','CHNL 32 ','CHNL 33 ','CHNL 34 ','CHNL 35 ','CHNL 36 ', &
    'CHNL 37 ','CHNL 38 ','CHNL 39 ','CHNL 40 ','CHNL 41 ','CHNL 42 ', &
    'CHNL 43 ','CHNL 44 ','CHNL 45 ','CHNL 46 ','CHNL 47 ','CHNL 48 ', &
    'CHNL 49 ','CHNL 50 ','CHNL 51 ','CHNL 52 ','CHNL 53 ','CHNL 54 ', &
    'CHNL 55 ','CHNL 56 ','CHNL 57 ','CHNL 58 ','CHNL 59 ','CHNL 60 ', &
    'CHNL 61 ','CHNL 62 ','CHNL 63 ','CHNL 64 ','CHNL 65 ','CHNL 66 ', &
    'CHNL 67 ','CHNL 68 ','CHNL 69 ','CHNL 70 ','CHNL 71 ','CHNL 72 ', &
    'CHNL 73 ','CHNL 74 ','CHNL 75 ','CHNL 76 ','CHNL 77 ','CHNL 78 ', &
    'CHNL 79 ','CHNL 80 ','CHNL 81 ','CHNL 82 ','CHNL 83 ','CHNL 84 ', &
    'CHNL 85 ','CHNL 86 ','CHNL 87 ','CHNL 88 ','CHNL 89 ','CHNL 90 ', &
    'CHNL 91 ','CHNL 92 ','CHNL 93 ','CHNL 94 ','CHNL 95 ','CHNL 96 ', &
    'CHNL 97 ','CHNL 98 ','CHNL 99 ','CHNL 100','CHNL 101','CHNL 102', &
    'CHNL 103','CHNL 104','CHNL 105','CHNL 106','CHNL 107','CHNL 108', &
    'CHNL 109','CHNL 110','CHNL 111','CHNL 112','CHNL 113','CHNL 114', &
    'CHNL 115','CHNL 116','CHNL 117','CHNL 118','CHNL 119','CHNL 120', &
    'CHNL 121','CHNL 122','CHNL 123','CHNL 124','CHNL 125','CHNL 126', &
    'CHNL 127','CHNL 128','CHNL 129','CHNL 130','CHNL 131','CHNL 132', &
    'CHNL 133','CHNL 134','CHNL 135','CHNL 136','CHNL 137','CHNL 138', &
    'CHNL 139','CHNL 140','CHNL 141','CHNL 142','CHNL 143','CHNL 144', &
    'CHNL 145','CHNL 146','CHNL 147','CHNL 148','CHNL 149','CHNL 150'/

 IF (PRFL == 0) THEN  ! Forget profiles, write output in temporal form.
   WRITE(NW,4)
   WRITE(NW,5) RXPLT(1,1:NRX1)
   WRITE(NW,6) RXPLT(2,1:NRX1)
   WRITE(NW,7) RXPLT(3,1:NRX1)
   WRITE(NW,'(3X)')

   DO JT = 1,NCHNL
     WRITE(NW,'(I4,F10.3,T18,35G13.4)') JT,TMS(JT),YTR(JT,1:NRX1)
   END DO
 ELSE

   WRITE(NW,1) CHN(1:NCHNL)
   WRITE(NW,2) TMS(1:NCHNL)
   WRITE(NW,'(3X)')
   DO JR = 1, NRX1
     WRITE(NW,3) JR,RXPLT(1:3,JR),YTR(1:NCHNL,JR)
   END DO
 END IF

 1 FORMAT(/T11,'RECEIVER POSITIONS',6X,35(:5X,A))
 2 FORMAT(T9,'Easting    Northing     Elev',T37,35G13.4)
 3 FORMAT(I3,2F12.1,F9.1,35G13.4)
 4 FORMAT(/T20,'RECEIVER COORDINATES (Top to Bottom): Easting, Northing, Elevation')
 5 FORMAT(/T2,'       Window',T16,35F13.2)
 6 FORMAT( T2,'      Centres',T16,35F13.2)
 7 FORMAT( T2,'Chnl   (ms)  ',T16,35F13.2)

 END SUBROUTINE WRTDP

 SUBROUTINE WRITE_FD (NW,NW1,KPRT,NFRQ,MCHNL,NLINES,MRXL,MCMP,NRX,SURVEY_TYPE,LINE,IDH,RX_TYPE,UNITS, &
                      SVAZM,TITLE,ISYS,PRFL,IPLT,YXZPLT,FREQ,HEADER_ID,CMP,BFTL,RDATA,RWTS)
!--------------------------------------------------------------------------------------------------

!  Writes frequency-domain output to unit NW
!  If KPRT = 1, write error structure.
!
!*** Called by: MAIN
!***     Calls:
!
!   NW, NW1         : output unit numbers for .out & .mf1 files
!   KPRT            = 0 => write model data only
!                   = 1 write model data plus error structurete
!                   = -1 write inversion data only
!   NFRQ            : number of frequencies
!   MCHNL           = NFRQ for Sampo;  = 2*NFRQ otherwise
!   NLINES          : number of lines
!   MRXL            : maximum number of receivers per line
!   MCMP            = 1 for Sampo;  = 3 otherwise
!   NRX             : number of receivers per line
!   SURVEY_TYPE     = 1: general transmitter with independent receiver arrays
!                   = 2: moving rectangular loop with one or more fixed offset MD receivers
!                   = 3: fixed offset MD - MD surface surface
!                   = 4: coincident Loop
!                   = 5: downhole MD-MD probe
!   LINE(L)         : line number of Line L
!   UNITS(L)        : unit designater of Line L
!   IDH(L)          : surface (0);  local U,V,A (1);  modified U,V,A (2)
!   RXID(L)         : receiver type Line L - mag dipole (1); electric dipole (2)
!   SVAZM(L)        : survey aximuth for Line L (radians)
!   PRFL            : 1 for profile output; = 0 for frequency output
!   SVAZM           : survey azimuth (radians) of Line L
!   ISYS            : Sampo output if ISYS = 2
!   IPLT(J,L)       : the response of Rx J of Line L is plotted at 1:Rx;  2:Tx-Rx midpoint;  3:Tx midpoint
!   YXZPLT(1:3,I,L) : GPS east, GPS north, RL  plot coordinate for Ith receiver of Line L
!   FREQ            : frequency array
!   HEADER_ID       : used to relate header to RX_TYPE and plot orientation
!   CMP             : component selection
!
!   BFTL(I,J,K,L)   : the Kth component measured response at frequency I from receiver J,
!                     of Line L, transmitter K (modelled)
!   RDATA(I,J,K,L)  : the Kth component measured response at frequency I from receiver J,
!                     of Line L, transmitter K (inversion data)
!   RWTS(I,J,K,L)   : inversion weights corresponding to RDATA

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 REAL, PARAMETER :: R2D= 180./3.14159
 INTEGER NW,NW1,IDT,MCHNL,MCMP,KPRT,NLINES,MRXL,NFRQ,PRFL,SURVEY_TYPE,ISYS,KDEG,NRX1,JL,JR,IPPM,LC
 INTEGER, DIMENSION(NLINES) :: NRX,CMP,LINE,UNITS,RX_TYPE,HEADER_ID,IDH,IPLT
 INTEGER RWTS(MCHNL,MRXL,MCMP,NLINES)
 REAL, DIMENSION(MCHNL,MRXL,MCMP,NLINES) :: BFTL,RDATA
 REAL SVAZM(NLINES),FREQ(NFRQ),YTR(NFRQ,MRXL)
 REAL(KIND=QL) YXZPLT(3,MRXL,NLINES),RXPLT(3,MRXL)
 LOGICAL SPIT
 CHARACTER(LEN=120) TITLE
 CHARACTER(LEN=20) UTXT(2),QL0,CTXT(2),PLT_PT(3),DTYPE(4)
 DATA PLT_PT /'Rx','Tx-Rx_MID','Tx'/
 DATA DTYPE /'model output','data','Model Output','Data'/

 IDT = 1
 IF (KPRT == -1) THEN
   IDT = 2
   BFTL = RDATA
 END IF
 IF (KPRT == 0) WRITE(NW,'(/T3,A)') TRIM( ADJUSTL (TITLE))
 IF (ISYS == 2) THEN
   WRITE(NW,7)
 ELSE
   IF (SURVEY_TYPE == 1) WRITE(NW,1) TRIM (ADJUSTL (DTYPE(IDT)))
   IF (SURVEY_TYPE == 2) WRITE(NW,2) TRIM (ADJUSTL (DTYPE(IDT)))
   IF (SURVEY_TYPE == 3) WRITE(NW,3) TRIM (ADJUSTL (DTYPE(IDT)))
   IF (SURVEY_TYPE == 5) WRITE(NW,5) TRIM (ADJUSTL (DTYPE(IDT)))
 END IF

 IDT = 3
 IF (KPRT == -1) IDT = 4

 DO JL = 1,NLINES
   LC = CMP(JL)
   IPPM = 0
   IF (UNITS(JL) > 30 .AND. UNITS(JL) < 39) IPPM = 1  ! Format control
   IF (ISYS == 2) IPPM = 0
   CALL GET_UNITS_TEXT (UNITS(JL),UTXT)
   WRITE(QL0,*) LINE(JL)
   READ(QL0,'(A)') CTXT(1)
   CTXT(2) = PLT_PT(IPLT(JL))
   KDEG = INT (R2D * SVAZM(JL))
   WRITE(NW1,17) TRIM (ADJUSTL (CTXT(1))),HEADER_ID(JL),KDEG,TRIM(UTXT(2)),TRIM (ADJUSTL (CTXT(2)))

   IF (RX_TYPE(JL) == 2) THEN
     WRITE(NW,12) TRIM (ADJUSTL (CTXT(1))),TRIM(UTXT(1))
   ELSE IF (RX_TYPE(JL) == 3) THEN
     WRITE(NW,13) TRIM (ADJUSTL (CTXT(1))),TRIM(UTXT(1))
   ELSE
     IF (SURVEY_TYPE == 1) THEN
       WRITE(NW,11) TRIM (ADJUSTL (CTXT(1))),KDEG,TRIM(UTXT(1))
     ELSE
       WRITE(NW,10) TRIM (ADJUSTL (CTXT(1))),KDEG,TRIM(UTXT(1))
     END IF
   END IF

   NRX1 = NRX(JL)
   DO JR = 1,NRX1
     RXPLT(1:3,JR) = YXZPLT(1:3,JR,JL)
   END DO

   SPIT = .FALSE.
   IF (RX_TYPE(JL) == 1 .AND. ISYS < 2) SPIT = .TRUE.
   IF (RX_TYPE(JL) == 3) SPIT = .TRUE.
   IF (SPIT) THEN
     DO JR = 1,NRX1
       IF (IPPM == 1) THEN
         SELECT CASE (CMP(JL))
         CASE(1)
           WRITE(NW1,16) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,1,JL)
         CASE(2)
           WRITE(NW1,16) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,2,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,2,JL)
         CASE(3)
           WRITE(NW1,16) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,3,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,3,JL)
         CASE(12)
           WRITE(NW1,16) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,1,JL), &
                                               BFTL(1:NFRQ,JR,2,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,2,JL)
         CASE(13)
           WRITE(NW1,16) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,1,JL), &
                                               BFTL(1:NFRQ,JR,3,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,3,JL)
         CASE(23)
           WRITE(NW1,16) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,2,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,2,JL), &
                                               BFTL(1:NFRQ,JR,3,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,3,JL)
         CASE(123)
           WRITE(NW1,16) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,1,JL), &
                                               BFTL(1:NFRQ,JR,2,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,2,JL), &
                                               BFTL(1:NFRQ,JR,3,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,3,JL)
         END SELECT
       ELSE
         SELECT CASE (CMP(JL))
         CASE(1)
           WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,1,JL)
         CASE(2)
           WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,2,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,2,JL)
         CASE(3)
           WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,3,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,3,JL)
         CASE(12)
           WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,1,JL), &
                                               BFTL(1:NFRQ,JR,2,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,2,JL)
         CASE(13)
           WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,1,JL), &
                                               BFTL(1:NFRQ,JR,3,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,3,JL)
         CASE(23)
           WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,2,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,2,JL), &
                                               BFTL(1:NFRQ,JR,3,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,3,JL)
         CASE(123)
           WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,1,JL), &
                                               BFTL(1:NFRQ,JR,2,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,2,JL), &
                                               BFTL(1:NFRQ,JR,3,JL), BFTL(NFRQ+1 : 2*NFRQ,JR,3,JL)
         END SELECT
       END IF
     END DO
   END IF

   IF (RX_TYPE(JL) == 1 .AND. ISYS < 2) THEN            ! 3 component magnetic dipole output
     IF (LC == 3 .OR. LC == 13 .OR. LC == 23 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! Z, A or W Inphase component
       CASE (0)
         WRITE(NW,23) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,26) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,29) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(1:NFRQ,JR,3,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (3,1)

       SELECT CASE (IDH(JL))               ! Z, A or W Quadrature component
       CASE (0)
         WRITE(NW,33) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,36) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,39) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(NFRQ+1:2*NFRQ,JR,3,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (3,2)
     END IF

     IF (LC == 1 .OR. LC == 13 .OR. LC == 12 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! X, U or S Inphase component
       CASE (0)
         WRITE(NW,21) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,24) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,27) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(1:NFRQ,JR,1,JL)
       END DO
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (1,1)
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)

       SELECT CASE (IDH(JL))               ! X, U or S Quadrature component
       CASE (0)
         WRITE(NW,31) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,34) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,37) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(NFRQ+1:2*NFRQ,JR,1,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (1,2)
     END IF

     IF (LC == 2 .OR. LC == 12 .OR. LC == 23 .OR. LC == 123) THEN
       SELECT CASE (IDH(JL))               ! Y, V or N Inphase component
       CASE (0)
         WRITE(NW,22) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,25) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,28) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(1:NFRQ,JR,2,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (2,1)

       SELECT CASE (IDH(JL))               ! Y, V or N Quadrature component
       CASE (0)
         WRITE(NW,32) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (1)
         WRITE(NW,35) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       CASE (2)
         WRITE(NW,38) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       END SELECT
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(NFRQ+1:2*NFRQ,JR,2,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (2,2)
     END IF

   ELSE IF (RX_TYPE(JL) == 2) THEN                          ! electric dipole output
     DO JR = 1,NRX1
       WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL), BFTL(NFRQ+1:2*NFRQ,JR,1,JL)
     END DO

     WRITE(NW,20) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
     DO JR = 1,NRX1
       YTR(1:NFRQ,JR) = BFTL(1:NFRQ,JR,1,JL)
     END DO
     CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
     IF (KPRT == 1) CALL WRITE_FD_MISFIT (1,1)
     WRITE(NW,30) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
     DO JR = 1,NRX1
       YTR(1:NFRQ,JR) = BFTL(NFRQ+1:2*NFRQ,JR,1,JL)
     END DO
     CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (1,2)

   ELSE IF (RX_TYPE(JL) == 3) THEN                          ! point electric field
     IF (LC == 1 .OR. LC == 13 .OR. LC == 12 .OR. LC == 123) THEN
       WRITE(NW,41) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(1:NFRQ,JR,1,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (1,1)

       WRITE(NW,44) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(NFRQ+1:2*NFRQ,JR,1,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (1,2)
     END IF

     IF (LC == 2 .OR. LC == 12 .OR. LC == 23 .OR. LC == 123) THEN
       WRITE(NW,42) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(1:NFRQ,JR,2,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (2,1)

       WRITE(NW,45) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(NFRQ+1:2*NFRQ,JR,2,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (2,2)
     END IF

     IF (LC == 3 .OR. LC == 13 .OR. LC == 23 .OR. LC == 123) THEN
       WRITE(NW,43) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(1:NFRQ,JR,3,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (3,1)

       WRITE(NW,46) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
       DO JR = 1,NRX1
         YTR(1:NFRQ,JR) = BFTL(NFRQ+1:2*NFRQ,JR,3,JL)
       END DO
       CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
       IF (KPRT == 1) CALL WRITE_FD_MISFIT (3,2)
     END IF

   ELSE IF (ISYS == 2) THEN                                 ! Sampo output
     DO JR = 1,NRX1
       WRITE(NW1,15) JR,YXZPLT(1:3,JR,JL), BFTL(1:NFRQ,JR,1,JL)
     END DO

     WRITE(NW,40) TRIM (ADJUSTL (DTYPE(IDT))), TRIM (ADJUSTL (CTXT(1)))
     DO JR = 1,NRX1
       YTR(1:NFRQ,JR) = BFTL(1:NFRQ,JR,1,JL)
     END DO
     CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
     IF (KPRT == 1) CALL WRITE_FD_MISFIT (1,3)

   END IF
 END DO

  1 FORMAT(/T3,'Frequency-domain ',A,' for transmitter(s) with independent receiver lines.')
  2 FORMAT(/T3,'Frequency-domain ',A,' for moving rectangular loop Tx with fixed offset receiver(s).')
  3 FORMAT(/T3,'Frequency-domain ',A,' for moving dipole Tx with fixed offset receiver(s).')
  5 FORMAT(/T3,'Frequency-domain ',A,' for downhole Tx-Rx probe.')
  7 FORMAT(/T3,'Sampo ',A,' is the ratio ABS (Bz / Br).')
 10 FORMAT(/T3,'Line ',A,4X,'Survey azimuth =',I4,' degrees',4X,'Units = ',A)
 11 FORMAT(/T3,'Line ',A,4X,'Magnetic dipole Rx    Survey azimuth =',I4,' degrees',4X,'Units = ',A)
 12 FORMAT(/T3,'Line ',A,4X,'Electric dipole Rx',4X,'Units = ',A)
 13 FORMAT(/T3,'Line ',A,4X,'Point electric field',4X,'Units = ',A)
 15 FORMAT(I5,2F12.1,F9.1,540G13.4)
 16 FORMAT(I5,2F12.1,F9.1,540F13.2)
 17 FORMAT(/T3,'Line ',A,4X,'HID:',I4,4X,'SVAZ:',I4,4X,'Units: ',A,4X,'PP: ',A)
 20 FORMAT(//T3,'Inphase ',A,' for Line ',A &
            /T3,'-------------------------------')
 21 FORMAT(//T10,'X Inphase - Radial Component ',A,' for Line ',A &
            /T10,'----------------------------------------------------')
 22 FORMAT(//T10,'Y Inphase - Tangential Component ',A,' for Line ',A &
            /T10,'--------------------------------------------------------')
 23 FORMAT(//T10,'Z Inphase - Vertical Component ',A,' for Line ',A &
            /T10,'------------------------------------------------------')
 24 FORMAT(//T10,'U Inphase - Slope Component ',A,' for Line ',A &
            /T10,'---------------------------------------------------')
 25 FORMAT(//T10,'V Inphase - Horizontal Component ',A,' for Line ',A &
            /T10,'--------------------------------------------------------')
 26 FORMAT(//T10,'A Inphase - Axial Component ',A,' for Line ',A &
            /T10,'-------------------------------------------------------')
 27 FORMAT(//T10,'S Inphase - In-section Component ',A,' for Line ',A &
            /T10,'--------------------------------------------------------')
 28 FORMAT(//T10,'N Inphase - Out-section Component ',A,' for Line ',A &
            /T10,'---------------------------------------------------------')
 29 FORMAT(//T10,'W Inphase - Axial Component ',A,' for Line ',A &
            /T10,'---------------------------------------------------')
 30 FORMAT(//T3,'Quadrature ',A,' for Line ',A &
            /T3,'----------------------------------')
 31 FORMAT(/T10,'X Quadrature - Radial Component ',A,' for Line ',A &
           /T10,'-------------------------------------------------------')
 32 FORMAT(//T10,'Y Quadrature - Tangential Component ',A,' for Line ',A &
           /T10,'------------------------------------------------------------')
 33 FORMAT(//T10,'Z Quadrature - Vertical Component ',A,' for Line ',A &
            /T10,'---------------------------------------------------------')
 34 FORMAT(//T10,'U Quadrature - Slope Component ',A,' for Line ',A &
            /T10,'------------------------------------------------------')
 35 FORMAT(//T10,'V Quadrature - Horizontal Component ',A,' for Line ',A &
            /T10,'-----------------------------------------------------------')
 36 FORMAT(//T10,'A Quadrature - Axial Component ',A,' for Line ',A &
            /T10,'------------------------------------------------------')
 37 FORMAT(//T10,'S Quadrature - In-section Component ',A,' for Line ',A &
            /T10,'-----------------------------------------------------------')
 38 FORMAT(//T10,'N Quadrature - Out-section Component ',A,' for Line ',A &
            /T10,'------------------------------------------------------------')
 39 FORMAT(//T10,'W Quadrature - Axial Component ',A,' for Line ',A &
            /T10,'------------------------------------------------------')
 40 FORMAT(//T3,'Abs (Bz / Bx Ratio  ',A,' for Line ',A &
            /T3,'-------------------------------------------')
 41 FORMAT(//T10,'X Inphase - Component ',A,' for Line ',A &
            /T10,'---------------------------------------------')
 42 FORMAT(//T10,'Y Inphase - Component ',A,' for Line ',A &
            /T10,'---------------------------------------------')
 43 FORMAT(//T10,'Z Inphase - Component ',A,' for Line ',A &
            /T10,'---------------------------------------------')
 44 FORMAT(//T10,'X Quadrature - Component ',A,' for Line ',A &
            /T10,'------------------------------------------------')
 45 FORMAT(//T10,'Y Quadrature - Component ',A,' for Line ',A &
            /T10,'------------------------------------------------')
 46 FORMAT(//T10,'Z Quadrature - Component ',A,' for Line ',A &
            /T10,'------------------------------------------------')

 CONTAINS

   SUBROUTINE WRITE_FD_MISFIT (JC,KC)
!  ----------------------------------

   IMPLICIT NONE
   INTEGER JF,JC,KC,J1,J2
   REAL DENOM,VM,VD

   J1 = 1;  J2 = NFRQ
   IF (KC == 2) THEN
     J1 = NFRQ + 1
     J2 = 2 * NFRQ
   END IF
   DO JR = 1,NRX1
     DO JF = J1,J2
       YTR(JF,JR) = 0.
       VD = RDATA(JF,JR,JC,JL)
       VM = BFTL(JF,JR,JC,JL)
       DENOM = SQRT( (VM*VM + VD*VD)/2.0)
       IF (DENOM > 0 .AND. RWTS(JF,JR,JC,JL) > 0 ) YTR(JF,JR) = 100. * (VD - VM) / DENOM
     END DO
   END DO

   IF (KC == 1) WRITE(NW,1) JC
   IF (KC == 2) WRITE(NW,2) JC
   IF (KC == 3) WRITE(NW,3)
   CALL WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)

 1 FORMAT(/T10,'Percent Inphase Misfit for Component',I2 &
          /T10,'--------------------------------------')
 2 FORMAT(/T10,'Percent Quadrature Misfit for Component',I2 &
          /T10,'------==---------------------------------')
 3 FORMAT(/T10,'Percent Sampo Misfit' &
          /T10,'--------------------')

   END SUBROUTINE WRITE_FD_MISFIT

 END SUBROUTINE WRITE_FD

 SUBROUTINE WRFDP (NW,PRFL,NFRQ,FREQ,MRXL,NRX1,RXPLT,IPPM,YTR)
!------------------------------------------------------------

!***  Called by WRSLV

!  Writes frequency-domain output in profile form.  IPPM = format control

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 INTEGER NW,PRFL,IPPM,NFRQ,MRXL,NRX1,JR,JF
 REAL FREQ(NFRQ),YTR(NFRQ,NRX1)
 REAL(KIND=QL) RXPLT(3,MRXL)

 IF (PRFL == 0) THEN  ! Forget profiles, write output in spectral form.
   WRITE(NW,4)
   WRITE(NW,5) RXPLT(1,1:NRX1)
   WRITE(NW,6) RXPLT(2,1:NRX1)
   WRITE(NW,7) RXPLT(3,1:NRX1)
   WRITE(NW,'(3X)')

   DO JF = 1,NFRQ
     WRITE(NW,'(I4,G12.4,T18,35G13.4)') JF,FREQ(JF),YTR(JF,1:NRX1)
   END DO

 ELSE
   WRITE(NW,1)
   IF (IPPM == 1) THEN
     WRITE(NW,9) FREQ(1:NFRQ)
     WRITE(NW,'(3X)')
     DO JR = 1, NRX1
       WRITE(NW,8) JR,RXPLT(1:3,JR),YTR(1:NFRQ,JR)
     END DO
   ELSE
     WRITE(NW,2) FREQ(1:NFRQ)
     WRITE(NW,'(3X)')
     DO JR = 1, NRX1
       WRITE(NW,3) JR,RXPLT(1:3,JR),YTR(1:NFRQ,JR)
     END DO
   END IF
 END IF

  1 FORMAT(/T14,'RECEIVER POSITIONS',T47,'FREQUENCIES')
  2 FORMAT(T10,'East        North     Elev',F10.2,100F13.2)
  3 FORMAT(I3,2F12.1,F9.1,300G13.4)
  4 FORMAT(/T20,'RECEIVER COORDINATES (Top to Bottom): Easting, Northing, Elevation')
  5 FORMAT(/T16,35F13.2)
  6 FORMAT(T16,35F13.2)
  7 FORMAT( T6,'Frequency',T16,35F13.4)
  8 FORMAT(I3,2F12.1,F9.1,300F12.1)
  9 FORMAT(T10,'East        North     Elev ',300F12.1)

 END SUBROUTINE WRFDP

  SUBROUTINE WRITE_LOG_FILE (NLG,MSG,MXERR,ERR_LVL)
! -------------------------------------------------

! This subroutine prints out warning and fatal error messages on the LOG file.
!
! NLG = output unit index
! MSG refers to error message index
! ERR_LVL = 1 for warnings;  = 2 for fatal errors
! MXERR = MAX (ERR_LVL)

 INTEGER ERR_LVL, MSG, NLG, MXERR

 IF (MXERR == 0) OPEN (NLG,FILE = 'Leroi.log',STATUS = 'REPLACE')

 MXERR = MAX (ERR_LVL,MXERR)
 IF (ERR_LVL == 1) WRITE(NLG,501)
 IF (ERR_LVL == 2) WRITE(NLG,502)

 IF (MSG == 1) WRITE(NLG,1)
 IF (MSG == 2) WRITE(NLG,2)
 IF (MSG == 3) WRITE(NLG,3)
 IF (MSG == 4) WRITE(NLG,4)
 IF (MSG == 5) WRITE(NLG,5)
 IF (MSG == 6) WRITE(NLG,6)
 IF (MSG == 7) WRITE(NLG,7)
 IF (MSG == 8) WRITE(NLG,8)
 IF (MSG == 9) WRITE(NLG,9)
 IF (MSG == 10) WRITE(NLG,10)
 IF (MSG == 11) WRITE(NLG,11)
 IF (MSG == 12) WRITE(NLG,12)
 IF (MSG == 13) WRITE(NLG,13)
 IF (MSG == 14) WRITE(NLG,14)
 IF (MSG == 15) WRITE(NLG,15)
 IF (MSG == 16) WRITE(NLG,16)
 IF (MSG == 17) WRITE(NLG,17)
 IF (MSG == 18) WRITE(NLG,18)
 IF (MSG == 19) WRITE(NLG,19)
 IF (MSG == 20) WRITE(NLG,20)
 IF (MSG == 21) WRITE(NLG,21)
 IF (MSG == 22) WRITE(NLG,22)
 IF (MSG == 23) WRITE(NLG,23)
 IF (MSG == 24) WRITE(NLG,24)
 IF (MSG == 25) WRITE(NLG,25)
 IF (MSG == 26) WRITE(NLG,26)
 IF (MSG == 27) WRITE(NLG,27)
 IF (MSG == 28) WRITE(NLG,28)
 IF (MSG == 29) WRITE(NLG,29)
 IF (MSG == 30) WRITE(NLG,30)
 IF (MSG == 31) WRITE(NLG,31)
 IF (MSG == 32) WRITE(NLG,32)
 IF (MSG == 33) WRITE(NLG,33)
 IF (MSG == 35) WRITE(NLG,35)
 IF (MSG == 40) WRITE(NLG,40)
 IF (MSG == 50) WRITE(NLG,50)
 IF (MSG == 51) WRITE(NLG,51)
 IF (MSG == 52) WRITE(NLG,52)
 IF (MSG == 54) WRITE(NLG,54)
 IF (MSG == 55) WRITE(NLG,55)
 IF (MSG == 56) WRITE(NLG,56)
 IF (MSG == 58) WRITE(NLG,58)
 IF (MSG == 60) WRITE(NLG,60)
 IF (MSG == 61) WRITE(NLG,61)
 IF (MSG == 62) WRITE(NLG,62)
 IF (MSG == 63) WRITE(NLG,63)
 IF (MSG == 100) WRITE(NLG,100)
 IF (MSG == 90) THEN
   WRITE(NLG,90)
   STOP
 END IF
  1 FORMAT(/T3,'The value for TDFD is outside the permitted range.' &
           /T3,'The allowed values are: 1 or 0 for time-domain or 2 for frequency domain.')
  2 FORMAT(/T3,'The allowed values for PRFL are: 1 or 11 for profile mode, 0 or 10 otherwise.' &
           /T3,'PRFL has been set to 1.')
  3 FORMAT(/T3,'The value for DO3D is outside the permitted range of -1, 0, 1, 2 or 3.' &
           /T3,'DO3D has been reset to 1.  A new model will be computed.')
  4 FORMAT(/T3,'The value for STEP is outside the permitted range.' &
           /T3,'The allowed values are: 0 or 1.')
  5 FORMAT(/T3,'The value for KRXW is outside the permitted range.' &
           /T3,'The allowed values are: 1 or 2.')
  6 FORMAT(/T3,'Only magnetic dipole receivers areallowed with magnnetic dipole transmitters')
  7 FORMAT(/T3,'This value for TOPN is outside the permitted range.' &
           /T3,'It must be > 0.')
  8 FORMAT(/T3,'Entry for SOURCE_TYPE is restricted to the values: 1 (loop), 2 (grounded wire) or 3 (magnetic dipole).')
  9 FORMAT(/T3,'RX_TYPE must be 1 (magnetic dipole), 2 (electric dipole), or 3 (point E-field)')
 10 FORMAT(/T3,'SURVEY_TYPE is only allowed values: 1, 2, 3, 4 or 5.')
 11 FORMAT(/T3,'NTX > NLINES.  A line must be specified for every transmitter position.')
 12 FORMAT(/T3,'The number of receivers for this line, NRX, has been reduced to MRXL,' &
           /T3,'the maximum set earlier in this control file.')
 13 FORMAT(/T3,'Primary field for dB/dt output is not implemented')
 14 FORMAT(/T3,'The number of transmitter positions for this line, KTXP, has been' &
           /T3,'reduced to MRXL, the maximum set earlier in this control file.')
 15 FORMAT(/T3,'UNITS must = 31 (ratio), 32 (percent), 33 (ppt), 34 (ppm) or 35 (ppb)' &
           /T3,'if normalised response (KNORM > 0) is specified.')
 16 FORMAT(/T3,'For time-domain, ISYS must = 4 for UTEM or 0 otherwise')
 17 FORMAT(/T3,'For frequency-domain, ISYS must = 0 or 2 for Sampo')
 18 FORMAT(/T3,'For UTEM, the number of channels must be 10 or 20.')
 19 FORMAT(/T3,'The magnetic receiver index exceeds the number of magnetic receivers')
 20 FORMAT(/T3,'This version of Leroi does not include magnetotellurics.' &
           /T3,'It is for controlled sources only.')
 21 FORMAT(/T3,'This lithology index is invalid.' &
           /T3,'No resistivity or conductance has been specified.')
 22 FORMAT(/T3,'Layer lithology indices must be an integer between 1 & NLITH')
 23 FORMAT(/T3,'Layer resistivities must be positive.')
 24 FORMAT(/T3,'The value given to CMP is not allowed.' &
           /T3,'CMP values must be 1, 2, 3, 12, 13, 23, or 123')
 25 FORMAT(/T3,'NLYR = number of layers including basement.' &
           /T3,'Thus NLYR must be an integer > 0.')
 26 FORMAT(/T3,'Plate lithology indices must be an integer between 1 & NLITH')
 27 FORMAT(/T3,'Plate conductance must be positive.')
 28 FORMAT(/T3,'Each plate must be contained entirely within the basement unless DO3D = 3.' &
           /T3,'One or more plates have been shifted downwards to comply with this rule.')
 29 FORMAT(/T3,'LEROI requires 0 <= DIP < 180 degrees.')
 30 FORMAT(/T3,'The point E-field receiver is not allowed for time-domain modelling')
 31 FORMAT(/T3,'The number of magnetic receivers must not exceed the number of electric receivers.')
 32 FORMAT(/T3,'The number of receivers NRX on at least one line exceeds MRXL.')
 33 FORMAT(/T3,'Plunge has been set to zero.  Setting DO3D = 3 allows plates in layers above' &
           /T3,'basement but requires all plates. to have zero plunge to facilitate computational' &
           /T3,'efficiency.  This restriction can be obviated by the perspicacious user.')
 35 FORMAT(/T3,'Problem with HEADER_ID.  Seek help !')
 40 FORMAT(/T3,'The Sampo option is not defined for time-domain applications.')
 50 FORMAT(/T3,'The value for LINE_CHK does not match LINE(J)')
 51 FORMAT(/T3,'The value for NSTAT does not match NRX(J)')
 52 FORMAT(/T3,'The value given to KMP is not allowed.')
 54 FORMAT(/T3,'The component(s) specified for inversion in CMP' &
           /T3,'are not present in the data as specified by KMP')
 55 FORMAT(/T3,'Conflict between NKMP(J) and KMP(J).' &
           /T3,'There must be at least NKMP data components to invert.')
 56 FORMAT(/T3,'Conflict between NKMP(j) and CMP(J).')
 58 FORMAT(/T3,'FD_ORDER must = 0, 1, or 2')
 60 FORMAT(/T3,'LINE_CHK must = Line number(J)')
 61 FORMAT(/T3,'NSTAT must = NRX(J)')
 62 FORMAT(/T3,'The data for each receiver must be presented in the same order as specified' &
           /T3,'in the receiver specification in Leroi.inv')
 63 FORMAT(/T3,'CNVRG must = 1, 2, 10 or 20')
 90 FORMAT(//T3,'MESSAGE FROM SCAT_MTRX_LU_DCMP: THE MATRIX IS SINGULAR.', &
            /T3,'COMPUTATION HALTED.  SEEK HELP.')
 100 FORMAT(1X)
 501 FORMAT(/T2,'INFORMATION / WARNING'/T2,'---------------------'/)
 502 FORMAT(/T2,'FATAL ERROR'/T2,'----- -----')

 END SUBROUTINE  WRITE_LOG_FILE

!==========================================================================================
!*****************************************
!
!       LAYERED HALFSPACE ROUTINES
!       --------------------------
!*****************************************


   SUBROUTINE HSBOSS_TD (NFRQ,FREQ,STEP,NSX,SWX,SWY,NPULS,PULSE,NTYPLS,NTYRP,TRP,NCHNL,  &
                         TOPN,TCLS,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM, &
                         NRXTX,RXID,MRXTX,MQVR,XRXTX,YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD, &
                         RES,RMUD,REPS,CHRG,CTAU,CFREQ,NCTD,BTD)
!----------------------------------------------------------------------------------------

!  Computes BTD, the time-domain layered earth response convolved with the
!  excitation waveform and the receiver channels per unit receiver area.
!  For impulse response, it computes dB/dt in T / s which is the same as
!  volts per unit area.

!  For step response, it computes B in Teslas.

!***  Called by MAIN

!***  Calls HSBOSS

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive north, Y (JC=2) positive east
!  and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
!                             Input
!                             -----
!         STEP = 1 for normal step response is to be computed
!              = 4 for UTEM pure step response
!         STEP = 1 iff step response is to be computed
!          NSX - number of points used to discretise transmitter signal
!          SWX - abscissae (seconds) of current waveform
!          SWY - dI/dt * Tx moment & nanotesla conversion at times SWX
!        NPULS - number of bipolar pulses of length PULSE
!        PULSE - length for single on pulse plus off-time
!       NTYPLS - number of TRP values in 1 PULSE
!        NTYRP - number of values in TRP for total signal length: 2 * NPULS *PULSE
!          TRP - array of time values for FD -> TD transformations
!        NCHNL - number of channels
!         TOPN - time at which receiver channel I opens.
!         TCLS - time at which receiver channel I closes.
!  SOURCE_TYPE = 1 : general loop
!              = 2 : grounded wire
!              = 3 : magnetic dipole
!              = 4 : coincident loop
!          NTX - number of transmitter positions
!       MXVRTX - maximum number of vertices for any transmitter
!     NVRTX(J) - number of vertices for transmitter J
!     SXE(I,J) = local east coordinate of vertex I for loop position J
!     SXN(I,J) = local coordinate of vertex I for loop position J
!     SXZ(J)   = z positive down depth of mag dipole Tx J
!     SXDIP(J) = dip (in radians) of dipole J (eg; vertical = 0, horizontal = 90)
!      SXAZM(J) = azimuth (in radians) of dipole J (north = 0, east = 90)
!        MRXTX - maximum number of receivers per transmitter
!     NRXTX(I) - number of receivers for transmitter I
!    RXID(I,J) - RX_TYPE of receiver I for transmitter J
!         MQVR - maximum number of vertices for all receivers (= 1 if all sources are magnetic dipoles)
! XRXTX(J,I,K) - north coordinate of the Kth vertex of the Jth receiver of transmitter I
! YRXTX(J,I,K) - east coordinate of the Kth vertex of the Jth receiver of transmitter I
!   ZRXTX(J,I) - depth of the Jth receiver of transmitter I
!         NLYR - number of layers
!          RES - layer resistivities
!         RMUD - mu(i) / mu(0)
!         REPS - array of relative dislectric constants
!         THKD - layer thicknesses
!         CHRG - chargeability
!         CTAU - array of layer relaxation times (sec).
!        CFREQ - array of layer frequency parameters.
!    NCTD(I,J) = number of components for receiver I for transmitter J
!
!                             Output
!                             ------
!  BTD(JT,JR,JS,I) - the Ith component of the layered earth response at
!                    time JT, receiver JR, station JS.
!           I = 1,2,3 = north, east and vertical componsnts respectively

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 REAL, PARAMETER :: TWOPI=6.283185307
 INTEGER NFRQ,STEP,STEPC,NSX,NPULS,NTYPLS,NTYRP,NCHNL,SOURCE_TYPE,NTX,MXVRTX,MXRHO,MRXTX, &
         NVRTX(NTX),NRXTX(NTX),NCTD(MRXTX,NTX),JC,JT,JF,JR,JS,MQVR,NLYR,RXID(MRXTX,NTX)
 REAL FREQ(NFRQ),WF(NFRQ),YFRQ(4,NFRQ),YPRM(4,NTYRP),YCUM(NCHNL),SWX(NSX),PULSE, &
      SWY(NSX,3),TRP(NTYRP),T,RHOTRP(MXRHO),ZRXTX(MRXTX,NTX),BTD(NCHNL,MRXTX,NTX,3),COSTRN
 REAL, DIMENSION (NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION (NCHNL) :: TOPN,TCLS
 REAL, DIMENSION (NTX) :: SXDIP,SXAZM,SXZ
 REAL, DIMENSION (MXVRTX,NTX) :: SXN,SXE
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL(KIND=QL) THKD(NLYR),RMUD(0:NLYR)
 COMPLEX BFD(NFRQ,MRXTX,NTX,3)

 BTD = 0.
 WF(1:NFRQ) = ALOG (TWOPI * FREQ(1:NFRQ))

 CALL HSBOSS (NFRQ,FREQ,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM, &
              NRXTX,MRXTX,RXID,MQVR,XRXTX,YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD, &
              RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)
 DO JS = 1,NTX
   DO JR = 1, NRXTX(JS)
     STEPC = STEP
     IF (RXID(JR,JS) == 2) THEN
       STEPC = 3                      ! Electrode E field Transform INTEGRAL { E dl } to TD
     END IF
     DO JC = 1,NCTD(JR,JS)
       DO JF = 1,NFRQ     ! Divide by -iw to set up step response
         IF (STEPC == 3) THEN
           YFRQ(1,JF) = REAL ( BFD(JF,JR,JS,JC) )
         ELSE
           YFRQ(1,JF) = -AIMAG ( BFD(JF,JR,JS,JC) ) / (TWOPI * FREQ(JF))
         END IF
       END DO
       CALL CUBSPL (WF,YFRQ,NFRQ)

       YPRM = 0.
       DO JT = 1, NTYRP   !  Convert to step-function time-domain.
         T = TRP(JT)
         YPRM(1,JT) = COSTRN (WF,YFRQ,NFRQ,T)
       END DO

       CALL FOLD_AND_CONVOLVE (STEPC,NSX,SWX,SWY,NPULS,PULSE,TRP,NTYRP,NTYPLS, &
                               NCHNL,TOPN,TCLS,YPRM,YCUM)

       BTD(1:NCHNL,JR,JS,JC) = YCUM(1:NCHNL)
     END DO
   END DO
 END DO
 END SUBROUTINE HSBOSS_TD

 SUBROUTINE HSBOSS (NFRQ,FREQ,SOURCE_TYPE,NTX,MXVRTX,NVRTX,SXN,SXE,SXZ,SXDIP,SXAZM, &
                    NRXTX,MRXTX,RXID,MQVR,XRXTX,YRXTX,ZRXTX,MXRHO,RHOTRP,NLYR,THKD,  &
                    RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)
!---------------------------------------------------------------------------------------

!***  Called by MAIN
!***  Calls SXMDB

!  Computes BFD, the frequency-domain layered earth response.
!  Magnetic fields are in Teslas per unit amp.  Electrode response is in volts.
!
! For magnetic dipole receivers, BFD(JF,JR,JS,1:3) contains the
! 1: north, 2: east & 3:vertical components for frequency JF, transmitter JS, receiver JR.
!
! The response for electric dipoles and loop receivers is contained in BFD_SCAT(JF,JR,JS,1),
! BFD(JF,JR,JS,2:3) is set to zero.
!
!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive north, Y (JC=2) positive east
!  and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
!                             Input
!                             -----
!         NFRQ - number of frequencies
!         FREQ - array of frequencies
!  SOURCE_TYPE = 1 : general loop
!              = 2 : grounded wire
!              = 3 : magnetic dipole
!              = 4 : coincident loop
!          NTX - number of transmitter positions
!       MXVRTX - maximum number of vertices for any transmitter
!     NVRTX(J) - number of vertices for transmitter J
!     SXE(I,J) = local east coordinate of vertex I for loop position J
!     SXN(I,J) = local coordinate of vertex I for loop position J
!     SXZ(J)   = depth of Tx J
!     SXDIP(J) = dip (in radians) of dipole J (eg; vertical = 0, horizontal = 90)
!     SXAZM(J) = azimuth (in radians) of dipole J (north = 0, east = 90)
!     NRXTX(I) - number of receivers for transmitter I
!        MRXTX - maximum number of receivers per transmitter
!    RXID(I,J) - RX_TYPE of receiver I for transmitter J. 1 => MD; 2 => ED
!         MQVR - maximum number of vertices for all receivers (= 1 if all sources are magnetic dipoles)
! XRXTX(J,I,K) - north coordinate of the Kth vertex of the Jth receiver of transmitter I
! YRXTX(J,I,K) - east coordinate of the Kth vertex of the Jth receiver of transmitter I
!   ZRXTX(J,I) - depth of the Jth receiver of transmitter I
!              - K = 1 only for mag dipole Rx; 1 to 2 for electric dipole Rx; 1 to 4 for loop Rx
!         NLYR - number of layers
!          RES - layer resistivities
!         RMUD - mu(i) / mu(0)
!         REPS - array of relative dislectric constants
!         THKD - layer thicknesses
!         CHRG - chargeability array
!         CTAU - array of layer relaxation times (sec).
!        CFREQ - array of layer frequency parameters.
!
!                             Output
!                             ------
!  BFD(JF,JR,JS,I) - the Ith component of the layered earth response at
!                    frequency JF, receiver JR, station JS.
!           I = 1,2,3 = north, east and vertical componsnts respectively

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 INTEGER NFRQ,SOURCE_TYPE,NTX,MXVRTX,RXLYR,SXLYR,NVRTX(NTX),MRXTX,MXRHO,NRHS,JS,JR, &
         JZ,NRXTX(NTX),RXID(MRXTX,NTX),MQVR,NLYR,NVRL
 REAL FREQ(NFRQ),SXN1,SXE1,SXDP1,SXAZM1,XRX,YRX,RHOTRP(MXRHO),ZRXTX(MRXTX,NTX)
 REAL, DIMENSION (NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION (NTX) :: SXDIP,SXAZM,SXZ
 REAL, DIMENSION (MXVRTX,NTX) :: SXN,SXE
 REAL, DIMENSION (MXVRTX) :: SXNL,SXEL
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 COMPLEX BFD(NFRQ,MRXTX,NTX,3)
 REAL(KIND=QL) RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR),ZS,ZR
 LOGICAL MDRX,EDRX,EPRX

 CALL SET_NRHS

 BFD = (0.,0.)
 DPTHL = 0._QL
 DO JZ = 2, NLYR
   DPTHL(JZ) = DPTHL(JZ-1) + THKD(JZ-1)
 END DO

 SELECT CASE (SOURCE_TYPE)
 CASE (1:2)                 ! Closed and Open Loops
   MDRX = .FALSE.
   EDRX = .FALSE.
   EPRX = .FALSE.
   DO JS = 1,NTX
     ZS = REAL (SXZ(JS),QL)
     SXLYR = 0                ! Identify layer containing loop or GW source
     DO JZ = NLYR,1,-1
       IF (ZS > DPTHL(JZ)) THEN
         SXLYR = JZ
         EXIT
       END IF
     END DO
     NVRL = NVRTX(JS)
     SXNL(1:NVRL) = SXN(1:NVRL,JS)
     SXEL(1:NVRL) = SXE(1:NVRL,JS)

     DO JR = 1, NRXTX(JS)                           ! Compute response for
       IF (RXID(JR,JS) == 1) MDRX = .TRUE.          !    Magnetic dipole receivers
       IF (RXID(JR,JS) == 2) EDRX = .TRUE.          !    Electric dipole receivers
       IF (RXID(JR,JS) == 3) EPRX = .TRUE.          !    Electric field at a point
     END DO

     IF (MDRX) CALL HSLPB (NFRQ,FREQ,SOURCE_TYPE,NTX,JS,SXLYR,NVRL,SXNL,SXEL,ZS,MRXTX,NRXTX,MQVR,RXID,XRXTX, &
                           YRXTX,ZRXTX,NRHS,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)

     IF (EPRX) CALL HSLPE (NFRQ,FREQ,SOURCE_TYPE,NTX,JS,SXLYR,NVRL,SXNL,SXEL,ZS,MRXTX,NRXTX,MQVR,RXID,XRXTX, &
                           YRXTX,ZRXTX,NRHS,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)

     IF (EDRX) CALL HSLPED (NFRQ,FREQ,SOURCE_TYPE,NTX,JS,SXLYR,NVRL,SXNL,SXEL,ZS,MRXTX,NRXTX,MQVR,RXID,XRXTX, &
                            YRXTX,ZRXTX,NRHS,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)
   END DO

 CASE (3)                    ! Magnetic dipole sources
   DO JS = 1,NTX
     SXN1 = SXN(1,JS)
     SXE1 = SXE(1,JS)
     ZS = REAL (SXZ(JS),QL)
     SXDP1 = SXDIP(JS)
     SXAZM1 = SXAZM(JS)

     SXLYR = 0                ! Identify layer containing dipole source
     DO JZ = NLYR,1,-1
       IF (ZS > DPTHL(JZ)) THEN
         SXLYR = JZ
         EXIT
       END IF
     END DO

     DO JR = 1, NRXTX(JS)
       XRX = XRXTX(JR,JS,1) - SXN1
       YRX = YRXTX(JR,JS,1) - SXE1  ! Compute receiver offsets.
       ZR = REAL (ZRXTX(JR,JS),QL)
       RXLYR = 0                    ! Set layer number and reference depth for receiver JR
       DO JZ = NLYR,1,-1
         IF (ZR > DPTHL(JZ)) THEN
           RXLYR = JZ
           EXIT
         END IF
       END DO
       CALL HSMDB (SXLYR,RXLYR,NFRQ,FREQ,ZS,SXDP1,SXAZM1,ZR,XRX,YRX,NLYR,THKD,DPTHL, &
                   RES,RMUD,REPS,CHRG,CTAU,CFREQ,JS,JR,NTX,MRXTX,BFD)

     END DO                 ! End of receivers for transmitter JS
   END DO

 CASE (4)                     ! Coincident loop option
   DO JS = 1,NTX
     ZS = REAL (SXZ(JS),QL)
     SXLYR = 0                ! Identify layer containing dipole source
     DO JZ = NLYR,1,-1
       IF (ZS > DPTHL(JZ)) THEN
         SXLYR = JZ
         EXIT
       END IF
     END DO

     NVRL = 4
     SXNL(1:4) = SXN(1:4,JS)
     SXEL(1:4) = SXE(1:4,JS)
     CALL HS_CDNT (NFRQ,FREQ,SXNL,SXEL,NRHS,RHOTRP,NLYR,THKD,RES, &
                   RMUD,REPS,CHRG,CTAU,CFREQ,JS,NTX,BFD)
   END DO

 END SELECT

 CONTAINS

   SUBROUTINE SET_NRHS
!  -------------------

   IMPLICIT NONE
   INTEGER JV,JRV,NRVR
   REAL HSMX,R1

!  Dimension RHOMAX FOR halfspace computations.  First set
!  maximum distance between transmitters and receivers.

   HSMX = 0.
   DO JS = 1,NTX
     DO JV = 1,NVRTX(JS)
       IF (SOURCE_TYPE == 4) THEN
         DO JRV = JV+1, NVRTX(JS)
           R1 = (SXN(JV,JS) - SXN(JRV,JS))**2 + (SXE(JV,JS) - SXE(JRV,JS))**2
           HSMX = MAX (HSMX,R1)
         END DO
       ELSE
         DO JR = 1, NRXTX(JS)
           NRVR = 1
           IF (RXID(JR,JS) == 2) NRVR = 2
           DO JRV = 1, NRVR
             R1 = (XRXTX(JR,JS,JRV) - SXN(JV,JS))**2 + (YRXTX(JR,JS,JRV) - SXE(JV,JS))**2
             HSMX = MAX (R1, HSMX)
           END DO
         END DO
       END IF
     END DO
   END DO
   HSMX = SQRT (HSMX)

   NRHS = 1
   DO JR = 2, MXRHO
     IF (RHOTRP(JR) < HSMX) NRHS = JR + 1
   END DO

   END SUBROUTINE SET_NRHS

 END SUBROUTINE HSBOSS

 SUBROUTINE COLRES_1D (FRQ,NLYR,RES,REPS,RMUD,CHRG,CTAU,CFREQ,SIGL,KSQL)
!-----------------------------------------------------------------------

! Computes KSQL & SIGL for each layer for each frequency.

!          Input
!          -----
!     FRQ - frequency
!    NLYR - number of layers
!     RES - layer resistivities
!    REPS - array of relative dielectric constants
!    RMUD - mu(i) / mu(0)
!    CHRG - chargeability array
!    CTAU - array of layer relaxation times (sec).
!   CFREQ - array of layer frequency parameters.
!           curently set to 5 cm.
!
!          Output for receiver in layer i
!          ------
!
!    SIGL - full wave complex conductivity
!    KSQL - full wave propagation constant


 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 REAL, PARAMETER :: TWOPI=6.2831853, MU0=12.56637E-7, EPS0=8.854156E-12
 COMPLEX, PARAMETER :: ONE=(1.,0.)
 INTEGER NLYR,J
 REAL FRQ
 REAL, DIMENSION(NLYR) :: RES,CHRG,CALF,CTAU,CFREQ,REPS
 REAL(KIND=QL) RMUD(0:NLYR)
 COMPLEX A1,IW,P
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL

 CALF = 1. - CHRG
 IW = TWOPI * CMPLX (0.,FRQ)
 DO J = 1,NLYR
   A1 = CMPLX (1./RES(J), 0.)
   P = (0.,0.)
   IF (CFREQ(J) > 1.E-6) P = (IW * CTAU(J) )**CFREQ(J)
   A1 = A1 * (ONE + P) / (ONE + CALF(J)*P)
   A1 = A1 + IW * EPS0 * REPS(J)  !  Add in displacement term
   SIGL(J) = CMPLX (A1,KIND=QL)
   A1 = IW * MU0* A1
   KSQL(J) = RMUD(J) * CMPLX (A1,KIND=QL)
 END DO

 END SUBROUTINE COLRES_1D

 SUBROUTINE HS_CDNT (NFRQ,FREQ,SXNL,SXEL,NRHS,RHOTRP,NLYR,THKD,RES, &
                     RMUD,REPS,CHRG,CTAU,CFREQ,JS,NTX,BFD)
!-------------------------------------------------------------------

!***  Called by HSBOSS
!***  Calls COLRES_1D, HSLPLP_HNK, CUBSPL, CDCUBVAL

!  Valid for any number of layers for coincident loops which
!  must lie flat on the surface.

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive north, Y (JC=2) positive east
!  and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

!                             Input
!                             -----
!         FREQ - array of NFRQ frequencies
!   SXEL, SXNL - east-north coordinates for each vertex of Loop JS
!       RHOTRP - interpolation array of dimension NRHS
!         NLYR - number of layers
!          RES - layer resistivities
!         RMUD - mu(i) / mu(0)   (i = 1:NLYR)
!         REPS - array of relative dislectric constants
!         THKD - layer thicknesses
!         CHRG - C-C chargeability
!         CTAU - C-C time constant in seconds
!        CFREQ - C-C frequency constant
!           JS - index of total of NTX transmitters
!
!                             Output
!                             ------
!  BFD(JF,1,JS,1)   - the layered earth response of flat coincident loop
!                     frequency JF, station JS.
!  BFD(JF,1,JS,2:3) = 0.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80), NDIP0=5, MXDIP=100
 REAL, PARAMETER :: DIPL0 = 5.
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER NFRQ,NRHS,NLYR,NTX,JF,JS,JX,JY,JR,JW,JD, &
         NDIP(4),NDPLP,NDPX,NDPY
 REAL FREQ(NFRQ),FRQ,SXNL(4),SXEL(4),RHOTRP(NRHS),X1,XLOC,XSL,YSL,DPX,DPY,WTMD,XWRX2,WYRL(4),DIPL(4)
 REAL, DIMENSION(NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION(4,NRHS) :: HL1R,HL1I
 REAL(KIND=QL) RMUD(0:NLYR),THKD(NLYR),YD,R1,DIPLD
 REAL, ALLOCATABLE :: YLOC(:),XRXMD(:),YRXMD(:),YWRX(:,:),RXRHO(:,:,:)
 COMPLEX HLYR(NRHS),BFD(NFRQ,1,NTX,3)
 COMPLEX(KIND=QL) SIGL(NLYR),KSQL(NLYR),CDS1,BZR,BZG,BZV

!  This routine will produce B for unit TX moment so VFAC = RMUD(Txlyr) * MU0 / (4 * PI)
!  In this routine, the source is on the surface so RMUD(Txlyr) = 1

!  The G potential contributions are integrated around the loop.

 CALL HS_CDNT_SET_RX (SXNL,SXEL,NDPX,NDPY,XSL,YSL,DPX,DPY)

!  The coincident loop has now been adjusted to a rectangular loop at vertices
!  (0,0), (XSL,0), (XSL,YSL), (0,YSL)


 NDPLP = NDPX * NDPY
 WTMD = XSL * YSL / REAL(NDPLP)
 ALLOCATE (YLOC(NDPY),YWRX(NDPLP,4),RXRHO(NDPLP,MXDIP,4),XRXMD(NDPLP),YRXMD(NDPLP))

 DO JY = 1,NDPY
   YLOC(JY) = (JY - 0.5) * DPY
 END DO

 DO JX = 1,NDPX
   XLOC = (JX - 0.5) * DPX
   DO JY = 1,NDPY
     JR = JY + (JX-1) * NDPY
     XRXMD(JR) = XLOC
     YRXMD(JR) = YLOC(JY)
   END DO
 END DO

 WYRL(1) = XSL; WYRL(2) = YSL

 DO JW = 1,2
   NDIP(JW) = CEILING (WYRL(JW) / DIPL0)  ! 5 m initial dipole length
   NDIP(JW) = MAX (NDIP(JW), NDIP0)       ! At least 5 dipoles per segment
   NDIP(JW) = MIN (NDIP(JW), MXDIP)
   DIPL(JW) = WYRL(JW) / REAL (NDIP(JW))
   NDIP(JW+2) = NDIP(JW)
   DIPL(JW+2) = DIPL(JW)
 END DO

 DO JR = 1,NDPLP
   YWRX(JR,1) = YRXMD(JR)
   YWRX(JR,2) = XSL - XRXMD(JR)
   YWRX(JR,3) = YSL - YRXMD(JR)
   YWRX(JR,4) = XRXMD(JR)

   DO JW = 1,4
     DO JD = 1,NDIP(JW)
       X1 = (JD - 0.5) * DIPL(JW)
       IF (JW == 1 .OR. JW == 3) THEN
         XWRX2 = (XRXMD(JR) - X1)**2
       ELSE
         XWRX2 = (YRXMD(JR) - X1)**2
       END IF
       RXRHO(JR,JD,JW) =   SQRT (YWRX(JR,JW)**2   + XWRX2)
     END DO
   END DO
 END DO

 DO JF = 1,NFRQ
   HLYR = (0.,0.)
   FRQ = FREQ(JF)
   CALL COLRES_1D (FRQ,NLYR,RES,REPS,RMUD,CHRG,CTAU,CFREQ,SIGL,KSQL)

   CALL HSLPLP_HNK (NRHS,RHOTRP,NLYR,KSQL,RMUD,THKD,HLYR)
   HL1R(1,1:NRHS) = REAL (HLYR(1:NRHS))
   CALL CUBSPL (RHOTRP,HL1R,NRHS)
   HL1I(1,1:NRHS) = AIMAG (HLYR(1:NRHS))
   CALL CUBSPL (RHOTRP,HL1I,NRHS)

!  Compute the contribution to the magnetic fields from the vertical magnetic (Gz)
!  potential due to a horizontal electric dipole.  For closed loops, the horizontal
!  fields are a simple integral of a J0 term around the loop.

   BZR = ZERO
   DO JR = 1,NDPLP               ! Sum over internal dipole receivers
     BZG = ZERO
     DO JW = 1,4             ! Sum over each wire segment of transmitting loop.
       BZV = ZERO
       YD = REAL (YWRX(JR,JW),KIND=QL)
       DIPLD = REAL (DIPL(JW),KIND=QL)
       DO JD = 1, NDIP(JW)                             ! Sum over each electric dipole
         R1 = REAL (RXRHO(JR,JD,JW), KIND=QL)       ! of each wire segment.
         CALL CDCUBVAL (RHOTRP,HL1R,HL1I,NRHS,R1,CDS1)
         BZV = BZV + DIPLD * CDS1 * YD/R1              ! Accumulate Bx for all dipole segments
       END DO                                          ! for wire JW.
       BZG = BZG + BZV                                 ! Accumulate magnetic field for all segments
     END DO                                            ! for internal receiver

     BZR = BZR + BZG                                  ! Integrate over internal Rx for each loop
   END DO
   BFD(JF,1,JS,1) = WTMD * CMPLX (BZR)
   BFD(JF,1,JS,2:3) = (0.,0.)
 END DO     !  Next frequency

 DEALLOCATE (YLOC,YWRX,RXRHO,XRXMD,YRXMD)
 END SUBROUTINE HS_CDNT

 SUBROUTINE HS_CDNT_SET_RX (SXNL,SXEL,NDPX,NDPY,XSL,YSL,DPX,DPY)
!---------------------------------------------------------------
!
!  For layered half space computations:
!  If the coincident is not rectangular replace it with a rectangle of the same
!  area and diagonal whose length is equal to that of the maximum inter-vertex
!  distance of the original loop.
!
! INPUT PARAMETERS:
! ----------------
!
!    SXNL,SXEL - north & east coordinates of coincident loop vertices

! OUTPUT PARAMETERS:
! -----------------
!
!    XCLP,YCLP - north & east coordinates of equivalent retngular coincident loop vertices
!    NDPX,NDPY - numbers of magnetic dipole receivers in each direction.
!    DPX,DPY - magnetic dipole spatial intervals in each direction.
!
 IMPLICIT NONE
 INTEGER, PARAMETER :: DELMD=20, MXDP=1000, MNDP1=3
 INTEGER NDPX,NDPY,NDPRX
 REAL SXNL(4),SXEL(4),XSL,YSL,DPX,DPY,ALPHA,DIST2D

 XSL    = 0.5* (DIST2D (SXNL(1), SXEL(1), SXNL(2), SXEL(2)) + &
                DIST2D (SXNL(3), SXEL(3), SXNL(4), SXEL(4)) )
 YSL    = 0.5* (DIST2D (SXNL(1), SXEL(1), SXNL(4), SXEL(4)) + &
                DIST2D (SXNL(3), SXEL(3), SXNL(2), SXEL(2)) )

 NDPX = CEILING (XSL / DELMD)
 NDPY = CEILING (YSL / DELMD)

 NDPX = MAX (NDPX,MNDP1)
 NDPY = MAX (NDPY,MNDP1)
 NDPRX = NDPX * NDPY
 IF (NDPRX > MXDP) THEN
   ALPHA = SQRT (REAL (MXDP) / REAL(NDPRX) )
   NDPX = CEILING (ALPHA * NDPX)
   NDPY = CEILING (ALPHA * NDPY)
 END IF
 DPX = XSL / REAL (NDPX)
 DPY = YSL / REAL (NDPY)

 END SUBROUTINE HS_CDNT_SET_RX

 SUBROUTINE HSLPLP_HNK (NRHS,RHOTRP,NLYR,KSQL,RMUD,THKD,HLYR)
!------------------------------------------------------------

!***  Calls HS_JMP, HSLPLP_KER
!***  Called by HSLPLP

!  VALID FOR ANY NUMBER OF LAYERS
!  LOOP TRANSMITTER FLAT ON EARTH SURFACE
!  MAGNETIC DIPOLE RECEIVER IN AIR OR AT ANY DEPTH IN ANY LAYER

!  Uses flow through Hankel transform to compute transform integrals HLYRD(NRHS)
!  which are used to compute vertical and horizontal frequency-domain magnetic
!  field components at the RX from pseudo X oriented  HED source.  The Hankel
!  uses a 15 points per decade filter coefficient set derived from Christensen's
!  FLTGEN program.

!  RHOTRP - array of NRHS logrithmically spaced (15 / decade) horizontal distances
!           at same spacing as lambda values for Hankel transform.
!     KER - stores kernel values from HSLPLP_KER

!  NLYR,KSQL,RMUD,THKD
!  are described in HSLPLP
!
!    OUTPUT is HLYR(1:NRHS,)  forward model components

 USE FILTER_COEFFICIENTS

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: VFAC0=1.0D-7
 INTEGER NRHS,NLYR,K,L,JR,K1,KBOT,KMAX,KMIN,LMAX,LMIN
 REAL(KIND=QL) DELTA,Y,Y1,LMBDA,THKD(NLYR),RMUD(0:NLYR),RHOD
 REAL RHOTRP(NRHS)
 COMPLEX HLYR(NRHS)
 COMPLEX(KIND=QL) KSQL(NLYR),HLYRD(NRHS),KER(JNLO-NRHS:JNHI)
 LOGICAL JUMP

 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)
 HLYRD = (0._QL, 0._QL)
 KER = (0._QL, 0._QL)

!  Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
!  compute most of the needed kernel range from the high end.  Note that
!  the filter is defined between JNLO < L < JNHI

 JR = 1
 RHOD = REAL (RHOTRP(1),KIND=QL)
 Y1 = -LOG (RHOD) - SHFTJN

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   CALL HSLPLP_KER (K,JR,L,LMBDA,NLYR,KSQL,RMUD,THKD,NRHS,KER,HLYRD,JUMP)
   IF (JUMP .AND. L > -40) EXIT
 END DO

 JUMP = .FALSE.           ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Maximum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL HSLPLP_KER (K,JR,L,LMBDA,NLYR,KSQL,RMUD,THKD,NRHS,KER,HLYRD,JUMP)
   IF (JUMP .AND. L < -60) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (NRHS)

 JR = NRHS
 RHOD = REAL (RHOTRP(NRHS),KIND=QL)
 Y1 = -LOG (RHOD) - SHFTJN
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.
   HLYRD(JR) = HLYRD(JR) + KER(K) * WJ1(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (Y)
     CALL HSLPLP_KER (K,JR,L,LMBDA,NLYR,KSQL,RMUD,THKD,NRHS,KER,HLYRD,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRHS-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     HLYRD(JR) = HLYRD(JR) + KER(K) * WJ1(L)
   END DO
 END DO

 DO JR = 1,NRHS
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   HLYRD(JR) = VFAC0 * HLYRD(JR) / RHOD
 END DO
 HLYR = CMPLX (HLYRD)

 END SUBROUTINE HSLPLP_HNK

 SUBROUTINE HSLPLP_KER (K,JR,L,LMBDA,NLYR,KSQL,RMUD,THKD,NRHS,KER,HLYRD,JUMP)
!----------------------------------------------------------------------------

!***  Called by HSLPLP__HNK

!  Computes the G potential kernels  for a flat surface loop transmitter
!  and a vertical component pseudo-receiver on the surface
!
!          Input
!          -----
!    NRHS - number of logrithmically horizontal distances
!       K - kernel index
!      JR - RHO index
!       L - filter index
!   LMBDA = Hankel transform variable
!    NLYR - number of layers
!    KSQL - array of proagation constants
!    RMUD - mu(i) / mu(0)
!    THKD - layer thicknesses
!
!          Output for receiver in layer i
!          ------
!
!      KER(NRHS) - kernel values for transform
!           JUMP - logical convergence indicator
!

 USE FILTER_COEFFICIENTS

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: EXP_TOL=80.D0, TOL=1.D-6, TOL2=1.D-35
 COMPLEX(KIND=QL), PARAMETER :: ONE=(1._QL,0._QL),ZERO=(0._QL,0._QL)
 INTEGER NRHS,K,JR,L,NLYR,J
 REAL(KIND=QL) THKD(NLYR),LMBDA,RMUD(0:NLYR),RMUSQ(NLYR),QR,QI
 COMPLEX(KIND=QL) LMBSQ,S0,T0,F0,XP1,FW,KER(JNLO-NRHS:JNHI),HLYRD(NRHS)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: KSQL,S,XP,T,F
 LOGICAL JUMP

 XP = ZERO; T = ZERO

 S0 = CMPLX (LMBDA, 0._QL, KIND=QL)
 LMBSQ = CMPLX (LMBDA * LMBDA, 0._QL, KIND=QL)
 DO J = NLYR, 1, -1
   RMUSQ(J) = RMUD(J) * RMUD(J)
   S(J) = SQRT (KSQL(J) + LMBSQ)

   IF (J == NLYR) CYCLE
   T(J)= ( (RMUSQ(J+1) - RMUSQ(J)) * LMBSQ + RMUSQ(J+1)* KSQL(J) - RMUSQ(J)* KSQL(J+1) )  &
                                           / (RMUD(J+1)*   S(J) +  RMUD(J)*   S(J+1) )**2
   XP1 = 2* S(J) * THKD(J)
   IF ( REAL (XP1) < EXP_TOL) XP(J) = EXP (-XP1)
 END DO

 T0 = ( (RMUSQ(1) - 1._QL) * LMBSQ - KSQL(1) ) / ( RMUD(1)*S0 + S(1) )**2
 F = ZERO
 DO J = NLYR - 1, 1, -1
   F(J) = XP(J) * (T(J) + F(J+1) )/ (ONE + T(J) * F(J+1))
 END DO
 F0 = (T0 + F(1) )/ (ONE + T0 * F(1))

! Assume pseudo vertical receiver is 3 cm above ground.

 KER(K) = (F0 + ONE) * LMBDA * EXP (-0.03 * LMBDA)

!  Accumulate Hankel transform integrals & check convergence

 FW = WJ1(L) * KER(K)
 HLYRD(JR) = HLYRD(JR) + FW

 JUMP = .TRUE.
 QR = ABS (REAL  (HLYRD(JR), KIND=QL) )
 QI = ABS (AIMAG (HLYRD(JR) ) )
 IF (QR > TOL2 .AND. ABS (REAL  (FW)) > TOL * QR) JUMP = .FALSE.
 IF (QI > TOL2 .AND. ABS (AIMAG (FW)) > TOL * QI) JUMP = .FALSE.

 END SUBROUTINE HSLPLP_KER

 SUBROUTINE HSLPB (NFRQ,FREQ,SOURCE_TYPE,NTX,JS,SXLYR,NVRL,SXNL,SXEL,ZS,MRXTX,NRXTX,MQVR,RXID,XRXTX, &
                   YRXTX,ZRXTX,NRHS,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)
!--------------------------------------------------------------------------------------------------

!***  Called by HSBOSS
!***  Calls COLRES_1D, HSCLB_HNK, HSGWB_HNK, CUBSPL, CDCUBVAL

!  Valid for any number of layers
!  Computes the frequency-domain layered earth B field for a flat-lying loop
!  or grounded wire source.
!  Closed Loop can be in air or at any depth in any layer
!  Open loop must have electrodes below earth surface.
!  Magnetic dipole receiver in air or at any depth in any layer


!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive north, Y (JC=2) positive east
!  and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

!                             Input
!                             -----
!         FREQ - array of NFRQ frequencies
!  SOURCE_TYPE - 1 => closed loop;  2 => open loop (grounded wire)
!          NTX - number of transmitters
!        SXLYR - layer containing transmitter JS
!         NVRL - number of vertices for loop JS
!         SXNL - north coordinates for vertices for loop JS
!         SXEL - east coordinates for vertices for loop JS
!           ZS - depth from surface for flat lying loop JS
!        MRXTX - maximum number of receivers per transmitter
!     NRXTX(I) - number of receivers for transmitter I
!         MQVR - maximum number of vertices for all receivers
!    RXID(I,J) - RX_TYPE of receiver I for transmitter J. 1 => MD; 2 => ED
! XRXTX(J,I,K) - north coordinate of the Kth vertex of the Jth receiver of transmitter I
! YRXTX(J,I,K) - east coordinate of the Kth vertex of the Jth receiver of transmitter I
!   ZRXTX(J,I) - depth of the Jth receiver of transmitter I
!       RHOTRP - NRHS size interpolation array for horizontal distances
!         NLYR - number of layers
!         THKD - layer thicknesses
!        DPTHL - depth to top of layer
!          RES - layer resistivities
!         RMUD - mu(i) / mu(0)   (i = 0:NLYR)
!         REPS - array of relative dislectric constants
!         CHRG - C-C chargeability
!         CTAU - C-C time constant in seconds
!        CFREQ - C-C frequency constant
!
!                             Output
!                             ------
!  BFD(JF,JR,JS,I) - the Ith component of the layered earth response at
!                    frequency JF, receiver JR, station JS.
!           I = 1,2,3 = north, east and vertical componsnts respectively

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80), NDIP0=5, MXDIP=100
 REAL, PARAMETER :: DIPL0 = 5.
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER NTX,MRXTX,NRXTX(NTX),MQVR,RXID(MRXTX,NTX),SXLYR,RXLYR,NFRQ,NRHS,SOURCE_TYPE,NVRL,NVRLS,NLYR, &
         J1,JS,JR,JF,JV,JV1,JD,NDIP(NVRL),I1,KFG,GAM
 REAL FREQ(NFRQ),FRQ,ZRXTX(MRXTX,NTX),RXVRT,SXNL(NVRL),SXEL(NVRL),XRX,YRX,RHOTRP(NRHS),X1,XWRX(NVRL,2,MRXTX), &
      BRFD(3),BIFD(3),BRFDM,BIFDM,A1,A2
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL, DIMENSION(NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION(NVRL) :: CPHI,SPHI,WYRL,DIPL
 REAL, DIMENSION(NVRL,MRXTX) :: YWRX
 REAL, DIMENSION(MXDIP,NVRL,MRXTX) :: XRXD,RXRHO
 REAL, DIMENSION(4,NRHS) :: HL1R,HL2R,HL3R,HL1I,HL2I,HL3I
 REAL(KIND=QL) ZR,ZS,ZPRV,RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR),YD,X1D,X2D,RHO1,RHO2,DIPLD,CPHID,SPHID
 COMPLEX HLYR(NRHS,3),BFD(NFRQ,MRXTX,NTX,3)
 COMPLEX(KIND=QL) SIGL(NLYR),KSQL(NLYR),CDS1,CDS2,BX,BY,BZ,BX0,BY0,BFDD(3)

!  The fields have two parts, a non-conservative (path-dependent) integration over all closed
!  segments and a conservative (path-independent) part which is integrated on a direct path
!  between electrodes.  Obviously this gives a null contribution for closed loops.

!  All parts have the commpn multiplier VFAC = RMUD(Txlyr) * MU0 / (4 * PI) = 1.D-7 * RMUD(Txlyr)

!  Loop contributions are evaluated segment by segment (open or closed) in an initial
!  coordinate system whose X component lies in the direction of the segment.  The local
!  field copmponents are rotated back into the survey soordinate system.

 BX0 = ZERO;  BY0 = ZERO    !  Open segment contributions

 NVRLS = NVRL
 IF (SOURCE_TYPE == 2) NVRLS = NVRL -1

 DO JV = 1, NVRL
   JV1 = JV + 1             ! Wire JV goes from vertex JV to vertex JV1
   IF (JV == NVRL) JV1 = 1

! Length & orientation of Wire JV between vertices JV & JV1

   WYRL(JV) = SQRT ( (SXNL(JV) - SXNL(JV1))**2 + (SXEL(JV) - SXEL(JV1))**2)
   CPHI(JV) = (SXNL(JV1) - SXNL(JV) ) / WYRL(JV)
   SPHI(JV) = (SXEL(JV1) - SXEL(JV) ) / WYRL(JV)

! Divide each wire into segment lengths of 5 m with a minimum of 5 segments per wire.

   NDIP(JV) = CEILING (WYRL(JV) / DIPL0)  ! 5 m initial dipole length
   NDIP(JV) = MAX (NDIP(JV), NDIP0)   ! At least 5 dipoles per segment
   NDIP(JV) = MIN (NDIP(JV), MXDIP)
   DIPL(JV) = WYRL(JV) / REAL (NDIP(JV))

   DO JR = 1,NRXTX(JS)
     IF (RXID(JR,JS) /= 1) CYCLE     ! This subroutine is only used for magnetic dipole receivers
     XRX = XRXTX(JR,JS,1)
     YRX = YRXTX(JR,JS,1)

! Distances from first vertex of wire segment JV to receiver  (Y = constant in new system)
     XWRX(JV,1,JR) = (XRX - SXNL(JV)) *  CPHI(JV) + (YRX-SXEL(JV)) * SPHI(JV)
     XWRX(JV,2,JR) = (XRX - SXNL(JV1)) * CPHI(JV) + (YRX-SXEL(JV1)) * SPHI(JV)

     YWRX(JV,JR) = -(XRX - SXNL(JV)) * SPHI(JV) + (YRX-SXEL(JV)) * CPHI(JV)

!  Compute distamces from each dipole centre to receiver in coordinate system
!  where wire runs along the new X (north) axis.

     DO JD = 1,NDIP(JV)
       X1 = (JD - 0.5) * DIPL(JV)
       XRXD(JD,JV,JR) =  XWRX(JV,1,JR) - X1
       RXRHO(JD,JV,JR) = SQRT (XRXD(JD,JV,JR)**2 + YWRX(JV,JR)**2)
       RXRHO(JD,JV,JR) = MAX (RXRHO(JD,JV,JR), 0.1)
     END DO
   END DO
 END DO

!  Interpolation array for Hankel integrals

 DO JF = 1,NFRQ
   FRQ = FREQ(JF)
   CALL COLRES_1D (FRQ,NLYR,RES,REPS,RMUD,CHRG,CTAU,CFREQ,SIGL,KSQL)

   ZPRV = -9.D4
   I1 = 2                              ! electric source
   DO JR = 1, NRXTX(JS)                ! Loop over receivers
     IF (RXID(JR,JS) /= 1) CYCLE       ! This subroutine is only used for magnetic dipole receivers
     BFDD = ZERO
     ZR = REAL (ZRXTX(JR,JS),QL)
     RXVRT = REAL (ABS (ZPRV - ZR))
     IF (RXVRT > .01_QL) THEN
       HLYR = (0.,0.)
       ZPRV = ZR
       RXLYR = 0
       DO J1 = NLYR,1,-1
         IF (ZR > DPTHL(J1)) THEN
           RXLYR = J1
           EXIT
         END IF
       END DO
       CALL SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)
       GAM = 0
       IF (RXLYR == SXLYR) THEN       !  Set whole space term
         IF (ZR > ZS) GAM = -1
         IF (ZR < ZS) GAM =  1
         IF (ABS (ZR - ZS) < 0.01_QL) GAM = 0
       END IF

       IF (SOURCE_TYPE == 2) THEN
         CALL HSGWB_HNK (NRHS,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,HLYR)
       ELSE
         CALL HSCLB_HNK (NRHS,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,HLYR)
       END IF

       HL1R(1,1:NRHS) = REAL (HLYR(1:NRHS,1))   ! For integration of non-conservative components over connected segments
       CALL CUBSPL (RHOTRP,HL1R,NRHS)
       HL1I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,1))
       CALL CUBSPL (RHOTRP,HL1I,NRHS)
       HL2R(1,1:NRHS) = REAL (HLYR(1:NRHS,2))
       CALL CUBSPL (RHOTRP,HL2R,NRHS)
       HL2I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,2))
       CALL CUBSPL (RHOTRP,HL2I,NRHS)
     END IF

!  Integrate non-conservative (path dependent) components around all closed segments

     BX = ZERO
     DO JV = 1,NVRLS
       YD = REAL (YWRX(JV,JR),KIND=QL)
       DIPLD = REAL (DIPL(JV),KIND=QL)
       CPHID = REAL (CPHI(JV),KIND=QL)
       SPHID = REAL (SPHI(JV),KIND=QL)
       BZ = ZERO; BY = ZERO
       DO JD = 1, NDIP(JV)                     !  Integrate over wire JV for By and Bz fields
         RHO1 = REAL (RXRHO(JD,JV,JR), KIND=QL)
         CALL CDCUBVAL (RHOTRP,HL1R,HL1I,NRHS,RHO1,CDS1)
         BY = BY + CDS1
         CALL CDCUBVAL (RHOTRP,HL2R,HL2I,NRHS,RHO1,CDS2)
         BZ = BZ + YD * CDS2
       END DO

! Rotate these back to the North, East, Z system.

       BY = DIPLD * BY
       BZ = DIPLD * BZ

       BFDD(1) = BFDD(1) + BX * CPHID - BY * SPHID
       BFDD(2) = BFDD(2) + BY * CPHID + BX * SPHID
       BFDD(3) = BFDD(3) + BZ

     END DO   !  Next wire

     IF (SOURCE_TYPE == 2) THEN
       HL3R(1,1:NRHS) = REAL (HLYR(1:NRHS,3))
       CALL CUBSPL (RHOTRP,HL3R,NRHS)
       HL3I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,3))
       CALL CUBSPL (RHOTRP,HL3I,NRHS)

!  Integrate conservative term over open side for GW source.
!  The open loop path is positive from vertex 1 to vertex NVRL, the reverse of
!  the closed loop path being positive from vertex NVRL to vertex 1.

       CPHI(NVRL) = (SXNL(NVRL) - SXNL(1) ) / WYRL(NVRL)
       SPHI(NVRL) = (SXEL(NVRL) - SXEL(1) ) / WYRL(NVRL)
       CPHID = REAL (CPHI(NVRL),KIND=QL)
       SPHID = REAL (SPHI(NVRL),KIND=QL)
       XWRX(NVRL,1,JR) =  (XRX - SXNL(1)) * CPHI(NVRL) + (YRX-SXEL(1)) * SPHI(NVRL)
       XWRX(NVRL,2,JR) =  (XRX - SXNL(NVRL)) * CPHI(NVRL) + (YRX-SXEL(NVRL)) * SPHI(NVRL)
       YWRX(NVRL,JR) = -(XRX - SXNL(1)) * SPHI(NVRL) + (YRX-SXEL(1)) * CPHI(NVRL)

       X1D = REAL (XWRX(NVRL,1,JR), KIND=QL)   ! for open segment.
       X2D = REAL (XWRX(NVRL,2,JR), KIND=QL)
       YD = REAL (YWRX(NVRL,JR),KIND=QL)
       RHO1 = SQRT (X1D**2 + YD**2)
       RHO2 = SQRT (X2D**2 + YD**2)

       CALL CDCUBVAL (RHOTRP,HL3R,HL3I,NRHS,RHO1,CDS1)
       CALL CDCUBVAL (RHOTRP,HL3R,HL3I,NRHS,RHO2,CDS2)
       BX0 = YD * (CDS2 - CDS1)
       BY0 = X1D * CDS1 - X2D * CDS2

       BFDD(1) = BFDD(1) + BX0 * CPHID - BY0 * SPHID
       BFDD(2) = BFDD(2) + BY0 * CPHID + BX0 * SPHID
     END IF

     BFD(JF,JR,JS,1:3) = CMPLX (BFDD(1:3))
     BRFD(1:3) =  REAL (BFD(JF,JR,JS,1:3) )
     BIFD(1:3) = AIMAG (BFD(JF,JR,JS,1:3) )
     BRFDM = MAXVAL (ABS (BRFD) )
     BIFDM = MAXVAL (ABS (BIFD) )
     DO JD = 1,3
       A1 = 0.;  A2 = 0.
       IF (ABS (BRFD(JD)) > 1.E-8 * BRFDM) A1 = BRFD(JD)
       IF (ABS (BIFD(JD)) > 1.E-8 * BIFDM) A2 = BIFD(JD)
       BFD(JF,JR,JS,JD) = CMPLX (A1,A2)
     END DO
   END DO     !  Next receiver
 END DO     !  Next frequency

END SUBROUTINE HSLPB

 SUBROUTINE HSCLB_HNK (NRHS,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,HLYR)
!-----------------------------------------------------------------------------------------------

!***  Calls HSCLB_KER
!***  Called by HSLPB


!  Uses flow through Hankel transform to compute electric dipole response integrals
!  HLYR(NRHS,1:NKR) used to compute the path-dependent frequency-domain magnetic
!  field components at the RX from pseudo X oriented  HED source.  The Hankel transform
!  uses a 15 points per decade filter coefficient set derived from Christensen's
!  FLTGEN program.

!          Input
!          -----
!    NRHS - number of logrithmically horizontal distances
!  RHOTRP - array of NRHS logrithmically spaced (15 / decade) horizontal distances
!           at same spacing as lambda values for Hankel transform.
!       L - filter index
!      ZS - depth of flat-lying loop
!      ZR - receiver depth
!    NLYR - number of layers
!    THKD - layer thicknesses
!   DPTHL - depth to top of layers (DPTHL = depth to basement
!    RMUD - mu(i) / mu(0)
!    SIGL - complex layer conductivities
!    KSQL - complex layer propagation constants
!     KFG - Tx-Rx indicator for coefficient generation
!   SXLYR - layer containing transmitter
!   RXLYR - layer containing receiver

!    OUTPUT is HLYR(1:NRHS,1:3)

 USE FILTER_COEFFICIENTS

 IMPLICIT NONE
 INTEGER, PARAMETER :: NKR=2
 REAL(KIND=QL), PARAMETER :: VFAC0=1.0D-7
 INTEGER KFG,NRHS,NLYR,GAM,RXLYR,SXLYR,K,L,JR,K1,KBOT,KMAX,KMIN,LMAX,LMIN
 REAL RHOTRP(NRHS)
 REAL(KIND=QL) ZS,ZR,DELTA,Y,Y1,LMBDA,RMUD(0:NLYR),RHOD
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX HLYR(NRHS,3)
 COMPLEX(KIND=QL) HLYRD(NRHS,3),KER(JNLO-NRHS:JNHI,NKR)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)
 HLYRD = (0._QL, 0._QL)
 KER = (0._QL, 0._QL)

!  Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
!  compute most of the needed kernel range from the high end.  Note that
!  the filter is defined between JNLO < L < JNHI

 JR = 1
 RHOD = REAL (RHOTRP(1),KIND=QL)
 Y1 = -LOG (RHOD) - SHFTJN

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
    CALL HSCLB_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                    KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L > -40) EXIT
 END DO

 JUMP = .FALSE.           ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Maximum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL HSCLB_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                   KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L < -60) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (NRHS)

 JR = NRHS
 RHOD = REAL (RHOTRP(NRHS),KIND=QL)
 Y1 = -LOG (RHOD) - SHFTJN
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.
   HLYRD(JR,1) = HLYRD(JR,1) + KER(K,1) * WJ0(L)
   HLYRD(JR,2) = HLYRD(JR,2) + KER(K,2) * WJ1(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (Y)
     CALL HSCLB_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                     KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRHS-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     HLYRD(JR,1) = HLYRD(JR,1) + KER(K,1) * WJ0(L)
     HLYRD(JR,2) = HLYRD(JR,2) + KER(K,2) * WJ1(L)
   END DO
 END DO

 DO JR = 1,NRHS
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   HLYRD(JR,2) = HLYRD(JR,2) / RHOD
   HLYRD(JR,1:2) = VFAC0 * RMUD(RXLYR) * HLYRD(JR,1:2) / RHOD
 END DO
 HLYR = CMPLX (HLYRD)

END SUBROUTINE HSCLB_HNK

 SUBROUTINE HSCLB_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                       KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
!----------------------------------------------------------------------------

!***  Called by HSLPM0__HNK

!  Closed loop kernels and dipole response integrals for the path-dependent response from a
!  flat closed-loop transmitter and a surface or downhole magnetic dipole receiver.
!  Correction of long-standing error: GAM is necessary in KER(K,1)
!
!          Input
!          -----
!    NRHS - number of logrithmically horizontal distances
!       K - kernel index
!      JR - RHO index
!       L - filter index
!   LMBDA = Hankel transform variable
!      ZS - depth of flat-lying loop
!      ZR - receiver depth
!    NLYR - number of layers
!    THKD - layer thicknesses
!   DPTHL - depth to top of layers (DPTHL = depth to basement
!    RMUD - mu(i) / mu(0)
!    SIGL - complex layer conductivities
!    KSQL - complex layer propagation constants
!     KFG - Tx-Rx indicator for coefficient generation
!   SXLYR - layer containing transmitter
!   RXLYR - layer containing receiver
!     NKR - kernel dimension
!
!          Output for receiver in layer i
!          ------
!
!    KER(NRHS,NKR) - kernel values for transform
!            HLYRD - dipole response integral
!             JUMP - logical convergence indicator

 USE FILTER_COEFFICIENTS

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: TOL=1.D-6, TOL2=1.D-35
 INTEGER NRHS,K,JR,L,NLYR,KFG,GAM,SXLYR,RXLYR,NKR,JINT
 REAL(KIND=QL) LMBDA,LMBDA2,ZR,ZS,RMUD(0:NLYR),QR,QI
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) S(0:NLYR),XI_V,F_V,F_H,ETA_V,G_V,G_H,SL,SM,XPDIR,FW(NKR), &
                  FACV,KER(JNLO-NRHS:JNHI,NKR),HLYRD(NRHS,3)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 CALL EDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,F_V,F_H,ETA_V,G_V,G_H)

 LMBDA2 = LMBDA**2
 SL = S(RXLYR)
 SM = S(SXLYR)
 FACV = RMUD(SXLYR) * SL / (RMUD(RXLYR) * SM)
 XPDIR = (0._QL,0._QL)
 IF (SXLYR == RXLYR) XPDIR = EXP (-SL * ABS ( ZR-ZS))

 IF (RXLYR < NLYR) XI_V =  XI_V * EXP (SL * (ZR - DPTHL(RXLYR+1)))
 IF (RXLYR > 0)   ETA_V = ETA_V * EXP (SL * (DPTHL(RXLYR) - ZR))

 KER(K,1) = FACV * (XI_V - ETA_V + GAM * XPDIR) * LMBDA     !  Local Y component
 KER(K,2) = (XI_V + ETA_V + XPDIR) * LMBDA2 / S(SXLYR)      !  Vertical component

!  Accumulate Hankel transform integrals & check convergence

 FW(1) = WJ0(L) * KER(K,1)
 FW(2) = WJ1(L) * KER(K,2)
 HLYRD(JR,1:NKR) = HLYRD(JR,1:NKR) + FW(1:NKR)

 JUMP = .TRUE.
 DO JINT = 1,NKR
   QR = ABS (REAL  (HLYRD(JR,JINT), KIND=QL) )
   QI = ABS (AIMAG (HLYRD(JR,JINT) ) )
   IF (QR > TOL2 .AND. ABS (REAL  (FW(JINT))) > TOL * QR) JUMP = .FALSE.
   IF (QI > TOL2 .AND. ABS (AIMAG (FW(JINT))) > TOL * QI) JUMP = .FALSE.
 END DO

END SUBROUTINE HSCLB_KER

 SUBROUTINE HSGWB_HNK (NRHS,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,HLYR)
!------------------------------------------------------------------------------------------------

!***  Calls HSGWB_KER
!***  Called by HSLPB


!  Uses flow through Hankel transform to compute electric dipole response integrals
!  HLYR(NRHS,1:NKR) used to compute the closed and openloop frequency-domain magnetic
!  field components at the RX from pseudo X oriented  HED source.  The Hankel transform
!  uses a 15 points per decade filter coefficient set derived from Christensen's
!  FLTGEN program.

!          Input
!          -----
!    NRHS - number of logrithmically horizontal distances
!  RHOTRP - array of NRHS logrithmically spaced (15 / decade) horizontal distances
!           at same spacing as lambda values for Hankel transform.
!       L - filter index
!      ZS - depth of flat-lying loop
!      ZR - receiver depth
!    NLYR - number of layers
!    THKD - layer thicknesses
!   DPTHL - depth to top of layers (DPTHL = depth to basement
!    RMUD - mu(i) / mu(0)
!    SIGL - complex layer conductivities
!    KSQL - complex layer propagation constants
!     KFG - Tx-Rx indicator for coefficient generation
!   SXLYR - layer containing transmitter
!   RXLYR - layer containing receiver

!    OUTPUT is HLYR(1:NRHS,1:3)

 USE FILTER_COEFFICIENTS

 IMPLICIT NONE
 INTEGER, PARAMETER :: NKR=3
 REAL(KIND=QL), PARAMETER :: VFAC0=1.0D-7
 INTEGER KFG,NRHS,NLYR,GAM,RXLYR,SXLYR,K,L,JR,K1,KBOT,KMAX,KMIN,LMAX,LMIN
 REAL RHOTRP(NRHS)
 REAL(KIND=QL) ZS,ZR,DELTA,Y,Y1,LMBDA,RMUD(0:NLYR),RHOD
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX HLYR(NRHS,3)
 COMPLEX(KIND=QL) HLYRD(NRHS,3),KER(JNLO-NRHS:JNHI,NKR)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)
 HLYRD = (0._QL, 0._QL)
 KER = (0._QL, 0._QL)

!  Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
!  compute most of the needed kernel range from the high end.  Note that
!  the filter is defined between JNLO < L < JNHI

 JR = 1
 RHOD = REAL (RHOTRP(1),KIND=QL)
 Y1 = -LOG (RHOD) - SHFTJN

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
    CALL HSGWB_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                    KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L > -40) EXIT
 END DO

 JUMP = .FALSE.           ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Maximum filter index used
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL HSGWB_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                   KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L < -60) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (NRHS)

 JR = NRHS
 RHOD = REAL (RHOTRP(NRHS),KIND=QL)
 Y1 = -LOG (RHOD) - SHFTJN
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.
   HLYRD(JR,1)   = HLYRD(JR,1) + WJ0(L) * KER(K,1)
   HLYRD(JR,2:3) = HLYRD(JR,2) + WJ1(L) * KER(K,2:3)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (Y)
     CALL HSGWB_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                     KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRHS-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     HLYRD(JR,1)   = HLYRD(JR,1) + WJ0(L) * KER(K,1)
     HLYRD(JR,2:3) = HLYRD(JR,2) + WJ1(L) * KER(K,2:3)
   END DO
 END DO

 DO JR = 1,NRHS
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   HLYRD(JR,2:3) = HLYRD(JR,2:3) / RHOD
   HLYRD(JR,1:3) = VFAC0 * RMUD(RXLYR) * HLYRD(JR,1:3) / RHOD
 END DO
 HLYR = CMPLX (HLYRD)

END SUBROUTINE HSGWB_HNK

 SUBROUTINE HSGWB_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                       KFG,GAM,SXLYR,RXLYR,NKR,KER,HLYRD,JUMP)
!----------------------------------------------------------------------------

!***  Called by HSLPM0__HNK
!***  Calls EDSX_COEF

!  Kernels and magnetic dipole response integrals for response from aclosed or grounded
!  loop transmitter and a surface or downhole magnetic dipole receiver.
!
!          Input
!          -----
!    NRHS - number of logrithmically horizontal distances
!       K - kernel index
!      JR - RHO index
!       L - filter index
!   LMBDA = Hankel transform variable
!      ZS - depth of flat-lying loop
!      ZR - receiver depth
!    NLYR - number of layers
!    THKD - layer thicknesses
!   DPTHL - depth to top of layers (DPTHL = depth to basement
!    RMUD - mu(i) / mu(0)
!    SIGL - complex layer conductivities
!    KSQL - complex layer propagation constants
!     KFG - Tx-Rx indicator for coefficient generation
!   SXLYR - layer containing transmitter
!   RXLYR - layer containing receiver
!     NKR - kernel dimension
!
!          Output for receiver in layer i
!          ------
!
!    KER(NRHS,NKR) - kernel values for transform
!            HLYRD - dipole response integral
!             JUMP - logical convergence indicator

 USE FILTER_COEFFICIENTS

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: TOL=1.D-6, TOL2=1.D-35
 INTEGER NRHS,K,JR,L,NLYR,KFG,GAM,RXLYR,SXLYR,NKR,JINT
 REAL(KIND=QL) LMBDA,LMBDA2,ZR,ZS,RMUD(0:NLYR),QR,QI
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) S(0:NLYR),XI_V,F_V,F_H,ETA_V,G_V,G_H,XP,XPDIR,SL,SM,FW(NKR), &
                  FACV,KER(JNLO-NRHS:JNHI,NKR),HLYRD(NRHS,NKR)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 CALL EDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,F_V,F_H,ETA_V,G_V,G_H)

 LMBDA2 = LMBDA**2
 SL = S(RXLYR)
 SM = S(SXLYR)
 XPDIR = (0._QL,0._QL)
 IF (SXLYR == RXLYR) XPDIR = EXP (-SL * ABS ( ZR-ZS))

 IF (RXLYR < NLYR) THEN
   XP = EXP (SL * (ZR - DPTHL(RXLYR+1)))
   F_H  = XP * F_H
   XI_V = XP * XI_V
 END IF
 IF (RXLYR > 0) THEN
   XP = EXP (SL * (DPTHL(RXLYR) - ZR))
   G_H   = XP * G_H
   ETA_V = XP * ETA_V
 END IF
 FACV = RMUD(SXLYR) * SL / (RMUD(RXLYR) * SM)
 KER(K,1) = FACV * (XI_V - ETA_V + GAM * XPDIR) * LMBDA   !  Local Y component
 KER(K,2) = (XI_V + ETA_V + XPDIR) * LMBDA2 / S(SXLYR)    !  Vertical component
 KER(K,3) = F_H + G_H + FACV * (ETA_V - XI_V)

!  Accumulate Hankel transform integrals & check convergence

 FW(1)   = WJ0(L) * KER(K,1)
 FW(2:3) = WJ1(L) * KER(K,2:3)
 HLYRD(JR,1:NKR) = HLYRD(JR,1:NKR) + FW(1:NKR)

 JUMP = .TRUE.
 DO JINT = 1,NKR
   QR = ABS (REAL  (HLYRD(JR,JINT), KIND=QL) )
   QI = ABS (AIMAG (HLYRD(JR,JINT) ) )
   IF (QR > TOL2 .AND. ABS (REAL  (FW(JINT))) > TOL * QR) JUMP = .FALSE.
   IF (QI > TOL2 .AND. ABS (AIMAG (FW(JINT))) > TOL * QI) JUMP = .FALSE.
 END DO

END SUBROUTINE HSGWB_KER

 SUBROUTINE HSLPE (NFRQ,FREQ,SOURCE_TYPE,NTX,JS,SXLYR,NVRL,SXNL,SXEL,ZS,MRXTX,NRXTX,MQVR,RXID,XRXTX, &
                   YRXTX,ZRXTX,NRHS,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)
!--------------------------------------------------------------------------------------------------

!***  Called by HSBOSS
!***  Calls COLRES_1D, HSCLE_HNK, HSGWE_HNK, CUBSPL, CDCUBVAL

!  Computes the three component, frequency-domain layered earth ELECTRIC FIELD
!  at any depth in any layer for a flat-lying loop source or grounded wire source.
!  Closed Loop can be in air or at any depth in any layer
!  Open loop must have electrodes below earth surface.
!  Valid for any number of layers

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive north, Y (JC=2) positive east
!  and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

!                               Input
!                               -----
!         FREQ - array of NFRQ frequencies
!  SOURCE_TYPE - 1 => closed loop;  2 => open loop (grounded wire)
!          NTX - number of transmitters
!        SXLYR - layer containing transmitter JS
!         NVRL - number of vertices for loop JS
!         SXNL - north coordinates for vertices for loop JS
!         SXEL - east coordinates for vertices for loop JS
!           ZS - depth from surface for flat lying loop JS
!        MRXTX - maximum number of receivers per transmitter
!     NRXTX(I) - number of receivers for transmitter I
!         MQVR - maximum number of vertices for all receivers
!    RXID(I,J) - RX_TYPE of receiver I for transmitter J. 1 => MD; 2 => ED
! XRXTX(J,I,K) - north coordinate of the Kth vertex of the Jth receiver of transmitter I
! YRXTX(J,I,K) - east coordinate of the Kth vertex of the Jth receiver of transmitter I
!   ZRXTX(J,I) - depth of the Jth receiver of transmitter I
!       RHOTRP - NRHS size interpolation array for horizontal distances
!         NLYR - number of layers
!         THKD - layer thicknesses
!        DPTHL - depth to top of layer
!          RES - layer resistivities
!         RMUD - mu(i) / mu(0)   (i = 0:NLYR)
!         REPS - array of relative dislectric constants
!         CHRG - C-C chargeability
!         CTAU - C-C time constant in seconds
!        CFREQ - C-C frequency constant
!
!                             Output
!                             ------
!  BFD(JF,JR,JS,1:3) = the layered earth voltage for frequency JF, receiver JR, station JS.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80), NDIP0=5, MXDIP=100
 REAL, PARAMETER :: DIPL0 = 5.
 COMPLEX (KIND=QL), PARAMETER :: ZERO=(0._QL, 0._QL)
 INTEGER NTX,MRXTX,NRXTX(NTX),MQVR,RXID(MRXTX,NTX),SXLYR,RXLYR,NFRQ,NRHS,SOURCE_TYPE,NVRL,NVRLS,NLYR, &
         J1,JF,JS,JR,JV,JV1,JD,NDIP(NVRL),KFG,I1
 REAL FREQ(NFRQ),FRQ,ZRXTX(MRXTX,NTX),RXVRT,RHOTRP(NRHS),SXNL(NVRL),SXEL(NVRL),X1,XWRX(NVRL,2,MRXTX),YWRX(NVRL,MRXTX)
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL, DIMENSION(NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION(NVRL) :: CPHI,SPHI,WYRL,DIPL
 REAL, DIMENSION(MXDIP,NVRL,MRXTX) :: XRXD,RXRHO
 REAL, DIMENSION(4,NRHS) :: HL1R,HL2R,HL3R,HL1I,HL2I,HL3I
 REAL, DIMENSION(MRXTX) :: XR,YR
 REAL(KIND=QL) ZR,ZPRV,ZS,RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR),YD,X1D,X2D,RHO1,RHO2,DIPLD,CPHID,SPHID
 COMPLEX HLYR(NRHS,3),BFD(NFRQ,MRXTX,NTX,3)
 COMPLEX(KIND=QL) CDS1,CDS2,EX1,EY1,EX2,EY2,EZ2,KSQL(NLYR),SIGL(NLYR)

!  The fields have two parts, a non-conservative (path-dependent) integration over all closed
!  segments and a conservative (path-independent) part which is integrated on a direct path
!  between electrodes.  Obviously this gives a null contribution for closed loops.

!  All parts have the common multiplier 1 / (4 * PI * SIGL (RXLYR))

!  Loop contributions are evaluated segment by segment (open or closed) in an initial
!  coordinate system whose X component lies in the direction of the segment.  The local
!  field copmponents are rotated back into the survey soordinate system.
!
!  There is no vertical E field for a flat closed loop but there is one for a flat open loop.
!  The vertical E field for the open loop will be computed if the value of the integer parameter

!  Voltage is computed as uniform integration of electric field over receiver length.

 XR = 0.;  YR = 0.

 DO JR = 1, NRXTX(JS)                ! Loop over receivers
   IF (RXID(JR,JS) /= 3 ) CYCLE       ! This subroutine is only used for the point electric field.
   XR(JR) = XRXTX(JR,JS,1)
   YR(JR) = YRXTX(JR,JS,1)
 END DO

!  The initial computations occur in a coordinate system whose X component
!  lies in the direction of transmitter tilt.  In other words we rotate
!  the system by SXAZM.
!  For loop sources, the Gzx potential contributions are integrated around the loop.
!  For grouned wire sources, the Fzx potential is path independent so it is
!  integrated across the open side instead of around all the wire segments.

 NVRLS = NVRL
 IF (SOURCE_TYPE == 2) NVRLS = NVRL -1

 DO JV = 1, NVRL
   JV1 = JV + 1             ! Wire JV goes from vertex JV to vertex JV1
   IF (JV == NVRL) JV1 = 1

! Length & orientation of Wire JV between vertices JV & JV1

   WYRL(JV) = SQRT ( (SXNL(JV) - SXNL(JV1))**2 + (SXEL(JV) - SXEL(JV1))**2)
   CPHI(JV) = (SXNL(JV1) - SXNL(JV) ) / WYRL(JV)
   SPHI(JV) = (SXEL(JV1) - SXEL(JV) ) / WYRL(JV)
   IF (SOURCE_TYPE == 2 .AND. JV == NVRL) THEN
     CPHI(NVRL) = (SXNL(NVRL) - SXNL(1) ) / WYRL(NVRL)
     SPHI(NVRL) = (SXEL(NVRL) - SXEL(1) ) / WYRL(NVRL)
   END IF

! Divide each source wire into segment lengths of 5 m with a minimum of 5 segments per wire.

   NDIP(JV) = CEILING (WYRL(JV) / DIPL0)  ! 5 m initial dipole length
   NDIP(JV) = MAX (NDIP(JV), NDIP0)       ! At least 5 dipoles per segment
   NDIP(JV) = MIN (NDIP(JV), MXDIP)
   DIPL(JV) = WYRL(JV) / REAL (NDIP(JV))

! Distances from both vertices of wire segment JV to each point of
! electric dipole receiver  (Y = constant in new system)

   DO JR = 1, NRXTX(JS)              ! Loop over receivers
     IF (RXID(JR,JS) /= 3) CYCLE     ! This subroutine is only used for electric dipole receivers

     XWRX(JV,1,JR) = (XR(JR) - SXNL(JV))  * CPHI(JV) + (YR(JR) - SXEL(JV))  * SPHI(JV)
     XWRX(JV,2,JR) = (XR(JR) - SXNL(JV1)) * CPHI(JV) + (YR(JR) - SXEL(JV1)) * SPHI(JV)
     YWRX(JV,JR) =  -(XR(JR) - SXNL(JV)) * SPHI(JV) + (YR(JR) - SXEL(JV))  * CPHI(JV)
     IF (SOURCE_TYPE == 2 .AND. JV == NVRL) THEN
       XWRX(NVRL,1,JR) = (XR(JR) - SXNL(1))    * CPHI(NVRL) + (YR(JR) - SXEL(1))    * SPHI(NVRL)
       XWRX(NVRL,2,JR) = (XR(JR) - SXNL(NVRL)) * CPHI(NVRL) + (YR(JR) - SXEL(NVRL)) * SPHI(NVRL)
       YWRX(NVRL,JR) =  -(XR(JR) - SXNL(1))    * SPHI(NVRL) + (YR(JR) - SXEL(1))    * CPHI(NVRL)
     END IF

!  Compute distamces from each dipole centre to each receiver point in coordinate system
!  where wire runs along the new X (north) axis.

     DO JD = 1,NDIP(JV)
       X1 = (JD - 0.5) * DIPL(JV)
       XRXD(JD,JV,JR) =  XWRX(JV,1,JR) - X1
       RXRHO(JD,JV,JR) = SQRT (XRXD(JD,JV,JR)**2 + YWRX(JV,JR)**2)
     END DO
   END DO
 END DO

 DO JF = 1,NFRQ
   FRQ = FREQ(JF)
   CALL COLRES_1D (FRQ,NLYR,RES,REPS,RMUD,CHRG,CTAU,CFREQ,SIGL,KSQL)

   ZPRV = -9.D4
   I1 = 2                              ! electric source
   DO JR = 1, NRXTX(JS)                ! Loop over receivers
     IF (RXID(JR,JS) /= 3) CYCLE       ! This subroutine is only used for electric dipole receivers
     ZR = REAL (ZRXTX(JR,JS),QL)
     ZR = MAX (0.001D0,ZR)               ! Electrodes must be in contact with earth
     RXVRT = REAL (ABS (ZPRV - ZR))
     IF (RXVRT > .01_QL) THEN
      HLYR = (0.,0.)
       ZPRV = ZR
       RXLYR = 1
       DO J1 = NLYR,2,-1
         IF (ZR > DPTHL(J1)) THEN
           RXLYR = J1
           EXIT
         END IF
       END DO
       CALL SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)

!  Interpolation array for Hankel integrals

       CALL HSLPE_HNK (NRHS,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,SXLYR,RXLYR,HLYR)

       HL1R(1,1:NRHS) = REAL (HLYR(1:NRHS,1))
       CALL CUBSPL (RHOTRP,HL1R,NRHS)
       HL1I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,1))
       CALL CUBSPL (RHOTRP,HL1I,NRHS)

       IF (SOURCE_TYPE == 2) THEN                      ! Include conservative terms for open loop source.
         HL2R(1,1:NRHS) = REAL (HLYR(1:NRHS,2))
         CALL CUBSPL (RHOTRP,HL2R,NRHS)
         HL2I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,2))
         CALL CUBSPL (RHOTRP,HL2I,NRHS)

         HL3R(1,1:NRHS) = REAL (HLYR(1:NRHS,3))
         CALL CUBSPL (RHOTRP,HL3R,NRHS)
         HL3I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,3))
         CALL CUBSPL (RHOTRP,HL3I,NRHS)
       END IF
     END IF

     EX2 = ZERO;  EY2 = ZERO
     DO JV = 1,NVRLS                ! Sum the contribution for each Tx loop segment
       YD = REAL (YWRX(JV,JR),KIND=QL)
       DIPLD = REAL (DIPL(JV),KIND=QL)
       CPHID = REAL (CPHI(JV),KIND=QL)
       SPHID = REAL (SPHI(JV),KIND=QL)
       EX1 = ZERO
       DO JD = 1, NDIP(JV)          !  Integrate over Tx segment
         X1D = REAL (XRXD(JD,JV,JR), KIND=QL)
         RHO1 = REAL (RXRHO(JD,JV,JR), KIND=QL)
         CALL CDCUBVAL (RHOTRP,HL1R,HL1I,NRHS,RHO1,CDS1)
         EX1 = EX1 + CDS1
       END DO
       EX1 = DIPLD * EX1       ! Ex for Tx loop segment JV in rotated system

! Rotate these back to the survey system & add to
! sum from previous wire segnment.

       EX2 = EX2 + EX1 * CPHID  ! Fields in survey system
       EY2 = EY2 + EX1 * SPHID
     END DO                                !  Next wire segment

     BFD(JF,JR,JS,1) =  CMPLX (EX2)
     BFD(JF,JR,JS,2) =  CMPLX (EY2)
     BFD(JF,JR,JS,3) =  (0.,0.)
   END DO                                      ! next integration point

   IF (SOURCE_TYPE == 2) THEN       ! Include conservative terms for open segment.
     DO JR = 1, NRXTX(JS)               !  Sum over dipole receivers
       IF (RXID(JR,JS) /= 3) CYCLE     ! This subroutine is only used for electric dipole receivers
       YD = REAL (YWRX(NVRL,JR),KIND=QL)
       X1D = REAL (XWRX(NVRL,1,JR), KIND=QL)   ! for open loop segment.
       X2D = REAL (XWRX(NVRL,2,JR), KIND=QL)
       RHO1 = SQRT (X1D**2 + YD**2)
       RHO2 = SQRT (X2D**2 + YD**2)
       CALL CDCUBVAL (RHOTRP,HL2R,HL2I,NRHS,RHO1,CDS1)
       CALL CDCUBVAL (RHOTRP,HL2R,HL2I,NRHS,RHO2,CDS2)
       CPHID = REAL (CPHI(NVRL),KIND=QL)
       SPHID = REAL (SPHI(NVRL),KIND=QL)

       EX1 = X2D * CDS2 - X1D * CDS1    ! Fields in rotated system
       EY1 = YD * (CDS2 - CDS1)
       EX2 = EX1 * CPHID - EY1 * SPHID                    ! Fields in survey system
       EY2 = EY1 * CPHID + EX1 * SPHID

       CALL CDCUBVAL (RHOTRP,HL3R,HL3I,NRHS,RHO1,CDS1)
       CALL CDCUBVAL (RHOTRP,HL3R,HL3I,NRHS,RHO2,CDS2)
       EZ2 = CDS2 - CDS1

       BFD(JF,JR,JS,1) = BFD(JF,JR,JS,1) + CMPLX (EX2)  ! add charge & induction contributions
       BFD(JF,JR,JS,2) = BFD(JF,JR,JS,2) + CMPLX (EY2)
       BFD(JF,JR,JS,3) = BFD(JF,JR,JS,3) + CMPLX (EZ2)
     END DO
   END IF
 END DO     !  Next frequency

 END SUBROUTINE HSLPE

 SUBROUTINE HSLPE_HNK (NRHS,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,SXLYR,RXLYR,HLYR)
!--------------------------------------------------------------------------------------------

!***  Calls HS_JMP, HSLPE_KER
!***  Called by HSLPE

!  Uses flow through Hankel transform to compute electric dipole response integrals
!  HLYR(NRHS,1:3) used to compute the closed and openloop frequency-domain electric
!  field components at the RX from pseudo X oriented  HED source.  The Hankel transform
!  uses a 15 points per decade filter coefficient set derived from Christensen's
!  FLTGEN program.

!          Input
!          -----
!    NRHS - number of logrithmically horizontal distances
!  RHOTRP - array of NRHS logrithmically spaced (15 / decade) horizontal distances
!           at same spacing as lambda values for Hankel transform.
!       L - filter index
!      ZS - depth of flat-lying loop
!      ZR - receiver depth
!    NLYR - number of layers
!    THKD - layer thicknesses
!   DPTHL - depth to top of layers (DPTHL = depth to basement
!    RMUD - mu(i) / mu(0)
!    SIGL - complex layer conductivities
!    KSQL - complex layer propagation constants
!     KFG - Tx-Rx indicator for coefficient generation
!   SXLYR - layer containing transmitter
!   RXLYR - layer containing receiver

!    OUTPUT is HLYR(1:NRHS,1:3)

 USE FILTER_COEFFICIENTS

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: FOURPI = 12.56637061_QL
 INTEGER KFG,GAM,NRHS,NLYR,RXLYR,SXLYR,K,L,JR,K1,KBOT,KMAX,KMIN,LMAX,LMIN
 REAL RHOTRP(NRHS)
 REAL(KIND=QL) ZS,ZR,DELTA,Y,Y1,LMBDA,RMUD(0:NLYR),RHOD
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX HLYR(NRHS,3)
 COMPLEX(KIND=QL) HLYRD(NRHS,3),KER(JNLO-NRHS:JNHI,3)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 DELTA = LOG (10.D0)/ DBLE (NDEC_JN)
 HLYRD = (0._QL, 0._QL)
 KER = (0._QL, 0._QL)
 GAM = 0
 IF (RXLYR == SXLYR) THEN       !  Set whole space term
   IF (ZR > ZS) GAM = -1
   IF (ZR < ZS) GAM =  1
   IF (ABS (ZR - ZS) < 0.01_QL) GAM = 0
 END IF

!  Set up KER for JR = 1 corresponding to minimum value of RHO.  This will
!  compute most of the needed kernel range from the high end.  Note that
!  the filter is defined between JNLO < L < JNHI

 JR = 1
 RHOD = REAL (RHOTRP(1),KIND=QL)
 Y1 = -LOG (RHOD) - SHFTJN

 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   LMAX = L                   ! Maximum filter index used
   K = L + 1 - JR             ! K is the kernel index.
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   CALL HSLPE_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L > -40) EXIT
 END DO

 JUMP = .FALSE.           ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   LMIN = L                   ! Maximum filter index used                            r4cs
   Y = Y1 + DBLE (L) * DELTA
   LMBDA = EXP (Y)
   K = L + 1 - JR             ! Compute the kernel index.
   CALL HSLPE_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,KER,HLYRD,JUMP)
   IF (JUMP .AND. L < -60) EXIT
 END DO

 KMIN = LMIN + 1 - JR  !  Define the range of kernel values
 KMAX = LMAX + 1 - JR  !  used for RHOTRP(1)

! Complete definition of kernel values by evaluating transform of
! maximum RHO = RHOTRP (NRHS)

 JR = NRHS
 RHOD = REAL (RHOTRP(NRHS),KIND=QL)
 Y1 = -LOG (RHOD) - SHFTJN
 KBOT = JNLO + 1 - JR
 K1 = MAX (KBOT,KMIN)

 DO K = K1, KMAX          ! Compute EHR for maximum RHO using previously
   L = K - 1 + JR         ! computed kernel values.
   HLYRD(JR,1) = HLYRD(JR,1) + KER(K,1) * WJ0(L)
   HLYRD(JR,2) = HLYRD(JR,2) + KER(K,2) * WJ1(L)
   HLYRD(JR,3) = HLYRD(JR,3) + KER(K,3) * WJ0(L)
 END DO

 IF (K1 > KBOT) THEN    !  Add low end kernel values until convergence.
   DO K = K1-1, KBOT, -1
     KMIN = K
     L = K - 1 + JR
     Y = Y1 + DBLE (L) * DELTA
     LMBDA = EXP (Y)
     CALL HSLPE_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,GAM,SXLYR,RXLYR,KER,HLYRD,JUMP)
     IF (JUMP) EXIT
   END DO
 END IF

 DO JR = 2, NRHS-1          !  Compute transforms for all other RHO values
   DO K = KMIN, KMAX         !  using previously computed kernel values.
     L = K - 1 + JR
     HLYRD(JR,1) = HLYRD(JR,1) + KER(K,1) * WJ0(L)
     HLYRD(JR,2) = HLYRD(JR,2) + KER(K,2) * WJ1(L)
     HLYRD(JR,3) = HLYRD(JR,3) + KER(K,3) * WJ0(L)
   END DO
 END DO

 DO JR = 1,NRHS
   RHOD = REAL (RHOTRP(JR),KIND=QL)
   HLYRD(JR,1:3) =  HLYRD(JR,1:3) / (FOURPI * SIGL(RXLYR) * RHOD)
   HLYRD(JR,2) = HLYRD(JR,2) / RHOD
 END DO
 HLYR = CMPLX (HLYRD)

 END SUBROUTINE HSLPE_HNK

 SUBROUTINE HSLPE_KER (NRHS,K,JR,L,LMBDA,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                       KFG,GAM,SXLYR,RXLYR,KER,HLYRD,JUMP)
!----------------------------------------------------------------------------

!***  Called by HSLPE__HNK
!***  Calls EDSX_COEF

!  Kernels and magnetic dipole response integrals for response from a closed or grounded
!  loop transmitter and a surface or downhole horizontal electric dipole receiver.
!
!          Input
!          -----
!    NRHS - number of logrithmically horizontal distances
!       K - kernel index
!      JR - RHO index
!       L - filter index
!   LMBDA = Hankel transform variable
!      ZS - depth of flat-lying loop
!      ZR - receiver depth
!    NLYR - number of layers
!    THKD - layer thicknesses
!   DPTHL - depth to top of layers (DPTHL = depth to basement
!    RMUD - mu(i) / mu(0)
!    SIGL - complex layer conductivities
!    KSQL - complex layer propagation constants
!     KFG - Tx-Rx indicator for coefficient generation
!   SXLYR - layer containing transmitter
!   RXLYR - layer containing receiver
!
!          Output for receiver in layer L
!          ------
!
!    KER(NRHS,3) - kernel values for transform
!            HLYRD - dipole response integral
!             JUMP - logical convergence indicator

 USE FILTER_COEFFICIENTS

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: TOL=1.D-6, TOL2=1.D-35
 INTEGER NRHS,K,JR,L,NLYR,KFG,GAM,RXLYR,SXLYR,J1
 REAL(KIND=QL) LMBDA,ZR,ZS,RMUD(0:NLYR),QR,QI
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) S(0:NLYR),SL,XI_V,F_V,F_H,ETA_V,G_V,G_H,XP,XPDIR,FACV,KV,KS,FW(3), &
                  KER(JNLO-NRHS:JNHI,3),HLYRD(NRHS,3)
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL
 LOGICAL JUMP

 CALL EDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,F_V,F_H,ETA_V,G_V,G_H)
 SL = S(RXLYR)
 FACV = RMUD(SXLYR) * KSQL(RXLYR) / (RMUD(RXLYR) * S(SXLYR))

 IF (RXLYR < NLYR) THEN
   XP = EXP (SL * (ZR - DPTHL(RXLYR+1)))
   F_H  = XP * F_H
   XI_V = XP * XI_V
 END IF
 XP = EXP (SL * (DPTHL(RXLYR) - ZR))
 XPDIR = (0._QL, 0._QL)
 IF (SXLYR == RXLYR) XPDIR = EXP (-SL * ABS (ZR-ZS))

 G_H   = XP * G_H
 ETA_V = XP * ETA_V

 KV = FACV * (XI_V + ETA_V + XPDIR)
 KS = (F_H - G_H + XPDIR) * SL
 KER(K,1) = -KV * LMBDA
 KER(K,2) = KS - KV
 KER(K,3) = -(F_H + G_H + GAM * XPDIR) * LMBDA   ! Vertical component

!  Accumulate Hankel transform integrals & check convergence

 FW(1) = WJ0(L) * KER(K,1)
 FW(2) = WJ1(L) * KER(K,2)
 FW(3) = WJ0(L) * KER(K,3)

 HLYRD(JR,1:3) = HLYRD(JR,1:3) + FW(1:3)

 JUMP = .TRUE.
 DO J1 = 1,3
   QR = ABS (REAL  (HLYRD(JR,J1), KIND=QL) )
   QI = ABS (AIMAG (HLYRD(JR,J1) ) )
   IF (QR > TOL2 .AND. ABS (REAL  (FW(J1))) > TOL * QR) JUMP = .FALSE.
   IF (QI > TOL2 .AND. ABS (AIMAG (FW(J1))) > TOL * QI) JUMP = .FALSE.
 END DO

 END SUBROUTINE HSLPE_KER

 SUBROUTINE HSLPED (NFRQ,FREQ,SOURCE_TYPE,NTX,JS,SXLYR,NVRL,SXNL,SXEL,ZS,MRXTX,NRXTX,MQVR,RXID,XRXTX, &
                    YRXTX,ZRXTX,NRHS,RHOTRP,NLYR,THKD,DPTHL,RES,RMUD,REPS,CHRG,CTAU,CFREQ,BFD)
!---------------------------------------------------------------------------------------------------

!***  Called by HSBOSS
!***  Calls COLRES_1D, HSCLE_HNK, HSGWE_HNK, CUBSPL, CDCUBVAL

!  Computes the frequency-domain layered earth voltage across a HORIZONTAL ELECTRIC DIPOLE
!  for a flat-lying loop or grounded wire source.
!  Closed Loop can be in air or at any depth in any layer
!  Open loop must have electrodes below earth surface.
!  Horizontal electric dipole receiver in at any depth in any layer
!  Valid for any number of layers

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive north, Y (JC=2) positive east
!  and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

!                               Input
!                               -----
!         FREQ - array of NFRQ frequencies
!  SOURCE_TYPE - 1 => closed loop;  2 => open loop (grounded wire)
!          NTX - number of transmitters
!        SXLYR - layer containing transmitter JS
!         NVRL - number of vertices for loop JS
!         SXNL - north coordinates for vertices for loop JS
!         SXEL - east coordinates for vertices for loop JS
!           ZS - depth from surface for flat lying loop JS
!        MRXTX - maximum number of receivers per transmitter
!     NRXTX(I) - number of receivers for transmitter I
!         MQVR - maximum number of vertices for all receivers
!    RXID(I,J) - RX_TYPE of receiver I for transmitter J. 1 => MD; 2 => ED
! XRXTX(J,I,K) - north coordinate of the Kth vertex of the Jth receiver of transmitter I
! YRXTX(J,I,K) - east coordinate of the Kth vertex of the Jth receiver of transmitter I
!   ZRXTX(J,I) - depth of the Jth receiver of transmitter I
!       RHOTRP - NRHS size interpolation array for horizontal distances
!         NLYR - number of layers
!         THKD - layer thicknesses
!        DPTHL - depth to top of layer
!          RES - layer resistivities
!         RMUD - mu(i) / mu(0)   (i = 0:NLYR)
!         REPS - array of relative dislectric constants
!         CHRG - C-C chargeability
!         CTAU - C-C time constant in seconds
!        CFREQ - C-C frequency constant
!
!                             Output
!                             ------
!  BFD(JF,JR,JS,1)   - the layered earth voltage for frequency JF, receiver JR, station JS.
!  BFD(JF,JR,JS,2:3) = 0.

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80), NDIP0=5, MXDIP=100
 REAL, PARAMETER :: DIPL0 = 5.
 COMPLEX (KIND=QL), PARAMETER :: ZERO=(0._QL, 0._QL)
 INTEGER NTX,MRXTX,NRXTX(NTX),MQVR,RXID(MRXTX,NTX),SXLYR,RXLYR,NFRQ,NRHS,SOURCE_TYPE,NVRL,NVRLS,NLYR, &
         J1,JF,JS,JR,JG,JV,JV1,JD,NDIP(NVRL),KFG,I1
 REAL FREQ(NFRQ),FRQ,ZRXTX(MRXTX,NTX),RXVRT,RHOTRP(NRHS),SXNL(NVRL),SXEL(NVRL),X1,CSRX(MRXTX),SNRX(MRXTX), &
      WRXD(5,MRXTX),DELR,DELX,DELY,XWRX(NVRL,2,5,MRXTX),YWRX(NVRL,5,MRXTX)
 REAL, DIMENSION(MRXTX,NTX,MQVR) :: XRXTX,YRXTX
 REAL, DIMENSION(NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL, DIMENSION(NVRL) :: CPHI,SPHI,WYRL,DIPL
 REAL, DIMENSION(MXDIP,NVRL,5,MRXTX) :: XRXD,RXRHO
 REAL, DIMENSION(4,NRHS) :: HL1R,HL2R,HL1I,HL2I
 REAL, DIMENSION(5,MRXTX) :: XRXED,YRXED
 REAL(KIND=QL) ZR,ZPRV,ZS,RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR),YD,X1D,X2D,RHO1,RHO2,DIPLD,CPHID,SPHID
 COMPLEX HLYR(NRHS,3),BFD(NFRQ,MRXTX,NTX,3)
 COMPLEX(KIND=QL) CDS1,CDS2,EX1,EY1,EX2,EY2,EXL,EYL,EDIP,KSQL(NLYR),SIGL(NLYR)

!  The fields have two parts, a non-conservative (path-dependent) integration over all closed
!  segments and a conservative (path-independent) part which is integrated on a direct path
!  between electrodes.  Obviously this gives a null contribution for closed loops.

!  All parts have the common multiplier 1 / (4 * PI * SIGL (RXLYR))

!  Loop contributions are evaluated segment by segment (open or closed) in an initial
!  coordinate system whose X component lies in the direction of the segment.  The local
!  field copmponents are rotated back into the survey soordinate system.
!  The voltage for the electric dipole receivers will be computed by integrating
!  the electric field between two electrodes using 5 point Simpson integration.
!
!  This version assumes flat receivers so no vertical E field is computed. (NKR = 2)
!  There is no vertical E field for a flat closed loop but there is one for a flat open loop.
!  The vertical E field for the open loop will be computed if the value of the integer parameter
!  NKR is changed from 2 to 3, but that also requires changing the integration path over the
!  segments to account for vertical extent.

!  Voltage is computed as uniform integration of electric field over receiver length.

 XRXED = 0.;  YRXED = 0.; WRXD = 0.

 DO JR = 1, NRXTX(JS)                ! Loop over receivers
   IF (RXID(JR,JS) /= 2) CYCLE       ! This subroutine is only used for electric dipole receivers
   XRXED(1,JR) = XRXTX(JR,JS,1)
   XRXED(5,JR) = XRXTX(JR,JS,2)
   YRXED(1,JR) = YRXTX(JR,JS,1)
   YRXED(5,JR) = YRXTX(JR,JS,2)

   DELX = (XRXED(5,JR) - XRXED(1,JR) ) / 4.
   DELY = (YRXED(5,JR) - YRXED(1,JR) ) / 4.
   DELR = SQRT (DELX**2 + DELY**2)                  ! Rx dipole length
   CSRX(JR) = DELX / DELR                           ! cos of angle Rx dipole makes w/ north
   SNRX(JR) = DELY / DELR

   DO JG = 2,4
     XRXED(JG,JR) = XRXED(JG-1,JR) + DELX
     YRXED(JG,JR) = YRXED(JG-1,JR) + DELY
   END DO

   WRXD(1,JR) = DELR /3.
   WRXD(2,JR) = 4. * WRXD(1,JR)
   WRXD(3,JR) = 2. * WRXD(1,JR)
   WRXD(4,JR) = WRXD(2,JR)
   WRXD(5,JR) = WRXD(1,JR)
 END DO

!  The initial computations occur in a coordinate system whose X component
!  lies in the direction of transmitter tilt.  In other words we rotate
!  the system by SXAZM.
!  For loop sources, the Gzx potential contributions are integrated around the loop.
!  For grouned wire sources, the Fzx potential is path independent so it is
!  integrated across the open side instead of around all the wire segments.

 NVRLS = NVRL
 IF (SOURCE_TYPE == 2) NVRLS = NVRL -1

 DO JV = 1, NVRL
   JV1 = JV + 1             ! Wire JV goes from vertex JV to vertex JV1
   IF (JV == NVRL) JV1 = 1

! Length & orientation of Wire JV between vertices JV & JV1

   WYRL(JV) = SQRT ( (SXNL(JV) - SXNL(JV1))**2 + (SXEL(JV) - SXEL(JV1))**2)
   CPHI(JV) = (SXNL(JV1) - SXNL(JV) ) / WYRL(JV)
   SPHI(JV) = (SXEL(JV1) - SXEL(JV) ) / WYRL(JV)
   IF (SOURCE_TYPE == 2 .AND. JV == NVRL) THEN
     CPHI(NVRL) = (SXNL(NVRL) - SXNL(1) ) / WYRL(NVRL)
     SPHI(NVRL) = (SXEL(NVRL) - SXEL(1) ) / WYRL(NVRL)
   END IF

! Divide each source wire into segment lengths of 5 m with a minimum of 5 segments per wire.

   NDIP(JV) = CEILING (WYRL(JV) / DIPL0)  ! 5 m initial dipole length
   NDIP(JV) = MAX (NDIP(JV), NDIP0)       ! At least 5 dipoles per segment
   NDIP(JV) = MIN (NDIP(JV), MXDIP)
   DIPL(JV) = WYRL(JV) / REAL (NDIP(JV))

! Distances from both vertices of wire segment JV to each point of
! electric dipole receiver  (Y = constant in new system)

   DO JR = 1, NRXTX(JS)              ! Loop over receivers
     IF (RXID(JR,JS) /= 2) CYCLE     ! This subroutine is only used for electric dipole receivers
     DO JG = 1,5                     ! Sum over Simpson integration points

       XWRX(JV,1,JG,JR) =  (XRXED(JG,JR) - SXNL(JV)) * CPHI(JV) + (YRXED(JG,JR) - SXEL(JV)) * SPHI(JV)
       XWRX(JV,2,JG,JR) =  (XRXED(JG,JR) - SXNL(JV1)) * CPHI(JV) + (YRXED(JG,JR) - SXEL(JV1)) * SPHI(JV)
       YWRX(JV,JG,JR) = -(XRXED(JG,JR) - SXNL(JV)) * SPHI(JV) + (YRXED(JG,JR) - SXEL(JV)) * CPHI(JV)
       IF (SOURCE_TYPE == 2 .AND. JV == NVRL) THEN
         XWRX(NVRL,1,JG,JR) = (XRXED(JG,JR) - SXNL(1))    * CPHI(NVRL) + (YRXED(JG,JR) - SXEL(1))    * SPHI(NVRL)
         XWRX(NVRL,2,JG,JR) = (XRXED(JG,JR) - SXNL(NVRL)) * CPHI(NVRL) + (YRXED(JG,JR) - SXEL(NVRL)) * SPHI(NVRL)
         YWRX(NVRL,JG,JR) =  -(XRXED(JG,JR) - SXNL(1))    * SPHI(NVRL) + (YRXED(JG,JR) - SXEL(1))    * CPHI(NVRL)
       END IF

!  Compute distamces from each dipole centre to each receiver point in coordinate system
!  where wire runs along the new X (north) axis.

       DO JD = 1,NDIP(JV)
         X1 = (JD - 0.5) * DIPL(JV)
         XRXD(JD,JV,JG,JR) =  XWRX(JV,1,JG,JR) - X1
         RXRHO(JD,JV,JG,JR) = SQRT (XRXD(JD,JV,JG,JR)**2 + YWRX(JV,JG,JR)**2)
       END DO
     END DO
   END DO
 END DO

 EDIP = ZERO
 DO JF = 1,NFRQ
   FRQ = FREQ(JF)
   CALL COLRES_1D (FRQ,NLYR,RES,REPS,RMUD,CHRG,CTAU,CFREQ,SIGL,KSQL)

   ZPRV = -9.D4
   I1 = 2                               ! electric source
   DO JR = 1, NRXTX(JS)                ! Loop over receivers
     IF (RXID(JR,JS) /= 2) CYCLE       ! This subroutine is only used for electric dipole receivers
     ZR = REAL (ZRXTX(JR,JS),QL)
     ZR = MAX (0.001D0,ZR)               ! Electrodes must be in contact with earth
     RXVRT = REAL (ABS (ZPRV - ZR))
     IF (RXVRT > .01_QL) THEN
      HLYR = (0.,0.)
       ZPRV = ZR
       RXLYR = 1
       DO J1 = NLYR,2,-1
         IF (ZR > DPTHL(J1)) THEN
           RXLYR = J1
           EXIT
         END IF
       END DO
       CALL SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)

!  Interpolation array for Hankel integrals

       CALL HSLPE_HNK (NRHS,RHOTRP,ZS,ZR,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL,KFG,SXLYR,RXLYR,HLYR)

       HL1R(1,1:NRHS) = REAL (HLYR(1:NRHS,1))
       CALL CUBSPL (RHOTRP,HL1R,NRHS)
       HL1I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,1))
       CALL CUBSPL (RHOTRP,HL1I,NRHS)

       IF (SOURCE_TYPE == 2) THEN                      ! Include conservative terms for open loop source.
         HL2R(1,1:NRHS) = REAL (HLYR(1:NRHS,2))
         CALL CUBSPL (RHOTRP,HL2R,NRHS)
         HL2I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,2))
         CALL CUBSPL (RHOTRP,HL2I,NRHS)
       END IF

!      HL3R(1,1:NRHS) = REAL (HLYR(1:NRHS,3))
!      CALL CUBSPL (RHOTRP,HL3R,NRHS)
!      HL3I(1,1:NRHS) = AIMAG (HLYR(1:NRHS,3))
!      CALL CUBSPL (RHOTRP,HL3I,NRHS)
     END IF

     EXL = ZERO;  EYL = ZERO
     DO JG = 1,5                      ! Integrate over the 5 receiver segments
       EX2 = ZERO;  EY2 = ZERO
       DO JV = 1,NVRLS                ! Sum the contribution for each Tx loop segment
         YD = REAL (YWRX(JV,JG,JR),KIND=QL)
         DIPLD = REAL (DIPL(JV),KIND=QL)
         CPHID = REAL (CPHI(JV),KIND=QL)
         SPHID = REAL (SPHI(JV),KIND=QL)
         EX1 = ZERO
         DO JD = 1, NDIP(JV)          !  Integrate over Tx segment
           X1D = REAL (XRXD(JD,JV,JG,JR), KIND=QL)
           RHO1 = REAL (RXRHO(JD,JV,JG,JR), KIND=QL)
           CALL CDCUBVAL (RHOTRP,HL1R,HL1I,NRHS,RHO1,CDS1)
           EX1 = EX1 + CDS1
         END DO
         EX1 = DIPLD * EX1       ! Ex for Tx loop segment JV in rotated system

! Rotate these back to the survey system & add to
! sum from previous wire segnment.

         EX2 = EX2 + EX1 * CPHID  ! Fields in survey system
         EY2 = EY2 + EX1 * SPHID
       END DO                                !  Next wire segment

       EXL = EXL + WRXD(JG,JR) * EX2         ! Simpson integration over Rx dipole
       EYL = EYL + WRXD(JG,JR) * EY2
     END DO                                      ! next integration point
     EDIP = CSRX(JR) * EXL + SNRX(JR) * EYL    ! resolve components along Rx dipole
     BFD(JF,JR,JS,1) =  CMPLX (EDIP)
     BFD(JF,JR,JS,2:3) =  (0.,0.)
   END DO                                      ! next integration point

   IF (SOURCE_TYPE == 2) THEN       ! Include conservative terms for open segment.
     DO JR = 1, NRXTX(JS)               !  Sum over dipole receivers
       IF (RXID(JR,JS) /= 2) CYCLE     ! This subroutine is only used for electric dipole receivers
       EXL = ZERO;  EYL = ZERO
       DO JG = 1,5                  !  Sum over Simpson integration points
         YD = REAL (YWRX(NVRL,JG,JR),KIND=QL)
         X1D = REAL (XWRX(NVRL,1,JG,JR), KIND=QL)   ! for open loop segment.
         X2D = REAL (XWRX(NVRL,2,JG,JR), KIND=QL)
         RHO1 = SQRT (X1D**2 + YD**2)
         RHO2 = SQRT (X2D**2 + YD**2)
         CALL CDCUBVAL (RHOTRP,HL2R,HL2I,NRHS,RHO1,CDS1)
         CALL CDCUBVAL (RHOTRP,HL2R,HL2I,NRHS,RHO2,CDS2)
         CPHID = REAL (CPHI(NVRL),KIND=QL)
         SPHID = REAL (SPHI(NVRL),KIND=QL)

         EX1 = X2D * CDS2 - X1D * CDS1    ! Fields in rotated system
         EY1 = YD * (CDS2 - CDS1)
         EX2 = EX1 * CPHID - EY1 * SPHID                    ! Fields in survey system
         EY2 = EY1 * CPHID + EX1 * SPHID

         EXL = EXL + WRXD(JG,JR) * EX2                      ! Simpson integration
         EYL = EYL + WRXD(JG,JR) * EY2

!        CALL CDCUBVAL (RHOTRP,HL3R,HL3I,NRHS,RHO1,CDS1)
!        CALL CDCUBVAL (RHOTRP,HL3R,HL3I,NRHS,RHO2,CDS2)
!        EZL = EXL + WRXD(JG,JR) * (CDS2 - CDS1)

       END DO
       EDIP = CSRX(JR) * EXL + SNRX(JR) * EYL              ! resolve components along dipole
       BFD(JF,JR,JS,1) = BFD(JF,JR,JS,1) + CMPLX (EDIP)  ! add charge & induction contributions
     END DO
   END IF
 END DO     !  Next frequency

 END SUBROUTINE HSLPED

 SUBROUTINE HSMDB (SXLYR,RXLYR,NFRQ,FREQ,ZS,SXDP1,SXAZM1,ZR,XRX,YRX,NLYR,THKD,DPTHL, &
                   RES,RMUD,REPS,CHRG,CTAU,CFREQ,JS,JR,NTX,MRXTX,BFD)
!----------------------------------------------------------------------------------

!***  Called by HSBOSS
!***  Calls HSMDB_HNK

!  Computes the frequency-domain layered earth magnetic field in RXLYR due to a
!  magnetic dipole transmitter (unit area/moment) in SXLYR.
!  SXLYR & RXLYR can have any value from 0 to NLYR

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!  SIGN CONVENTION:
!  ----------------
!  The normal layered earth field coordinate system used in this
!  subroutine has X (JC=1) positive north, Y (JC=2) positive east
!  and Z (JC=3) positive down.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

!                             Input
!                             -----
!        SXLYR - layer containing transmitter
!        RXLYR - layer containing receiver
!         FREQ - array of NFRQ frequencies
!           ZS - depth of dipole transmitter (negative above earth)
!        SXDP1 - dip of transmitter (radians)
!       SXAZM1 - azimuth of transmitter (radians)
!           ZR - receiver depth
!          XRX - north receiver offset from transmitter
!          YRX - east receiver offset from transmitter
!         NLYR - number of layers
!         THKD - layer thicknesses
!        DPTHL - depth to top of layer
!          RES - layer resistivities
!         RMUD - mu(i) / mu(0)   (i = 0:NLYR)
!         REPS - array of relative dislectric constants
!         CHRG - C-C chargeability
!         CTAU - C-C time constant in seconds
!        CFREQ - C-C frequency constant
!
!          NTX - number of transmitters
!        MRXTX - maximum number of receivers per transmitter
!
!                             Output
!                             ------
!  BFD(JF,JR,JS,I) - the Ith component of the layered earth response at
!                    frequency JF, receiver JR, transmitter JS.
!           I = 1,2,3 = north, east and vertical componsnts respectively

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL, 0._QL), ONE=(1._QL, 0._QL), TWO=(2._QL, 0._QL)
 INTEGER NFRQ,MRXTX,NTX,NLYR,KFG,RXLYR,SXLYR,JF,JS,JR,NINTG,I1
 REAL FREQ(NFRQ),FRQ,SXDP1,SXAZM1,XRX,YRX,RHO,XRP,YRP
 REAL, DIMENSION(NLYR) :: RES,REPS,CTAU,CFREQ,CHRG
 REAL(KIND=QL) ZS,ZR,RHOD,XRPD,YRPD,XBAR,YBAR,XBARSQ,CAZD,SAZD,CDPD,SDPD, &
               RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR)
 COMPLEX(KIND=QL) SIGL(NLYR),KSQL(NLYR),HLYR(6),BZZ,BXX,BZX,BXZ,BYX,BYZ,BX,BY,BZ,BZZD,BXXD, &
                  BZXD,BXZD,BYXD,BYZD,BFDD(NFRQ,MRXTX,NTX,3)
 COMPLEX BFD(NFRQ,MRXTX,NTX,3)

!  This routine will produce B for unit TX moment so VFAC0 = mu0 / (4 * PI)

!  The initial computations occur in a coordinate system whose X component
!  lies in the direction of transmitter tilt.  In other words we rotate
!  the system by SXAZM

 I1 = 1
 CALL SET_KFG (I1,NLYR,SXLYR,RXLYR,KFG)

 NINTG = 5
 IF (RXLYR > 0 .AND. SXLYR > 0) NINTG = 6

 HLYR = ZERO

 BZZD = ZERO
 BZXD = ZERO
 BXXD = ZERO
 BXZD = ZERO
 BYXD = ZERO
 BYZD = ZERO

 XRP = XRX * COS (SXAZM1) + YRX * SIN (SXAZM1)
 YRP = YRX * COS (SXAZM1) - XRX * SIN (SXAZM1)
 XRPD = REAL (XRP, KIND=QL)
 YRPD = REAL (YRP, KIND=QL)

 RHO = SQRT(XRX**2 + YRX**2)
 RHOD = REAL (RHO, KIND=QL)

 IF (RHOD > 1.D-2) THEN
   XBAR = XRPD / RHOD
   YBAR = YRPD / RHOD
 ELSE
   RHOD = 0.01_QL
   XBAR = 0._QL
   YBAR = 0._QL
 END IF
 XBARSQ = XBAR**2

 DO JF = 1,NFRQ
   FRQ = FREQ(JF)
   CALL COLRES_1D (FRQ,NLYR,RES,REPS,RMUD,CHRG,CTAU,CFREQ,SIGL,KSQL)

   IF (RXLYR == SXLYR) CALL B_DIRECT

   CALL HSMDB_HNK (KFG,SXLYR,RXLYR,NLYR,THKD,DPTHL,SIGL,KSQL,RMUD,ZS,ZR,RHOD,NINTG,HLYR)

   BZZ = BZZD + HLYR(1)
   BXZ = BXZD - XBAR * HLYR(2)
   BYZ = BYZD - YBAR * HLYR(2)
   BZX = BZXD - XBAR * HLYR(3)
   BXX = BXXD - (ONE - TWO* XBARSQ) * HLYR(4) - XBARSQ * HLYR(5)
   BYX = BYXD + XBAR*YBAR * (TWO * HLYR(4) - HLYR(5))

   IF (NINTG == 6) BXX = BXX - HLYR(6)  ! add HED terms

   CDPD = REAL (COS (SXDP1),KIND=QL)
   SDPD = REAL (SIN (SXDP1),KIND=QL)
   CAZD = REAL (COS (SXAZM1),KIND=QL)
   SAZD = REAL (SIN (SXAZM1),KIND=QL)

   BX = BXZ * CDPD + BXX * SDPD
   BY = BYZ * CDPD + BYX * SDPD
   BZ = BZZ * CDPD + BZX * SDPD

! Rotate these from the system where X points to the Tx azimuth
! to the North, East, Z system.  Retain 12 digit accuracy.

   BFDD(JF,JR,JS,1) = BX * CAZD - BY * SAZD
   BFDD(JF,JR,JS,2) = BY * CAZD + BX * SAZD
   BFDD(JF,JR,JS,3) = BZ

   BFD(JF,JR,JS,1:3) = CMPLX (BFDD(JF,JR,JS,1:3) ) ! Convert to standard precision
 END DO

  CONTAINS

   SUBROUTINE B_DIRECT
!  --------------------

! If both transmitter and receiver are in layer RXLYR, B_DIRECT computes the direct
! magnetic fields in a whole space with the geoelectric properties of layer RXLYR.

   IMPLICIT NONE
   COMPLEX(KIND=QL), PARAMETER :: THREE=(3._QL, 0._QL)
   REAL(KIND=QL) RSQ,RR,R3,XRD,YRD,ZRD
   COMPLEX(KIND=QL) Q,QSQ,FAC,B0,B1

   RSQ = RHOD**2 + (ZS - ZR)**2
   RR = SQRT (RSQ)
   R3 = RR * RSQ
   XRD = XRP / RR
   YRD = YRP / RR
   ZRD = (ZR - ZS) / RR
   IF (RXLYR == 0) THEN
     FAC = (1.0D-7, 0.D0) / (R3)
     BZZD = FAC * (THREE*ZRD**2 - ONE)
     BXZD = THREE * FAC * XRD * ZRD
     BYZD = THREE * FAC * YRD * ZRD
     BXXD = FAC * (THREE*XRD**2 - ONE)
     BZXD = THREE * FAC * ZRD * XRD
     BYXD = THREE * FAC * YRD * XRD
   ELSE
     QSQ = RSQ * KSQL(RXLYR)
     Q = SQRT (QSQ)
     FAC = 1.0D-7 * RMUD(SXLYR) * EXP (-Q) / (R3)
     B0 = FAC * (ONE + Q + QSQ)
     B1 = FAC * (3.*(ONE + Q) + QSQ)
     BZZD = B1 * ZRD**2  - B0
     BXXD = B1 * XRD**2  - B0
     BZXD = B1 * XRD * ZRD
     BYXD = B1 * YRD * XRD
     BXZD = BZXD
     BYZD = B1 * YRD * ZRD
   END IF
   END SUBROUTINE B_DIRECT

 END SUBROUTINE HSMDB

 SUBROUTINE HSMDB_HNK (KFG,SXLYR,RXLYR,NLYR,THKD,DPTHL,SIGL,KSQL,RMUD,ZS,ZR,RHOD,NINTG,HLYR)
!-------------------------------------------------------------------------------------------

!***  Calls HSMDB_KER, HSMD0B_KER, HSMDNB_KER
!***  Called by HSMDB

!  Computes transform integrals HLYR (1:NINTG) which are used to compute vertical
!  and horizontal frequency-domain magnetic field components at the RX from
!  VMD and HMD sources.  It evaluates the Hankel transform integral using a
!  15 points per decade filter coefficient set derived from Christensen's
!  FLTGEN program.

!    RHOD - horizontal TX -> RX distance.
!     KER - stores kernel values from HSMDB_KER

!  NLYR,SIGL,KSQL,RMUD,RXLYR,SXLYR,ZS,ZR
!  are described in HSMDB
!
!    OUTPUT is HLYR(1:NINTG)  forward model components

 USE FILTER_COEFFICIENTS

 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: VFAC0=1.0D-7
 INTEGER NLYR,KFG,RXLYR,SXLYR,NINTG,L
 REAL(KIND=QL) RMUD(0:NLYR),THKD(NLYR),DPTHL(NLYR),DEL_JN,RHO_JN,Y,LMBDA,RHOD,ZS,ZR
 COMPLEX(KIND=QL) SIGL(NLYR),KSQL(NLYR),HLYR(NINTG)
 LOGICAL JUMP

 DEL_JN = LOG (10.D0)/ DBLE (NDEC_JN)
 RHO_JN = -LOG (RHOD) - SHFTJN

 HLYR = (0._QL, 0._QL)
 DO L = -50, JNHI             ! Start at L = -50 to pick up low values.
   Y = RHO_JN + DBLE(L) * DEL_JN
   LMBDA = EXP (Y)
   CALL HSMDB_KER (L,LMBDA,NLYR,THKD,DPTHL,SIGL,KSQL,RMUD,KFG,RXLYR,SXLYR,ZS,ZR,RHOD,NINTG,HLYR,JUMP)
   IF (JUMP .AND. L > -40) EXIT
 END DO

 JUMP = .FALSE.           ! Finish off the low end for RHOTRP(1)
 DO L = -51, JNLO, -1
   Y = RHO_JN + DBLE (L) * DEL_JN
   LMBDA = EXP (Y)
   CALL HSMDB_KER (L,LMBDA,NLYR,THKD,DPTHL,SIGL,KSQL,RMUD,KFG,RXLYR,SXLYR,ZS,ZR,RHOD,NINTG,HLYR,JUMP)
   IF (JUMP .AND. L < -60) EXIT
 END DO

 HLYR = VFAC0 * HLYR / RHOD

 END SUBROUTINE HSMDB_HNK

 SUBROUTINE HSMDB_KER (L,LMBDA,NLYR,THKD,DPTHL,SIGL,KSQL,RMUD,KFG,RXLYR,SXLYR,ZS,ZR,RHOD,NINTG,HLYR,JUMP)
!--------------------------------------------------------------------------------------------------------

!***  Called by HSMDB_HNK
!***  Calls PROPAGATE

!  Computes the frequency-domain layered-earth response kernel QFD for subsurface magnetic
!  dipole transmitters above basement for magnetic dipole receivers in any layer.
!________________________________________________________________________________
!  IMPORTANT NOTE: DPTHL(J) is the depth from the surface to the TOP of layer J.
!---------------------------------------------------------------------------------
!          Input
!          -----
!       L - Hankel filter index
!   LMBDA = Hankel transform variable
!    NLYR - number of layers
!     RES - layer resistivities
!    REPS - array of relative dielectric constants
!    RMUD - mu(i) / mu(0)
!    THKD - thickness of layer J
!   DPTHL - depth to TOP of layer J
!   RXLYR - layer containing receiver
!   SXLYR - layer containing transmitter
!      ZS - depth of dipole transmitter (negative above earth)
!      ZR - receiver depth relative to surface
!    RHOD - horizontal receiver - transmitter separation
!   NINTG - number of integrals
!
!          Output for receiver in layer i
!          ------
!  HLYR(1:NINTG) updated frequency-Hankel domain integrals for either frequency or
!  time-domain responses

 USE FILTER_COEFFICIENTS
 IMPLICIT NONE
 REAL(KIND=QL), PARAMETER :: TOL=1.D-6, TOL2=1.D-35
 COMPLEX(KIND=QL), PARAMETER :: ZERO=(0._QL,0._QL)
 INTEGER L,NLYR,KFG,RXLYR,SXLYR,J,NINTG
 REAL(KIND=QL) RMUD(0:NLYR),LMBDA,LMBDA2,LMBDA3,ZS,ZR,RHOD,QR,QI,RM,RL
 REAL(KIND=QL), DIMENSION (NLYR) :: THKD,DPTHL
 COMPLEX(KIND=QL) XI_V,XI_H,F_V,ETA_V,ETA_H,G_V,QFD(5),XP1,SL,SM,KSQL(NLYR), &
                  SIGL(NLYR),S(0:NLYR),FW(NINTG),HLYR(NINTG)
 LOGICAL JUMP

 CALL MDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                 ZS,S,XI_V,XI_H,F_V,ETA_V,ETA_H,G_V)

 SL = S(RXLYR)
 SM = S(SXLYR)
 RL = RMUD(RXLYR)
 RM = RMUD(SXLYR)
 LMBDA2 = LMBDA**2
 LMBDA3 = LMBDA**3
 XP1 = ZERO
 IF (RXLYR < NLYR) THEN
   XP1 = EXP (SL * (ZR - DPTHL(RXLYR+1)))
   F_V  = XP1 * F_V
   XI_V = XP1 * XI_V
   XI_H = XP1 * XI_H
 END IF
 IF (RXLYR > 0) THEN
   XP1 = EXP (SL * (DPTHL(RXLYR) - ZR))
   G_V   = XP1 * G_V
   ETA_V = XP1 * ETA_V
   ETA_H = XP1 * ETA_H
 END IF

!  Accumulate Hankel transform integrals & check convergence

 QFD(1) = RM * (XI_V + ETA_V) * LMBDA3 / SM          ! Bzz
 QFD(2) = RM * (XI_V - ETA_V) * LMBDA2 * SL / SM     ! Bxz and Byz
 QFD(3) = RM * (XI_H + ETA_H) * LMBDA2               ! Bzx and Bzy
 QFD(4) = RM * (XI_H - ETA_H) * SL
 FW(1) = WJ0(L) * QFD(1)
 FW(2) = WJ1(L) * QFD(2)
 FW(3) = WJ1(L) * QFD(3)

 IF (NINTG == 6) THEN                       ! F potential terms for Bxx, Byy, and Bxy
   QFD(5) = RL * (F_V + G_V) * KSQL(SXLYR) / SM
   QFD(4) = QFD(4) - QFD(5)
   FW(6) = WJ0(L) * QFD(5) * LMBDA
 END IF
 FW(4) = WJ1(L) * QFD(4) / RHOD
 FW(5) = WJ0(L) * QFD(4) * LMBDA

 HLYR(1:NINTG) = HLYR(1:NINTG) + FW(1:NINTG)

 JUMP = .TRUE.
 DO J = 1,NINTG
   QR = ABS (REAL  (HLYR(J) ) )
   QI = ABS (AIMAG (HLYR(J) ) )
   IF (QR > TOL2 .AND. ABS (REAL  (FW(J))) > TOL * QR) JUMP = .FALSE.
   IF (QI > TOL2 .AND. ABS (AIMAG (FW(J))) > TOL * QI) JUMP = .FALSE.
 END DO

 END SUBROUTINE HSMDB_KER

 SUBROUTINE EDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                       ZS,S,XI_V,F_V,F_H,ETA_V,G_V,G_H)
!---------------------------------------------------------------------------

!***  Called by
!***  Calls PROPAGATE

!  For an arbitrarily oriented electric dipole source at ZS in layer SXLYR of a
!  halfspace with NLYR-1 layers above basement, EDSX_COEF computes the coefficients
!  for the vertical and horizontal F (electric) and G (magnetic) potentials
!  These coefficients can be used to compute magnetic and electric fields.
!
!          Input
!          -----
!  KFG indicates source type and layer locations of source and receiver: air, basement or intermediate
!
!  KFG = IJK : I = type; J = Tx layer; K = Rx layer
!
!        I = 2 => electric dipole source
!
!        J = 0 => Transmitter in air
!        J = 3 => Transmitter in basement
!
!        K = 0 => Receiver in air
!        K = 3 => Receiver in basement
!
!        When BOTH tranamitter nor receiver are in an intermediate layer:
!
!        JK = 11 => Transmitter and recever are in the same layer
!        JK = 12 => Transmitter layer is above receiver layer
!        JK = 21 => Transmitter layer is below receiver layer
!
!   RXLYR - layer containing receiver
!   SXLYR - layer containing transmitter
!   LMBDA - Hankel transform variable
!    NLYR - number of layers
!    THKD - thickness of layer J
!   DPTHL - depth to TOP of layer J
!    RMUD - mu(i) / mu(0)
!    SIGL - complex conductivity including Cole-Cole + dimagnetic terms
!    KSQL - iwu * SIGL
!      ZS - depth of dipole transmitter (negative above earth)
!
!          Output
!          ------
!             S - Layer property variable in Hankel space
!   XI_V, ETA_V - coefficients for the vertical magnetic Schelkunoff potentials.
!
!   F_V,  G_V   - coefficients for the vertical electric Schelkunoff potentials.
!   F_H,  G_H   - coefficients for the horizontal electric Schelkunoff potential
!
!  P (F_V, F_H, XI_V)  are all used in the generic form:  P * EXP (S(RXLYR) * (ZR - DPTHL(RXLYR+1)))
!  Q (G_V, G_H, ETA_V) are all used in the generic form:  Q * EXP (S(RXLYR) * (DPTHL(RXLYR) - ZR))
!                               where ZR = receiver depth relative to surface

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 COMPLEX(KIND=QL), PARAMETER :: ONE=(1._QL,0._QL),ZERO=(0._QL,0._QL)
 INTEGER NLYR,KFG,RXLYR,SXLYR,J
 REAL(KIND=QL) RMUD(0:NLYR),LMBDA,ZS
 REAL(KIND=QL), DIMENSION (NLYR) ::  THKD,DPTHL,RMUSQ
 COMPLEX(KIND=QL) SM,SL,VLG,VMG,AMG,VLF,VMF,AMF,XPA,XP1,XP2,XQ1,XQ2,DENOMF,DENOMG, &
                  PRJG,PRJF,XI_V,F_V,F_H,ETA_V,G_V,G_H,XI_VBAR, &
                  F_VBAR,F_HBAR,ETA_VBAR,G_VBAR,G_HBAR,LMBSQ,VACHI
 COMPLEX(KIND=QL), DIMENSION (0:NLYR) :: S,T,R,AF,AG,VF,VG,CHI
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL

 S = ZERO; T = ZERO; R = ZERO; AF = ZERO; AG = ZERO; VF = ZERO; VG = ZERO; CHI = ONE
 F_V = ZERO;  F_H = ZERO;  XI_V  = ZERO;
 G_V = ZERO;  G_H = ZERO;  ETA_V = ZERO;


! VF & VG are the downward propagators for the F & G potentials respectively.
! AF & AG are the upward propagators.
! T & R are the magnetic and magnetic reflection coefficients


 LMBSQ = CMPLX (LMBDA * LMBDA, 0._QL, KIND=QL)

 S(0) = CMPLX (LMBDA, 0._QL,KIND=QL)
 DO J = 1,NLYR
   S(J) = SQRT (KSQL(J) + LMBSQ)
   RMUSQ(J) = RMUD(J) * RMUD(J)
 END DO
 T(0) = ( (RMUSQ(1) - 1._QL) * LMBSQ - KSQL(1) ) / ( RMUD(1)*S(0) + S(1) )**2
 R(0) = ONE
 DO J = 1,NLYR-1
   T(J) = (RMUSQ(J+1) - RMUSQ(J)) * LMBSQ + (RMUSQ(J+1) * KSQL(J) - RMUSQ(J) * KSQL(J+1)) &
        / (RMUD(J+1) * S(J) + RMUD(J) * S(J+1))**2
   R(J) = (SIGL(J+1) * S(J) - SIGL(J) * S(J+1)) / (SIGL(J+1) * S(J) + SIGL(J) * S(J+1))
   CHI(J) = EXP (-2._QL * S(J) * THKD(J))
 END DO

 SM = S(SXLYR)
 SL = S(RXLYR)

 SELECT CASE (KFG)

 CASE (200)                                  ! ED Tx in Air - Rx in Air
   CALL PROPAGATE (0)
   XI_V = VMG * EXP (SM * ZS)

 CASE (201)                                  ! ED Tx in Air - Rx in Layer
   XPA = SM * ZS
   DO J = 1, RXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG
   CALL PROPAGATE (0)
   DO J = 0, RXLYR-1
     PRJG = PRJG * (ONE + VG(J)) / (ONE + VG(J+1) * CHI(J+1))
     PRJF = PRJF * (ONE + VF(J)) / (ONE + VF(J+1) * CHI(J+1))
   END DO
   ETA_V = PRJG
   G_V = PRJF
   G_H = -G_V
   XP1 = EXP (-SL * THKD(RXLYR))
   XI_V  = VLG * XP1 * ETA_V
   F_V  = VLF * XP1 * G_V
   F_H = -F_V

 CASE (203)                                  ! ED Tx in Air - Rx in Basement
   XPA = SM * ZS
   DO J = 1, RXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG
   CALL PROPAGATE (0)
   DO J = 0, RXLYR-1
     PRJG = PRJG * (ONE + VG(J)) / (ONE + VG(J+1) * CHI(J+1))
     PRJF = PRJF * (ONE + VF(J)) / (ONE + VF(J+1) * CHI(J+1))
   END DO
   ETA_V = PRJG
   G_V = PRJF
   G_H = -G_V

 CASE (210)                                   ! ED Tx in Layer - Rx in Air
   XP1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   XP2 = EXP (SM * (ZS - DPTHL(SXLYR+1) - THKD(SXLYR)))
   CALL PROPAGATE (1)
   XI_VBAR = (XP1 + VMG * XP2) / DENOMG

   XPA = ZERO
   DO J = RXLYR+1, SXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)

   DO J = RXLYR, SXLYR-1
     PRJG = PRJG * (ONE + AG(J+1)) / (ONE + AG(J) * CHI(J))
   END DO
   XI_V = PRJG * XI_VBAR

 CASE (230)                              ! ED Tx in basement - Rx in Air
   XPA = ZERO
   DO J = RXLYR+1, NLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)

   CALL PROPAGATE (2)
   DO J = RXLYR, SXLYR-1
     PRJG = PRJG * (ONE + AG(J+1)) / (ONE + AG(J) * CHI(J))
   END DO
   XI_V = PRJG * EXP (SM * (DPTHL(SXLYR) - ZS))

 CASE (211)                                  ! ED Tx in Rx Layer
   XP1 = EXP (SM * (DPTHL(SXLYR) - THKD(SXLYR) - ZS))
   XP2 = EXP (SM * (ZS - DPTHL(SXLYR+1)))
   XQ1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   XQ2 = EXP (SM * (ZS - DPTHL(SXLYR+1) - THKD(SXLYR)))

   CALL PROPAGATE (1)
   F_V  = VMF * (AMF * XP1 + XP2) / DENOMF
   F_H  = VMF * (AMF * XP1 - XP2) / DENOMF
   XI_V = VMG * (AMG * XP1 + XP2) / DENOMG

   G_V   = AMF * (XQ1 + VMF * XQ2) / DENOMF
   G_H   = AMF * (XQ1 - VMF * XQ2) / DENOMF
   ETA_V = AMG * (XQ1 + VMG * XQ2) / DENOMG

 CASE (212)                                  ! ED Tx Layer above Rx Layer
   XQ1 = EXP (SM * (DPTHL(SXLYR) - ZS - THKD(SXLYR)))
   XQ2 = EXP (SM * (ZS - DPTHL(SXLYR+1)))
   CALL PROPAGATE (1)
   G_VBAR   = (AMF * XQ1 + XQ2) / DENOMF
   G_HBAR   = (AMF * XQ1 - XQ2) / DENOMF
   ETA_VBAR = (AMG * XQ1 + XQ2) / DENOMG

   XPA = ZERO
   DO J = SXLYR+1, RXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG
   DO J = SXLYR, RXLYR-1
     PRJG = PRJG * (ONE + VG(J)) / (ONE + VG(J+1) * CHI(J+1))
     PRJF = PRJF * (ONE + VF(J)) / (ONE + VF(J+1) * CHI(J+1))
   END DO
   G_V   = PRJF * G_VBAR
   G_H   = PRJF * G_HBAR
   ETA_V = PRJG * ETA_VBAR
   XP1   = EXP (-SL * THKD(RXLYR))
   F_V   = VLF * XP1 * G_V
   F_H   = VLF * XP1 * G_H
   XI_V  = VLG * XP1 * ETA_V

 CASE (213)                                  ! ED Tx in Layer - Rx in Basement
   XQ1 = EXP (SM * (DPTHL(SXLYR) - ZS - THKD(SXLYR)))
   XQ2 = EXP (SM * (ZS - DPTHL(SXLYR+1)))
   CALL PROPAGATE (1)
   ETA_VBAR = (AMG * XQ1 + XQ2) / DENOMG
   G_VBAR   = (AMF * XQ1 + XQ2) / DENOMF
   G_HBAR   = (AMF * XQ1 - XQ2) / DENOMF

   XPA = ZERO
   DO J = SXLYR+1, RXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG
   DO J = SXLYR, RXLYR-1
     PRJG = PRJG * (ONE + VG(J)) / (ONE + VG(J+1) * CHI(J+1))
     PRJF = PRJF * (ONE + VF(J)) / (ONE + VF(J+1) * CHI(J+1))
   END DO
   G_V   = PRJF * G_VBAR
   G_H   = PRJF * G_HBAR
   ETA_V = PRJG * ETA_VBAR

 CASE (221)                                  ! ED Tx Layer below Rx Layer
   XP1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   XP2 = EXP (SM * (ZS - DPTHL(SXLYR+1) - THKD(SXLYR)))
   CALL PROPAGATE (1)
   F_VBAR  = (XP1 + VMF * XP2) / DENOMF
   F_HBAR  = (XP1 - VMF * XP2) / DENOMF
   XI_VBAR = (XP1 + VMG * XP2) / DENOMG

   XPA = ZERO
   DO J = RXLYR+1, SXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG

   DO J = RXLYR, SXLYR-1
     PRJG = PRJG * (ONE + AG(J+1)) / (ONE + AG(J) * CHI(J))
     PRJF = PRJF * (ONE + AF(J+1)) / (ONE + AF(J) * CHI(J))
   END DO
   F_V  = PRJF * F_VBAR
   F_H  = PRJF * F_HBAR
   XI_V = PRJG * XI_VBAR

   XQ1 = EXP (-SL * THKD(RXLYR))
   G_V   = AF(RXLYR) * XQ1 * F_V
   G_H   = AF(RXLYR) * XQ1 * F_H
   ETA_V = AG(RXLYR) * XQ1 * XI_V

 CASE (231)                                  ! ED Tx in basement - Rx in Layer
   XPA = ZERO
   DO J = RXLYR+1, NLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG

   CALL PROPAGATE (2)
   DO J = RXLYR, NLYR-1
     PRJG = PRJG * (ONE + AG(J+1)) / (ONE + AG(J) * CHI(J))
     PRJF = PRJF * (ONE + AF(J+1)) / (ONE + AF(J) * CHI(J))
   END DO

   XP1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   F_V  = PRJF * XP1
   F_H  = F_V
   XI_V = PRJG * XP1

   XQ1 = EXP (-SL * THKD(RXLYR))
   G_V   = AF(RXLYR) * XQ1 * F_V
   G_H   = G_V
   ETA_V = AG(RXLYR) * XQ1 * XI_V

 CASE (233)                                 ! ED Tx in basement - Rx in Basement
   XQ1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   CALL PROPAGATE (2)
   ETA_V = AMG * XQ1
   G_V   = AMF * XQ1
   G_H   = G_V
 END SELECT

 CONTAINS

   SUBROUTINE PROPAGATE (LSX)
!  --------------------------

   INTEGER LSX

   SELECT CASE (LSX)
   CASE (0)                  !  SXLYR = 0 - Build downward source propagator only
     VG(NLYR-1) = T(NLYR-1)
     VF(NLYR-1) = R(NLYR-1)
     DO J = NLYR-2, 0, -1
       VACHI = VG(J+1) * CHI(J+1)
       VG(J) = (VACHI + T(J)) / (ONE + VACHI* T(J))
       VACHI = VF(J+1) * CHI(J+1)
       VF(J) = (VACHI + R(J)) / (ONE + VACHI* R(J))
     END DO
     VLG = VG(RXLYR)
     VMG = VG(0)
     VLF = VF(RXLYR)
     VMF = VF(0)

   CASE (2)                 ! SXLYR = NLYR - Build upward source propagator only
     AG(1) = -T(0)
     AF(1) = -ONE
     DO J = 2, NLYR
       VACHI = AG(J-1) * CHI(J-1)
       AG(J) = (VACHI - T(J-1)) / (ONE - VACHI* T(J-1))
       VACHI = AF(J-1) * CHI(J-1)
       AF(J) = (VACHI - R(J-1)) / (ONE - VACHI* R(J-1))
     END DO
     AMG = AG(NLYR)
     AMF = AF(NLYR)

   CASE (1)                     ! Build downward and upward source propagators
     VG(NLYR-1) = T(NLYR-1)
     VF(NLYR-1) = R(NLYR-1)
     DO J = NLYR-2, 0, -1
       VACHI = VG(J+1) * CHI(J+1)
       VG(J) = (VACHI + T(J)) / (ONE + VACHI* T(J))
       VACHI = VF(J+1) * CHI(J+1)
       VF(J) = (VACHI + R(J)) / (ONE + VACHI* R(J))
     END DO

     AG(1) = -T(0)
     AF(1) = -ONE
     DO J = 2, NLYR
       VACHI = AG(J-1) * CHI(J-1)
       AG(J) = (VACHI - T(J-1)) / (ONE - VACHI* T(J-1))
       VACHI = AF(J-1) * CHI(J-1)
       AF(J) = (VACHI - R(J-1)) / (ONE - VACHI* R(J-1))
     END DO
     VLG = VG(RXLYR)
     VLF = VF(RXLYR)
     VMG = VG(SXLYR)
     VMF = VF(SXLYR)
     AMG = AG(SXLYR)
     AMF = AF(SXLYR)
     DENOMG = ONE - VMG * AMG * CHI(SXLYR)
     DENOMF = ONE - VMF * AMF * CHI(SXLYR)

   END SELECT

   END SUBROUTINE PROPAGATE

 END SUBROUTINE EDSX_COEF

 SUBROUTINE MDSX_COEF (SXLYR,RXLYR,KFG,LMBDA,NLYR,THKD,DPTHL,RMUD,SIGL,KSQL, &
                       ZS,S,XI_V,XI_H,F_V,ETA_V,ETA_H,G_V)
!---------------------------------------------------------------------------

!***  Called by
!***  Calls PROPAGATE

!  For an arbitrarily oriented magnetic dipole source at ZS in layer SXLYR of a
!  halfspace with NLYR-1 layers above basement, MDSX_COEF computes the coefficients
!  for the vertical and horizontal F (electric) and G (magnetic) potentials
!  These coefficients can be used to compute magnetic and electric fields.
!
!          Input
!          -----
!  KFG indicates source type and layer locations of source and receiver: air, basement or intermediate
!
!  KFG = IJK : I = type; J = Tx layer; K = Rx layer
!
!        I = 1 => magnetic dipole source
!
!        J = 0 => Transmitter in air
!        J = 3 => Transmitter in basement
!
!        K = 0 => Receiver in air
!        K = 3 => Receiver in basement
!
!        When BOTH tranamitter nor receiver are in an intermediate layer:
!
!        JK = 11 => Transmitter and recever are in the same layer
!        JK = 12 => Transmitter layer is above receiver layer
!        JK = 21 => Transmitter layer is below receiver layer
!
!   RXLYR - layer containing receiver
!   SXLYR - layer containing transmitter
!   LMBDA - Hankel transform variable
!    NLYR - number of layers
!    THKD - thickness of layer J
!   DPTHL - depth to TOP of layer J
!    RMUD - mu(i) / mu(0)
!    SIGL - complex conductivity including Cole-Cole + dimagnetic terms
!    KSQL - iwu * SIGL
!      ZS - depth of dipole transmitter (negative above earth)
!
!          Output
!          ------
!             S - Layer property variable in Hankel space
!   XI_V, ETA_V - coefficients for the vertical magnetic Schelkunoff potentials.
!   XI_H, ETA_H - coefficients for the horizontal magnetic Schelkunoff potential
!
!   F_V,  G_V   - coefficients for the vertical electric Schelkunoff potentials.
!
!  P (F_V, XI_V,  XI_H)  are all used in the generic form:  P * EXP (S(RXLYR) * (ZR - DPTHL(RXLYR+1)))
!  Q (G_V, ETA_V, ETA_H) are all used in the generic form:  Q * EXP (S(RXLYR) * (DPTHL(RXLYR) - ZR))
!                               where ZR = receiver depth relative to surface

 IMPLICIT NONE
 INTEGER, PARAMETER :: QL=SELECTED_REAL_KIND(12,80)
 COMPLEX(KIND=QL), PARAMETER :: ONE=(1._QL,0._QL),ZERO=(0._QL,0._QL)
 INTEGER NLYR,KFG,RXLYR,SXLYR,J
 REAL(KIND=QL) RMUD(0:NLYR),LMBDA,ZS
 REAL(KIND=QL), DIMENSION (NLYR) ::  THKD,DPTHL,RMUSQ
 COMPLEX(KIND=QL) SM,SL,VLG,VMG,AMG,VLF,VMF,AMF,XPA,XP1,XP2,XQ1,XQ2,DENOMF,DENOMG, &
                  PRJG,PRJF,XI_V,XI_H,F_V,ETA_V,ETA_H,G_V,XI_VBAR,XI_HBAR, &
                  F_VBAR,ETA_VBAR,ETA_HBAR,G_VBAR,LMBSQ,VACHI
 COMPLEX(KIND=QL), DIMENSION (0:NLYR) :: S,T,R,AF,AG,VF,VG,CHI
 COMPLEX(KIND=QL), DIMENSION (NLYR) :: SIGL,KSQL

 S = ZERO; T = ZERO; R = ZERO; AF = ZERO; AG = ZERO; VF = ZERO; VG = ZERO; CHI = ONE
 F_V = ZERO;  XI_V  = ZERO;  XI_H  = ZERO
 G_V = ZERO;  ETA_V = ZERO;  ETA_H = ZERO


! VF & VG are the downward propagators for the F & G potentials respectively.
! AF & AG are the upward propagators.
! T & R are the magnetic and magnetic reflection coefficients


 LMBSQ = CMPLX (LMBDA * LMBDA, 0._QL, KIND=QL)
 S(0) = CMPLX (LMBDA, 0._QL,KIND=QL)
 DO J = 1,NLYR
   S(J) = SQRT (KSQL(J) + LMBSQ)
   RMUSQ(J) = RMUD(J) * RMUD(J)
 END DO
 T(0) = ( (RMUSQ(1) - 1._QL) * LMBSQ - KSQL(1) ) / ( RMUD(1)*S(0) + S(1) )**2
 R(0) = ONE
 DO J = 1,NLYR-1
   T(J) = (RMUSQ(J+1) - RMUSQ(J)) * LMBSQ + (RMUSQ(J+1) * KSQL(J) - RMUSQ(J) * KSQL(J+1)) &
        / (RMUD(J+1) * S(J) + RMUD(J) * S(J+1))**2
   R(J) = (SIGL(J+1) * S(J) - SIGL(J) * S(J+1)) / (SIGL(J+1) * S(J) + SIGL(J) * S(J+1))
   CHI(J) = EXP (-2._QL * S(J) * THKD(J))
 END DO

 SM = S(SXLYR)
 SL = S(RXLYR)

 SELECT CASE (KFG)
 CASE (100)                                  ! MD Tx in Air - Rx in Air
   CALL PROPAGATE (0)
   XI_V = VMG * EXP (SM * ZS)
   XI_H = -XI_V

 CASE (101)                                  ! MD Tx in air - Rx in Layer
   XPA = ZERO
   DO J = 1, RXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   CALL PROPAGATE (0)
   DO J = 0, RXLYR-1
     PRJG = PRJG * (ONE + VG(J)) / (ONE + VG(J+1) * CHI(J+1))
   END DO
   ETA_V = PRJG * EXP (SM * ZS)
   XI_V  = VLG * EXP (-SL * THKD(RXLYR)) * ETA_V
   XI_H  = -XI_V
   ETA_H = -ETA_V

 CASE (103)                                  ! MD Tx in Air - Rx in Basement
   XPA = ZERO
   DO J = 1, RXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   CALL PROPAGATE (0)
   PRJG = EXP (XPA)
   DO J = 0, RXLYR-1
     PRJG = PRJG * (ONE + VG(J)) / (ONE + VG(J+1) * CHI(J+1))
   END DO
   ETA_V = PRJG * EXP (SM * ZS)
   ETA_H = -ETA_V

 CASE (110)                                  ! MD Tx in Layer - Rx in Air
   XP1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   XP2 = EXP (SM * (ZS - DPTHL(SXLYR+1) - THKD(SXLYR)))
   CALL PROPAGATE (1)
   XI_VBAR = (XP1 + VMG * XP2) / DENOMG
   XI_HBAR = (XP1 - VMG * XP2) / DENOMG

   XPA = ZERO
   DO J = RXLYR+1, SXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)

   DO J = RXLYR, SXLYR-1
     PRJG = PRJG * (ONE + AG(J+1)) / (ONE + AG(J) * CHI(J))
   END DO
   XI_V = PRJG * XI_VBAR
   XI_H = PRJG * XI_HBAR

 CASE (130)                              ! MD Tx in basement - Rx in Air
   XPA = ZERO
   DO J = RXLYR+1, NLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)

   CALL PROPAGATE (2)
   DO J = RXLYR, SXLYR-1
     PRJG = PRJG * (ONE + AG(J+1)) / (ONE + AG(J) * CHI(J))
   END DO
   XI_V = PRJG * EXP (SM * (DPTHL(SXLYR) - ZS))
   XI_H = XI_V

 CASE (111)                                  ! MD Tx in Rx Layer
   XP1 = EXP (SM * (DPTHL(SXLYR) - THKD(SXLYR) - ZS))
   XP2 = EXP (SM * (ZS - DPTHL(SXLYR+1)))
   XQ1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   XQ2 = EXP (SM * (ZS - DPTHL(SXLYR+1) - THKD(SXLYR)))

   CALL PROPAGATE (1)
   XI_V = VMG * (AMG * XP1 + XP2) / DENOMG
   XI_H = VMG * (AMG * XP1 - XP2) / DENOMG
   F_V  = VMF * (AMF * XP1 + XP2) / DENOMF

   ETA_V = AMG * (XQ1 + VMG * XQ2) / DENOMG
   ETA_H = AMG * (XQ1 - VMG * XQ2) / DENOMG
   G_V   = AMF * (XQ1 + VMF * XQ2) / DENOMF

 CASE (112)                                  ! MD Tx Layer above Rx Layer
   XQ1 = EXP (SM * (DPTHL(SXLYR) - ZS - THKD(SXLYR)))
   XQ2 = EXP (SM * (ZS - DPTHL(SXLYR+1)))
   CALL PROPAGATE (1)
   ETA_VBAR = (AMG * XQ1 + XQ2) / DENOMG
   ETA_HBAR = (AMG * XQ1 - XQ2) / DENOMG
   G_VBAR   = (AMF * XQ1 + XQ2) / DENOMF

   XPA = ZERO
   DO J = SXLYR+1, RXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG
   DO J = SXLYR, RXLYR-1
     PRJG = PRJG * (ONE + VG(J)) / (ONE + VG(J+1) * CHI(J+1))
     PRJF = PRJF * (ONE + VF(J)) / (ONE + VF(J+1) * CHI(J+1))
   END DO
   ETA_V = PRJG * ETA_VBAR
   ETA_H = PRJG * ETA_HBAR
   G_V   = PRJF * G_VBAR
   XP1   = EXP (-SL * THKD(RXLYR))
   XI_V  = VLG * XP1 * ETA_V
   XI_H  = VLG * XP1 * ETA_H
   F_V   = VLF * XP1 * G_V

 CASE (113)                                  ! MD Tx in Layer - Rx in Basement
   XQ1 = EXP (SM * (DPTHL(SXLYR) - ZS - THKD(SXLYR)))
   XQ2 = EXP (SM * (ZS - DPTHL(SXLYR+1)))
   CALL PROPAGATE (1)
   ETA_VBAR = (AMG * XQ1 + XQ2) / DENOMG
   ETA_HBAR = (AMG * XQ1 - XQ2) / DENOMG
   G_VBAR   = (AMF * XQ1 + XQ2) / DENOMF

   XPA = ZERO
   DO J = SXLYR+1, RXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG
   DO J = SXLYR, RXLYR-1
     PRJG = PRJG * (ONE + VG(J)) / (ONE + VG(J+1) * CHI(J+1))
     PRJF = PRJF * (ONE + VF(J)) / (ONE + VF(J+1) * CHI(J+1))
   END DO
   ETA_V = PRJG * ETA_VBAR
   ETA_H = PRJG * ETA_HBAR
   G_V   = PRJF * G_VBAR

 CASE (121)                                  ! MD Tx Layer below Rx Layer
   XP1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   XP2 = EXP (SM * (ZS - DPTHL(SXLYR+1) - THKD(SXLYR)))
   CALL PROPAGATE (1)
   XI_VBAR = (XP1 + VMG * XP2) / DENOMG
   XI_HBAR = (XP1 - VMG * XP2) / DENOMG
   F_VBAR  = (XP1 + VMF * XP2) / DENOMF

   XPA = ZERO
   DO J = RXLYR+1, SXLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG

   DO J = RXLYR, SXLYR-1
     PRJG = PRJG * (ONE + AG(J+1)) / (ONE + AG(J) * CHI(J))
     PRJF = PRJF * (ONE + AF(J+1)) / (ONE + AF(J) * CHI(J))
   END DO
   XI_V = PRJG * XI_VBAR
   XI_H = PRJG * XI_HBAR
   F_V  = PRJF * F_VBAR

   XQ1 = EXP (-SL * THKD(RXLYR))
   ETA_V = AG(RXLYR) * XQ1 * XI_V
   ETA_H = AG(RXLYR) * XQ1 * XI_H
   G_V   = AF(RXLYR) * XQ1 * F_V

 CASE (131)                                  ! MD Tx in basement - Rx in Layer
   XPA = ZERO
   DO J = RXLYR+1, NLYR-1
     XPA = XPA - S(J) * THKD(J)
   END DO
   PRJG = EXP (XPA)
   PRJF = PRJG

   CALL PROPAGATE (2)
   DO J = RXLYR, NLYR-1
     PRJG = PRJG * (ONE + AG(J+1)) / (ONE + AG(J) * CHI(J))
     PRJF = PRJF * (ONE + AF(J+1)) / (ONE + AF(J) * CHI(J))
   END DO
   XP1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   F_V  = PRJF * XP1
   XI_V = PRJG * XP1
   XI_H = XI_V

   XQ1 = EXP (-SL * THKD(RXLYR))
   G_V   = AF(RXLYR) * XQ1 * F_V
   ETA_V = AG(RXLYR) * XQ1 * XI_V
   ETA_H = ETA_V

 CASE (133)                              ! MD Tx in basement - Rx in Basement
   XQ1 = EXP (SM * (DPTHL(SXLYR) - ZS))
   CALL PROPAGATE (2)
   ETA_V = AMG * XQ1
   G_V   = AMF * XQ1
   ETA_H = ETA_V
 END SELECT

 CONTAINS

   SUBROUTINE PROPAGATE (LSX)
!  --------------------------

   INTEGER LSX

   SELECT CASE (LSX)
   CASE (0)                  !  SXLYR = 0 - Build downward source propagator only
     VG(NLYR-1) = T(NLYR-1)
     VF(NLYR-1) = R(NLYR-1)
     DO J = NLYR-2, 0, -1
       VACHI = VG(J+1) * CHI(J+1)
       VG(J) = (VACHI + T(J)) / (ONE + VACHI* T(J))
       VACHI = VF(J+1) * CHI(J+1)
       VF(J) = (VACHI + R(J)) / (ONE + VACHI* R(J))
     END DO
     VLG = VG(RXLYR)
     VMG = VG(0)

   CASE (2)                 ! SXLYR = NLYR - Build upward source propagator only
     AG(1) = -T(0)
     AF(1) = -ONE
     DO J = 2, NLYR
       VACHI = AG(J-1) * CHI(J-1)
       AG(J) = (VACHI - T(J-1)) / (ONE - VACHI* T(J-1))
       VACHI = AF(J-1) * CHI(J-1)
       AF(J) = (VACHI - R(J-1)) / (ONE - VACHI* R(J-1))
     END DO
     AMG = AG(NLYR)
     AMF = AF(NLYR)

   CASE (1)                     ! Build downward and upward source propagators
     VG(NLYR-1) = T(NLYR-1)
     VF(NLYR-1) = R(NLYR-1)
     DO J = NLYR-2, 0, -1
       VACHI = VG(J+1) * CHI(J+1)
       VG(J) = (VACHI + T(J)) / (ONE + VACHI* T(J))
       VACHI = VF(J+1) * CHI(J+1)
       VF(J) = (VACHI + R(J)) / (ONE + VACHI* R(J))
     END DO

     AG(1) = -T(0)
     AF(1) = -ONE
     DO J = 2, NLYR
       VACHI = AG(J-1) * CHI(J-1)
       AG(J) = (VACHI - T(J-1)) / (ONE - VACHI* T(J-1))
       VACHI = AF(J-1) * CHI(J-1)
       AF(J) = (VACHI - R(J-1)) / (ONE - VACHI* R(J-1))
     END DO
     VLG = VG(RXLYR)
     VLF = VF(RXLYR)
     VMG = VG(SXLYR)
     VMF = VF(SXLYR)
     AMG = AG(SXLYR)
     AMF = AF(SXLYR)
     DENOMG = ONE - VMG * AMG * CHI(SXLYR)
     DENOMF = ONE - VMF * AMF * CHI(SXLYR)

   END SELECT

   END SUBROUTINE PROPAGATE

 END SUBROUTINE MDSX_COEF

!********************************************************************************************
!
!    Matlab / MEX code starts here.
!
!********************************************************************************************

! Include Matlab Fortran header:
#include "fintrf.h"

!********************************************************************************************
! Module containing subroutines for Matlab / MEX bits:
MODULE MEX_LEROI

  ! Name of the MEX function:
  CHARACTER(LEN = 9), PARAMETER :: MEXNAME = 'LEROI_TEM'

  CONTAINS

  !******************************************************************************************
  ! Error messaging subroutine:
  SUBROUTINE ERRORMESSAGE(MSGTYPE, MSG)
    ! No implicits:
    IMPLICIT NONE
    ! Standard message bits:
    CHARACTER(LEN = *), PARAMETER   :: TYPELABEL = 'MATLAB'
    CHARACTER(LEN = *), PARAMETER   :: TYPESEP   = ':'
    ! Inputs:
    CHARACTER(LEN = *), INTENT(IN)  :: MSGTYPE, MSG
    ! Character for error type:
    CHARACTER(LEN = :), ALLOCATABLE :: ERRTYPE
    ! Allocate ERRTYPE:
    ALLOCATE(CHARACTER(LEN = LEN(TYPELABEL) + LEN(TYPESEP) + LEN(MEXNAME) + &
                             LEN(TYPESEP) + LEN(MSGTYPE)) :: ERRTYPE)
    ! Put ERRTYPE message together:
    ERRTYPE = TYPELABEL // MEXNAME // TYPESEP // MSGTYPE
    ! Send error message to Matlab:
    CALL MEXERRMSGIDANDTXT(TRIM(ERRTYPE), MSG)
  END SUBROUTINE ERRORMESSAGE

  !******************************************************************************************
  ! Double checking:
  SUBROUTINE CHECKDOUBLE(MP, MSGTYPE, MSG, D)
    ! No implicits:
    IMPLICIT NONE
    ! Inputs:
    MWPOINTER                      :: MP
    CHARACTER(LEN = *), INTENT(IN) :: MSGTYPE, MSG
    ! Double output:
    MWPOINTER                      :: MPD
    REAL(KIND = 8), INTENT(OUT)    :: D
    ! Matlab functions:
    MWPOINTER                      :: MXISDOUBLE, MXISCOMPLEX
    MWPOINTER                      :: MXGETM, MXGETN
    MWPOINTER                      :: MXGETPR
    ! Check is scalar double:
    IF ((MXISDOUBLE(MP) .NE. 1) .OR.  &
        (MXISCOMPLEX(MP) .EQ. 1) .OR. &
        (MXGETM(MP) .NE. 1) .OR.      &
        (MXGETN(MP) .NE. 1)) THEN
      ! Send error message to Matlab:
      CALL ERRORMESSAGE(MSGTYPE, MSG)
    ELSE
      ! Get pointer:
      MPD = MXGETPR(MP)
      ! Copy value to D:
      call MXCOPYPTRTOREAL8(MPD, D, 1)
    END IF
  END SUBROUTINE CHECKDOUBLE

  !******************************************************************************************
  ! Integer checking:
  SUBROUTINE CHECKINTEGER(MP, MSGTYPE, MSG, I)
    ! No implicits:
    IMPLICIT NONE
    ! Inputs:
    MWPOINTER                      :: MP
    CHARACTER(LEN = *), INTENT(IN) :: MSGTYPE, MSG
    ! Integer output is actually a REAL ... :
    MWPOINTER                      :: MPI
    REAL(KIND = 8), INTENT(OUT)    :: I
    ! Matlab functions:
    MWPOINTER                      :: MXISDOUBLE, MXISCOMPLEX
    MWPOINTER                      :: MXGETM, MXGETN
    MWPOINTER                      :: MXGETPR
    ! Check is scalar double:
    IF ((MXISDOUBLE(MP) .NE. 1) .OR.  &
        (MXISCOMPLEX(MP) .EQ. 1) .OR. &
        (MXGETM(MP) .NE. 1) .OR.      &
        (MXGETN(MP) .NE. 1)) THEN
      ! Send error message to Matlab:
      CALL ERRORMESSAGE(MSGTYPE, MSG)
    ELSE
      ! Get pointer:
      MPI = MXGETPR(MP)
      ! Copy value to I:
      call MXCOPYPTRTOREAL8(MPI, I, 1)
      ! Check is integer:
      IF (I .NE. FLOOR(I)) THEN
        ! Send error message to Matlab:
        CALL ERRORMESSAGE(MSGTYPE, MSG)
      END IF
    END IF
  END SUBROUTINE CHECKINTEGER

  !******************************************************************************************
  ! 1D array checking:
  SUBROUTINE CHECK1DARRAY(MP, N, MSGTYPE, MSG, A)
    ! No implicits:
    IMPLICIT NONE
    ! Inputs:
    MWPOINTER                                              :: MP
    INTEGER, INTENT(IN)                                    :: N
    CHARACTER(LEN = *), INTENT(IN)                         :: MSGTYPE, MSG
    ! Characters for error message:
    CHARACTER(LEN = *), PARAMETER                          :: MSGPRE = ' Expected size: 1x'
    CHARACTER(LEN = 8)                                     :: MCHAR, NCHAR
    CHARACTER(LEN = :), ALLOCATABLE                        :: MSGOUT
    ! Output ... :
    MWPOINTER                                              :: MPA
    REAL(KIND = 8), ALLOCATABLE, DIMENSION(:), INTENT(OUT) :: A
    ! Matlab functions:
    MWPOINTER                                              :: MXISDOUBLE, MXISCOMPLEX
    MWPOINTER                                              :: MXGETM, MXGETN
    MWPOINTER                                              :: MXGETPR
    ! Write N value to NCHAR:
    WRITE(NCHAR, '(I0)'), N
    ! Allocate ERRTYPE:
    ALLOCATE(CHARACTER(LEN = LEN(MSG) + LEN(MSGPRE) + LEN(TRIM(NCHAR))) &
                             :: MSGOUT)
    ! Set MSGOUT:
    MSGOUT = MSG // MSGPRE // TRIM(NCHAR)
    ! Check is double and check dimension:
    IF ((MXISDOUBLE(MP) .NE. 1) .OR.  &
        (MXISCOMPLEX(MP) .EQ. 1) .OR. &
        (MXGETM(MP) .NE. 1) .OR.  &
        (MXGETN(MP) .NE. N)) THEN
      ! Send error message to Matlab:
      CALL ERRORMESSAGE(MSGTYPE, MSGOUT)
    ELSE
      ! Allocate A:
      ALLOCATE(A(N))
      ! Get pointer:
      MPA = MXGETPR(MP)
      ! Copy value to A:
      CALL MXCOPYPTRTOREAL8(MPA, A, SIZE(A))
    END IF
  END SUBROUTINE CHECK1DARRAY

  !******************************************************************************************
  ! 2D array checking:
  SUBROUTINE CHECK2DARRAY(MP, M, N, MSGTYPE, MSG, A)
    ! No implicits:
    IMPLICIT NONE
    ! Inputs:
    MWPOINTER                                                :: MP
    INTEGER, INTENT(IN)                                      :: M, N
    CHARACTER(LEN = *), INTENT(IN)                           :: MSGTYPE, MSG
    ! Characters for error message:
    CHARACTER(LEN = *), PARAMETER                            :: MSGPRE = ' Expected size: '
    CHARACTER(LEN = *), PARAMETER                            :: MSGSEP = 'x'
    CHARACTER(LEN = 8)                                       :: MCHAR, NCHAR
    CHARACTER(LEN = :), ALLOCATABLE                          :: MSGOUT
    ! Output ... :
    MWPOINTER                                                :: MPA
    REAL(KIND = 8), ALLOCATABLE, DIMENSION(:,:), INTENT(OUT) :: A
    ! Matlab functions:
    MWPOINTER                                                :: MXISDOUBLE, MXISCOMPLEX
    MWPOINTER                                                :: MXGETM, MXGETN
    MWPOINTER                                                :: MXGETPR
    ! Write MCHAR and NCHAR:
    WRITE(MCHAR, '(I0)'), M
    WRITE(NCHAR, '(I0)'), N
    ! Allocate ERRTYPE:
    ALLOCATE(CHARACTER(LEN = LEN(MSG) + LEN(MSGPRE) + LEN(TRIM(MCHAR)) + &
                             LEN(MSGSEP) + LEN(TRIM(NCHAR))) :: MSGOUT)
    ! Set MSGOUT:
    MSGOUT = MSG // MSGPRE // TRIM(MCHAR) // MSGSEP // TRIM(NCHAR)
    ! Check is double and check dimension:
    IF ((MXISDOUBLE(MP) .NE. 1) .OR.  &
        (MXISCOMPLEX(MP) .EQ. 1) .OR. &
        (MXGETM(MP) .NE. M) .OR.      &
        (MXGETN(MP) .NE. N)) THEN
      ! Send error message to Matlab:
      CALL ERRORMESSAGE(MSGTYPE, MSGOUT)
    ELSE
      ! Allocate A:
      ALLOCATE(A(M, N))
      ! Get pointer:
      MPA = MXGETPR(MP)
      ! Copy value to A:
      CALL MXCOPYPTRTOREAL8(MPA, A, SIZE(A))
    END IF
  END SUBROUTINE CHECK2DARRAY

END MODULE MEX_LEROI

!********************************************************************************************
! Matlab MEXFUNCTION gateway subroutine:
SUBROUTINE MEXFUNCTION(NLHS, PLHS, NRHS, PRHS)

  ! Use LEROI's MAIN_CODE module:
  USE MAIN_CODE
  ! Use MEX_LEROI for MEX related bits:
  USE MEX_LEROI
  ! No implicits:
  IMPLICIT NONE

  !******************************************************************************************
  ! MEXFUNCTION arguments:
  MWPOINTER           :: PLHS(*), PRHS(*)
  INTEGER, INTENT(IN) :: NLHS, NRHS
  ! Matlab / MEX Functions:
  MWPOINTER           :: MXGETPR
  MWPOINTER           :: MXISDOUBLE, MXISCOMPLEX
  MWPOINTER           :: MXGETM, MXGETN
  MWPOINTER           :: MXCREATEDOUBLEMATRIX
  ! Variables for array checking:
  INTEGER             :: I
  CHARACTER(LEN = 10) :: NCHAR

  ! Model inputs which _could_ come from Matlab.
  ! Variables named to match those in the LEROI code.
  !
  ! Names in provided Leroi.cfl
  ! * TDFD, DO3D, ISYS, PRFL, ISTOP
  REAL(KIND = 8)                                :: TDFD, DO3D, ISYS, PRFL, ISTOP
  ! * STEP, NSX, KRXW
  REAL(KIND = 8)                                :: STEP, NSX, KRXW
  ! * SURVEY_TYPE
  REAL(KIND = 8)                                :: SURVEY_TYPE
  ! * NLINES, MRXL, NTX, SOURCE_TYPE, MVRTX, NTRN
  REAL(KIND = 8)                                :: NLINES, MRXL, NTX, SOURCE_TYPE, MXVRTX, A1
  ! * NVRTX TxZ
  REAL(KIND = 8), ALLOCATABLE, DIMENSION(:)     :: NVRTX, TXZ
  ! * LINE, RX_TYPE, NRX, UNITS
  ! * IDTX
  REAL(KIND = 8), ALLOCATABLE, DIMENSION(:)     :: LINE, RX_TYPE, NRX, UNITS
  REAL(KIND = 8), ALLOCATABLE, DIMENSION(:,:)   :: LNTR
  ! * CMP, KNORM, IPLT, IDH, RXMNT
  ! * SV_AZM
  REAL(KIND = 8), ALLOCATABLE, DIMENSION(:)     :: CMP, KNORM, IPLT, IDH, RXMNT
  REAL(KIND = 8)                                :: SV_AZM

  ! These inputs really do come from Matlab, and are used in the
  ! READ_MODEL_DATA subroutine:
  !
  ! * NCHNL
  REAL(KIND = 8)                                :: NCHNL
  ! * REFTYM, OFFTYM
  REAL(KIND = 8)                                :: REFTYM, OFFTYM
  ! * TXON, TXAMP
  REAL(KIND = 8), ALLOCATABLE, DIMENSION(:)     :: TXON, WAVEFORM
  ! * TOPN, TCLS
  REAL(KIND = 8), ALLOCATABLE, DIMENSION(:)     :: TOPN, TCLS
  ! * SXE, SXN
  REAL(KIND = 8), ALLOCATABLE, DIMENSION(:)     :: SXE, SXN
  REAL(KIND = 8), ALLOCATABLE, DIMENSION(:,:)   :: SXED, SXND
  ! * RXE, RXN
  ! * RXZ
  REAL(KIND = 8)                                :: QD1IN, QD2IN, RXZIN
  REAL(KIND = 8), ALLOCATABLE, DIMENSION(:,:,:) :: QD1, QD2
  REAL(KIND = 8), ALLOCATABLE, DIMENSION(:,:)   :: RXZ
  ! * NLYR, NPLT, NLITH
  REAL(KIND = 8)                                :: NLYR, NPLT, NLITH
  ! * LYTH
  REAL(KIND = 8), ALLOCATABLE, DIMENSION(:,:)   :: LYTH
  ! * LITHL
  REAL(KIND = 8), ALLOCATABLE, DIMENSION(:)     :: LITHL
  ! * THK
  REAL(KIND = 8), ALLOCATABLE, DIMENSION(:)     :: THK

  ! Matlab output:
  REAL(KIND = 8), DIMENSION(20)                 :: FORWARD_MODEL
  MWSIZE                                        :: MSFORWARD_MODEL
  MWPOINTER                                     :: MPFORWARD_MODEL

  !******************************************************************************************
  ! Check number of input and output arguments.
  ! Seventeen input arguments required:
  IF(NRHS .NE. 18) THEN
    CALL ERRORMESSAGE('nargin', &
                      'Eighteen input arguments expected')
  ! Only one output value will be provided:
  ELSE IF(NLHS .GT. 1) THEN
    CALL ERRORMESSAGE('nargout', &
                      'One output argument expected')
  END IF

  !******************************************************************************************
  ! Default / static values as provided.
  !
  ! Leroi.cfl values:
  ! 1 0 0 1 0 ! TDFD, DO3D, ISYS, PRFL, ISTOP
  TDFD  = 1
  DO3D  = 0
  ISYS  = 0
  PRFL  = 1
  ISTOP = 0
  ! 0 4 1 ! STEP, NSX, KRXW
  STEP   =    0
  NSX    =    4
  KRXW   =    1
  ! 1 ! SURVEY_TYPE
  SURVEY_TYPE = 1
  ! 1 1 1 1 4 1 ! NLINES, MRXL, NTX, SOURCE_TYPE, MVRTX, NTRN
  NLINES      = 1
  MRXL        = 1
  NTX         = 1
  SOURCE_TYPE = 1
  MXVRTX      = 4
  A1          = 1
  ! 4 0  ! NVRTX TxZ
  ALLOCATE(NVRTX(INT(NTX)))
  NVRTX = (/4/)
  ALLOCATE(TXZ(INT(NTX)))
  TXZ   = (/0/)
  ! 1 1 1 1 4 ! LINE IDTX, RX_TYPE, NRX, UNITS
  ALLOCATE(LINE(INT(NLINES)))
  LINE    = (/1/)
  ALLOCATE(LNTR(4, INT(NLINES)))
  LNTR    = 1
  ALLOCATE(RX_TYPE(INT(NLINES)))
  RX_TYPE = (/1/)
  ALLOCATE(NRX(INT(NLINES)))
  NRX     = (/1/)
  ALLOCATE(UNITS(INT(NLINES)))
  UNITS   = (/4/)
  ! 3 0 0 1 0 1 ! CMP SV_AZM, KNORM, IPLT, IDH, RXMNT
  ALLOCATE(CMP(INT(NLINES)))
  CMP     = (/3/)
  SV_AZM  = 0
  ALLOCATE(KNORM(INT(NLINES)))
  KNORM   = (/0/)
  ALLOCATE(IPLT(INT(NLINES)))
  IPLT    = (/1/)
  ALLOCATE(IDH(INT(NLINES)))
  IDH     = (/0/)
  ALLOCATE(RXMNT(INT(NLINES)))
  RXMNT   = (/1/)

  !******************************************************************************************
  ! Get Matlab provided input
  !
  ! First argument is NCHNL.
  ! Check is integer. If successful, returns NCHNL:
  CALL CHECKINTEGER(PRHS(1), 'typeargin',                       &
                    'Argument 1 (NCHNL) should be an integer.', &
                    NCHNL)
  ! Second argument is REFTYM.
  ! Check is double. If successful, returns REFTYM:
  CALL CHECKDOUBLE(PRHS(2), 'typeargin',                      &
                   'Argument 2 (REFTYM) should be a double.', &
                   REFTYM)
  ! Third argument is OFFTYM.
  ! Check is double. If successful, returns OFFTYM:
  CALL CHECKDOUBLE(PRHS(3), 'typeargin',                      &
                   'Argument 3 (OFFTYM) should be a double.', &
                   OFFTYM)
  ! Fourth argument is TXON:
  ! Check is array of correct size:
  CALL CHECK1DARRAY(PRHS(4), INT(NSX), 'typeargin',                 &
                    'Argument 4 (TXON) Should be a double vector.', &
                    TXON)
  ! Fifth argument is TXAMP:
  ! Check is array of correct size:
  CALL CHECK1DARRAY(PRHS(5), INT(NSX), 'typeargin',                  &
                    'Argument 5 (TXAMP) Should be a double vector.', &
                    WAVEFORM)
  ! Sixth argument is TOPN:
  ! Check is array of correct size:
  CALL CHECK1DARRAY(PRHS(6), INT(NCHNL), 'typeargin',               &
                    'Argument 6 (TOPN) Should be a double vector.', &
                    TOPN)
  ! Seventh argument is TCLS:
  ! Check is array of correct size:
  CALL CHECK1DARRAY(PRHS(7), INT(NCHNL), 'typeargin',               &
                    'Argument 7 (TCLS) Should be a double vector.', &
                    TCLS)
  ! Eigth argument is SXE:
  ! Check is array of correct size:
  CALL CHECK1DARRAY(PRHS(8), INT(MXVRTX), 'typeargin',             &
                    'Argument 8 (SXE) Should be a double vector.', &
                    SXE)
  ! SXED is reshaped SXE:
  ALLOCATE(SXED(INT(MXVRTX), INT(NTX)))
  SXED = RESHAPE(SXE, SHAPE(SXED))
  ! Ninth argument is SXN:
  ! Check is array of correct size:
  CALL CHECK1DARRAY(PRHS(9), INT(MXVRTX), 'typeargin',             &
                    'Argument 9 (SXN) Should be a double vector.', &
                    SXN)
  ! SXND is reshaped SXN:
  ALLOCATE(SXND(INT(MXVRTX), INT(NTX)))
  SXND = RESHAPE(SXN, SHAPE(SXND))
  ! Tennth argument is RXE.
  ! Check is double. If successful, returns QD1IN:
  CALL CHECKDOUBLE(PRHS(10), 'typeargin',                   &
                   'Argument 10 (RXE) should be a double.', &
                   QD1IN)
  ! QD1 is actually a 3D array ... :
  ALLOCATE(QD1(INT(MRXL), INT(NLINES), 2))
  QD1 = QD1IN
  ! Eeleventh argument is RXN.
  ! Check is double. If successful, returns QD2IN:
  CALL CHECKDOUBLE(PRHS(11), 'typeargin',                   &
                   'Argument 11 (RXN) should be a double.', &
                   QD2IN)
  ! QD2 is actually a 3D array ... :
  ALLOCATE(QD2(INT(MRXL), INT(NLINES), 2))
  QD2 = QD2IN
  ! Twelfth argument is RXZ.
  ! Check is double. If successful, returns RXZIN:
  CALL CHECKDOUBLE(PRHS(12), 'typeargin',                   &
                   'Argument 12 (RXN) should be a double.', &
                   QD2IN)
  ! RXZ is actually a 2D array ... :
  ALLOCATE(RXZ(INT(MRXL), INT(NLINES)))
  RXZ = RXZIN
  ! Thirteenth argument is NLYR.
  ! Check is integer. If successful, returns NLYR:
  CALL CHECKINTEGER(PRHS(13), 'typeargin',                      &
                    'Argument 13 (NLYR) should be an integer.', &
                    NLYR)
  ! Fourteenth argument is NPLT.
  ! Check is integer:
  CALL CHECKINTEGER(PRHS(14), 'typeargin',                      &
                    'Argument 14 (NPLT) should be an integer.', &
                    NPLT)
  ! Fifteenth argument is NLITH.
  ! Check is integer:
  CALL CHECKINTEGER(PRHS(15), 'typeargin',                       &
                    'Argument 15 (NLITH) should be an integer.', &
                    NLITH)
  ! Sixteenth argument is LYTH.
  ! Check is array of correct size. If succesful, returns LYTH. 7 = NPROP:
  CALL CHECK2DARRAY(PRHS(16), INT(NLITH), 7, 'typeargin',        &
                    'Argument 16 (LYTH) Should be a 2D vector.', &
                    LYTH)
  ! Seventeenth argument is LITHL:
  ! Check is array of correct size:
  CALL CHECK1DARRAY(PRHS(17), INT(NLYR), 'typeargin',                      &
                    'Argument 17 (LITHL) Should be a vector of integers.', &
                    LITHL)
  ! Check for integers:
  DO I = 1, INT(NLYR)
    IF (LITHL(I) .NE. FLOOR(LITHL(I))) THEN
      ! Get number of expected values:
      WRITE(NCHAR, '(I0)'), INT(NLYR)
      CALL ERRORMESSAGE('typeargin',                                          &
                        'Argument 17 (LITHL) Should be a vector of integers.' &
                        // ' Expected size: 1x' // TRIM(NCHAR))
    END IF
  END DO
  ! Eighteenth argument is THK:
  CALL CHECK1DARRAY(PRHS(18), INT(NLYR - 1), 'typeargin',           &
                    'Argument 18 (THK) Should be a double vector.', &
                    THK)

  !******************************************************************************************
  ! Call LEROI_TEM with inputs from Matlab and static values.
  ! Returns : FORWARD_MODEL
  CALL LEROI_TEM(TDFD, DO3D, ISYS, PRFL, ISTOP,              &
                 STEP, NSX, NCHNL, KRXW, REFTYM, OFFTYM,     &
                 TXON, WAVEFORM,                             &
                 TOPN, TCLS,                                 &
                 SURVEY_TYPE,                                &
                 NLINES, MRXL, NTX, SOURCE_TYPE, MXVRTX, A1, &
                 NVRTX, TXZ,                                 &
                 SXED, SXND,                                 &
                 LINE, LNTR, RX_TYPE, NRX, UNITS,            &
                 CMP, SV_AZM, KNORM, IPLT, IDH, RXMNT,       &
                 QD1, QD2, RXZ,                              &
                 NLYR, NPLT, NLITH,                          &
                 LYTH,                                       &
                 LITHL,                                      &
                 THK,                                        &
                 FORWARD_MODEL)

  ! Get output size:
  MSFORWARD_MODEL = SIZE(FORWARD_MODEL)
  ! Create Matlab matrix as first output:
  PLHS(1) = MXCREATEDOUBLEMATRIX(MSFORWARD_MODEL, 1, 0)
  ! Get pointer to first Matlab output:
  MPFORWARD_MODEL = MXGETPR(PLHS(1))
  ! Send the output back to Matlab:
  CALL MXCOPYREAL8TOPTR(FORWARD_MODEL, MPFORWARD_MODEL, MSFORWARD_MODEL)
  ! Return:
  RETURN
  !******************************************************************************************

END SUBROUTINE MEXFUNCTION
!********************************************************************************************
