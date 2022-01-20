## LEROI_TEM

The file `LEROI_TEM.F90` is adapted from the source file `Leroi_FM_TEM_subs.f90`, so that the `LEROI_TEM` can be run via Matlab's MEX interface.

### Compiling

The MEX interface can be compiled with:

```
mex LEROI_TEM.F90
```

Compiler optimisation options can be specified with `FOPTIMFLAGS`, e.g.:

```
mex -v LEROI_TEM.F90 FOPTIMFLAGS='-O2'
```

The Fortran `.mod` files can be removed after compilation.

### Input

Various input values are hard coded in the Fortran source, and are equivalent to the values in the provided `Leroi.cfl` file:

```
Offset Loop with 1 receiver - Model: 2 layers over basement, 2 plates
1 0 0 1 0             ! TDFD, DO3D, ISYS, PRFL, ISTOP
0 4 1                 ! STEP, NSX, KRXW
1                     ! SURVEY_TYPE
1 1 1 1 4 1           ! NLINES, MRXL, NTX, SOURCE_TYPE, MVRTX, NTRN
4 0                   ! NVRTX TxZ
1 1 1 1 4             ! LINE IDTX, RX_TYPE, NRX, UNITS
3 0 0 1 0 1           ! CMP SV_AZM, KNORM, IPLT, IDH, RXMNT
```

These values are used by the Leroi subroutine `READ_SYSTEM_AND_SURVEY_DATA`.

Eighteen additional input values are required from Matlab for the variables `NCHNL`, `REFTYM`, `OFFTYM`, `TXON`, `TXAMP`, `TOPN`, `TCLS`, `SXE`, `SXN`, `RXE`, `RXN`, `RXZ`, `NLYR`, `NPLT`, `NLITH`, `LYTH`, `LITHL` and `THK`.

In the `Leroi.cfl` these would be specified as:

```
20 1.05 1.05          ! NCHNL, REFTYM, OFFTYM
0.0     0.0
0.001   1.0
1.0492  1.0
1.05    0.0           ! TXON, TXAMP(NSX)
0.006000  0.007625
0.007625  0.009750
0.009750  0.012500
0.012500  0.015880
0.015880  0.020250
0.020250  0.025880
0.025880  0.033000
0.033000  0.042130
0.042130  0.053750
0.053750  0.068500
0.068500  0.087380
0.087380  0.111400
0.111400  0.151700
0.151700  0.181100
0.181100  0.231000
0.231000  0.294600
0.294600  0.375900
0.375900  0.479500
0.479500  0.611600
0.611600  0.780100    ! TOPN, TCLS(NCHNL) (in ms)
5 5
-5 5
-5 -5
5 -5                  ! SXE, SXN(MVRTX)
15 0 0                ! RXE, RXN, RXZ    

3 0 3                 !! NLYR, NPLT, NLITH
9.9773 -1 1 1 0 0 1
2035.84 -1 1 1 0 0 1 
887.959 -1 1 1 0 0 1  !! LYTH(NLITH, 7)
1 94.2462
2 34.1019             !! LITHL(NLYR - 1), THK(NLYR - 1)
3                     !! LITH(NLYR) 
```

In Matlab, these would be specified as:

```
NCHNL  = 20;
REFTYM = 1.05;
OFFTYM = 1.05;
TXON   = [0.0, 0.001, 1.0492, 1.05];
TXAMP  = [0.0, 1.0, 1.0, 0.0];
TOPN   = [0.006000, 0.007625, 0.009750, 0.012500, 0.015880, ...
          0.020250, 0.025880, 0.033000, 0.042130, 0.053750, ...
          0.068500, 0.087380, 0.111400, 0.151700, 0.181100, ...
          0.231000, 0.294600, 0.375900, 0.479500, 0.611600];
TCLS   = [0.007625, 0.009750, 0.012500, 0.015880, 0.020250, ...
          0.025880, 0.033000, 0.042130, 0.053750, 0.068500, ...
          0.087380, 0.111400, 0.151700, 0.181100, 0.231000, ...
          0.294600, 0.375900, 0.479500, 0.611600, 0.780100];
SXE    = [5, -5, -5, 5];
SXN    = [5, 5, -5, -5];
RXE    = 15;
RXN    = 0;
RXZ    = 0;

NLYR   = 3;
NPLT   = 0;
NLITH  = 3;
LYTH   = [9.9773, -1, 1, 1, 0, 0, 1; ...
          2035.84, -1, 1, 1, 0, 0, 1; ...
          887.959, -1, 1, 1, 0, 0, 1];
LITHL  = [1, 2, 3];
THK    = [94.2462, 34.1019];
```

Then, to run the model in Matlab:

```
LEROI_TEM(NCHNL, REFTYM, OFFTYM, TXON, TXAMP, TOPN, TCLS, SXE, SXN, ...
          RXE, RXN, RXZ, NLYR, NPLT, NLITH, LYTH, LITHL, THK)
```

This will return a single output double vector, e.g.:

```
OUT = LEROI_TEM(NCHNL, REFTYM, OFFTYM, TXON, TXAMP, TOPN, TCLS, SXE, SXN, ...
                RXE, RXN, RXZ, NLYR, NPLT, NLITH, LYTH, LITHL, THK)
```

The Fortran code will validate input values before running, e.g.:

```
...
>> NLYR  = 3.3;                                                                                
>> NPLT  = 0;
>> NLITH = 3;
>> LYTH  = [9.9773, -1, 1, 1, 0, 0, 1; 2035.84, -1, 1, 1, 0, 0, 1; 887.959, -1, 1, 1, 0, 0, 1];
>> LITHL = [1, 2, 3];
>> THK   = [94.2462, 34.1019];
>> 
>> LEROI_TEM(NCHNL, REFTYM, OFFTYM, TXON, TXAMP, TOPN, TCLS, SXE, SXN, ...
             RXE, RXN, RXZ, NLYR, NPLT, NLITH, LYTH, LITHL, THK)
Error using LEROI_TEM
Argument 13 (NLYR) should be an integer.
```

### Limitations

The values which are hard coded in the Fortran source, and passed to the `READ_SYSTEM_AND_SURVEY_DATA` subroutine, affect the way that model runs, and what input is required.

If these values were to be changed, additional changes would likely also be required elsewhere in the code. For example, changing the `SURVEY_TYPE` value will likely change the type of input data required.
