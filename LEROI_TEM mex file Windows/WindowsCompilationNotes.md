# Windows compilation notes

The MEX file included here was Compiled with Intel Fortran 2015 and Matlab 2016a.

## Requirements

Compilation requires the following components:

  * .NET framework 4.0
  * Windows SDK 7.1
  * Intel fortran compiler
  * Matlab

# Compilation

Using a Windows SDK commmand prompt:

```
set mldir=c:\Program Files\MATLAB\R2016a
set include=%include%;%mldir%\extern\include
set lib=%lib%;%mldir%\extern\lib\win64\microsoft
"C:\Program Files (x86)\Intel\Composer XE\bin\intel64\ifort.exe" ifort /Qprec /fpp /LD /DMX_COMPAT_32 /DMATLAB_MEX_FILE LEROI_TEM.F90 libmx.lib libmex.lib /link /export:MEXFUNCTION /out:LEROI_TEM.mexw64
```

