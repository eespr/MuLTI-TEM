% salinity.m                                       by:  Edward T Peltzer, MBARI
%                                                  revised:  2007 Apr 28.
%
% SALINITY CALCULATION (pss) FROM CONDUCTIVITY.
%
% Reference: Fofonff, P. and Millard, R.C. Jr.  Unesco 1983.
%    Algorithms for computation of fundamental properties of seawater.
%    Unesco Tech. Pap. in Mar. Sci., No. 44, 53 pp.
%
% Input:       	C   - in situ conductivity  (S/m)
%               T   - temperature in degree C
%               P   - pressure in dbars  (not SI)
%               C15 - conductivity at 15 deg C
%                     MBARI default value = 4.2914
%
% Internal Variables:
%
%		R   - ratio of in situ conductivity to standard conductivity
%		rt  - ratio of conductivity of standard sea water S=35,T=t to S=35,T=15
%		Rp  - ratio of in situ conductivity to same at P=0
%		Rt  - ratio of sample conductivity to standard conductivity at T=t
%
%		S   - salinity at T=15
%		dS  - delta correction for T not equal 15
%
% Output:      
%              Salt = salinity(C,T,P).


function [Salt] = salinity(C,T,P)


% CHECK INPUTS ARE SAME DIMENSIONS

[mc,nc] = size(C);
[mt,nt] = size(T);
[mp,np] = size(P);

if ~(mc==mt | mc==mp | nc==nt | nc==np)
  error('sw_salt.m: cndr,T,P must all have the same dimensions')
end


% DEFINE CONSTANTS, ETC FOR CALCULATION

  C15= 4.2914;

  a0=  0.008;
  a1= -0.1692;
  a2= 25.3851;
  a3= 14.0941;
  a4= -7.0261;
  a5=  2.7081;

  b0=  0.0005;
  b1= -0.0056;
  b2= -0.0066;
  b3= -0.0375;
  b4=  0.0636;
  b5= -0.0144;

  c0=  0.6766097;
  c1=  2.00564e-2;
  c2=  1.104259e-4;
  c3= -6.9698e-7;
  c4=  1.0031e-9;

  d1=  3.426e-2;
  d2=  4.464e-4;
  d3=  4.215e-1;
  d4= -3.107e-3;

% The e# coefficients reflect the use of pressure in dbar
%   rather that in Pascals (SI).

  e1=  2.07e-5;
  e2= -6.37e-10;
  e3=  3.989e-15;

  k= 0.0162;


% Calculate internal variables

R = C ./ C15;
rt = c0+(c1+(c2+(c3+c4.*T).*T).*T).*T;
Rp = 1.0 + (e1+(e2+e3.*P).*P).*P ./ (1.0 + (d1+d2.*T).*T + (d3+d4.*T).*R);
Rt = R ./ Rp ./ rt;
sqrt_Rt = sqrt(Rt);


% Calculate salinity

Salt = a0 + (a1+(a3+a5.*Rt).*Rt).*sqrt_Rt + (a2+a4.*Rt).*Rt;

dS = b0 + (b1+(b3+b5.*Rt).*Rt).*sqrt_Rt + (b2+b4.*Rt).*Rt;
dS = dS .* (T-15) ./ (1+k.*(T-15));

Salt = Salt + dS;
