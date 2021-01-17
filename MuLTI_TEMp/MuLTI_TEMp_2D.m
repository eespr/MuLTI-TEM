%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               2D MuLTI TEMp                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This Matlab script runs a quasi 2D transdimensional MCMC 
% inversion for TEM data by running multiple 1D soundings, 
% in parallel, along a 2D line. 

% Adapted from the original MuLTI TEM code writted by:
% Siobhan Killingbeck / Phil Livermore in 2019 
% School of Earth and Environemnt, The University of Leeds
% It is based on Matlab code written by Thomas Bodin.

% The petrophysical (p) version was created in 2021 by:
% Siobhan Killingbeck,
% Department of Geography & Environmental Management, 
% The University of Waterloo
% funded by the W. Garfield Weston Foundation as part of the 
% Canadian SEARCHArtic project. 

% The physical model consists of internal layers, each with an associated resistivity (R), sampled as log(R) defined by Voronoi nuclei.

% The domain is divided into a number of layers, num_layers (which could be one)
% each with its own prior distribution on R.

% Each layer has a special nuclei that cannot leave its layer ("confined" nuclei).
% npt is the number of "floating" nuclei that can change layer.

% The total number of nuclei is therefore npt_max + num_layers

%            TEM SURVEY PARAMETERS             %
% ! THIS CODE INPUTS ONLY ONE BASE FREQUENCY ! %

% NCHNL = Number of time gates (normally either 20 or 30, depending on repetition frequency)
% REFTYM = Time (in ms) from which TMS or TOPN & TCLS are measured. For example, this could be signal off-time or start of downward ramp.
% OFFTYM = time (milliseconds) between end of one pulse and the start of the next pulse (of opposite sign) since a bipolar waveform is assumed (lambda/4). For systems which have a signal which is always on, OFFTIME = 0.
% TXON = digitised time (in milliseconds) of each point in the waveform (set at 4 points). In most cases, TXON(1) = 0, TXON(2) = pulse on-time, TXON(3) = pulse off-time, TXON(4) = REFTYM where TXON(4) - TXON(3) = turn off time e.g., [0.0, 0.001, 1.0492, 1.05];
% TXAMP = transmitter current in amps at time TXON(J), normally signal is normalised e.g., [0.0, 1.0, 1.0, 0.0]; 
% TOPN = Start times (in ms) of receiver windows, (1x20);
% TCLS = End times (in ms) of receiver windows, (1x20);
% SXE = east coordinate of vertex I for loop position J (m), fixed at 4 vertices e.g., [5, -5, -5, 5]; Note the transmitter is fixed on the ground (Z=0) in this adapted Leroi code, for airborne data more parameters will need to be passed through the mex file to model.
% SXN = north coordinate of vertex I for loop position J (m), fixed at 4 vertices e.g., [5, 5, -5, -5]; Note the transmitter is fixed on the ground (Z=0) in this adapted Leroi code, for airborne data more parameters will need to be passed through the mex file to model.
% RXE = receiver easting (m);
% RXN = receiver northing (m);
% RXZ = receiver z (always be 0 for ground based TEM);

clear all % clear all the variables previously allocated
close all % close all the figures

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                    SET UP 2D LINE                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set up sounding locations along line
cmp_s = 0; % location of first sounding, starting point
cmp_e = 3500; % location of last sounding, end point
bin = 500; % interval moved along line 
no_curves = (((cmp_e - cmp_s) / bin) + 1); % number of 1D soundings along 2D line

offsets = linspace(cmp_s,cmp_e,no_curves); % location of each sounding along the line

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                    LOAD 2D DATA                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This includes setting the time gates and loading received voltages

load('2D_input_data.mat'); %load 2D received voltages

% matrix of layer depths at each sounding location along the line, 
% The length of matrix should be equal to "no_curves".
layer1 = 800.*ones(1,(no_curves)); % e.g. layer 1 goes from 0 - 800 m depth.
layer2 = [nan 805 805 805 805 805 nan nan]; % e.g. layer 2 goes from 800 - 805 m depth at sounding 2 to 6
layer3 = [835 865 895 925 955 985 1015 1105];

% layers = the number of depth constraints to apply in the inversion
% layers = 1 apply no depth constrainst (1 layer) 
% layers = 2 apply one depth constraint (2 layers)
% layers = 3 apply 2 depth constraints (3 layers)
layers = 2.*ones(1,no_curves); % e.g.constrained base layer 1 only
% layers = [2 3 3 3 3 3 2 2]; % e.g. constrained base layer 1 at sounding
% 1, 7 and 8. And constarined base layer 1 and layer 2 at soundings 2-6.

running_mode = 1; %1 -> find posterior; 0 -> Find priors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        TEM SURVEY PARAMETERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SXE    = [250, -250, -250, 250]; % transmitter corners e.g. 500 m x 500 m loop
SXN    = [250, 250, -250, -250];
RXE    = 0; % receiver location e.g. centre of loop
RXN    = 0;
RXZ    = 0;
NCHNL = 30; % number of time gates
TXAMP  = [0.0, 25.0, 25.0, 0.0]; % current in Amps

% This versions inputs ONE BASE FREQUENCY
%      7.5 Hz     %
REFTYM = 33.25;
OFFTYM = 33.25;
TXON   = [0.0, 0.46 32.79, 33.25];
TOPN   = [0.032000, 0.040000, 0.050500, 0.063500, 0.081000, ...
          0.10300, 0.13100, 0.16500, 0.208000, 0.260000, ...
          0.320000, 0.385000, 0.470000, 0.580000, 0.715000, ...
          0.890000, 1.115000, 1.40000, 1.765000, 2.23000, ...
          2.8200000, 3.575000, 4.53500, 5.760000, 7.325000, ...
          9.320000, 11.86500, 15.11500, 19.2600, 24.54500];
TCLS   = [0.040000, 0.050500, 0.063500, 0.081000, ...
          0.10300, 0.13100, 0.16500, 0.208000, 0.260000, ...
          0.320000, 0.385000, 0.470000, 0.580000, 0.715000, ...
          0.890000, 1.115000, 1.40000, 1.765000, 2.23000, ...
          2.8200000, 3.575000, 4.53500, 5.760000, 7.325000, ...
          9.320000, 11.86500, 15.11500, 19.2600, 24.54500, 31.2850];
Centre = (TCLS-TOPN)./2 + TOPN;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     VERY IMPORTANT PARAMETERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%th

burn_in=100; % burn-in period
nsample=2000; % total number of samples

npt_max = 100; % maximum number of floating cells
npt_init=0; % initial number of floating nuclei

sigma_change_R = 3; % std deviation of Gaussian proposal on Change R value
sigma_move_depth = 500; % std deviation of Gaussian proposal on MOVE (change depth)
sigma_birth_R = 3; % std deviation of Gaussian proposal on BIRTH
% (this number is also present in the DEATH
% acceptance term when taking in acount the reverse jump )

rng('default');
rng(1);
% You can change the 1 to any other number to change the seed.

% Define the limits of your model
x_min = 0; % always has to be 0 
x_max = 1200; % max depth (m)
dis=1200; % steps to discretize the model. Trade-off between computational time and accuracy.    
x =linspace(x_min,x_max,dis); % discretize the model

show=1000; % show statistics of the chain every "show" samples
thin = 1000; % thining
num=ceil((nsample-burn_in)*0.025/thin); % number of collected samples

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     Setting up 2D variables for inversion    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Preallocation of variables for parfor
best_model_R_2D = nan(dis,no_curves);
forward_model_best_2D = cell(1, no_curves); %setting up best forward model
Acceptance_Rate_birth_2D = cell(1,no_curves); %setting up 2D acceptance rates
Acceptance_Rate_death_2D = cell(1,no_curves);
Acceptance_Rate_R_2D = cell(1,no_curves);
Acceptance_Rate_position_2D = cell(1,no_curves);
nnucleihist_2D = cell(1,no_curves);
CI_density_2D = cell(1,no_curves);
CI_density_limit_2D_N = cell(1,no_curves);
CI_density_salt_2D = cell(1,no_curves);
CI_density_poro_2D = cell(1,no_curves);
CI_density_FM_2D = cell(1,no_curves);
CI_density_FMN_2D = cell(1,no_curves);
R_mode_2D = nan(dis,no_curves);
R_median_2D = nan(dis,no_curves);
salt_mode_2D = nan(dis,no_curves);
salt_median_2D = nan(dis,no_curves);
porosity_mode_2D = nan(dis,no_curves);
porosity_median_2D = nan(dis,no_curves);

AV_2D=zeros(dis,no_curves);

depth_edge_2D = cell(1,no_curves);
R_edge_2D = cell(1,no_curves);
FM_edge_2D =cell(1,no_curves);
time_edge_2D = cell(1,no_curves);
salt_edge_2D = cell(1,no_curves);
poro_edge_2D = cell(1,no_curves);
y_2D = cell(1,no_curves);
time_x_2D = cell(1,no_curves);
yVolts_2D = cell(1,no_curves);
y_salt_2D = cell(1,no_curves);
y_porosity_2D = cell(1,no_curves);
best=zeros(dis,no_curves);
cov=zeros(nsample,no_curves);
inbetween=zeros((dis*2),no_curves);
nnuclei=zeros(nsample,no_curves);
sup=zeros(dis,no_curves);
infmax=zeros(dis,no_curves);
MINI_R_2D=zeros(dis,num,no_curves);
MAXI_R_2D=zeros(dis,num,no_curves);
priors_OK2D=NaN(nsample,no_curves);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%           Petrophysical modelling          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ARCHIE LAW
Archie = 1;  % Archie = 0, no petrophysical modelling
			 % Archie = 1, calcuate porosity from Archie's law using input 
             % Rfluid model from known data e.g., borehole or lake
             % measurements
			 % Archie = 2, calculate Rfluid and salinity from Archie's 
			 % law using input porosity model
if Archie == 0 
    Rfluid2D = zeros(dis,no_curves);
    porosity2D = zeros(dis,no_curves);
else
    if Archie == 1
        load('Rfluid2D.mat') % estimate of pore fluid resistivity
        porosity2D = zeros(dis,no_curves); % unknown
    else % Archie == 2 
        load('porosity2D.mat') % estimate of porosity
        Rfluid2D = zeros(dis,no_curves); % unknown
    end
    cementation = 1.5; % m
    sigma_cementation = 0.15; % estimate of uncertainty in m
    % Tempreture and pressure estimates for salinity modelling
    % PRACTICAL SALINITY SCALE
    temp = 0; % degrees C
    sigma_temp = 2; % uncertainty 
    pressure = 500; % pressure in dbars
    sigma_pressure = pressure.*0.04; % uncertainty
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   start 2D TEM inversion loop                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

parfor c = 1:no_curves

    data = data_2D{1,c}(:,2);% observed data, received voltage in nanoVolts
    timegate_centre = data_2D{1,c}(:,1);% define time gate centre in ms
    nd =numel(data); % nd is the number of data points
    weighting = data.*0.075; %determine weighting to be applied in inversion in nV

    if Archie == 1
        Rfluid = 10.^(Rfluid2D(:,c)); % convert to log scale
        sigma_Rfluid = Rfluid.*0.1; % input estimate of pore fluid resistivity
    elseif Archie == 2
        porosity = porosity2D(:,c);
        sigma_porosity = porosity*0.1; % input estimate of porosity
    end

%%%%%%%%%%%%%%%%%%%%%% SETTING UP PRIORS %%%%%%%%%%%%%%%%%%%%%%%%%%%
priors=struct();

%define number of layers at each dispersion curve
num_layers = layers(c);

%Uniform prior on depths for nuclei
priors.depth_min=0; % Cannot be changed, always from the surface.
priors.depth_max=x_max;
priors.npt_max = npt_max;
priors.npt_min = 0; 

priors.layer_depths = zeros(num_layers-1,1); %the last layer depth is infinity (and is not defined).
priors.Rmin = zeros(num_layers,1);
priors.Rmax = zeros(num_layers,1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Define the priors and layer geometry
if num_layers == 3
    priors.layer_depths(1) = layer1(1,c); % layer 1 depth.
    priors.layer_depths(2) = layer2(1,c); % layer 2 depth.
    priors.Rmin(1) = log10(5000); % layer 1
    priors.Rmax(1) = log10(20000); 
    priors.Rmin(2) = log10(0.1); % layer 2
    priors.Rmax(2) = log10(100);
    priors.Rmin(3) = log10(1); % layer 3
    priors.Rmax(3) = log10(5000);
elseif num_layers == 2
    priors.layer_depths(1) = layer1(1,c); % layer 1 depth.
    priors.Rmin(1) = log10(5000); % layer 2
    priors.Rmax(1) = log10(20000);
    priors.Rmin(2) = log10(0.1); % layer 3
    priors.Rmax(2) = log10(5000);
else % num_layers = 1 
    priors.Rmin(1) = log10(0.1); % wide range of constraints 
    priors.Rmax(1) = log10(50000);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      Preallocation of 1D variables

b=0;
bb=0;
AV=zeros(dis,1);

AB=0;
AD=0;
PB=0;
PD=0;

AcV=0;
PV=0;
AP=0;
PP=0;

y = linspace(min(priors.Rmin), max(priors.Rmax), dis); %y limits are Vs priors limits
time_x=linspace(log10(timegate_centre(1,1)), log10(timegate_centre(end,1)), dis);%discretize the forward model (time-axis) 
yVolts = linspace(log10(min(data-weighting)), log10(max(data+weighting)), dis); %data (recieved voltage limits)
y_salt = linspace(log10(0.1), log10(500), dis); %y limits of salinity in log10 space
y_porosity = linspace(0.01,1,dis); % y limits of porosity
best=zeros(dis,1);
val_min_R=zeros(dis,1);
val_max_R=zeros(dis,1);
val_min=zeros(dis,1);
val_max=zeros(dis,1);
ind_min_R=zeros(dis,1);
ind_max_R=zeros(dis,1);
hist_R=zeros(dis,1);
hist_density=zeros(dis,1);
hierhist=zeros(nsample,1);
change_points=zeros(nsample*(priors.npt_max+num_layers),1);
inf=zeros(dis,1);
MINI_R=zeros(dis,num);
MAXI_R=zeros(dis,num);
CI_density=zeros((dis-1),(dis-1));
CI_density_salt = zeros((dis-1),(dis-1));
CI_density_poro = zeros((dis-1),(dis-1));
CI_density_FM = zeros((dis-1),(dis-1));

nnucleihist=zeros(priors.npt_max+num_layers,1);
nuclei_depths=zeros(priors.npt_max+num_layers,1);
nuclei_R = zeros(priors.npt_max+num_layers,1);
thickness = zeros(priors.npt_max+num_layers,1);
R = zeros(priors.npt_max+num_layers,1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialize - Define randomly the first model of the chain. Make sure
% that it satisfies the priors.

npt=npt_init;

    for i=1:npt+num_layers
% define the layer depth. The first num_layer nuclei are special: the ith
% nuclei must reside in the ith layer. 

        if i <= num_layers
            if i == 1
                top_of_layer = priors.depth_min;
                    if num_layers > 1
                        bottom_of_layer = priors.layer_depths(1);
                    else
                        bottom_of_layer = priors.depth_max;
                    end
                
            elseif i < num_layers
                top_of_layer = priors.layer_depths(i-1);
                bottom_of_layer = priors.layer_depths(i);
            else 
                top_of_layer = priors.layer_depths(i-1);
                bottom_of_layer = priors.depth_max;
            end
                
            nuclei_depths(i)= (bottom_of_layer + top_of_layer) / 2; % fix nuclei to be in middle of layer
        else    
            nuclei_depths(i)=priors.depth_min+rand*(priors.depth_max-priors.depth_min); % position of floating nuclei
        end
       
    % For each nuclei, find out which layer it is in:
        layer = num_layers;
        for j = 1:num_layers - 1
            if nuclei_depths(i) <= priors.layer_depths(j)
                layer = j;
                break
            end
        end              
      
     % the variable 'layer' is the layer of the nuclei:
       nuclei_R(i)=priors.Rmin(layer)+rand*(priors.Rmax(layer)-priors.Rmin(layer)); % R
    end 
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMPUTE INITIAL MISFIT
% (Here, 'like' is the misfit )
like=0;
[thickness, R, priors_OK] = thicknesses_and_priors(nuclei_depths, nuclei_R, npt, num_layers, priors); 
R = 10.^(R);
if priors_OK == 1
    thickness = thickness(1,1:(length(thickness)-1)); % no thickness value for halfspace
    Res = zeros(length(R),7);
    for i = 1:length(R)
        Res(i,:) = [R(i) -1 1 1 0 0 1];
    end
    %%%%%%%%%% computing the forward model %%%%%%%%%%%
    if running_mode == 1
        forward_model = LEROI_TEM(NCHNL, REFTYM, OFFTYM, TXON, TXAMP, TOPN, TCLS, SXE, SXN, RXE, RXN, RXZ, npt+num_layers, 0, npt + num_layers, Res, linspace(1,(npt+num_layers),(npt+num_layers)), thickness);
        forward_model = round(forward_model,6,'significant');
    %%%%%%%%%% computing the misfit %%%%%%%%%%%
    misfit = NaN(length(data),1);
    for i = 1:length(timegate_centre) % time samples, this should match the time samples.
        misfit(i,1) = data(i,1) - forward_model(i,1);
    end %end misfit
    like = nansum( (misfit).^2 ./(2 * weighting.^2) );
    else
    like = 1;
    end
end

like_best=1e99;
like_init=like;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%% START RJ-MCMC SAMPLING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for s=1:nsample
    out=1;
    if (mod(s,show)==0)
        number_of_samples = s;
        number_of_nuclei = npt;
        if (s>burn_in)
            Acceptance_Rate_birth = 100*AB/PB;
            Acceptance_Rate_death = 100*AD/PD;
            Acceptance_Rate_R = 100*AcV/PV;
            Acceptance_Rate_position = 100*AP/PP;
        end
    end
    
    birth=0;
    move=0;
    death=0;
    
    nuclei_R_prop=nuclei_R;
    nuclei_depths_prop = nuclei_depths;
    
    like_prop = like;
    %----------------------------------------------------------------------
    % Every even iteration, propose a new changed parameter value
    if (mod(s,2)==0) % Change Value
        if (s>burn_in)
            PV=PV+1;
        end
        npt_prop = npt;
        ind=ceil(rand*(npt+num_layers));
        nuclei_R_prop(ind) = nuclei_R(ind) + randn * sigma_change_R;

        %-----------------------------------------------------------------------
        % Every odd iteration change the nuclei tesselation
    else % Change position

        %u=1; % turning off birth/death
        u=rand; % Chose randomly between 3 different types of moves
        if (u<0.333) % BIRTH ++++++++++++++++++++++++++++++++++++++
            birth=1;
            if (s>burn_in)
                PB=PB+1;
            end
            npt_prop = npt+1;
            nuclei_depths_prop(1:npt+num_layers) = nuclei_depths(1:npt+num_layers);
            nuclei_depths_prop(npt+num_layers+1) = priors.depth_min+rand*(priors.depth_max-priors.depth_min);
            ind=whichnuclei(nuclei_depths(1:npt+num_layers),nuclei_depths_prop(npt+num_layers+1), num_layers, priors);

            nuclei_R_prop(npt+num_layers+1)=nuclei_R(ind)+randn*sigma_birth_R;
            
 % find which layer it's in and find the product of Priors:
 
        layer = num_layers;
        Prod_delta_prior = priors.Rmax(layer)-priors.Rmin(layer);
        
        for j = 1:num_layers - 1
            if nuclei_depths_prop(npt+num_layers+1) <= priors.layer_depths(j)
                layer = j;
                Prod_delta_prior = (priors.Rmax(j)-priors.Rmin(j) );
                break
            end
        end  
               
       prob = 1.0 / (sigma_birth_R*sqrt(2*pi)) * exp(-( nuclei_R_prop(num_layers+npt+1) - nuclei_R(ind) )^2/(2*sigma_birth_R^2));
            
        elseif (u<0.666) % DEATH +++++++++++++++++++++++++++++++++++++++++
            death=1;
            if (s>burn_in)
                PD=PD+1;
            end
            
            npt_prop = npt-1;   
  % choose a floating nuclei to remove
            ind=ceil(rand*npt)+num_layers;
            
            nuclei_depths_prop(1:num_layers+npt-1) = [nuclei_depths(1:ind-1) ; nuclei_depths(ind+1:num_layers+npt)];
            nuclei_R_prop(1:num_layers + npt-1)= [nuclei_R(1:ind-1) ; nuclei_R(ind+1:num_layers+npt)];
           
                death_pt_R = nuclei_R(ind);
                death_pt_depth = nuclei_depths(ind);
                
                        
    %GET prob
                node=whichnuclei(nuclei_depths_prop(1:npt_prop+num_layers),death_pt_depth, num_layers,priors);
                %prob=(1/(sigmav*sqrt(2*pi)))*exp(-(pt(ind,2)-pt_prop(node,2))^2/(2*sigmav^2));
              
 % find which layer it's in and find the product of Priors:
        layer = num_layers;
        Prod_delta_prior = priors.Rmax(layer)-priors.Rmin(layer);
        for j = 1:num_layers - 1
            if death_pt_depth <= priors.layer_depths(j)
                layer = j;
                Prod_delta_prior = priors.Rmax(j)-priors.Rmin(j) ;
                break
            end
        end  
                
        % the case of npt_prop = -1 is a rather special case but it results in an error. 
        % when num_layers = 1. It is never excepted.
        if npt_prop == -1 
            prob = 1; %set to anything.
        else
            prob = 1.0 / (sigma_birth_R*sqrt(2*pi)) * exp(-( nuclei_R_prop(node) - death_pt_R )^2/(2*sigma_birth_R^2));
        end
            
            
            
        else % MOVE +++++++++++++++++++++++++++++++++++++++++++++++++++++++
            if (s>burn_in)
                PP=PP+1;
            end
            move=1;
            npt_prop = npt;
 % choose the nuclei to move
            ind=ceil(rand*(npt+num_layers));
 
            if num_layers == 1 || ind > num_layers %if ind is a 'floating' nuclei or depth constraints are not applied, move nuclei randomly using sigma_move_depth
                nuclei_depths_prop(ind) = nuclei_depths(ind)+randn*sigma_move_depth;
            else %if ind is a 'confined' nuclei, move nuclei randomly within the range of the layer depths
                if ind == 1
                top_of_layer = priors.depth_min;
                bottom_of_layer = priors.layer_depths(1);
                elseif ind < num_layers
                top_of_layer = priors.layer_depths(ind-1);
                bottom_of_layer = priors.layer_depths(ind);
                else
                top_of_layer = priors.layer_depths(ind-1);
                bottom_of_layer = priors.depth_max;
                end
                nuclei_depths_prop(ind) = (bottom_of_layer-top_of_layer).*rand(1) + top_of_layer;
            end
                           
% Find move probability

% find which layer the nuclei is currently in:
        layer = num_layers;
        move_prob1 = priors.Rmax(layer)-priors.Rmin(layer);
        for j = 1:num_layers - 1
            if nuclei_depths(ind) <= priors.layer_depths(j)
                layer = j;
                move_prob1 = priors.Rmax(j)-priors.Rmin(j) ;
                break
            end
        end  
        
% find which layer the nuclei will move to:
        layer = num_layers;
        move_prob2 = priors.Rmax(layer)-priors.Rmin(layer);
        for j = 1:num_layers - 1
            if nuclei_depths_prop(ind) <= priors.layer_depths(j)
                layer = j;
                move_prob2 = priors.Rmax(j)-priors.Rmin(j) ;
                break
            end
        end  
        move_prob = move_prob1 / move_prob2;
        
        end   
          
    end % Change the position
    %----------------------------------------------------------------------

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % COMPUTE MISFIT OF THE PROPOSED MODEL
    % If the proposed model is not outside the bounds of the uniform prior,
    % compute its misfit : "like_prop"
    if out==1
        like_prop=0;
        [thickness, R, priors_OK] = thicknesses_and_priors(nuclei_depths_prop, nuclei_R_prop, npt_prop, num_layers, priors);
        R = 10.^(R);
        if priors_OK == 0 
            out = 0;
            like_prop = 0;
        else
           thickness = thickness(1,1:(length(thickness)-1)); % no thickness value for halfspace
           Res = zeros(length(R),7);
            for i = 1:length(R)
                Res(i,:) = [R(i) -1 1 1 0 0 1];
            end
            %%%%%%%%%% computing the forward model %%%%%%%%%%%
            if running_mode == 1
                forward_model_p = forward_model; 
                forward_model = LEROI_TEM(NCHNL, REFTYM, OFFTYM, TXON, TXAMP, TOPN, TCLS, SXE, SXN, RXE, RXN, RXZ, npt_prop+num_layers, 0, npt_prop + num_layers, Res, linspace(1,(npt_prop+num_layers),(npt_prop+num_layers)), thickness);
                forward_model = round(forward_model,6,'significant');  
            %%%%%%%%%% computing the misfit %%%%%%%%%%%
            misfit = NaN(length(data),1);
            for i = 1:length(timegate_centre) % time samples, this should match the time samples.
                misfit(i,1) = data(i,1) - forward_model(i,1);
            end %end multimodal misfit
            like_prop = nansum( (misfit).^2 ./(2 * weighting.^2) );
            else
                like_prop = 1;
            end
            
        end
        
    end %if (out==1)
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% SEE WHETHER MODEL IS ACCEPTED
    
    accept=0;
 % This avoids runtime errors, as if out=0 Matlab insists on evaluating
 % Prod_delta_prior even though it never affects the calculation if out==0
 % (for the result is always 0).
 
    if out == 0 
    Prod_delta_prior = 1;
    prob = 1;
    forward_model_p = nan(length(data),1);
    end
    
    % The acceptance term takes different
    % values according the the proposal that has been made.
    % BIRTH
    if (birth==1)        
        if (rand<((1/(Prod_delta_prior*prob))*exp(log(out)-like_prop+like)))
            accept=1;
            if (s>burn_in)
                AB=AB+1;
            end
        end
    % DEATH
    elseif (death==1)
        
        if (rand<(Prod_delta_prior*prob*exp(log(out)-like_prop+like)))
            accept=1;
            if (s>burn_in)
                AD=AD+1;
            end
        end
    % MOVE
    elseif (move == 1) % NO JUMP, i.e no change in dimension
        
        if (rand<(move_prob * exp(log(out)-like_prop+like)))
            accept=1;
            if (s>burn_in)
            AP=AP+1;       
            end %if (s>burn_in)
        end
    % CHANGE R
    else %change R
       if  (rand<exp(log(out)-like_prop+like))
        accept=1;
            if (s>burn_in)
            AcV=AcV+1;       
            end %if (s>burn_in)      
       end
    end
    
    % If accept, update the values
    if (accept==1)
        npt=npt_prop;
        nuclei_depths = nuclei_depths_prop;
        nuclei_R = nuclei_R_prop;
        like=like_prop;
        if running_mode == 1
            forward_model_p = forward_model;
        end
    end
    
    % Convert model into the ensemble matrix
        for i=1:dis
            ind=whichnuclei(nuclei_depths(1:npt+num_layers),x(i),num_layers,priors);
            hist_R(i)=nuclei_R(ind);
        end
        
        [N]=histcounts2(x,hist_R',x,y);% Resistivity matrix
        
        neg = any(forward_model_p<0);
        if neg == 1 % no negative TEM responses counted in the PDF as they are not in the plots scale, only showing positive TEM responses
            [FM] = zeros((dis-1),(dis-1));
        else
            fmp_interp = interp1(log10(timegate_centre),log10(forward_model_p),time_x);
            [FM]=histcounts2(fmp_interp,time_x,yVolts,time_x); % TEM response matrix
        end
        R_edge=y(1:(dis-1));
        depth_edge=x(1:(dis-1));
        FM_edge=yVolts(1:(dis-1));
        time_edge=time_x(1:(dis-1));
    
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % petrophysical modelling 
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		if Archie > 0		
			if Archie == 1 
				% calculate porosity with known Rfluid (e.g., lake or borehole measurement)
				RF = normrnd(Rfluid,sigma_Rfluid); % normal distribution of porosity
				C = normrnd(cementation,sigma_cementation); % normal distribution of m
				porosity = nthroot((RF./(10.^(hist_R))),C);
				[P]=histcounts2(x,porosity',x,y_porosity); % Porosity matrix
				poro_edge=y_porosity(1:(dis-1));
				
			else % Archie = 2   
				% calculate R fluid using Archie Law
				P = normrnd(porosity,sigma_porosity); % normal distribution of porosity
				C = normrnd(cementation,sigma_cementation); % normal distribution of m
				Rfluid = 10.^(hist_R).*P.^C; % Archie's Law
						
				% Convert to salinity where there is porosity
				Cond = 1./Rfluid; % in situ conductivity (S/m)
				T = normrnd(temp,sigma_temp); % sampling a normal distribution of temp
				P = normrnd(pressure,sigma_pressure); % sampling a normal distribution of pressure
				Salt = salinity(Cond,T,P); % convert R to salinity
                % Reference: Fofonff, P. and Millard, R.C. Jr.  Unesco 1983. 
				% Algorithms for computation of fundamental properties of seawater. 
				% Unesco Tech. Pap. in Mar. Sci., No. 44, 53 pp.
				for i = 1:length(Salt) % no negative values
					if Salt(i,1) < 0
						Salt(i,1) = NaN;
					end
				end
				Salt = log10(Salt); % convert to log10 scale
				[S]=histcounts2(x,Salt',x,y_salt); % Salinity matrix
				salt_edge=y_salt(1:(dis-1));
			end
		end
        
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %  We collect the samples for the ensemble solution   %
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    if (s>burn_in)
        if (mod(s,thin)==0)
            b=b+1;
            % DO THE AVERAGE
            
            for i=1:dis
                ind=whichnuclei(nuclei_depths,x(i), num_layers,priors);
                
                AV(i,1)=AV(i,1)+nuclei_R(ind);               
                
                % Do the 95% credible interval for R
                if (b<=num)
                    MINI_R(i,b)=nuclei_R(ind);
                    MAXI_R(i,b)=nuclei_R(ind);
                    if (b==num)
                        [val_min_R(i) ind_min_R(i)]=min(MAXI_R(i,:));
                        [val_max_R(i) ind_max_R(i)]=max(MINI_R(i,:));
                    end
                    
                else
                    if (nuclei_R(ind)>val_min_R(i))
                        MAXI_R(i,ind_min_R(i))=nuclei_R(ind);
                        [val_min_R(i) ind_min_R(i)]=min(MAXI_R(i,:));
                    end
                    if (nuclei_R(ind)<val_max_R(i))
                        MINI_R(i,ind_max_R(i))=nuclei_R(ind);
                        [val_max_R(i) ind_max_R(i)]=max(MINI_R(i,:));
                    end
                end
                
            end
            nnucleihist(npt+num_layers,1)=nnucleihist(npt+num_layers,1)+1;
            %Do the histogram on change points
            nuclei=nuclei_depths(1:npt+num_layers);
            nuclei=sort(nuclei);
            for i = 1:npt-1+num_layers
                bb=bb+1;
                cp= (nuclei(i+1)+nuclei(i))/2;
                change_points(bb)=cp;
            end
        end
        
            CI_density = CI_density + N;% Resistivity ensemble
            if running_mode == 1
                CI_density_FM = CI_density_FM + FM; % TEM response ensemble
            end
			if Archie > 0
				if Archie == 1
					CI_density_poro = CI_density_poro + P; % porosity ensemble
				else %Archie == 0
					CI_density_salt = CI_density_salt + S; % salinity ensemble
				end
			end
        
    end %if burn-in
    
    MINI_R_2D(:,:,c) = MINI_R;
    MAXI_R_2D(:,:,c) = MAXI_R;
    cov(s,c)=like; % Convergence of the misfit
    priors_OK2D(s,c)=priors_OK;
    nnuclei(s,c)=npt+num_layers; % Convergence of number of cells
    
    % Get the best model
    if priors_OK ==1 && (like<like_best) 
        depths_best = nuclei_depths;
        R_best = nuclei_R;
        npt_best = npt;
        like_best = like;
        if running_mode ==1
            forward_model_best = forward_model;
        end
    end   
   
    
end% the Sampling of the MCMC

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     save 2D outputs      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if running_mode == 1
	forward_model_best_2D{1,c} = forward_model_best;
	CI_density_FM_2D{1,c} = CI_density_FM;
end
Acceptance_Rate_birth_2D{1,c} = Acceptance_Rate_birth; 
Acceptance_Rate_death_2D{1,c} = Acceptance_Rate_death;
Acceptance_Rate_R_2D{1,c} = Acceptance_Rate_R;
Acceptance_Rate_position_2D{1,c} = Acceptance_Rate_position;
nnucleihist_2D{1,c} = nnucleihist;
CI_density_2D{1,c} = CI_density;
if Archie > 0
	if Archie == 1
		CI_density_poro_2D{1,c} = CI_density_poro;
		y_porosity_2D{1,c} = y_porosity;
		poro_edge_2D{1,c} = poro_edge;
	else
		CI_density_salt_2D{1,c} = CI_density_salt;
		y_salt_2D{1,c} = y_salt;
		salt_edge_2D{1,c} = salt_edge;
	end
end
depth_edge_2D{1,c} = depth_edge;
R_edge_2D{1,c} = R_edge;
y_2D{1,c} = y;
time_x_2D{1,c} = time_x;
yVolts_2D{1,c} = yVolts;
FM_edge_2D{1,c} =FM_edge;
time_edge_2D{1,c} = time_edge;

% CALCULATE AVERAGE SOLUTION
AV=AV./b;

% CALCULATE BEST SOLUTION
best_model_R = zeros(1,dis);
for i=1:dis
    ind=whichnuclei(depths_best(1:num_layers+npt_best),x(i),num_layers, priors);
    best_model_R(i)=R_best(ind);
end
% SAVE INTO 2D VARIABLES
best_model_R_2D(:,c) = 10.^(best_model_R).';
AV_2D(:,c) = 10.^(AV);

end % END of 2D inversion. END parfor loop.

%%%%%%%%%%%%%%%%END OF TEM INVERSION LOOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         Calculating Normalised PDF's         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 for c = 1:no_curves
    % Calculating the 95% credible interval
    for i=1:dis
    [val_min, ind_min]=min(MAXI_R_2D(i,:,c));
    [val_max, ind_max]=max(MINI_R_2D(i,:,c));
    sup(i,c)=val_min;
    infmax(i,c)=val_max;
    end
   
    x2 = [x'; flipud(x')];
    inbetween(:,c) = [infmax(:,c); flipud(sup(:,c))];
    y = y_2D{1,c};
    
    % Normalise the ensemble PDF's
    % R
    sizeCI = size(CI_density_2D{1,c});
    len = sizeCI(1,1);
    wid = sizeCI(1,2);
    CI_density_limit_2D_N{1,c} = zeros(len,wid);
    for i = 1:len
        m = sum(CI_density_2D{1,c}(i,:));
        for j = 1:wid
            CI_density_limit_2D_N{1,c}(i,j) = CI_density_2D{1,c}(i,j)/m;
        end
    end
    % FM
    if running_mode ==1
        sizeCI = size(CI_density_FM_2D{1,c});
        len = sizeCI(1,1);
        wid = sizeCI(1,2);
        CI_density_FMN_2D{1,c} = zeros(len,wid);
        for i = 1:len
            m = sum(CI_density_FM_2D{1,c}(i,:));
            for j = 1:wid
                CI_density_FMN_2D{1,c}(i,j) = CI_density_FM_2D{1,c}(i,j)/m;
            end
        end
    end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%       Calculating MODE SOLUTIONS       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
    mode = zeros((dis-1),(dis-1));
    mode_poro = zeros((dis-1),(dis-1));
    mode_salt = zeros((dis-1),(dis-1));
    for i=1:(dis-1)
        [M,I]=max(CI_density_2D{1,c}(i,:));
        mode(i,I)=1;
		if Archie > 0
			 if Archie == 1
				[Mp, Ip] = max(CI_density_poro_2D{1,c}(i,:));
				mode_poro(i,Ip)=1;
			 else% Archie == 2
				[Ms, Is] = max(CI_density_salt_2D{1,c}(i,:));
				mode_salt(i,Is)=1;
			 end
		end
    end
    R_mode=zeros((dis-1),(dis-1));
    poro_mode=zeros((dis-1),(dis-1));
    salt_mode=zeros((dis-1),(dis-1));
    for i=1:(dis-1)
    R_mode(i,:) = mode(i,:).* R_edge_2D{1,c};
		if Archie > 0
			if Archie == 1
			   poro_mode(i,:) = mode_poro(i,:).* poro_edge_2D{1,c}; 
			else
			   salt_mode(i,:) = mode_salt(i,:).* salt_edge_2D{1,c};
			end
		end
    end   
    R_mode_n0 = nan((dis-1),1); % removing the zeros in the solution
    poro_mode_n0 = nan((dis-1),1);
    salt_mode_n0 = nan((dis-1),1);
    for i=1:(dis-1)
    R_mode_n0(i,1) = R_mode(i,find(R_mode(i,:)));
		if Archie > 0
			if Archie == 1
			poro_mode_n0(i,1) = poro_mode(i,find(poro_mode(i,:)));
			else
			salt_mode_n0(i,1) = salt_mode(i,find(salt_mode(i,:)));
			end
		end
    end
    R_mode_n0(dis,1) = R_mode_n0(dis-1,1);
	if Archie > 0
		if Archie == 1
		poro_mode_n0(dis,1) = poro_mode_n0(dis-1,1);
		else
		salt_mode_n0(dis,1) = salt_mode_n0(dis-1,1);
		end
	end
    %populating the 2D matrix
    R_mode_2D(:,c) = R_mode_n0;
	if Archie > 0
		if Archie == 1
		   porosity_mode_2D(:,c) = poro_mode_n0;
		else
			salt_mode_2D(:,c) = salt_mode_n0;
		end
	end

 end % end for c = 1:no_curves
 
%%%%%%%%%% CALCULATING THE INTERQUARTILE RANGE %%%%%%%%%%%%%%%%%
%
% we use the width of the spread from 25th to the 75th quartile 
% calculated in log10 space, e.g 1 == one order of magnitude, 
% 2 == two orders of magnitude etc. 
%
% Calculte the 50th quartile for the MEDIUM SOLUTION          %
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% axis are in log10 sapce
R_edge = R_edge_2D{1,1};
salt_edge = salt_edge_2D{1,1};
poro_edge = poro_edge_2D{1,1};

% set the medium solution matrix
R_median_2D = nan(length(dis),no_curves);
porosity_median_2D = nan(length(dis),no_curves);
salt_median_2D = nan(length(dis),no_curves);
 
% set the uncertainty matrix
s=nan(dis,no_curves);
s_salt = nan(dis,no_curves);
s_poro = nan(dis,no_curves);
 
 for j = 1:no_curves
    % create cumlative count matrix
    for i=1:(dis-1) 
        cum_count=zeros(1,length(CI_density_2D{1,j}(i,:)));
		if Archie > 0		
			if Archie == 1
				cum_count_poro=zeros(1,length(CI_density_poro_2D{1,j}(i,:))); 
			else
				cum_count_salt=zeros(1,length(CI_density_salt_2D{1,j}(i,:)));
			end
		end
        % populate cumlative matirx - Res
        for cum=1:length(CI_density_2D{1,j}(i,:)) 
                if cum == 1
                    cum_count(1,cum) = CI_density_2D{1,j}(i,cum);
                else
                    cum_count(1,cum) = CI_density_2D{1,j}(i,cum) + cum_count(1,cum-1);
                end
        end
		if Archie > 0
			% populate cumlative matirx - Porosity
			if Archie == 1
				for cum=1:length(CI_density_poro_2D{1,j}(i,:))
					if cum == 1
					   cum_count_poro(1,cum)=CI_density_poro_2D{1,j}(i,cum);
					else
					   cum_count_poro(1,cum)=CI_density_poro_2D{1,j}(i,cum)+ cum_count_poro(1,cum-1);
					end
				end
			else % populate cumlative matirx - Salt
               for cum=1:length(CI_density_salt_2D{1,j}(i,:))
					if cum == 1
						cum_count_salt(1,cum)=CI_density_salt_2D{1,j}(i,cum);
					else
						cum_count_salt(1,cum)=CI_density_salt_2D{1,j}(i,cum)+ cum_count_salt(1,cum-1);
					end
                end 
			end
		end
        % make into percentage quartiles
        cum_count_percent = cum_count./(cum_count(1,length(CI_density_2D{1,j}(i,:))));
		if Archie > 0
			if Archie == 1
				cum_count_poro_percent=cum_count_poro./(cum_count_poro(1,length(CI_density_poro_2D{1,j}(i,:))));
			else
				cum_count_salt_percent=cum_count_salt./(cum_count_salt(1,length(CI_density_salt_2D{1,j}(i,:))));
			end
		end
         % make sure the 25th and 75th percentage quartile exists
        if cum_count_percent(1,1) > 0.25
            cum_count_percent(1,1) = 0.249;
        end
		if Archie > 0
			if Archie == 1
				if cum_count_poro_percent(1,1) > 0.25
				   cum_count_poro_percent(1,1) = 0.249;
				end
			else
				if cum_count_salt_percent(1,1) > 0.25
				   cum_count_salt_percent(1,1) = 0.249;
				end
			end
		end
        % find the 25th and 75th quartiles
        iqr25 = find(cum_count_percent<0.25);%find the 25% quartile
        R_25 = R_edge(1,max(iqr25));%find the 25% quartile
        iqr75 = find(cum_count_percent>0.75);%find the 75% quartile
        R_75 = R_edge(1,min(iqr75));%find the 75% quartile
        if abs(R_25) > 0 % if a real number 
			s(i,j) = R_75 - R_25; %popluate matrix s with iqr values
        end
		% find the 50th quartile
        [val50 iqr50] = min(abs(cum_count_percent-0.5));%find the 50% quartile
        R_median_2D(i,j) = R_edge(1,iqr50);%find the 50% quartile
		if Archie > 0
			if Archie == 1
				iqr25 = find(cum_count_poro_percent<0.25);%find the 25% quartile
				poro_25 = poro_edge(1,max(iqr25));%find the 25% quartile
				iqr75 = find(cum_count_poro_percent>0.75);%find the 75% quartile
				poro_75 = poro_edge(1,min(iqr75));%find the 75% quartile
				if abs(poro_25) > 0 % if a real number
					s_poro(i,j) = poro_75 - poro_25; %popluate matrix s with iqr values
				end
				% find the 50th quartile
				[val50p iqr50p] = min(abs(cum_count_poro_percent-0.5));%find the 50% quartile
				porosity_median_2D(i,j) = poro_edge(1,iqr50p);%find the 50% quartile
			else
				iqr25 = find(cum_count_salt_percent<0.25);%find the 25% quartile
				salt_25 = salt_edge(1,max(iqr25));%find the 25% quartile
				iqr75 = find(cum_count_salt_percent>0.75);%find the 75% quartile
				salt_75 = salt_edge(1,min(iqr75));%find the 75% quartile
				if abs(salt_25) > 0 % is a real number
				% calculate the width of the spread from 25th - 75th
					s_salt(i,j) = salt_75 - salt_25; %popluate matrix s with iqr values
				end
				[val50s iqr50s] = min(abs(cum_count_salt_percent-0.5));%find the 50% quartile
				salt_median_2D(i,j) = salt_edge(1,iqr50s);%find the 50% quartile
			end
		end
    end
 end
 
s(dis,:) = s(dis-1,:);%making matrix same size 
R_median_2D(dis,:) = R_median_2D(dis-1,:);
if Archie > 0
	if Archie == 1
		s_poro(dis,:) = s_poro(dis-1,:);%making matrix same size 
		porosity_median_2D(dis,:) = porosity_median_2D(dis-1,:);
	else
		s_salt(dis,:) = s_salt(dis-1,:);%making matrix same size 
		salt_median_2D(dis,:) = salt_median_2D(dis-1,:);
	end
end
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             PLOT 2D RESULTS                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% PLOT MODE SOLUTION    

% R
figure
subplot(2,1,1)
contourf(offsets, x, R_mode_2D, 1000, 'edgecolor', 'none')
hold on
plot(offsets, layer1,'k','linewidth',1)
hold on
plot(offsets, layer2,'k','linewidth',1)
hold on
plot(offsets, layer3,'k','linewidth',1)
set (gca,'Ydir','reverse')
title('Mode R model');
colormap(gca,'pink')
caxis([-1 5])
colorbar
ylim([700 1200])

% porosity
if Archie == 1
    subplot(2,1,2)
    contourf(offsets, x, porosity_mode_2D, 500, 'edgecolor', 'none')
    hold on
    plot(offsets, layer1,'k','linewidth',1)
    hold on
    plot(offsets, layer2,'k','linewidth',1)
    hold on
    plot(offsets, layer3,'k','linewidth',1)
    set (gca,'Ydir','reverse')
    title('Mode porosity model')
    colormap(gca, flipud(hot))
    caxis([0 1])
    colorbar
    ylim([700 1200])
% salinity
elseif Archie == 2
    subplot(2,1,2)
    contourf(offsets, x, salt_mode_2D, 500, 'edgecolor', 'none')
    hold on
    plot(offsets, layer1,'k','linewidth',1)
    hold on
    plot(offsets, layer2,'k','linewidth',1)
    hold on
    plot(offsets, layer3,'k','linewidth',1)
    set (gca,'Ydir','reverse')
    title('Mode salinity model')
    colormap(gca, flipud(hot))
    caxis([0 3])
    colorbar
    ylim([700 1200])
end

% PLOT MEDIUM SOLUTION 

% R
figure
subplot(2,1,1)
contourf(offsets, x, R_median_2D, 1000, 'edgecolor', 'none')
hold on
plot(offsets, layer1,'k','linewidth',1)
hold on
plot(offsets, layer2,'k','linewidth',1)
hold on
plot(offsets, layer3,'k','linewidth',1)
set (gca,'Ydir','reverse')
title('Medium R model');
colormap(gca,'pink')
caxis([-1 5])
colorbar
ylim([700 1200])

% porosity
if Archie == 1
    subplot(2,1,2)
    contourf(offsets, x, porosity_median_2D, 1000, 'edgecolor', 'none')
    hold on
    plot(offsets, layer1,'k','linewidth',1)
    hold on
    plot(offsets, layer2,'k','linewidth',1)
    hold on
    plot(offsets, layer3,'k','linewidth',1)
    set (gca,'Ydir','reverse')
    title('Medium porosity model')
    colormap(gca, flipud(hot))
    caxis([0 1])
    colorbar
    ylim([700 1200])
% salinity
elseif Archie == 2
    subplot(2,1,2)
    contourf(offsets, x, salt_median_2D, 1000, 'edgecolor', 'none')
    hold on
    plot(offsets, layer1,'k','linewidth',1)
    hold on
    plot(offsets, layer2,'k','linewidth',1)
    hold on
    plot(offsets, layer3,'k','linewidth',1)
    set (gca,'Ydir','reverse')
    title('Medium salinity model')
    colormap(gca, flipud(hot))
    caxis([0 3])
    colorbar
    ylim([700 1200])
end

% PLOT UNCERTAINTY

% Interquartile range R
figure
subplot(2,1,1)
contourf(offsets, x, s, 2000, 'edgecolor', 'none')
hold on
plot(offsets, layer1,'k','linewidth',1)
hold on
plot(offsets, layer2,'k','linewidth',1)
hold on
plot(offsets, layer3,'k','linewidth',1)
set (gca,'Ydir','reverse')
set(gca,'FontSize',14)
caxis([0 2])
colormap cool
colorbar
title('Estimated Uncertainty')
xlabel('Distance (m)')
ylabel('Depth (m)');
ylim([700 1200])

% Interquartile range porosity
if Archie == 1
    subplot(2,1,2)
    contourf(offsets, x, s_poro, 500, 'edgecolor', 'none')
    hold on
    plot(offsets, layer1,'k','linewidth',1)
    hold on
    plot(offsets, layer2,'k','linewidth',1)
    hold on
    plot(offsets, layer3,'k','linewidth',1)
    set (gca,'Ydir','reverse')
    set(gca,'FontSize',14)
    caxis([0 2])
    colormap cool
    colorbar
    title('Estimated Uncertainty Porosity')
    xlabel('Distance (m)')
    ylabel('Depth (m)');
    ylim([700 1200])
% Interquartile range Salinity
elseif Archie == 2
    subplot(2,1,2)
    contourf(offsets, x, s_salt, 500, 'edgecolor', 'none')
    hold on
    plot(offsets, layer1,'k','linewidth',1)
    hold on
    plot(offsets, layer2,'k','linewidth',1)
    hold on
    plot(offsets, layer3,'k','linewidth',1)
    set (gca,'Ydir','reverse')
    set(gca,'FontSize',14)
    caxis([0 2])
    colormap cool
    colorbar
    title('Estimated Uncertainty Salinity')
    xlabel('Distance (m)')
    ylabel('Depth (m)');
    ylim([700 1200])
end
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%SAVING WORKSPACE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%input file name remember to change name for each inversion
% save('output_2D_results.mat') 