%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  MuLTI TEM                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This Matlab script runs a transdimensional MCMC 
% inversion for TEM data 

% Author: Siobhan Killingbeck / Phil Livermore 
% School of Earth and Environemnt, The University of Leeds
% It is based on Matlab code written by Thomas Bodin.

% The physical model consists of internal layers, each with an associated resistivity (R), sampled as log(R) defined by Voronoi nuclei.

% The domain is divided into a number of layers, num_layers (which could be one)
% each with its own prior distribution on R.

% Each layer has a special nuclei that cannot leave its layer ("confined" nuclei).
% npt is the number of "floating" nuclei that can change layer.

% The total number of nuclei is therefore npt_max + num_layers

clear all % clear all the variables previously allocated
close all % close all the figures

%%%%%%%%%%%%%%%%%%%%%%%%%LOAD DATA%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%Loading time gates and received voltages %%%%%%%%%%%%%

load('input_1D_data.mat'); %load received voltages 
timegate = data(:,1:2);% define start and end times of the time gates in ms, this goes into the forward model calculation
timegate_centre = data(:,3);% define time gate centre in ms
data = data(:,4); % set observed data, received voltage in nanoVolts
nd =numel(data); % nd is the number of data points

%determine weighting to be applied in inversion in nV
weighting = data.*0.05; % 5% weighting 

running_mode = 1; %1 -> find posterior; 0 -> Find priors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        TEM SURVEY PARAMETERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

NCHNL = 20; % Number of time gates
REFTYM = 1.05; % Time (in ms) from which TMS or TOPN & TCLS are measured. 
               % For example, this could be signal off-time or start of downward ramp.
OFFTYM = 1.05; % time (milliseconds) between end of one pulse and the start of the next pulse 
               % (of opposite sign) since a bipolar waveform is assumed (lambda/4). 
               % For systems which have a signal which is always on, OFFTIME = 0.
TXON   = [0.0, 0.001, 1.0492, 1.05]; % digitised time (in milliseconds) of each point in the waveform (set at 4 points). 
                                     % In most cases, TXON(1) = 0, TXON(2) = pulse on-time, TXON(3) = pulse off-time, TXON(4) = REFTYM 
                                     % where TXON(4) - TXON(3) = turn off time e.g., [0.0, 0.001, 1.0492, 1.05];
TXAMP  = [0.0, 1.0, 1.0, 0.0]; % transmitter current in amps at time TXON(J), normally signal is normalised e.g., [0.0, 1.0, 1.0, 0.0];
TOPN   = [0.006000, 0.007625, 0.009750, 0.012500, 0.015880, ...
          0.020250, 0.025880, 0.033000, 0.042130, 0.053750, ...
          0.068500, 0.087380, 0.111400, 0.151700, 0.181100, ...
          0.231000, 0.294600, 0.375900, 0.479500, 0.611600]; % Start times (in ms) of receiver windows, (1x20);
TCLS   = [0.007625, 0.009750, 0.012500, 0.015880, 0.020250, ...
          0.025880, 0.033000, 0.042130, 0.053750, 0.068500, ...
          0.087380, 0.111400, 0.151700, 0.181100, 0.231000, ...
          0.294600, 0.375900, 0.479500, 0.611600, 0.780100]; % End times (in ms) of receiver windows, (1x20);
SXE    = [5, -5, -5, 5]; % east coordinate of vertex I for loop position J (m), fixed at 4 vertices e.g., [5, -5, -5, 5];
                         % Note the transmitter is fixed on the ground (Z=0) in this adapted Leroi code, 
                         % for airborne data more parameters will need to be passed through the mex file to model.
SXN    = [5, 5, -5, -5]; % north coordinate of vertex I for loop position J (m), fixed at 4 vertices e.g., [5, 5, -5, -5];
                         % Note the transmitter is fixed on the ground (Z=0) in this adapted Leroi code, 
                         % for airborne data more parameters will need to be passed through the mex file to model.
RXE    = 15; % receiver easting (m) ;
RXN    = 0; % receiver northing (m) ;
RXZ    = 0; % receiver z (always be 0 for ground based TEM);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     VERY IMPORTANT PARAMETERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

burn_in=10000; % burn-in period
nsample=1000000; % total number of samples

%Uniform prior on depths for nuclei
priors.depth_min=0; % Cannot be changed, always from the surface.
priors.depth_max=160;
priors.npt_max = 80;
priors.npt_min = 0; 

num_layers = 3; % depth constraining parameter, if set to 1 no depth constraints will be applied.

priors.layer_depths = zeros(num_layers-1,1); %the last layer depth is infinity (and is not defined).
priors.Rmin = zeros(num_layers,1);
priors.Rmax = zeros(num_layers,1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Define the priors and layer geometry
if num_layers == 3
    priors.layer_depths(1) = 3.4 ; % layer 1 depth.
    priors.layer_depths(2) = 26.3 ; % layer 2 depth.
    priors.Rmin(1) = log(1); % layer 1
    priors.Rmax(1) = log(1000); 
    priors.Rmin(2) = log(1000); % layer 2
    priors.Rmax(2) = log(100000);
    priors.Rmin(3) = log(1); % layer 3
    priors.Rmax(3) = log(100000);
elseif num_layers == 2
    priors.layer_depths(1) = 23 ; % layer 1 depth.
    priors.Rmin(1) = log(1000); % layer 2
    priors.Rmax(1) = log(100000);
    priors.Rmin(2) = log(1); % layer 3
    priors.Rmax(2) = log(100000);
else % num_layers = 1 
    priors.Rmin(1) = log(1); % wide range of constraints 
    priors.Rmax(1) = log(100000);
end

npt_init=0; % initial number of floating nuclei

sigma_change_R = 2; % std deviation of Gaussian proposal on Change R value
sigma_move_depth = 10; % std deviation of Gaussian proposal on MOVE (change depth)
sigma_birth_R = 2; % std deviation of Gaussian proposal on BIRTH
% (this number is also present in the DEATH
% acceptance term when taking in acount the reverse jump )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LESS IMPORTANT PARAMETERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rng('default');
rng(1);
% You can change the 1 to any other number to change the seed.

% Define the limits of your model
x_min = priors.depth_min;
x_max = priors.depth_max;
dis=100; % steps to discretize the model. Trade-off between computational time and accuracy. 

show=10000; % show statistics of the chain every "show" samples
thin = 100; % thining

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
x =linspace(x_min,x_max,dis); % discretize the model
y = linspace(min(priors.Rmin), max(priors.Rmax), dis); %y limits are R priors limits
num=ceil((nsample-burn_in)*0.025/thin); % number of collected samples

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      Preallocation of variables

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

errors_Leroi = nan(nsample,(1 + (priors.npt_max+num_layers)*2));
best=zeros(dis,1);
val_min=zeros(dis,1);
val_max=zeros(dis,1);
ind_min_R=zeros(dis,1);
ind_max_R=zeros(dis,1);
hist_R=zeros(dis,1);
hist_density=zeros(dis,1);
hierhist=zeros(nsample,1);
change_points=zeros(nsample*(priors.npt_max+num_layers),1);
cov=zeros(nsample,1);
nnuclei=zeros(nsample,1);
sup=zeros(dis,1);
inf=zeros(dis,1);
MINI_R=zeros(dis,num);
MAXI_R=zeros(dis,num);
CI_density=zeros((dis-1),(dis-1));
forward_model_ensemble = NaN(length(timegate),(nsample-burn_in)); 

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
R = exp(R);
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
        for i = 1:length(timegate) % time samples, this should match the time samples.
        misfit(i,1) = data(i,1) - forward_model(i,1);
        end %end multimodal misfit
    
    like = nansum( (misfit).^2 ./(2 * weighting.^2) );
    else
    like = 1;
    end %end running_mode
end

like_best=1e99;
like_init=like;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%% START RJ-MCMC SAMPLING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fprintf('Total number of samples %i\n',nsample);
fprintf('Acceptance rates:\n');
fprintf('Iteration  Change R    Move Depth    Birth     Death\n');
for s=1:nsample
    out=1;
    % Print statistics of the chain, The best is to "tune" these
    % ratios to 44 %. (Rosental 2000).
    if (mod(s,show)==0)
        number_of_samples = s;
        number_of_nuclei = npt;
        if (s>burn_in)
            fprintf('%7i     %5.2f         %5.2f       %5.2f     %5.2f\n',s, 100*AcV/PV, 100*AP/PP, 100*AB/PB,100*AD/PD);
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
        R = exp(R);
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
                forward_model = LEROI_TEM(NCHNL, REFTYM, OFFTYM, TXON, TXAMP, TOPN, TCLS, SXE, SXN, RXE, RXN, RXZ, npt_prop+num_layers, 0, npt_prop + num_layers, Res, linspace(1,(npt_prop+num_layers),(npt_prop+num_layers)), thickness);
                forward_model = round(forward_model,6,'significant'); 
                %%%%%%%%%% computing the misfit %%%%%%%%%%%
                misfit = NaN(length(data),1);
                for i = 1:length(timegate) % time samples, this should match the time samples.
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
    end
    
    % The acceptance term takes different
    % values according the the proposal that has been made.
    
    if (birth==1)        
        if (rand<((1/(Prod_delta_prior*prob))*exp(log(out)-like_prop+like)))
            accept=1;
            if (s>burn_in)
                AB=AB+1;
            end
        end
    elseif (death==1)
        
        if (rand<(Prod_delta_prior*prob*exp(log(out)-like_prop+like)))
            accept=1;
            if (s>burn_in)
                AD=AD+1;
            end
        end
        
    elseif (move == 1) % NO JUMP, i.e no change in dimension
        
        if (rand<(move_prob * exp(log(out)-like_prop+like)))
            accept=1;
            if (s>burn_in)
            AP=AP+1;       
            end %if (s>burn_in)
        end
        
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
    end
        for i=1:dis
            ind=whichnuclei(nuclei_depths(1:npt+num_layers),x(i),num_layers,priors);
            hist_R(i)=nuclei_R(ind);
        end
        [N]=histcounts2(x,hist_R',x,y);
        R_edge=y(1:(dis-1));
        depth_edge=x(1:(dis-1));
    
           
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % We collect the samples for the ensemble solution
    
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
        
            CI_density = CI_density + N;
            if running_mode==1
                forward_model_ensemble(:,(s-burn_in)) = forward_model;
            end
        
    end %if burn-in
    
    cov(s)=like; % Convergence of the misfit
    nnuclei(s)=npt+num_layers; % Convergence of number of nuclei
            
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
   
    
end% the Sampling of the mcmc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%% Take average and credible intervals %%%%%%%%%%%
AV=AV./b;
%
best_model_R = zeros(1,dis);

for i=1:dis
    ind=whichnuclei(depths_best(1:num_layers+npt_best),x(i),num_layers, priors);
    best_model_R(i)=R_best(ind);
end

%%%%%%%%%%%%%%%%%%%%%%% PLOT RESULTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot the best, average and mode solutions

% R
for i=1:dis
    [val_min ind_min]=min(MAXI_R(i,:));
    [val_max ind_max]=max(MINI_R(i,:));
    sup(i)=val_min;
    inf(i)=val_max;
end
%
x2 = [x'; flipud(x')];
inbetween = [inf; flipud(sup)];
%%%%%%%%%%%%%%%%%%%%%%
N = histcounts2(x2,inbetween,x,y);
for j=1:(dis-1)
    f=find(N(j,:),1,'first');
    l=find(N(j,:),1,'last');
    for i = f:l
        N(j,i)=1;
    end
end
CI_density_limit=CI_density.*N;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%normalising density plot
sizeCI = size(CI_density_limit);
len = sizeCI(1,1);
wid = sizeCI(1,2);
CI_density_limitN = zeros(len,wid);
for i = 1:len
    m = sum(CI_density_limit(i,:));
    for j = 1:wid
        CI_density_limitN(i,j) = CI_density_limit(i,j)/m;
    end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%find mode solution 
%calculating modal solution from CI_density plot
    mode = zeros((dis-1),(dis-1));
    for i=1:(dis-1)
        [M,I]=max(CI_density_limit(i,:));
        mode(i,I)=1;
    end
    R_mode=zeros((dis-1),(dis-1));
    for i=1:(dis-1)
    R_mode(i,:) = mode(i,:).* R_edge;
    end   
%removing the zeros in the solution
    R_mode_n0 = nan((dis-1),1);
    for i=1:(dis-1)
    R_mode_n0(i,1) = max(R_mode(i,:));
    end
    R_mode_n0(dis,1) = R_mode_n0(dis-1,1);
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% log(R) plot (linear scale, log(R)) 
figure
imagesc(R_edge,depth_edge,CI_density_limit) %pdf plot
hold on
plot(best_model_R,x,'k','LineWidth',2); %best solution
hold on
plot(inbetween, x2,'g'); %95% credible interval
hold on
plot(R_mode_n0,x,'k','LineWidth',2); % mode solution
hold on
plot(AV(:,1),x,'r','LineWidth',2); %average solution
title('Log(Resistivity) PDF','FontSize',14);
ylabel('Depth (m)','FontSize',14);
xlabel('log(R) (Ohm.m)','FontSize',14);
set(gca,'XMinorTick','on') 
set(gca,'Ydir','reverse')
colormap jet
colorbar
caxis([0 100000])

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% True R plot (Normalised, log scale, true R)
R_edge_true = exp(R_edge);
best_model_R_true = exp(best_model_R);
inbetween_true = exp(inbetween);
R_mode_n0_true = exp(R_mode_n0);
AV_true = exp(AV(:,1));
% R plot
figure
contourf(R_edge_true, depth_edge,CI_density_limitN, 1000, 'LineColor', 'none') %pdf plot
hold on
plot(R_mode_n0_true,x,'k','LineWidth',2); % mode solution
hold on
plot(AV_true,x,'r','LineWidth',2); %average solution
title('Resistivity PDF','FontSize',14);
ylabel('Depth (m)','FontSize',14);
xlabel('R (Ohm.m)','FontSize',14);
set(gca,'XScale','log') 
set(gca,'XMinorTick','on') 
set(gca,'Ydir','reverse')
colormap jet
colorbar
caxis([0 0.4])
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%plot data fit with 200 random ensemble of models accepted
if running_mode == 1
   figure;
   plot(timegate_centre, data, '*');
   hold on;
   plot(timegate_centre, forward_model_ensemble(:,(nsample-burn_in-200):(nsample-burn_in)), 'r');
   set(gca, 'XScale', 'log','FontSize',14)
   set(gca, 'YScale', 'log','FontSize',14)
   ylabel('Received voltage (nV)','FontSize',14);
   xlabel('Time (ms)','FontSize',14);
   title('Comparison of data fit','FontSize',14);
end
%%%%%%%%%%%%%%% Plot Statistics of the chain %%%%%%%%%%%%%%%%
figure
subplot(2,1,1)
semilogy(cov);
hold on
%line([burn_in burn_in],[0 cov(1)],'LineWidth',4,'Color',[1 0 0]);
xlabel('iterations','FontSize',14)


title('Data Misfit','FontSize',16)
subplot(2,1,2);
line([burn_in burn_in],[0 priors.npt_max+num_layers],'LineWidth',4,'Color',[1 0 0]);
hold on
plot(nnuclei)
xlabel('iterations','FontSize',14)
title('Number of nuclei','FontSize',16)


%%%%%%%%%%%%%% Plot Marginal posteriors %%%%%%%%%%%%%%%%%%%

% Plot histogram on change points
figure
subplot(2,1,1)
hist(change_points(1:bb),500)
title('Probability of change points','FontSize',14)

% Plot histogram on number of nuclei
subplot(2,1,2);
bar(nnucleihist)
title('Posterior Distribution on number of nuclei ','FontSize',14)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%SAVING WORKSPACE%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%input file name remember to change name for each inversion
save('output_1D_results.mat') 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%      THE END                              %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

