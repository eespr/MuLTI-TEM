function [thickness, layer_R, priors_OK] = ...
    thicknesses_and_priors(nuclei_positions_unsorted, nuclei_R, num_floating_nuclei, num_layers, priors)
% Function thickness_and_priors
% (i) Tests prior information
% (ii) If all priors OK, converts from Voronoi description to layer description

% 

priors_OK = 1;  % 1 means all OK, 0 means priors not satisfied, reject model
thickness  = zeros(1, num_floating_nuclei+num_layers);
% 

if num_floating_nuclei > priors.npt_max  | num_floating_nuclei < priors.npt_min
    priors_OK = 0; thickness = 0; layer_R = 0; %early return
    return;
end 

% checking the special domain-confined (unsorted) nuclei
if num_layers > 1 % dont check if num_layers = 1
    for i = 1:num_layers
        if i == 1
            top_of_layer = priors.depth_min;
            bottom_of_layer = priors.layer_depths(1);
        elseif i < num_layers
            top_of_layer = priors.layer_depths(i-1);
            bottom_of_layer = priors.layer_depths(i);
        else
            top_of_layer = priors.layer_depths(i-1);
            bottom_of_layer = priors.depth_max;
        end
        if nuclei_positions_unsorted(i) < top_of_layer | nuclei_positions_unsorted(i) > bottom_of_layer
           priors_OK = 0; thickness = 0; layer_R = 0; %early return
        return;
        end
    end
end

[nuclei_positions,I] = sort(nuclei_positions_unsorted(1:num_floating_nuclei+num_layers));

% define the variables. Note that since everything is sorted by
% depth, we can do this in one pass:

layer_R = [nuclei_R(I)];

% Test priors on R
for i=1: num_floating_nuclei + num_layers
    
% Find the layer of the ith nuclei
    layer = num_layers;
        for j = 1:num_layers - 1
            if nuclei_positions(i) <= priors.layer_depths(j)
                layer = j;
                break;
            end
        end
% Test the prior
    if layer_R(i) < priors.Rmin(layer) || layer_R(i) > priors.Rmax(layer) 
       priors_OK = 0;
       return; % early return
    end
end

if( max( nuclei_positions ) > priors.depth_max | min( nuclei_positions ) < priors.depth_min )
    priors_OK = 0;
    return;
end

% Assemble the layer thicknesses, taking note also of the fixed domain
% boundaries.

starting_index = 1;

for layer = 1:num_layers
    if num_layers ==1
        layer_indices = 1:num_floating_nuclei+num_layers;
        top_of_layer = 0 ;
    else
        if layer == 1
            layer_indices = find( nuclei_positions < priors.layer_depths(layer));
            top_of_layer = 0;
        elseif layer < num_layers
            layer_indices = find( nuclei_positions < priors.layer_depths(layer) & nuclei_positions > priors.layer_depths(layer-1));
            top_of_layer =  priors.layer_depths(layer-1);
        else
            layer_indices = find(nuclei_positions > priors.layer_depths(layer-1));
            top_of_layer =  priors.layer_depths(layer-1);
        end
    end
        
if numel(layer_indices) == 1  %If there is only one point in the domain, then....
    if layer == 1
        if num_layers == 1
            thickness(starting_index) = 0; %one point, and one layer...
        else
        thickness(starting_index) = priors.layer_depths(1);
        end
    elseif layer < num_layers
        thickness(starting_index) = priors.layer_depths(layer) - priors.layer_depths(layer-1);
    else
        thickness(starting_index) = 0; %set last thickness to 0  -- it represents the half space.  
    end
    
else %more than one nuclei within the layer.
    thickness(starting_index) = 0.5 * (nuclei_positions(layer_indices(1)) + nuclei_positions(layer_indices(2))) - top_of_layer;
    % fill intermediate layers
    for i=2: numel(layer_indices)-1
    thickness(i+starting_index-1) = 0.5 * (nuclei_positions(layer_indices(i+1))+nuclei_positions(layer_indices(i))) - 0.5 * (nuclei_positions(layer_indices(i)) + nuclei_positions(layer_indices(i-1)));
    end
    % now do last internal layer
    if layer == num_layers
        thickness(starting_index-1 + numel(layer_indices)) = 0; %set last layer to have depth 0
    else
        thickness(starting_index-1 + numel(layer_indices)) = priors.layer_depths(layer) - sum( thickness(1:starting_index + numel(layer_indices)-1 ));
    end
end
starting_index = starting_index + numel(layer_indices);
end



end %end of function