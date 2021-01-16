%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is a function called by the main inversion program 
% that finds the nuclei index corresponding to the depth.
% 

function index = whichnuclei(points, depth,num_layers, priors)

%find out which domain the nuclei is in:
    layer = num_layers;
    for j = 1:num_layers - 1
        if depth  <= priors.layer_depths(j)
            layer = j;
            break
        end
    end    

% Find the top/bottom of the layer
    if layer == 1
        if num_layers == 1
            top_of_layer = priors.depth_min;
            bottom_of_layer = priors.depth_max;
        else
            top_of_layer = priors.depth_min;
            bottom_of_layer = priors.layer_depths(1);
        end
    elseif layer < num_layers
        top_of_layer = priors.layer_depths(layer-1);
        bottom_of_layer = priors.layer_depths(layer);
    else 
        top_of_layer = priors.layer_depths(layer-1);
        bottom_of_layer = priors.depth_max;
    end
            
        
  indices = find( (points < bottom_of_layer) & (points >= top_of_layer) );
    
  [Y,index] = min(abs( depth - points(indices)));
  index = indices(index);
    
end

