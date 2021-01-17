% This matlab script plots all the TEM responses PDFs output along a 2D line 
% and all the 1D resistivity PDFs output along a 2D line.
% using multiple (3) base frequencies

load('cmap_jet.mat')

load('outputIII_2D_results.mat')

for c = 1:no_curves
    figure
    %%%%%%%%%%%%%%%%%%% 30 hz
    subplot(2,3,[1 4])
    imagesc(time_edge_2D{1,c},FM_edge_2D{1,c},CI_density_FM_2D{1,c}) %pdf plot
    hold on;
    plot( log10(Centre1), log10(data_2D{1,c}(:,2)), '.k','markersize',8);
    hold on;
    plot( log10(Centre1), log10(data_2D{1,c}(:,2).*0.95),'--k','markersize',8);
    hold on;
    plot( log10(Centre1), log10(data_2D{1,c}(:,2).*1.05), '--k','markersize',8);
    % Create ylabel
    ylabel('Received voltage (nV)');
    % Create xlabel
    xlabel('Time (ms)');
    colormap(cm_jet)
    colorbar
    caxis([0 1000])
    set(gca,'YDir','normal')
    %%%%%%%%%%%%%%%%%%% 7.5 hz
    subplot(2,3,[2 5])
    imagesc(time_edge2_2D{1,c},FM_edge2_2D{1,c},CI_density_FM2_2D{1,c}) %pdf plot
    hold on;
    plot( log10(Centre2), log10(data_2D{2,c}(:,2)), '.k','markersize',8);
    hold on;
    plot( log10(Centre2), log10(data_2D{2,c}(:,2).*0.925),'--k','markersize',8);
    hold on;
    plot( log10(Centre2), log10(data_2D{2,c}(:,2).*1.075), '--k','markersize',8);
    % Create ylabel
    ylabel('Received voltage (nV)');
    % Create xlabel
    xlabel('Time (ms)');
    colormap(cm_jet)
    colorbar
    caxis([0 1000])
    set(gca,'YDir','normal')
    %%%%%%%%%%%%%%%%%% 3 hz
    subplot(2,3,[3 6])
    imagesc(time_edge3_2D{1,c},FM_edge3_2D{1,c},CI_density_FM3_2D{1,c}) %pdf plot
    hold on;
    plot( log10(Centre3), log10(data_2D{3,c}(:,2)), '.k','markersize',8);
    hold on;
    plot( log10(Centre3), log10(data_2D{3,c}(:,2).*0.9),'--k','markersize',8);
    hold on;
    plot( log10(Centre3), log10(data_2D{3,c}(:,2).*1.1), '--k','markersize',8);
    % Create ylabel
    ylabel('Received voltage (nV)');
    % Create xlabel
    xlabel('Time (ms)');
    colormap(cm_jet)
    colorbar
    caxis([0 1000])
    set(gca,'YDir','normal')
end

figure
for c = 1:no_curves
    subplot(1,no_curves,c)
    imagesc(R_edge_2D{1,c},depth_edge_2D{1,c},CI_density_limit_2D_N{1,c}) %pdf plot
    %hold on
    %plot(R_mode_2D(:,c),x,'r','LineWidth',2); % mode solution
    hold on
    plot(R_median_2D(:,c),x,'k','LineWidth',2); %med solution
    ylabel('Depth (m)','FontSize',14);
    xlabel('10^R (Ohm.m)','FontSize',14);
    set(gca,'XMinorTick','on') 
    set(gca,'Ydir','reverse')
    colormap(cm_jet)
    caxis([0 0.05]) 
    ylim([700 1200])
end