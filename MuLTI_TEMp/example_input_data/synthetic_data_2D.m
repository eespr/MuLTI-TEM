
%%%%%%%%%%%%%%%%%%%%%%%%%% 7.5 Hz BASE FREQUENCY %%%%%%%%%%%%%%%%%%%%%%%%%

% TEM survey design
NCHNL = 30;
REFTYM = 33.25;
OFFTYM = 33.25;
TXON   = [0.0, 0.46 32.79, 33.25];
TXAMP  = [0.0, 25.0, 25.0, 0.0];
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
SXE    = [250, -250, -250, 250];
SXN    = [250, 250, -250, -250];
RXE    = 0;
RXN    = 0;
RXZ    = 0;
Centre = (TCLS-TOPN) + TOPN;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Synthetic 2D models
% 0
LYTH   = [10000, -1, 1, 1, 0, 0, 1; ...
          100, -1, 1, 1, 0, 0, 1; ...
          1000, -1, 1, 1, 0, 0, 1];
LITHL  = [1, 2, 3];
THK    = [800, 35];
NLYR   = 3;
NPLT   = 0;
NLITH  = 3;

out1 = LEROI_TEM(NCHNL, REFTYM, OFFTYM, TXON, TXAMP, TOPN, TCLS, SXE, SXN, RXE, RXN, RXZ, ...
          NLYR, NPLT, NLITH, LYTH, LITHL, THK);

% 500
LYTH   = [10000, -1, 1, 1, 0, 0, 1; ...
          3, -1, 1, 1, 0, 0, 1; ...
          100, -1, 1, 1, 0, 0, 1; ...
          1000, -1, 1, 1, 0, 0, 1];
LITHL  = [1, 2, 3, 4];
THK    = [800, 5, 60];
NLYR   = 4;
NPLT   = 0;
NLITH  = 4;

out2 = LEROI_TEM(NCHNL, REFTYM, OFFTYM, TXON, TXAMP, TOPN, TCLS, SXE, SXN, RXE, RXN, RXZ, ...
          NLYR, NPLT, NLITH, LYTH, LITHL, THK);

% 1000
LYTH   = [10000, -1, 1, 1, 0, 0, 1; ...
          3, -1, 1, 1, 0, 0, 1; ...
          100, -1, 1, 1, 0, 0, 1; ...
          1000, -1, 1, 1, 0, 0, 1];
LITHL  = [1, 2, 3, 4];
THK    = [800, 5, 90];
NLYR   = 4;
NPLT   = 0;
NLITH  = 4;

out3 = LEROI_TEM(NCHNL, REFTYM, OFFTYM, TXON, TXAMP, TOPN, TCLS, SXE, SXN, RXE, RXN, RXZ, ...
          NLYR, NPLT, NLITH, LYTH, LITHL, THK);

% 1500
LYTH   = [10000, -1, 1, 1, 0, 0, 1; ...
          3, -1, 1, 1, 0, 0, 1; ...
          100, -1, 1, 1, 0, 0, 1; ...
          1000, -1, 1, 1, 0, 0, 1];
LITHL  = [1, 2, 3, 4];
THK    = [800, 5, 120];
NLYR   = 4;
NPLT   = 0;
NLITH  = 4;

out4 = LEROI_TEM(NCHNL, REFTYM, OFFTYM, TXON, TXAMP, TOPN, TCLS, SXE, SXN, RXE, RXN, RXZ, ...
          NLYR, NPLT, NLITH, LYTH, LITHL, THK);
% 2000
LYTH   = [10000, -1, 1, 1, 0, 0, 1; ...
          3, -1, 1, 1, 0, 0, 1; ...
          100, -1, 1, 1, 0, 0, 1; ...
          1000, -1, 1, 1, 0, 0, 1];
LITHL  = [1, 2, 3, 4];
THK    = [800, 5, 150];
NLYR   = 4;
NPLT   = 0;
NLITH  = 4;

out5 = LEROI_TEM(NCHNL, REFTYM, OFFTYM, TXON, TXAMP, TOPN, TCLS, SXE, SXN, RXE, RXN, RXZ, ...
          NLYR, NPLT, NLITH, LYTH, LITHL, THK);

% 2500
LYTH   = [10000, -1, 1, 1, 0, 0, 1; ...
          3, -1, 1, 1, 0, 0, 1; ...
          100, -1, 1, 1, 0, 0, 1; ...
          1000, -1, 1, 1, 0, 0, 1];
LITHL  = [1, 2, 3, 4];
THK    = [800, 5, 180];
NLYR   = 4;
NPLT   = 0;
NLITH  = 4;

out6 = LEROI_TEM(NCHNL, REFTYM, OFFTYM, TXON, TXAMP, TOPN, TCLS, SXE, SXN, RXE, RXN, RXZ, ...
          NLYR, NPLT, NLITH, LYTH, LITHL, THK);

% 3000
LYTH   = [10000, -1, 1, 1, 0, 0, 1; ...
          100, -1, 1, 1, 0, 0, 1; ...
          1000, -1, 1, 1, 0, 0, 1];
LITHL  = [1, 2, 3];
THK    = [800, 215];
NLYR   = 3;
NPLT   = 0;
NLITH  = 3;

out7 = LEROI_TEM(NCHNL, REFTYM, OFFTYM, TXON, TXAMP, TOPN, TCLS, SXE, SXN, RXE, RXN, RXZ, ...
          NLYR, NPLT, NLITH, LYTH, LITHL, THK);     
      
% 3500
LYTH   = [10000, -1, 1, 1, 0, 0, 1; ...
          100, -1, 1, 1, 0, 0, 1; ...
          1000, -1, 1, 1, 0, 0, 1];
LITHL  = [1, 2, 3];
THK    = [800, 305];
NLYR   = 3;
NPLT   = 0;
NLITH  = 3;

out8 = LEROI_TEM(NCHNL, REFTYM, OFFTYM, TXON, TXAMP, TOPN, TCLS, SXE, SXN, RXE, RXN, RXZ, ...
          NLYR, NPLT, NLITH, LYTH, LITHL, THK);     
      
data_2D{2,1}(:,1) = Centre;
data_2D{2,1}(:,2) = out1;
data_2D{2,2}(:,1) = Centre;
data_2D{2,2}(:,2) = out2;
data_2D{2,3}(:,1) = Centre;
data_2D{2,3}(:,2) = out3;
data_2D{2,4}(:,1) = Centre;
data_2D{2,4}(:,2) = out4;
data_2D{2,5}(:,1) = Centre;
data_2D{2,5}(:,2) = out5;
data_2D{2,6}(:,1) = Centre;
data_2D{2,6}(:,2) = out6;
data_2D{2,7}(:,1) = Centre;
data_2D{2,7}(:,2) = out7;
data_2D{2,8}(:,1) = Centre;
data_2D{2,8}(:,2) = out8;

figure 
plot(Centre, out1, 'k','linewidth',3)
hold on
plot(Centre, out2, 'k','linewidth',3)
hold on
plot(Centre, out3, 'c','linewidth',3)
hold on
plot(Centre, out4, 'g','linewidth',3)
hold on
plot(Centre, out5, 'y','linewidth',3)
hold on
plot(Centre, out6, 'r','linewidth',3)
hold on
plot(Centre, out7, 'm','linewidth',3)
hold on
plot(Centre, out8, 'g','linewidth',3)
hold on
plot([0.005 100],[1 1],'--r')
set(gca, 'XScale', 'log','FontSize',12)
set(gca, 'YScale', 'log','FontSize',12)
ylabel('Received voltage (nV)','FontSize',12);
xlabel('Time (ms)','FontSize',12);
title('Lake system 2D','FontSize',12);
xlim([0.005 100])
ylim([0.001 10000])

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
save('2D_input_data.mat','data_2D')
