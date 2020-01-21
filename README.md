# EAD_
 
 %% ======================================  DOKUMENTATION ============================================================
% V 0.1 New resource consumption algorithm - Now the model uses highest 
% intensities from the DMM for the modules. This helps that the modules are
% not cheaper than before (see Literature)

% V 0.15 Added new variable C_DENSITY that replaces the old C_HETEROGENEITY
% C_DENSITY is responsible for the customer need share across customer 
% C_HETEROGENEITY is responsible for the different requirements in th
% eproduct specification 

% V 0.2 Improvements in the functional requirement generation 

% V 0.2 Implementation of product variant tracking . Now two excelfiles

% will generated with an aggregated and variant-related output. 
% V 0.3 Implementation of manufacturing costs analyses. The algorithms are

% able to differentiate between manufacturing and other costs. (unit-level
% vs. other) 
% V 0.4 Found a bug regarding the component distribution. If there wasn't
% enough components, there was no accumulation of the Design parameters. (6818)
% Additionally, I add the VARIANT_M measures that allows to see the
% unit-level costs in detail. 
% V 0.5 Usage after Nelly, performing systzematic analyses for thesis. 

% V 2.00 The simulation model gets a refresh of some functions and names.
% Preparing dissertation experiments. 

% V 2.10 Upgrading the DSM_Overdesign ; Increases speed and relaibility of
% reulsts; Changing clustering to determiniation; 190409
% V 2.11 New descriptive measurements 


% V 2.2 ; Bug fix! There was the possibility that activities are not fully
% filled with non zero values. The algorithm noticed that but have not
% saved it but indirectly put in a new random number. 

% V 2.3 ; Product architecture density has been included ; 25/4/19 Several
% test provided evidences that solely the lower triangular makes sence for
% up- and downscaling; 

%
