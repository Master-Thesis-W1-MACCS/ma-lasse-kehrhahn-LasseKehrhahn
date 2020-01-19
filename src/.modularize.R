#
.overdesign_ <- function(EAD,NUMB_CN,NUMB_C,TQ) {
 
  OVERDESIGN = 3 
  
  

  
  
  for CN=1:CNs
  FR_CM_ARRAY_SINGLE(CN,1) = FR_CM_ARRAY(CN,1);
  seed = cell2mat(FR_CM_ARRAY(CN,1))';
    for FR = 2:FRs
        pre = cell2mat(FR_CM_ARRAY(CN,FR))';
  check = ~ismember(pre,seed);
  FR_CM_ARRAY_SINGLE(CN,FR)= {pre(check)'};
        seed = pre; % Set new seed for comparsion
    end 
end 

%%
CLUSTER_COMPONENT_MATRIX = zeros(CMs,CMs); % This is the maximum size for this matrix 
CM_ALL = 1:CMs; %How many CMs are in the system?


% OVERDESIGNIN THE MODULES IN ACCODANCE TO THE CNs
for CN=1:CNs
       %This path when modular design
       CMs = cell2mat(FR_CM_ARRAY(CN,FR_LEVEL));
       CLUSTER_COMPONENT_MATRIX(CN,1:numel(CMs))=  CMs;
end
% Fid THE REST FOR BEING SINGLE COMPONENTS;
CM_USED = CLUSTER_COMPONENT_MATRIX(CLUSTER_COMPONENT_MATRIX~=0);
check = ~ismember(CM_ALL',CM_USED);
SINGLE_CMs = sum(check>0); 
REMAINING_CMs = CM_ALL(check')';
                       CLUSTER_COMPONENT_MATRIX((CNs+1):(CNs+SINGLE_CMs),1) = REMAINING_CMs;
                       
                       
                       %% TRUNCATE ZEROS
                       
                       CLUSTER_COMPONENT_MATRIX( ~any(CLUSTER_COMPONENT_MATRIX,2), : ) = [];  %clean rows of zero
                       
                       %%
                         
                         %%======================= STEP 2 - CHECK DOUBLE COMPONENTS =========================
                         %CHECK FOR DUPLICATES % ASSUMPTION EVERY COMPONENT CAN ONLY BE ONE PART OF
                       %A CERTAIN MODULE. 
                       % Find the unique values
                       USED_COMPONENTS = CLUSTER_COMPONENT_MATRIX(CLUSTER_COMPONENT_MATRIX~=0); % clear all zeros.
                       DOUBLECOMPONENT=  find(hist(USED_COMPONENTS,unique(USED_COMPONENTS))>1);
                       
                       for i=1:numel(DOUBLECOMPONENT)
                       
                       [Clusterindex,idx] = find(CLUSTER_COMPONENT_MATRIX==DOUBLECOMPONENT(i));
                       for k=2:numel(Clusterindex)
                       DSMm.CLUSTER_COMPONENT_MATRIX(Clusterindex(k,:),idx(k,:)) = 0;
                       end
                       end
                       DSMm.CLUSTER_COMPONENT_MATRIX = CLUSTER_COMPONENT_MATRIX;
                       
                       
                       end 
  
  
}