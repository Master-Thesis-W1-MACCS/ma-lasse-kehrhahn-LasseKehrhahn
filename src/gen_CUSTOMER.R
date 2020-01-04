#

gen_CUSTOMER <- function(EAD,NUMB_CN,NUMB_C,TQ) {
  
# Customer generation function
#  % 23.4. - Generates the customers in the market 
#  % 24.7. - Make customers' attributes more precise, in particular CN and FRN
#   01.10 - Code simplification 

NUMB_CN = 3 
NUMB_C =3
## Which customers exist and how many needs do they got? 
    
    # Customer.NEEDS 
    # Explanation; 
    
    #       CN1  CN2  CNm 
    # C1    1     1   1       [does have this necessity]  
    # C2    1     0   1       [does not have this necessity]
    # Cn    n     n   nm
  
  
    C_CN.matrix_pre = runif(NUMB_CN*NUMB_C) #draw random numbers
    ## 1/0 DENSITY
    C_CN.matrix <- matrix(ifelse(C_CN.matrix_pre > 0.4, 0,1),NUMB_CN, NUMB_C)
    

#     % MANUAL EXCPTION SET THE MAXIMUM TO A LEVEL OF THREE 
#     CN_TO_FR_MAXIMUM(1:NUMB_CN) = NUMB_MAX_FR;
#     
#     % CN_TO_FR_MAXIMUM EXPLANATION
#     %                   CustomersNeed1         CustomersNeed2  
#     % Amount of FR      4 [four different FR]  [1 just one FR]  
#     
#     Customer.FR = CN_TO_FR_MAXIMUM;
#     
# ## ========================= Determining customers demand for the functional =========================
# # Each customer need (CN) has at least one functional requirement (FR).
# # Each customer does not always need the highest FR; 
# # A Functional requirement can be an ordinary specification such as increasing performance of an engine (1 = 100 HP, 2 = 200 HP) 
# # or an option such as a color (blue = 1, green = 2 .... ]
# # Here we build a reference market sturcture that is automatically the ideal structure for a product family 
# #(see Du et al 2001, Martin (1996))
# 
#     
#     
# MAX_DESIGN_CN_FR = Customer.NEEDS.*CN_TO_FR_MAXIMUM; % Generating for each customer the maximum FR 
# Customer.VAM_Reference = MAX_DESIGN_CN_FR; % Transfering to the value to the struct for later usage
# 
# %% Determining the market structure by performance 
# % % % PERFORMANCE_CUSTOMER = zeros(Customer.NUMB,1);
# % % % LOW_PERFORMANCE_MIN = 0.2;
# % % % LOW_PERFORMANCE_MAX = 0.5;
# % % % LOW_PERFORMANCE= LOW_PERFORMANCE_MIN + (LOW_PERFORMANCE_MAX-LOW_PERFORMANCE_MIN).*rand(1,1); %Scalar Uniformley distribution (0,1)
# % % % CUSTOMER_OF_HIGH_PERFORMANCE = ceil(NUMB_AGENTS*0.2); 
# % % % CUSTOMER_OF_LOW_PERFORMANCE = ceil((NUMB_AGENTS-CUSTOMER_OF_HIGH_PERFORMANCE)*LOW_PERFORMANCE) + CUSTOMER_OF_HIGH_PERFORMANCE;
# % % % % % % PERFORMANCE_CUSTOMER(1:CUSTOMER_OF_HIGH_PERFORMANCE,:)=HIGH ; 
# % % % % % % PERFORMANCE_CUSTOMER((CUSTOMER_OF_HIGH_PERFORMANCE+1):CUSTOMER_OF_LOW_PERFORMANCE,:)=LOW ; 
# % % % % % % 
#   
#   % Random allocation of functional requirements to each customer. 
# % VALUE(ROW, COLUMN) 
# 
# for i=1:Customer.NUMB
# for j=1:Customer.NUMB_CN
# if MAX_DESIGN_CN_FR(i,j)>0   
# Customer.CN_FR_MAP(i,j)= randi([NUMB_MIN_FR max(MAX_DESIGN_CN_FR(i,j))],1,1);
# else 
#   Customer.CN_FR_MAP(i,j)= 0;
# end 
# end
# end
# 
# NUMB_MIN_FR = 1; #Auxilliar variable that declares the minimum level of product specs.




}

  