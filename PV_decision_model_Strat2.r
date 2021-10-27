### Decision model for the certification of PuroVerde's agroforestry sites

# Install and load package "decisionSupport"
#install.packages("decisionSupport")
library(decisionSupport)

## Prepare data
PrepareData <- {  
  format(read.csv("data/PV_Input_variables_strat2.csv")[,1:5], scientific=FALSE)
  
  
  make_variables<-function(est,n=1)
  { x<-random(rho=est, n=n)
  for(i in colnames(x)) assign(i,
                               as.numeric(x[1,i]),envir=.GlobalEnv)
  }
  
  make_variables(estimate_read_csv("data/PV_Input_variables_strat2.csv"))
}




benefit.sim.PV.strat2 <- function(){
  
  
  ##########GENERAL#############  
  
  ###Create general "Year" data frame (To start counting of years before certification
  ###actually starts)
  Year <- data.frame(Year=integer())
  for (y in 1:n_years) {
    # Create data frame to store results
    Year[y, "Year"] <- y-2}
  
  ## Chance and size of new area being added
  # Create data frame to store results
  NewArea <- data.frame(Year=integer(), 
                        NewAreaChance=numeric(), 
                        NewAreaSize = numeric(),
                        NewAreaCert = numeric(),
                        TotalAreaSize=numeric(), 
                        CertfiedArea = numeric())
  
  #Vector with random size of new area
  
  vv_new_area_size <- vv(new_area_size, var_CV, n=n_years)
  vv_new_area_size_df <- data.frame(Year$Year, vv_new_area_size)
  
  for (y in 1:n_years) {
    NewArea[y, "Year"] <- Year[y, "Year"]
    # Chance of new area being added
    NewArea[y,"NewAreaChance"] <- 
      chance_event(new_area_chance, value_if = 1)
    
    if(NewArea[y,"NewAreaChance"] == "0"){
      NewArea[y,"NewAreaSize"] <- 0
    }
    else{
      NewArea[y,"NewAreaSize"] <- vv_new_area_size_df[y, "vv_new_area_size"]
    }
    
    # Maximum size for Total Area
    if((sum(NewArea[1:y, "NewAreaSize"]) + initial_area_size)
       < total_area_max) {
      NewArea[y, "TotalAreaSize"] <-
        sum(NewArea[1:y,"NewAreaSize"]) + initial_area_size
    }
    else{
      NewArea[y, "TotalAreaSize"] <- total_area_max 
    }
    
    if (NewArea[y-1, "TotalAreaSize"] >= total_area_max &&
        NewArea[y, "Year"] >= 0){
      NewArea[y, "TotalAreaSize"] <- total_area_max
    }
    #NewArea becomes 0 when total area size is reached
    if(NewArea[y, "Year"] > -1 &&
       NewArea[y-1,"TotalAreaSize"] == total_area_max){
      NewArea[y, "NewAreaSize"] <- 0 
    }
    
    #Create vector for certification of new area
    NewArea[y, "NewAreaCert"] <- 0  
    
    if(NewArea[y, "TotalAreaSize"] >= 30 && 
       sum(NewArea[0:y, "NewAreaCert"]) == 0) {
      NewArea[y, "NewAreaCert"] <- 1
    }
    if(NewArea[y, "TotalAreaSize"] == 75 && 
       sum(NewArea[0:y, "NewAreaCert"]) == 1) {
      NewArea[y, "NewAreaCert"] <- 1
    }
    
    # Create vector for area that is certified
    
    # In first year of certification
    NewArea[1:2, "CertfiedArea"] <- 0
    NewArea[3, "CertfiedArea"] <- NewArea[3, "TotalAreaSize"]
    
    # Between first year and first "new area certification"
    if(NewArea[y, "Year"] > 1 && sum(NewArea[1:y, "NewAreaCert"] == 0)) {
      NewArea[y, "CertfiedArea"] <- NewArea[3, "TotalAreaSize"]
    }  
    
    # First "new area certification"
    if(NewArea[y, "NewAreaCert"] == 1) {
      NewArea[y, "CertfiedArea"] <- NewArea[y, "TotalAreaSize"]
    }
    
    # Between first and second "new area certification"
    if(sum(NewArea[1:y, "NewAreaCert"]) >= 1
       && NewArea[y, "NewAreaCert"] == 0){
      NewArea[y, "CertfiedArea"] <- NewArea[y-1, "CertfiedArea"]
    }
    
    # After second "new area certification"
    if(NewArea[y, "TotalAreaSize"] == 75){
      NewArea[y, "CertfiedArea"] <- NewArea[y, "TotalAreaSize"]
    }
  } # End of new area loop
  
  
  ##########BENEFITS#############
  
  #######Benefits from sold VER units#######
  
  ##########  PERs, VERs, Buffer etc. #############
  ##Different steps of VER (CO2 certificates) sale
  
  VER_Benefits <- data.frame(Year = integer(), 
                                    VER_Available = numeric(),
                                    VERforSale = numeric(),
                                    VER_Income = numeric(),
                                    TotalVERBenefits = numeric(),
                                    stringsAsFactors = F)
  
  vv_ver_price <- vv(ver_price, var_CV, n=n_years)
  vv_ver_prive_df <- data.frame(Year$Year, vv_ver_price)  
  
  for (y in 1:n_years) {
    VER_Benefits [y, "Year"] <- Year[y, "Year"]    
    
    #VERs that are generated
    VER_Benefits [y, "VER_Available"] <- 
      co2_sequestration*NewArea[y, "CertfiedArea"] # without vv, as CO2 sequestration doesn't differ from year to year, but only from run to run
    
    # VERs that are issued and do not go to the buffer
    VER_Benefits [y, "VERforSale"] <-  
      VER_Benefits [y, "VER_Available"] - (VER_Benefits [y, "VER_Available"]*ver_buffer)
    
    # Benefits from sales of VERs
    VER_Benefits [y, "VER_Income"] <-  
      VER_Benefits [y, "VERforSale"]*vv_ver_prive_df[y, "vv_ver_price"]
    
    # Final benefits for PV shareholders after sales commission
    VER_Benefits [y, "TotalVERBenefits"] <- 
      VER_Benefits [y,"VER_Income"]-(VER_Benefits [y,"VER_Income"]*sales_comm)
    
    # Delete VERs in years before 
    VER_Benefits [1:2, "TotalVERBenefits"] <-0
    VER_Benefits [1:2, "VER_Available"] <-0
    }
  
  
  #######Benefits from additional memberships#######
  CompanyShares <- data.frame(Year = integer(), 
                              TotalCompSharesBenefits_strat2 = numeric())
  
  {
    vv_company_shares_additional_strat2 <- vv(company_shares_additional_strat2, var_CV, n=n_years)
    vv_company_shares_additional_df <- data.frame(Year$Year,
                                                  vv_company_shares_additional_strat2)
  }
  
  for (y in 1:n_years) {
    CompanyShares[y, "Year"] <- Year[y, "Year"] 
    
    ## Benefits from additional company shares
    
    # Marketing strategy 2
    CompanyShares[y, "TotalCompSharesBenefits_strat2"] <- 
      vv_company_shares_additional_df[y, "vv_company_shares_additional_strat2"]*
      company_shares_price
    CompanyShares[1:2, "TotalCompSharesBenefits_strat2"] <- 0
  } 
  
  #######Benefits from additional product sales#######
  
  ProductSales <- data.frame(Year = integer(),
                             AddProductSalesStrat2 = numeric())
  {
    # Marketing strategy 2
    vv_productsales_additional_strat2 <- vv(productsales_additional_strat2, var_CV, n=n_years)
    vv_productsales_df <- data.frame(Year$Year, 
                                     vv_productsales_additional_strat2)
  }
  
  for (y in 1:n_years){
    ProductSales[y, "Year"] <- Year[y, "Year"]
    
     # Marketing strategy 2
    ProductSales[y, "AddProductSalesStrat2"] <-
      vv_productsales_df[y, "vv_productsales_additional_strat2"]*NewArea[y, "CertfiedArea"]
  }
  
  
  ##########COSTS#############
  
  #######Marketing Costs#######
  
  MarketingCosts <- data.frame(Year = integer(), 
                               marketing_cost_strat2 = numeric(),
                               stringsAsFactors = F)
  for (y in 1:n_years) {
    MarketingCosts[y, "Year"] <- Year[y, "Year"]
    
      # Marketing strategy 2
    if (MarketingCosts[y, "Year"]==1){
      MarketingCosts[y, "marketing_cost_strat2"] <- marketing_strat2}
    else {MarketingCosts[y, "marketing_cost_strat2"] <- 0}
    }
  
  
  #######Calculate GS fee#######
  
  ##Data frame to store total GS fee
  GsFees <- data.frame(Year = integer(), 
                       RegistryAccount = numeric(),
                       PrelReview = numeric(),
                       ProjDesign = numeric(),
                       PerfRevFee = numeric(),
                       IssuanceFee = numeric(), 
                       NewAreaCertFee = numeric(),
                       ExchangeRate = numeric(),
                       TotalGsFeesUSD = numeric(),
                       stringsAsFactors = F)
  
  
  for (y in 1:n_years) {
    GsFees[y, "Year"] <- Year[y, "Year"]
    
    # Variation of USD to EUR exchange rate
    
    ## Fees before start of project
    
    # Preliminary Review
    if (GsFees[y, "Year"] == 0)
    { 
      GsFees[y, "PrelReview"] <- gs_fee_prel_review
    } else {
      GsFees[y, "PrelReview"] <- 0
    }
    
    ## Other recurring fees
    
    # Performance Review
    # Create vector for years in which PerfRev occurs
    IfPerfRev <-  as.numeric(Year[1, "Year"]:Year[32, "Year"] %% perf_rev_recur==0)
    IfPerfRevDF <- data.frame(Year, IfPerfRev)
    IfPerfRevDF[2, "IfPerfRev"] <- 0
    IfPerfRevDF[32, "IfPerfRev"] <- 0
    
    
    # Insert actual fees for PerfRev
    if (IfPerfRevDF[y,"IfPerfRev"] == "1" && GsFees[y, "Year"] > 1){
      GsFees[y,"PerfRevFee"] <- gs_fee_perfor_rev
    } else {
      GsFees[y,"PerfRevFee"] <- 0}
    
    
    
    ## Fees per issuance #### 
    # Also have to be paid for strat. 1 and 3
    # Project Design Review 
    if (GsFees[y, "Year"] == 1){
      GsFees[y, "ProjDesign"] <- 
        (sum(VER_Benefits [3:7, "VER_Available"])*0.15)-gs_fee_prel_review
    } else {
      GsFees[y, "ProjDesign"] <- 0
    } 
    if (GsFees[y, "ProjDesign"] < 0) {GsFees[y, "ProjDesign"] <- 0}
    
    
    
    # Issuance fee
    if (IfPerfRevDF[y,"IfPerfRev"] == "1"){
      GsFees[y, "IssuanceFee"] <- ((VER_Benefits [y, "VER_Available"]*5)*0.30)-
        gs_fee_perfor_rev 
    } else {
      GsFees[y, "IssuanceFee"] <- 0
    } 
    if (GsFees[y,"IssuanceFee"] < 0){GsFees[y,"IssuanceFee"] <- 0}
    
    if(NewArea[y, "NewAreaCert"] == 1){
      GsFees[y, "NewAreaCertFee"] <- gs_fee_new_area
    }
    else {  GsFees[y, "NewAreaCertFee"] <- 0}
    
    ## Annual fees  
    # Registry Account fee
    if (GsFees[y, "Year"] > 0){ 
      GsFees[y, "RegistryAccount"] <- gs_fee_registry_account
    } else {
      GsFees[y, "RegistryAccount"] <- 0
    }
    
    
    ## Total fees per year
    GsFees[y, "TotalGsFeesUSD"] <- sum(GsFees[y, "RegistryAccount"], 
                                       GsFees[y, "PrelReview"],
                                       GsFees[y, "ProjDesign"],
                                       GsFees[y, "PerfRevFee"],
                                       GsFees[y, "IssuanceFee"],
                                       GsFees[y, "NewAreaCertFee"])
    
  } 
  
  vv_exchange_rate <- vv(exchange_rate, var_CV, n=n_years)
  
  GsFees$TotalGsFeesEUR <- GsFees$TotalGsFeesUSD*vv_exchange_rate  
  
  ## End of calculating GS fees
  
  
  
  #######Calculate salary costs#######
  
  ##Create data frame with all internal salaries
  SalaryCosts <- data.frame(Year = integer(), 
                            SalaryPVPF = numeric(),
                            SalaryQD = numeric(),
                            Consultant = numeric(),
                            TotalSalaryCosts = numeric(),
                            stringsAsFactors = F)
  
  ###Internal salary costs
  {
    vv_hours_qd_initial <- vv(hours_qd_initial, var_CV, n=n_years)
    vv_hours_qd_perfrev <- vv(hours_qd_perfrev, var_CV, n=n_years)
    vv_hours_qd_recurring <- vv(hours_qd_recurring, var_CV, n=n_years)
    vv_hours_pvpf_initial <- vv(hours_pvpf_initial, var_CV, n=n_years)
    vv_hours_pvpf_perfrev <- vv(hours_pvpf_perfrev, var_CV, n=n_years)
    vv_hours_pvpf_recurring <- vv(hours_pvpf_recurring, var_CV, n=n_years)
    vv_hours_df <- data.frame(Year, vv_hours_qd_initial, vv_hours_qd_perfrev, vv_hours_qd_recurring,
                              vv_hours_pvpf_initial, vv_hours_pvpf_perfrev, vv_hours_pvpf_recurring)
    vv_salary_pvpf <- vv(salary_pvpf, var_CV, n=n_years)
    vv_salary_qd <- vv(salary_qd, var_CV, n=n_years)
    vv_salaries_df <- data.frame(Year, vv_salary_pvpf, vv_salary_qd)
  }
  ##Initial salaries (only paid in first year)
  for (y in 1:n_years) {
    SalaryCosts[y, "Year"] <- Year[y, "Year"]
    # Costs for years before certification
    #QD
    if (SalaryCosts[y, "Year"] <= 0){
      SalaryCosts[y, "SalaryQD"] <-
        vv_hours_df[y, "vv_hours_qd_initial"]*vv_salaries_df[y, "vv_salary_qd"]
    }
    #PVPF
    if (SalaryCosts[y, "Year"] <= 0){
      SalaryCosts[y, "SalaryPVPF"] <-
        vv_hours_df[y, "vv_hours_pvpf_initial"]*vv_salaries_df[y, "vv_salary_pvpf"]
    }
    ##Normal years
    #QD
    if (SalaryCosts[y, "Year"] > 0 && IfPerfRevDF[y,"IfPerfRev"] == "0"){
      SalaryCosts[y, "SalaryQD"] <-
        vv_hours_df[y, "vv_hours_qd_recurring"]*vv_salaries_df[y, "vv_salary_qd"]
    }
    #PVPF
    if (SalaryCosts[y, "Year"] > 0 && IfPerfRevDF[y,"IfPerfRev"] == "0"){
      SalaryCosts[y, "SalaryPVPF"] <-
        vv_hours_df[y, "vv_hours_pvpf_recurring"]*vv_salaries_df[y, "vv_salary_pvpf"]
    }
    ##Performance review years
    #QD
    if (IfPerfRevDF[y,"IfPerfRev"] == "1"){
      SalaryCosts[y, "SalaryQD"] <-
        vv_hours_df[y, "vv_hours_qd_perfrev"]*vv_salaries_df[y, "vv_salary_qd"]
    }
    #PVPF
    if (IfPerfRevDF[y,"IfPerfRev"] == "1"){
      SalaryCosts[y, "SalaryPVPF"] <-
        vv_hours_df[y, "vv_hours_pvpf_perfrev"]*vv_salaries_df[y, "vv_salary_pvpf"]
    }
    ### External consultant costs
    {
      vv_consultant_cost_initial <- vv(consultant_cost_initial, var_CV, n=n_years)
      vv_consultant_cost_perfrev <- vv(consultant_cost_perfrev, var_CV, n=n_years)
      vv_consultant_df <- data.frame(Year, vv_consultant_cost_perfrev, vv_consultant_cost_initial)
    }
    if (SalaryCosts[y, "Year"] <= 0){
      SalaryCosts[y, "Consultant"] <-
        vv_consultant_df[y, "vv_consultant_cost_initial"]
    }
    else {SalaryCosts[y, "Consultant"] <-0}
    if (IfPerfRevDF[y,"IfPerfRev"] == "1"){
      SalaryCosts[y, "Consultant"] <-
        vv_consultant_df[y, "vv_consultant_cost_perfrev"]
    }
  }
  
  ##Total salary costs
  SalaryCosts["TotalSalaryCosts"] <-
    as.numeric(SalaryCosts$SalaryPVPF +
                 SalaryCosts$SalaryQD +
                 SalaryCosts$Consultant)
  
  
  ##########ALL COSTS AND BENEFITS#############
  
   # Strategy 2
  Cashflow_PV_Strat2 <-  (VER_Benefits$TotalVERBenefits+
                            CompanyShares$TotalCompSharesBenefits_strat2+
                            ProductSales$AddProductSalesStrat2)-
                            (SalaryCosts$TotalSalaryCosts +
                            GsFees$TotalGsFeesEUR + 
                            MarketingCosts$marketing_cost_strat2)
  
  NPV_PV_Strat2 <- discount(Cashflow_PV_Strat2, discount_rate = discount_rate,
                            calculate_NPV = TRUE)
  

  return(list(NPV_PV_Strat2 = NPV_PV_Strat2,
              Cashflow_NPV_PV_Strat2_ = Cashflow_PV_Strat2))
  
  
} # End of benefit.sim.PV.strat2 function


decisionSupport(inputFilePath = "data/PV_Input_variables_strat2.csv", #input file with estimates
                outputPath = file.path(getwd(),"MCResults_PV_Strat2"), #output folder
                write_table = TRUE,
                welfareFunction = benefit.sim.PV.strat2,
                numberOfModelRuns = 10000,
                functionSyntax = "plainNames")


source("empirical_EVPI.R")
source("compound_figure.R")
source("interpolate_gaps.R")

mc_PV_2<-read.csv("MCResults_PV_Strat2/mcSimulationResults.csv")
legend_table_2<-read.csv("data/PV_legend_strat2.csv")
mc_EVPI_PV_2<-mc_PV_2[,-grep("Cashflow",colnames(mc_PV_2))]
dir.create("Figures_PV_Strat2")
empirical_EVPI(mc_EVPI_PV_2,"NPV_PV_Strat2",write_table=TRUE,fileformat="png",outfolder="Figures_PV",
               p_spearman=0.05, legend_table=read.csv("data/PV_legend_strat2.csv"),#legend_table,
               output_legend_table=read.csv("data/PV_legend_strat2.csv"))#legend_table)



for (variable_name in c("NPV_PV_Strat2"))
  compound_figure(variable_name=variable_name,
                  MC_table=mc_PV_2,
                  PLS_table=read.csv(paste("MCResults_PV_Strat2/",variable_name,"_pls_results.csv",sep="")),
                  EVPI_table=read.csv(paste("Figures_PV/","EVPI_table_",variable_name,".csv",sep="")),
                  cash_flow_vars=paste("Cashflow_",variable_name,sep=""),
                  nbreaks=100,scaler="auto",percentile_remove=c(.01,.99),
                  npls=15,plsthreshold=0.8,colorscheme="quant_col",MCcolor="mango",fonttype='sans',
                  borderlines=FALSE,lwd=2,
                  fileformat="png",filename=paste("Figures_PV/","Combined_",variable_name,sep=""),
                  legend_table=read.csv("data/PV_legend_strat2.csv"))

