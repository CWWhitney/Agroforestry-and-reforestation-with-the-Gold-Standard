### Decision model for the certification of additional BaumInvest areas

# Install and load package "decisionSupport"
install.packages("decisionSupport")
library(decisionSupport)

## Prepare data

PrepareData <- {  
  format(read.csv("data/BI_Input_variables.csv")[,1:5], scientific=FALSE)
  
  make_variables<-function(est,n=1)
  { x<-random(rho=est, n=n)
  for(i in colnames(x)) assign(i,
                               as.numeric(x[1,i]),envir=.GlobalEnv)
  }
  
  make_variables(estimate_read_csv("data/BI_Input_variables.csv"))
}

sequestered_CO2 <- read.csv("data/BI_CO2_sequestration_R.csv")


benefit.sim.BI <- function(){
  
  ##########GENERAL############# 
  
  ###Create general "Year" data frame (To start counting of years before certification
  ###actually starts)
  {Year <- data.frame(Year=integer())
  for (y in 1:n_years) {
    Year[y, "Year"] <- y-2}}
  
  ##########BENEFITS#############

#######Benefits from sold PER units#######
  
##Different steps of PER (CO2 certificates) sale
    # Read file with CO2 from growth model
    
VER_Benefits <- data.frame(Year = integer(), 
                           VER_Available = numeric(),
                           VERforSale = numeric(),
                           VER_Income = numeric(),
                           TotalVERBenefits = numeric(),
                           stringsAsFactors = F)

# Variation in VER prices
vv_ver_price <- vv(ver_price, var_CV, n=n_years)
vv_ver_price_df <- data.frame(Year$Year, vv_ver_price)

# Variation in CO2 sequestration
vv_sequestered_CO2 <- vv(sequestered_CO2$CO2_sequestration, n=17, var_CV = 5)
vv_sequestered_CO2_df <- data.frame(Year$Year, vv_sequestered_CO2)

for (y in 1:n_years) {
  VER_Benefits[y, "Year"] <- Year[y, "Year"]    
  
  # VERs that are generated (from CO2 sequ. model until year 5, 
  # variation introduced afterwards for adapted model)
  if (VER_Benefits[y, "Year"] <= 5){
  VER_Benefits[y, "VER_Available"] <- 
    sequestered_CO2[y, "CO2_sequestration"]-(
      sequestered_CO2[y, "CO2_sequestration"]*growth_change_cc)
  }  
  # from year 5 on
  else { 
     VER_Benefits[y, "VER_Available"] <- 
    vv_sequestered_CO2_df[y, "vv_sequestered_CO2"]-(
    vv_sequestered_CO2_df[y, "vv_sequestered_CO2"]*growth_change_cc) 
  }
  
  
  # VERs that are issued and do not go to the buffer
  VER_Benefits[y, "VERforSale"] <-  
    VER_Benefits[y, "VER_Available"] - (VER_Benefits[y, "VER_Available"]*ver_buffer)
  
  # Benefits from sales of VERs
  VER_Benefits[y, "VER_Income"] <-  
    VER_Benefits[y, "VERforSale"]*vv_ver_price_df[y, "vv_ver_price"]
  
  # Final benefits for BI shareholders after sales commission
  VER_Benefits[y, "TotalVERBenefits"] <- 
    VER_Benefits[y,"VER_Income"]-(VER_Benefits[y,"VER_Income"]*sales_comm)
  
  # Delete VERs in years before 
  VER_Benefits[1:2, "TotalVERBenefits"] <-0
}

        
#######Benefits from additional sales of stock units#######
StockUnits <- data.frame(Year = integer(), 
                          AdditionalSU = numeric(),
                          TotalSUBenefits = numeric())
{
vv_stock_price <- vv(ver_price, var_CV, n=n_years)
vv_stock_sales_additional <-  vv(stock_sales_additional, var_CV, n=n_years)
vv_stock_sales_df <- data.frame(Year$Year, vv_stock_price, vv_stock_sales_additional)  
}

for (y in 1:n_years) {
  StockUnits[y, "Year"] <- Year[y, "Year"] 
  
 if(StockUnits[y, "Year"] <4) {
   StockUnits[y, "AdditionalSU"] <- vv_stock_sales_df[y, "vv_stock_sales_additional"]
 }
  else StockUnits[y, "AdditionalSU"] <- 0
  
  StockUnits[y, "TotalSUBenefits"] <- 
    StockUnits[y, "AdditionalSU"]*
    vv_stock_sales_df[y, "vv_stock_price"]
  }

    
##########COSTS#############

### Marketing costs

MarketingCosts <- data.frame(Year = integer(), 
                             marketing_costs_BI = numeric(),
                             stringsAsFactors = F)
for (y in 1:n_years) {
  MarketingCosts[y, "Year"] <- Year[y, "Year"]
  
  # Marketing strategy 1
  if (MarketingCosts[y, "Year"]==1){
    MarketingCosts[y, "marketing_costs_BI"] <- marketing_BI}
  else {MarketingCosts[y, "marketing_costs_BI"] <- 0}
}

### GS Fees

GsFees <- data.frame(Year = integer(), 
                     RegistryAccount = numeric(),
                     PerfRevFee = numeric(),
                     IssuanceFee = numeric(), 
                     NewAreaCertFee = numeric(),
                     TotalGsFeesUSD = numeric(),
                     stringsAsFactors = F)

for (y in 1:n_years) {
  GsFees[y, "Year"] <- Year[y, "Year"]
  
  ## Fee for certification of new area
  if(GsFees[y, "Year"] == 0){
    GsFees[y, "NewAreaCertFee"] <- gs_fee_new_area  
  }
  else {GsFees[y, "NewAreaCertFee"] <- 0}
  

  ## Recurring fees
  
  # Performance Review
  # Create vector for years in which PerfRev occurs
  IfPerfRev <-  as.numeric(Year[1, "Year"]:Year[17, "Year"] %% perf_cert_recur==0)
  IfPerfRevDF <- data.frame(Year, IfPerfRev)
  IfPerfRevDF[2, "IfPerfRev"] <- 0
  IfPerfRevDF[17, "IfPerfRev"] <- 0
  
  
  # Insert actual fees for PerfRev
  if (IfPerfRevDF[y,"IfPerfRev"] == "1" && GsFees[y, "Year"] > 1){
    GsFees[y,"PerfRevFee"] <- gs_fee_perfor_rev
  } else {
    GsFees[y,"PerfRevFee"] <- 0}
  
  # Issuance fee (0.30$ per credit, )
  if (IfPerfRevDF[y,"IfPerfRev"] == "1"){
   GsFees[y, "IssuanceFee"] <- ((VER_Benefits[y, "VER_Available"])*0.30)-
    gs_fee_perfor_rev 
  } else {
    GsFees[y, "IssuanceFee"] <- 0
  } 
  if (GsFees[y,"IssuanceFee"] < 0){GsFees[y,"IssuanceFee"] <- 0}
  
  ## Annual fees  
  # Registry Account fee
  if (GsFees[y, "Year"] > 0){ 
    GsFees[y, "RegistryAccount"] <- gs_fee_registry_account
  } else {
    GsFees[y, "RegistryAccount"] <- 0
  }

    
  ## Total fees per year
  GsFees[y, "TotalGsFeesUSD"] <- sum(GsFees[y, "RegistryAccount"], 
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
      vv_hours_df[y, "vv_hours_pvpf_perfrev"]*
      vv_salaries_df[y, "vv_salary_pvpf"]
  }
  ### External consultant costs
  {
    vv_consultant_cost_initial <- vv(consultant_cost_initial, var_CV, n=n_years)
    vv_consultant_cost_perfrev <- vv(consultant_cost_perfrev, var_CV, n=n_years)
    vv_consultant_df <- data.frame(Year, vv_consultant_cost_perfrev, 
                                   vv_consultant_cost_initial)
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

Cashflow_BI <- (VER_Benefits$TotalVERBenefits+
                 StockUnits$TotalSUBenefits)-
            (SalaryCosts$TotalSalaryCosts +
               GsFees$TotalGsFeesEUR +
               MarketingCosts$marketing_costs_BI)

NPV_BI <- discount(Cashflow_BI, discount_rate = discount_rate,
                           calculate_NPV = TRUE)

return(list(NPV_BI = NPV_BI,
            Cashflow_NPV_BI = Cashflow_BI))


}# End of benefit.sim.BI function


decisionSupport(inputFilePath = "data/BI_Input_variables.csv", #input file with estimates
                outputPath = file.path(getwd(),"MCResults_BI"), #output folder
                write_table = TRUE,
                welfareFunction = benefit.sim.BI,
                numberOfModelRuns = 10000,
                functionSyntax = "plainNames")

##### Analysis
source("empirical_EVPI.R")
source("compound_figure.R")
source("interpolate_gaps.R")

mc<-read.csv("MCResults_BI/mcSimulationResults.csv")
legend_table<-read.csv("data/BI_legend.csv")
mc_EVPI<-mc[,-grep("Cashflow",colnames(mc))]
dir.create("Figures_BI")
empirical_EVPI(mc_EVPI,"NPV_BI",write_table=TRUE,fileformat="png",outfolder="Figures_BI",
               p_spearman=0.05, legend_table=read.csv("data/BI_legend.csv"),#legend_table,
               output_legend_table=read.csv("data/BI_legend.csv"))#legend_table)

for (variable_name in c("NPV_BI"))
  compound_figure(variable_name=variable_name,
                  MC_table=mc,
                  PLS_table=read.csv(paste("MCResults_BI/",variable_name,"_pls_results.csv",sep="")),
                  EVPI_table=read.csv(paste("Figures_BI/","EVPI_table_",variable_name,".csv",sep="")),
                  cash_flow_vars=paste("Cashflow_",variable_name,sep=""),
                  nbreaks=100,scaler="auto",percentile_remove=c(.01,.99),
                  npls=15,plsthreshold=0.8,colorscheme="quant_col",MCcolor="mango",fonttype='sans',
                  borderlines=FALSE,lwd=2,
                  fileformat="png",filename=paste("Figures_BI/","Combined_",variable_name,sep=""),
                  legend_table=read.csv("data/BI_legend.csv"))


