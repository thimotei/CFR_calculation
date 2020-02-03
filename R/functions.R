# Format CI text ------------------------------------------------

c.text<-function(x,sigF=3){
  bp1=signif(c(median(x),quantile(x,0.025),quantile(x,0.975)),sigF)
  paste(bp1[1]," (",bp1[2],"-",bp1[3],")",sep="")
}

c.input.text<-function(bp1,sigF=3){
  bp1 <- signif(100*bp1,sigF)
  paste(bp1[1]," (",bp1[2],"-",bp1[3],")",sep="")
}

# Calculation binomial CIs ------------------------------------------------

bin_conf <- function(x,n){
  htest <- binom.test(x,n)
  h_out <- c(x/n, htest$conf.int[1], htest$conf.int[2])
  h_out
}


# Plot native CFR ---------------------------------------------------------

plot_cfr_basic <- function(main_data_in){
  
  # Load data and omit initial missing entries
  data_1 <- main_data_in[!is.na(main_data_in$new_cases_china),] 
  data_1 <- data_1 %>% mutate(cases = scaled_reporting*new_cases_china,
                              deaths = new_deaths_china) # scale up based on exported case estimates

  # - - 
  # Estimate fatality risk
  
  # Check have enough of distribution
  distn_needed <- sum(cumsum(onset_to_death(0:30))<=0.95) # make sure have at 95% of the cumulative distn
  
  # Calculate adjusted CFR
  mov_window <- distn_needed
  store_cfr <- NULL
  for(tt in 1:nrow(data_1)){
    store_cfr <- rbind(store_cfr,scale_cfr(data_1[max(0,tt-mov_window):tt,],delay_fun = onset_to_death))
  }
  store_cfr <- as_tibble(store_cfr); names(store_cfr) <- c("naive","cCFR","deaths","known_outcomes","cases")

  # - - 
  # Estimate severity risk
  
  # Check have enough of distribution
  distn_needed <- sum(cumsum(onset_to_hosp(0:30))<=0.95) # make sure have at 95% of the cumulative distn
  
  # Calculate adjusted severity risk
  mov_window <- distn_needed
  data_2 <- data_1; 
  data_2 <- data_2 %>% mutate(cases = new_cases_china,
                              deaths = new_sev_cases_china) # scale up based on exported case estimates
  data_2 <- data_2[!is.na(data_2$new_sev_cases_china),]# omit NAs
  
  store_sev <- NULL
  for(tt in 1:nrow(data_2)){
    store_sev <- rbind(store_sev,scale_cfr(data_2[max(0,tt-mov_window):tt,],delay_fun = onset_to_hosp))
  }
  store_sev <- as_tibble(store_sev); names(store_sev) <- c("naive","cCFR","severe","known_outcomes","cases")
  
  
  # - - - 
  # Calculate final CI and output
  tt_max <- nrow(data_1)
  
  CI_nCFR_raw <- c.input.text(bin_conf(data_1[tt_max,]$cumulative_deaths_china,data_1[tt_max,]$cumulative_cases_china))
  CI_nCFR <- c.input.text(bin_conf(data_1[tt_max,]$cumulative_deaths_china,scaled_reporting*data_1[tt_max,]$cumulative_cases_china))
  CI_cCFR <- c.input.text(bin_conf(store_cfr[tt_max,]$deaths,store_cfr[tt_max,]$known_outcomes))
  
  output_estimates <- rbind(c("Naive CFR (raw data)",CI_nCFR_raw),
                      c("Naive CFR (scaled data)",CI_nCFR),
                      c("Adjusted CFR (scaled data)",CI_cCFR))
  output_estimates <- as_tibble(output_estimates)
  names(output_estimates) <- c("Data/method","Estimate")

  write_csv(output_estimates,paste0("plots/cfr_table_scale_",scaled_reporting,"_mean_",meanG,"_",tail(data_1,1)$date,".csv"))
  
  # calculate moving CFR onincidence -- deprecated
  # store_cfr_incidence <- NULL
  # for(tt in 1:nrow(data_1)){
  #   store_cfr_incidence <- rbind(store_cfr_incidence,scale_cfr_incidence(data_1,tt))
  # }
  # store_cfr_incidence <- as_tibble(store_cfr_incidence); names(store_cfr_incidence) <- c("naive","cCFR")
  
  # Plot calculations
  par(mfrow=c(3,1),mar=c(3,3,1,1),mgp=c(2,0.7,0))
  
  # Plot delay
  seq_xx <- seq(0,20,1)
  ymax <- 1.1*max(onset_to_death(seq_xx))
  let_t <- 1
  
  plot(seq_xx,onset_to_death(seq_xx),pch=19,ylab="P(death on given day | death)",xlab="days after onset",ylim=c(0,ymax))
  lines(seq_xx,onset_to_death(seq_xx),col="black",lwd=0.5)
  title(LETTERS[let_t],adj=0);let_t <- let_t+1

  # Plot data
  par(mar=c(2,3,1,1))
  xrange <- c(min(data_1$date),max(data_1$date))
  deaths_plot <- data_1$deaths; deaths_plot[deaths_plot==0] <- 0.1 # Show 0 on log plot
  
  ymax <- 1e6
  plot(data_1$date,data_1$cases/scaled_reporting,ylab="incidence",type="l",log="y",yaxt="n",xlab="",ylim=c(0.1,ymax),xlim=xrange,lwd=1,col="white")
  polygon(c(cfr_current_date,cfr_max_date,cfr_max_date,cfr_current_date),c(0.1,0.1,ymax,ymax),col="light grey",border=0)
  lines(data_1$date,data_1$cases,col="blue",lwd=1)
  lines(data_1$date,deaths_plot,col="red",lwd=1)
  lines(data_1$date,data_1$cases/scaled_reporting,col="black",lwd=1)
  text(labels="new cases (scaled from traveller data)",x=min(data_1$date),y=0.5*ymax,adj=0,col="blue")
  text(labels="new cases (raw)",x=min(data_1$date),y=0.09*ymax,adj=0)
  text(labels="new deaths (raw)",x=min(data_1$date),y=0.015*ymax,adj=0,col="red")
  text(labels="extrapolation",x=max(data_1$date)-1,y=1,adj=1,col=rgb(0.3,0.3,0.3))
  axis(side = 2, at = 10^c(-1:5),labels=c(0,10^c(0:5)))
  title(LETTERS[let_t],adj=0);let_t <- let_t+1
  
  # Plot naive and corrected CFR
  ymax <- 5
  plot(data_1$date,store_cfr$naive,col="white",ylab=paste0("CFR (%)"),xlab="",xlim=xrange,ylim=c(0,ymax))
  data_c2 <- data_1[data_1$date>=cfr_from_date,]  # Only display recent more robust points 
  store_cfr_2 <- store_cfr[data_1$date>=cfr_from_date,]
  polygon(c(cfr_current_date,cfr_max_date,cfr_max_date,cfr_current_date),c(0,0,ymax,ymax),col="light grey",border=0)
  
  grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
  
  lines(data_c2$date,100*store_cfr_2$naive,col="blue",lty=2,lwd=1)
  lines(data_c2$date,100*data_c2$cumulative_deaths_china/data_c2$cumulative_cases_china,col="black",lty=2,lwd=1) # Use raw data too
  lines(data_c2$date,100*store_cfr_2$cCFR,col="blue",lwd=1)
  text(labels="corrected CFR estimate from traveller data (solid)",x=min(data_1$date),y=0.9*ymax,adj=0,col="blue")
  text(labels="naive CFR from traveller data (dashed)",x=min(data_1$date),y=0.8*ymax,adj=0,col="blue")
  text(labels="naive CFR from raw data",x=min(data_1$date),y=0.7*ymax,adj=0,col="black")
  

  title(LETTERS[let_t],adj=0);let_t <- let_t+1
  
  # Plot naive and corrected severe illness
  # ymax <- 25
  # plot(data_2$date,store_sev$naive,col="white",ylab=paste0("Severe (%)"),xlab="",xlim=xrange,ylim=c(0,ymax))
  # data_s2 <- data_2[data_2$date>=cfr_from_date,]  # Only display recent more robust points 
  # store_sev_2 <- store_sev[data_2$date>=cfr_from_date,]
  # polygon(c(cfr_current_date,cfr_max_date,cfr_max_date,cfr_current_date),c(0,0,ymax,ymax),col="light grey",border=0)
  # grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
  # 
  # lines(data_s2$date,100*store_sev_2$naive,col="blue",lty=2,lwd=1)
  # lines(data_s2$date,100*data_s2$severe_total/data_s2$cumulative_cases,col="black",lty=2,lwd=1) # Use raw data too
  # lines(data_s2$date,100*store_sev_2$cCFR,col="blue",lwd=1)
  # text(labels="corrected severity estimate from traveller data (solid)",x=min(data_1$date),y=0.9*ymax,adj=0,col="blue")
  # text(labels="naive severity from traveller data (dashed)",x=min(data_1$date),y=0.8*ymax,adj=0,col="blue")
  # text(labels="naive severity from raw data",x=min(data_1$date),y=0.7*ymax,adj=0,col="black")
  # 
  # title(LETTERS[let_t],adj=0);let_t <- let_t+1
  
  # Plot naive and corrected CFR - by incidence --  DEPRECATED
  # ymax <- 1
  # plot(data_1$date,store_cfr_incidence$cCFR,col="white",ylab=paste0(mov_window," day moving CFR"),xlab="",ylim=c(0,ymax))
  # lines(data_1$date,store_cfr_incidence$cCFR,col="blue")
  # text(labels="corrected CFR estimate (solid)",x=min(data_1$date),y=0.8*ymax,adj=0,col="blue")
  # 
  dev.copy(png,paste0("plots/cfr_estimate_scale_",scaled_reporting,"_mean_",meanG,"_",tail(data_1,1)$date,".png"),
           units="cm",width=10,height=15,res=150)
  dev.off()
  

  

}


# Calculate scaling factor for CFR ----------------------------------------


# Calculate CFR scaling

scale_cfr <- function(data_1_in,delay_fun = onset_to_death){
  
  # DEBUG   data_1_in <- data_1[1:tt,]
  
  cumulative_known_t <- 0 # cumulative cases with known outcome at time tt
  
  # Sum over cases up to time tt
  for(ii in 1:nrow(data_1_in)){
    
    known_i <- 0 # number of cases with known outcome at time ii
    
    for(jj in 0:(ii-1)){
      known_jj <- (data_1_in[ii-jj,]$cases * delay_fun(jj) )
      known_i <- known_i + known_jj
    }
    known_i
    cumulative_known_t <- cumulative_known_t + known_i # Tally cumulative known
  }
  
  # naive CFR value
  b_tt <- sum(data_1_in$deaths)/sum(data_1_in$cases) 
  
  # nCFR estimator
  p_tt <- sum(data_1_in$deaths)/cumulative_known_t
  
  c(b_tt,p_tt,sum(data_1_in$deaths),round(cumulative_known_t),sum(data_1_in$cases) )
  
}

# Calculate CFR using incidence data  - DEPRECATED

# scale_cfr_incidence <- function(data_1_in,tt){
#   
#   # DEBUG   data_1_in <- data_1[1:tt,]
#   
#   cumulative_known_t <- 0 # cumulative cases with known outcome at time tt
#   
#   # Focus on data point tt
#   ii <- tt
#     
#   known_i <- 0 # number of cases with known outcome at time ii
#   
#   for(jj in 0:(ii-1)){
#     known_jj <- (data_1_in[ii-jj,]$cases * onset_to_death(jj) )
#     known_i <- known_i + known_jj
#   }
#   known_i
#   
#   # naive CFR value
#   b_tt <- sum(data_1[tt,]$deaths)/sum(data_1[tt,]$cases) 
#   
#   # nCFR estimator
#   p_tt <- data_1[tt,]$deaths/known_i 
#   
#   c(b_tt,p_tt)
#   
# }

