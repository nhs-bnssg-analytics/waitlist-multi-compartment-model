rm(list=ls())
require(tidyverse)
require(lubridate)
require(forecast)
require(timeDate)
setwd("S:/Finance/Shared Area/BNSSG - BI/8 Modelling and Analytics/projects/corona/rtt/ode model (2023) - with time stocks/")

##################################################################
# USER INPUTS

calibrate_period_months<-12                            
proj_last_val_or_calibrate_period<-"period"
top_up_initial_stock<-TRUE 
backtest_duration<-6
proj_ref_cap<-"auto"
proj_ref_cap_auto_past_window<-24
proj_ref_cap_auto_future_window<-24
proj_ref_cap_manual<-read.csv("proj_ref_cap_manual.csv")

# calibrate_period_months: this is the number of most recent months for which the reneging and capacity parameters are calibrated
#                          over, in order to avoid over-reliance on just the last month's data. covered in section 2.4 of papaer.
# proj_last_val_or_calibrate_period: 'baseline' referrals and capacity are calculated either on the values of the last month or the
#                                    average values over the last 'calibrate_period_months' months.
# top_up_initial_stock: TRUE if any negative reneges from first stock are to be added to the arrival rate.
# proj_ref_cap: auto (derive from tbats projections) or manual (provide table).
# proj_ref_cap_manual: this is the table containing ratios of the 'baseline' referrals and capacity, per the scenarios defined by the user.


# USER INPUTS
##################################################################

##################################################################
# load processed raw data (monthly nhs-e data, processed at trust/spec level to referrals/incomplete/complete - see nick howlett about this part)
full_dat_trusts<-read.csv("calc-dat-monthly.csv") %>%
  filter(period>as.Date(max(period)) %m-% months(1) -years(2))
trusts<-unique(full_dat_trusts$trust)

# calculate england total (note: can do a similar thing for the regions, e.g. midlands, south-west, london, etc)
full_dat_trusts_eng<-full_dat_trusts %>%
  group_by(specialty,period,type,months_waited) %>%
  summarise(value=sum(value)) %>%
  mutate(trust="ENGLAND")
full_dat_trusts<-bind_rows(full_dat_trusts,full_dat_trusts_eng)

##################################################################
# specify workdir for all outputs (puts outputs in a dedicated folder, so not to overwrite existing ones)
outdir<-paste0(getwd(),"/outputs_latest_data=",max(full_dat_trusts$period),
"_calib_period=",calibrate_period_months,"_(created_",gsub(" ","_",gsub(":","-",Sys.time())),")/")
dir.create(outdir)


##################################################################
# set up trust/spec modelling granularity list for analysis 

#analysis_list<-data.frame(trust=trusts,specialty="Total")
analysis_list<-data.frame(trust=character(0),specialty=character(0)) %>%
  add_row(trust="ENGLAND",specialty="Total") #%>%
  #add_row(trust="NORTH BRISTOL NHS TRUST",specialty="Total")

##################################################################
# for each modelling granularity...
for (analysis_unit in c(1:nrow(analysis_list))) {
  
  analysis_trust<-analysis_list$trust[analysis_unit]
  analysis_spec<-analysis_list$specialty[analysis_unit]
  
  avgworkdaysinmonth<-(365-52*2-8)/12
  workdays<-data.frame(dates=seq(floor_date(as.Date(min(full_dat_trusts$period)),"month"),
                       as.Date(max(full_dat_trusts$period)),
                       by=1)) %>%
    mutate(wkday=weekdays(dates)) %>%
    filter(!wkday %in% c("Saturday","Sunday")) %>%
    filter(!dates %in% as.Date(holidayLONDON(2010:2030))) %>%
    mutate(period=ceiling_date(dates,"month")-1) %>%
    mutate(period=as.character(period)) %>%
    group_by(period) %>%
    summarise(nworkdays=n())
  
  
  # process data
  x1<-full_dat_trusts %>%
    filter(trust==analysis_trust,specialty==analysis_spec) %>%
    select(-trust,-specialty) %>%
    #left_join(workdays,by="period") %>%
    #mutate(days_in_month=days_in_month(period)) %>%
    #mutate(value=ifelse(type %in% c("Referrals","Complete"),value/nworkdays*avgworkdaysinmonth,value)) %>%
    #select(-nworkdays) %>%
    mutate(months_waited=ifelse(type=="Referrals","<1",months_waited)) %>%
    mutate(months_waited_id=recode(months_waited,"<1"="0","1-2"="1","2-3"="2","3-4"="3","4-5"="4","5-6"="5",
                                   "6-7"="6","7-8"="7","8-9"="8","9-10"="9","10-11"="10","11-12"="11","12-13"="12","13-14"="12",
                                   "14-15"="12","15-16"="12","16-17"="12","17-18"="12","18-19"="12","19-20"="12","20-21"="12",
                                   "21-22"="12","22-23"="12","23-24"="12","24+"="12")) %>%
    mutate(months_waited=recode(months_waited,"12-13"=">12","13-14"=">12",
                                "14-15"=">12","15-16"=">12","16-17"=">12","17-18"=">12","18-19"=">12","19-20"=">12","20-21"=">12",
                                "21-22"=">12","22-23"=">12","23-24"=">12","24+"=">12")) %>%
    group_by(period,type,months_waited,months_waited_id) %>%
    summarise(value=sum(value)) %>%
    ungroup() %>%
    mutate(months_waited_id=as.numeric(months_waited_id)) %>%
    mutate(period_id=interval(min(period),period) %/% months(1))
  
  
  # only try do modelling for those with complete/decent processed data
  test_consec_months_in_data<-max(x1$period_id)-min(x1$period_id)==length(unique(x1$period_id))-1
  if (test_consec_months_in_data==FALSE) {
    print(paste0("PROBLEM FOR ",analysis_trust,", ",analysis_spec,": DOES NOT CONTAIN CONSECUTIVE MONTHS DATA"),quote=FALSE) 
  }
  
  
  months_waited_look_up<-x1 %>%
    select(months_waited,months_waited_id) %>%
    distinct() %>%
    arrange(months_waited_id) %>%
    mutate(months_waited=paste0("Waiting ",months_waited," month"))
  
  period_look_up<-x1 %>%
    select(period,period_id) %>%
    distinct() %>%
    mutate(period=as.Date(period)) %>%
    bind_rows(
      data.frame(
        period=ceiling_date(max(.$period) %m+% months(1:100),unit="month")- days(1),
        period_id=max(.$period_id)+1:100)
    )
  
  ##################################
  # reneging and capacity params
  
  reneg_cap<-expand_grid(period_id=(min(x1$period_id)+1):max(x1$period_id),
                         months_waited_id=min(x1$months_waited_id,na.rm=TRUE):max(x1$months_waited_id,na.rm=TRUE))
  reneg_cap$node_inflow<-unlist(sapply(1:nrow(reneg_cap),function(y) {
    if(reneg_cap$months_waited_id[y]==0) {
      x1 %>% filter(type=="Referrals",period_id==reneg_cap$period_id[y]) %>% .$value
    } else if (reneg_cap$months_waited_id[y]==max(reneg_cap$months_waited_id)) {
      x1 %>% filter(type=="Incomplete",months_waited_id==reneg_cap$months_waited_id[y]-1,period_id==reneg_cap$period_id[y]-1) %>% .$value +
        x1 %>% filter(type=="Incomplete",months_waited_id==reneg_cap$months_waited_id[y],period_id==reneg_cap$period_id[y]-1) %>% .$value
    } else {
      x1 %>% filter(type=="Incomplete",months_waited_id==reneg_cap$months_waited_id[y]-1,period_id==reneg_cap$period_id[y]-1) %>% .$value
    }
  }))
  reneg_cap$treatments<-sapply(1:nrow(reneg_cap),function(y) {
    x1 %>% filter(type=="Complete",months_waited_id==reneg_cap$months_waited_id[y],period_id==reneg_cap$period_id[y]) %>% .$value})
  reneg_cap$waiting_same_node<-sapply(1:nrow(reneg_cap),function(y) {
    x1 %>% filter(type=="Incomplete",months_waited_id==reneg_cap$months_waited_id[y],period_id==reneg_cap$period_id[y]) %>% .$value})
  reneg_cap$Reneges<-reneg_cap$node_inflow-reneg_cap$treatments-reneg_cap$waiting_same_node
  reneg_cap$renege_param<-reneg_cap$Reneges/reneg_cap$node_inflow
  reneg_cap$cap_param<-reneg_cap$treatments/reneg_cap$node_inflow
  reneg_cap2<-reneg_cap %>%
    select(period_id,months_waited_id,Reneges,renege_param,cap_param) %>%
    pivot_longer(cols=c(Reneges,renege_param,cap_param),names_to="type",values_to="value")
  
  ##################################
  # bring together
  
  x2<-x1 %>%
    select(-months_waited,-period) %>%
    bind_rows(reneg_cap2) %>%
    {if (top_up_initial_stock==TRUE) {
      pivot_wider(.,names_from=type,values_from=value) %>%
        mutate(Referrals=ifelse(months_waited_id==0 & Reneges<0,Referrals+abs(Reneges),Referrals)) %>%   #changed from case_when to ifelse on 29 nov 2024
        mutate(Reneges=ifelse(months_waited_id==0 & Reneges<0,0,Reneges)) %>%
        mutate(renege_param=ifelse(months_waited_id==0 & renege_param<0,0,renege_param)) %>%
        pivot_longer(cols=-c("months_waited_id","period_id"),names_to="type",values_to="value")
    } else {.}}
  
  renege_cap_params<-reneg_cap2 %>%
    filter(period_id>=(max(period_id-calibrate_period_months+1))) %>%
    filter(type %in% c("renege_param","cap_param")) %>%
    drop_na() %>%
    group_by(months_waited_id,type) %>%
    mutate(meanval=mean(value)) %>%
    mutate(value=ifelse(value<0,0,value)) %>%
    summarise(value=ifelse(unique(meanval)<0,0,mean(value)))
  
  
  ##################################
  # plot renege and cap parameters
  
  date_calibrate_from<-as.Date(max(x1$period)) %m-% months(calibrate_period_months)
  
  fig3<-x2 %>%
    filter(type %in% c("renege_param","cap_param")) %>%
    mutate(type=recode(type,"renege_param"="Renege parameter","cap_param"="Capacity parameter")) %>%
    left_join(period_look_up,by="period_id") %>%
    left_join(months_waited_look_up,by="months_waited_id") %>%
    mutate(months_waited=factor(months_waited,levels=as.character(months_waited_look_up$months_waited))) %>%
    left_join(renege_cap_params %>%
                mutate(type=recode(type,"renege_param"="Renege parameter","cap_param"="Capacity parameter")),
              by=c("type","months_waited_id")) %>%
    ggplot(aes(x=period)) +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=date_calibrate_from,colour="darkgrey")+
    geom_step(aes(y=value.x),direction="vh",alpha=0.8) +
    geom_line(aes(y=value.y),colour="red",alpha=0.8) +
    facet_grid(type~months_waited,scales="free_y",labeller=labeller(months_waited=label_wrap_gen(12))) +
    scale_x_date(breaks=seq(as.Date("2023-01-01"),as.Date("2100-01-01"),by="1 year"),date_labels="%b\n%Y")+
    labs(title=paste0(analysis_trust,", ",analysis_spec),
         subtitle="Compartment-level capacity and reneging rates over time") +
    theme_bw() +
    theme(plot.subtitle=element_text(face="italic"),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_text(size=7),
          strip.text.x=element_text(size=7),
          panel.spacing=unit(0.2,"lines"))
  
  png(paste0(outdir,"fig3_",analysis_trust,"_",analysis_spec,".png"),height=3.5,width=9,units="in",res=400)
  print(fig3)
  dev.off()
  
  ##################################
  # backtest
  
  btest<-function(backtest_duration) {
  
    bt_reneg_cap<-expand_grid(period_id=(max(x1$period_id)-backtest_duration-calibrate_period_months+1):(max(x1$period_id)-backtest_duration),
                              months_waited_id=min(x1$months_waited_id,na.rm=TRUE):max(x1$months_waited_id,na.rm=TRUE))
    bt_reneg_cap$node_inflow<-sapply(1:nrow(bt_reneg_cap),function(y) {
      if(bt_reneg_cap$months_waited_id[y]==0) {
        x1 %>% filter(type=="Referrals",period_id==bt_reneg_cap$period_id[y]) %>% .$value
      } else if (bt_reneg_cap$months_waited_id[y]==max(bt_reneg_cap$months_waited_id)) {
        x1 %>% filter(type=="Incomplete",months_waited_id==bt_reneg_cap$months_waited_id[y]-1,period_id==bt_reneg_cap$period_id[y]-1) %>% .$value +
          x1 %>% filter(type=="Incomplete",months_waited_id==bt_reneg_cap$months_waited_id[y],period_id==bt_reneg_cap$period_id[y]-1) %>% .$value
      } else {
        x1 %>% filter(type=="Incomplete",months_waited_id==bt_reneg_cap$months_waited_id[y]-1,period_id==bt_reneg_cap$period_id[y]-1) %>% .$value
      }
    })
    bt_reneg_cap$treatments<-sapply(1:nrow(bt_reneg_cap),function(y) {
      x1 %>% filter(type=="Complete",months_waited_id==bt_reneg_cap$months_waited_id[y],period_id==bt_reneg_cap$period_id[y]) %>% .$value})
    bt_reneg_cap$waiting_same_node<-sapply(1:nrow(bt_reneg_cap),function(y) {
      x1 %>% filter(type=="Incomplete",months_waited_id==bt_reneg_cap$months_waited_id[y],period_id==bt_reneg_cap$period_id[y]) %>% .$value})
    bt_reneg_cap$Reneges<-bt_reneg_cap$node_inflow-bt_reneg_cap$treatments-bt_reneg_cap$waiting_same_node
    bt_reneg_cap$renege_param<-bt_reneg_cap$Reneges/bt_reneg_cap$node_inflow
    bt_reneg_cap$cap_param<-bt_reneg_cap$treatments/bt_reneg_cap$node_inflow
    bt_reneg_cap2<-bt_reneg_cap %>%
      select(period_id,months_waited_id,Reneges,renege_param,cap_param) %>%
      pivot_longer(cols=c(Reneges,renege_param,cap_param),names_to="type",values_to="value")
    
    bt_renege_cap_params<-bt_reneg_cap2 %>%
      filter(period_id>=(max(period_id-calibrate_period_months+1))) %>%
      filter(type %in% c("renege_param","cap_param")) %>%
      drop_na() %>%
      group_by(months_waited_id,type) %>%
      mutate(meanval=mean(value)) %>%
      mutate(value=ifelse(value<0,0,value)) %>%
      summarise(value=ifelse(unique(meanval)<0,0,mean(value)))
    
    
    bt_incompletes_init<-x2 %>%
      filter(type=="Incomplete",period_id==(max(period_id)-backtest_duration)) %>%
      select(months_waited_id,value) %>%
      rename('incompletes'='value')
    
    bt_proj_ref_cap2<-x2 %>%
      filter(period_id %in% (max(period_id)-backtest_duration+1):max(period_id)) %>%
      filter(type %in% c("Referrals","Complete")) %>%
      pivot_wider(names_from=type,values_from=value) %>%
      group_by(period_id) %>%
      summarise(referrals=sum(Referrals,na.rm=TRUE),capacity=sum(Complete,na.rm=TRUE)) %>%
      select(period_id,referrals,capacity)
    
    bt_res<-data.frame(period_id=numeric(0),months_waited_id=numeric(0),
                       incompletes_bt=numeric(0),reneges_bt=numeric(0))
    
    # backtest projections
    bt_scen_periods<-bt_proj_ref_cap2 %>% arrange(period_id) %>% .$period_id
    bt_current_incompletes<-bt_incompletes_init
    for (p in bt_scen_periods) {
      if (sum(bt_current_incompletes$incompletes)>0) {
        res_p<-bt_renege_cap_params %>% pivot_wider(names_from=type,values_from=value)
        res_p$node_inflow<-sapply(res_p$months_waited_id,function(m) {
          if(m==0) {
            bt_proj_ref_cap2 %>% filter(period_id==p) %>% .$referrals
          } else if (m==max(res_p$months_waited_id)) {
            bt_current_incompletes %>% filter(months_waited_id==m-1) %>% .$incompletes +
              bt_current_incompletes %>% filter(months_waited_id==m) %>% .$incompletes
          } else {
            bt_current_incompletes %>% filter(months_waited_id==m-1) %>% .$incompletes
          }
        })
        res_p$reneges<-res_p$renege_param*res_p$node_inflow
        res_p$treatments<-bt_proj_ref_cap2 %>% filter(period_id==p) %>% .$capacity
        res_p$cap_num<-res_p$cap_param*res_p$node_inflow
        res_p$cap_denom<-sum(res_p$cap_param*res_p$node_inflow)
        res_p$tment<-res_p$treatments*res_p$cap_num/res_p$cap_denom
        # note: the next few lines are to prevent negative 'incompletes' - any overshoot gets distributed as evenly as possible
        tmp_incompletes<-res_p$node_inflow-res_p$tment-res_p$reneges    
        while (any(tmp_incompletes<0)) {
          tmp2a<-sum(tmp_incompletes[tmp_incompletes<0])
          tmp_incompletes[which(tmp_incompletes<0)]<-0
          tmp_incompletes[which(tmp_incompletes>0)]<-tmp_incompletes[which(tmp_incompletes>0)]+tmp2a/length(tmp_incompletes[which(tmp_incompletes>0)])
        }
        res_p$incompletes<-tmp_incompletes
        bt_current_incompletes<-res_p %>%
          select(months_waited_id,incompletes)
        bt_res<-bind_rows(bt_res,res_p %>% 
                            select(months_waited_id,incompletes,reneges) %>% 
                            mutate(period_id=p)%>%
                            rename("incompletes_bt"=incompletes,
                                   "reneges_bt"=reneges)) 
      } else {
        bt_res<-bind_rows(bt_res,data.frame(period_id=p,
                                                  months_waited_id=min(x1$months_waited_id,na.rm=TRUE):max(x1$months_waited_id,na.rm=TRUE),
                                                  incompletes_bt=0,
                                                  reneges_bt=0))
      }
    }
    
    x2_bt<-bt_res %>%
      pivot_longer(cols=c(incompletes_bt,reneges_bt),names_to="type",values_to="value") %>%
      bind_rows(x2) %>%
      left_join(period_look_up,by="period_id") %>%
      filter(type %in% c("Incomplete","incompletes_bt","Reneges","reneges_bt")) %>%
      mutate(type=recode(type,Incomplete="incomplete_Data",
                         incompletes_bt="incomplete_Predicted",
                         Reneges="reneges_Data",
                         reneges_bt="reneges_Predicted")) %>%
      separate(type,into=c("type","metric"),sep="_") %>%
      mutate(type=factor(type,levels=c("incomplete","reneges"))) %>%
      mutate(type=recode(type,incomplete="Number of patients waiting",
                         reneges="Number of patients reneging"))
    
    date_backtest_from<-as.Date(max(x1$period)) %m-% months(backtest_duration)
    date_backtest_calibrate_from<-as.Date(max(x1$period)) %m-% months(backtest_duration+calibrate_period_months)
    
    x2_bt2<-x2_bt %>%
      filter(type=="Number of patients waiting") %>%
      select(-type) %>%
      filter(period_id>=max(x2_bt$period_id)-backtest_duration+1) %>%
      left_join(months_waited_look_up,by="months_waited_id") %>%
      select(-months_waited_id)
    
    x2_bt3<-x2_bt2 %>%
      bind_rows(x2_bt2 %>%
                  group_by(period_id,period,metric) %>%
                  summarise(value=sum(value)) %>%
                  mutate(months_waited="Total"))
    
    bt2_mape_monthswaited<-x2_bt3 %>%
      pivot_wider(names_from=metric,values_from=value) %>%
      mutate(ape=abs(100*(Predicted-Data)/Data)) %>%
      group_by(months_waited) %>%
      summarise(mape=mean(ape)) %>%
      mutate(date=max(x2_bt$period)) %>%
      mutate(months_waited=factor(months_waited,levels=c(as.character(months_waited_look_up$months_waited),"Total")))
    
    return(list(x2_bt3,bt2_mape_monthswaited))
  
  }
  
  bt6<-btest(6)
  bt12<-btest(12)
  
  bt2_mape_monthswaited<-bind_rows(
    bt6[[2]] %>% mutate(btdur="6 month backtest"),
    bt12[[2]] %>% mutate(btdur="12 month backtest")
  ) %>% mutate(btdur=factor(btdur,levels=c("6 month backtest","12 month backtest")))
  
  x2_bt3<-bind_rows(
    bt6[[1]] %>% mutate(btdur="6 month backtest"),
    bt12[[1]] %>% mutate(btdur="12 month backtest")
  ) %>% mutate(btdur=factor(btdur,levels=c("6 month backtest","12 month backtest")))
  
  fig4<-x2_bt3 %>%
    bind_rows(x2_bt3 %>%
                group_by(btdur) %>%
                filter(period==min(period)) %>%
                mutate(period=floor_date(period,"month"))) %>%
    mutate(months_waited=factor(months_waited,levels=c("Total",as.character(months_waited_look_up$months_waited)))) %>%
    pivot_wider(names_from=metric,values_from=value) %>%
    mutate(percent_error=(Predicted-Data)/Data) %>%
    ggplot(aes(x=period,y=percent_error)) +
    geom_hline(yintercept=0,alpha=0.8,colour="black") +
    geom_step(direction="vh",alpha=0.8,colour="red") +
    geom_text(data=bt2_mape_monthswaited,aes(label=glue::glue("{round(mape,1)}%"),x=date,y=-Inf),vjust=-1,hjust=1,inherit.aes=FALSE,size=2.5) +
    facet_grid(btdur~months_waited,labeller=labeller(months_waited=label_wrap_gen(10))) +
    scale_x_date(breaks=seq(as.Date("2000-04-01"),as.Date("2100-01-01"),by="6 month"),date_labels="%b\n%Y") +
    scale_y_continuous(expand=c(0.2,0),labels=scales::percent) +
    #coord_trans(y='log') +
    ylab("Percent error in number of patients waiting") +
    labs(title=paste0(analysis_trust,", ",analysis_spec),
         subtitle="Backtest percent error in projected waitlist size, over most recent 6 and 12 months") +
    theme_bw() +
    theme(plot.subtitle=element_text(face="italic"),
          legend.title=element_blank(),
          legend.position="bottom",
          strip.text.x=element_text(size=7),
          axis.text.x=element_text(size=7),
          axis.title.x=element_blank(),
          panel.grid.minor = element_blank())
  
  png(paste0(outdir,"fig4_",analysis_trust,"_",analysis_spec,".png"),height=4,width=9,units="in",res=400)
  print(fig4)
  dev.off()
  
  
  ##################################
  # scenarios: set up projected referrals and capacity
  
  scen_x2<-x2 %>%
    filter(type %in% c("Complete","Referrals")) %>%
    group_by(period_id,type) %>%
    summarise(value=sum(value,na.rm=TRUE)) %>%
    ungroup() %>%
    filter(period_id>(max(period_id)-proj_ref_cap_auto_past_window)) %>%
    left_join(period_look_up,by="period_id")
  
  ts_ref<-ts(scen_x2 %>% filter(type=="Referrals") %>% arrange(period_id) %>% .$value,frequency=12)
  #plot(forecast(auto.arima(ts_ref),h=proj_ref_cap_auto_future_window))
  tbats_ref<-forecast(tbats(ts_ref),h=proj_ref_cap_auto_future_window)
  tbats_ref2<-tbats_ref %>%
    as.data.frame() %>%
    select(`Point Forecast`,`Lo 80`,`Hi 80`) %>%
    rename("nm"=`Point Forecast`,"lo"=`Lo 80`,"hi"=`Hi 80`) %>%
    mutate(period_id=1:nrow(.)) %>%
    mutate(period_id=period_id+max(x2$period_id)) %>%
    pivot_longer(cols=-period_id,names_to="scen",values_to="value")
  
  ts_cap<-ts(scen_x2 %>% filter(type=="Complete") %>% arrange(period_id) %>% .$value,frequency=12)
  #plot(forecast(auto.arima(ts_cap),h=proj_ref_cap_auto_future_window))
  tbats_cap<-forecast(tbats(ts_cap),h=proj_ref_cap_auto_future_window)
  tbats_cap2<-tbats_cap %>%
    as.data.frame() %>%
    select(`Point Forecast`,`Lo 80`,`Hi 80`) %>%
    rename("nm"=`Point Forecast`,"lo"=`Lo 80`,"hi"=`Hi 80`) %>%
    mutate(period_id=1:nrow(.)) %>%
    mutate(period_id=period_id+max(x2$period_id)) %>%
    pivot_longer(cols=-period_id,names_to="scen",values_to="value")
  
  
  # plot the projections
  fig5<-bind_rows(tbats_ref2 %>% mutate(type="Referrals"),tbats_cap2 %>% mutate(type="Treatments")) %>%
    left_join(period_look_up,by="period_id") %>%
    bind_rows(bind_rows(tbats_ref2 %>% mutate(type="Referrals"),tbats_cap2 %>% mutate(type="Treatments")) %>%
                left_join(period_look_up,by="period_id") %>%
                group_by(scen,type) %>%
                arrange(period_id) %>%
                slice(1) %>%
                mutate(period=floor_date(period,"month"))) %>%
    bind_rows(scen_x2 %>% mutate(scen="data") %>% mutate(type=recode(type,"Complete"="Treatments"))) %>%
    bind_rows(scen_x2 %>% mutate(scen="data") %>% mutate(type=recode(type,"Complete"="Treatments")) %>%
                filter(period_id==1) %>% mutate(period=floor_date(period,"month"))) %>%
    pivot_wider(names_from=scen,values_from=value) %>%
    mutate(type=factor(type,levels=c("Referrals","Treatments"))) %>%
    ggplot(aes(x=period)) +
    geom_vline(xintercept=max(as.Date(x1$period)),colour="darkgrey") +
    geom_step(aes(y=data/1000),direction="vh") +
    geom_step(aes(y=nm/1000),direction="vh") +
    geom_step(aes(y=lo/1000),direction="vh",linetype="dotted") +
    geom_step(aes(y=hi/1000),direction="vh",linetype="dotted") +
    scale_x_date(date_breaks="1 year",date_labels="%b\n%Y") +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~type) +
    ylab("Numbers per month (000s)") +
    labs(title=paste0(analysis_trust,", ",analysis_spec),
         subtitle="Forecasted new referrals and treatment capacity by TBATS method") +
    theme_bw()+
    theme(plot.subtitle=element_text(face="italic"),
          axis.title.x=element_blank(),
          panel.spacing=unit(1,"lines"))
  
  png(paste0(outdir,"fig5_",analysis_trust,"_",analysis_spec,".png"),height=3.5,width=7,units="in",res=400)
  print(fig5)
  dev.off()
  
  scen_combs<-expand_grid(unique(tbats_ref2$scen),unique(tbats_cap2$scen))
  names(scen_combs)<-c("ref","cap")
  
  proj_ref_cap2<-do.call("bind_rows",lapply(1:nrow(scen_combs), function(y) {
    data.frame(scenario=paste0(scen_combs$ref[y],"_ref_",scen_combs$cap[y],"_cap"),
               period_id=unique(tbats_ref2$period_id),
               referrals=tbats_ref2$value[which(tbats_ref2$scen==scen_combs$ref[y])],
               capacity=tbats_cap2$value[which(tbats_cap2$scen==scen_combs$cap[y])])
  }))
    
  
  
  
  ##################################
  # initialise (need to know numbers waiting at time of latest data)
  
  current_incompletes_init<-x2 %>%
    filter(type=="Incomplete",period_id==max(period_id)) %>%
    select(months_waited_id,value) %>%
    rename('incompletes'='value')
  
  res<-data.frame(scenario=character(0),period_id=numeric(0),months_waited_id=numeric(0),
                  incompletes=numeric(0),reneges=numeric(0))
  
  ##################################
  # do projections
  
  for (proj_scen in unique(proj_ref_cap2$scenario)) {
    scen_periods<-proj_ref_cap2 %>% filter(scenario==proj_scen) %>% arrange(period_id) %>% .$period_id
    current_incompletes<-current_incompletes_init
    for (p in scen_periods) {
      if (sum(current_incompletes$incompletes)>0) {
        res_p<-renege_cap_params %>% pivot_wider(names_from=type,values_from=value)
        res_p$node_inflow<-sapply(res_p$months_waited_id,function(m) {
          if(m==0) {
            proj_ref_cap2 %>% filter(scenario==proj_scen,period_id==p) %>% .$referrals
          } else if (m==max(res_p$months_waited_id)) {
            current_incompletes %>% filter(months_waited_id==m-1) %>% .$incompletes +
              current_incompletes %>% filter(months_waited_id==m) %>% .$incompletes
          } else {
            current_incompletes %>% filter(months_waited_id==m-1) %>% .$incompletes
          }
        })
        res_p$reneges<-res_p$renege_param*res_p$node_inflow
        res_p$treatments<-proj_ref_cap2 %>% filter(scenario==proj_scen,period_id==p) %>% .$capacity
        res_p$cap_num<-res_p$cap_param*res_p$node_inflow
        res_p$cap_denom<-sum(res_p$cap_param*res_p$node_inflow)
        res_p$tment<-res_p$treatments*res_p$cap_num/res_p$cap_denom
        # note: the next few lines are to prevent negative 'incompletes' - any overshoot gets distributed as evenly as possible
        tmp_incompletes<-res_p$node_inflow-res_p$tment-res_p$reneges    
        while (any(tmp_incompletes<0)) {
          tmp2a<-sum(tmp_incompletes[tmp_incompletes<0])
          tmp_incompletes[which(tmp_incompletes<0)]<-0
          tmp_incompletes[which(tmp_incompletes>0)]<-tmp_incompletes[which(tmp_incompletes>0)]+tmp2a/length(tmp_incompletes[which(tmp_incompletes>0)])
        }
        res_p$incompletes<-tmp_incompletes
        current_incompletes<-res_p %>%
          select(months_waited_id,incompletes)
        res<-bind_rows(res,res_p %>% 
                         select(months_waited_id,incompletes,reneges) %>% 
                         mutate(period_id=p,scenario=proj_scen))
      } else {
        res<-bind_rows(res,data.frame(period_id=p,
                                      months_waited_id=min(x1$months_waited_id,na.rm=TRUE):max(x1$months_waited_id,na.rm=TRUE),
                                      incompletes=0,
                                      reneges=0,
                                      scenario=proj_scen))
      }
    }
  }
  
  
  tmp_scen_maps<-data.frame(scenario=c("hi_ref_hi_cap","hi_ref_nm_cap","hi_ref_lo_cap",
                                       "nm_ref_hi_cap","nm_ref_nm_cap","nm_ref_lo_cap",
                                       "lo_ref_hi_cap","lo_ref_nm_cap","lo_ref_lo_cap"),
                            Referrals=c("High","High","High","Expected","Expected","Expected","Low","Low","Low"),
                            Capacity=c("High","Expected","Low","High","Expected","Low","High","Expected","Low"))
  
  fig6dat<-res %>%
    left_join(period_look_up,by="period_id") %>%
    mutate(scenario=factor(scenario)) %>%
    left_join(months_waited_look_up,by="months_waited_id") %>%
    #select(-months_waited_id) %>%
    select(scenario,period_id,incompletes,period,months_waited) %>%
    pivot_wider(names_from=months_waited,values_from=incompletes) %>%
    mutate(Total=rowSums(across(starts_with("Waiting")))) %>%
    pivot_longer(cols=-c(scenario,period,period_id),names_to="months_waited",values_to="value") %>%
    mutate(months_waited=factor(months_waited,levels=c("Total",as.character(months_waited_look_up$months_waited)))) %>%
    left_join(tmp_scen_maps,by="scenario") %>%
    mutate(Referrals=factor(Referrals,levels=c("Low","Expected","High"))) %>%
    mutate(Capacity=factor(Capacity,levels=c("Low","Expected","High"))) 
  
  fig6<-fig6dat %>%
    bind_rows(fig6dat %>% filter(period==min(period)) %>% mutate(period=floor_date(period,"month"))) %>%
    ggplot() +
    geom_step(aes(x=period,y=value/1000,colour=Capacity,linetype=Referrals),direction="vh",alpha=0.8) +
    facet_wrap(~months_waited,scales="free_y") +
    scale_x_date(date_breaks="1 year",date_labels="%b\n%Y") +
    scale_y_continuous(labels = scales::comma) +
    scale_colour_manual(values=c("red","darkorange","darkgreen")) +
    scale_linetype_manual(values=c("solid","dashed","dotted")) +
    ylab("Number of patients waiting (000s)") +
    labs(title=paste0(analysis_trust,", ",analysis_spec),
         subtitle="Projected waitlist size across different wait durations") +
    theme_bw() + 
    theme(plot.subtitle=element_text(face="italic"),
          axis.title.x=element_blank(),
          legend.position="bottom",
          panel.spacing=unit(0.5,"lines"),
          plot.margin = margin(5,10,5,5, unit="pt"))
  
  
  png(paste0(outdir,"fig6_",analysis_trust,"_",analysis_spec,".png"),height=7,width=8,units="in",res=400)
  print(fig6)
  dev.off()
  
  x2 %>%
    filter(type=="Incomplete",period_id==max(period_id)) %>%
    summarise(sum(value))
  
  res %>%
    filter(scenario=="lo_ref_hi_cap",period_id==max(period_id)) %>%
    summarise(sum(incompletes))
  res %>%
    filter(scenario=="hi_ref_lo_cap",period_id==max(period_id)) %>%
    summarise(sum(incompletes))
  
  # waittime distribution - incompletes
  dist_res<-res %>%
    select(-reneges) %>%
    bind_rows(x2 %>%
                mutate(scenario="data") %>%
                filter(type=="Incomplete") %>%
                select(-type) %>%
                rename("incompletes"=value)) %>%
    group_by(scenario,period_id) %>%
    summarise(under_3mo=sum(incompletes[months_waited_id<3])/sum(incompletes),
              under_6mo=sum(incompletes[months_waited_id<6])/sum(incompletes),
              under_9mo=sum(incompletes[months_waited_id<9])/sum(incompletes),
              under_12mo=sum(incompletes[months_waited_id<12])/sum(incompletes)) %>%
    pivot_longer(cols=c(under_3mo,under_6mo,under_9mo,under_12mo),names_to="type",values_to="value") %>%
    mutate(scenario=factor(scenario,levels=c(unique(proj_ref_cap2$scenario),"data"))) %>%
    left_join(period_look_up,by="period_id") %>%
    mutate(type=recode(type,"under_12mo"="Under 12 months","under_9mo"="Under 9 months","under_6mo"="Under 6 months","under_3mo"="Under 3 months")) %>%
    mutate(type=factor(type,levels=c("Under 12 months","Under 9 months","Under 6 months","Under 3 months")))
  
  fig7<-dist_res %>%
    bind_rows(dist_res %>%
                filter(scenario!="data") %>%
                filter(period_id==min(period_id)) %>%
                mutate(period=floor_date(period,"month"))) %>%
    
    pivot_wider(names_from=scenario,values_from=value) %>%
    pivot_longer(cols=-c(period_id,type,period,data),names_to="scenario",values_to="value") %>%
    left_join(tmp_scen_maps,by="scenario") %>%
    mutate(Referrals=factor(Referrals,levels=c("Low","Expected","High"))) %>%
    mutate(Capacity=factor(Capacity,levels=c("Low","Expected","High"))) %>%
    
    ggplot(aes(x=period,y=value,linetype=Referrals,colour=Capacity)) +
    geom_vline(xintercept=max(as.Date(x1$period)),colour="darkgrey") +
    geom_step(aes(x=period,y=data),colour="black",alpha=0.8) +
    geom_step(direction="vh",alpha=0.8) + 
    facet_wrap(~type,nrow=4,scales="free_y") +
    #labs(title=paste0(analysis_trust,", ",analysis_spec)) +
    scale_x_date(breaks=seq(as.Date("2000-01-01"),as.Date("2100-01-01"),by="6 month"),date_labels="%b\n%Y") +
    scale_y_continuous(labels=scales::percent) +
    scale_colour_manual(values=c("red","darkorange","darkgreen")) +
    scale_linetype_manual(values=c("solid","dashed","dotted")) +
    ylab("Waiting patients waiting under certain durations") +
    labs(title=paste0(analysis_trust,", ",analysis_spec),
         subtitle="Projected quantiles of wait time distribution") +
    theme_bw() +
    theme(plot.subtitle=element_text(face="italic"),
          axis.title.x=element_blank(),
          legend.position="bottom",
          legend.box="vertical",
          plot.margin = margin(5,10,5,5, unit="pt"),
          legend.spacing.y=unit(0,"cm"))
  
  png(paste0(outdir,"fig7_",analysis_trust,"_",analysis_spec,".png"),height=8,width=7,units="in",res=400)
  print(fig7)
  dev.off()
  
  resx<-bind_rows(res,
                  x2 %>%
                    mutate(scenario="data") %>%
                    mutate(type=tolower(type)) %>%
                    filter(type %in% c("reneges","incomplete")) %>%
                    pivot_wider(names_from=type,values_from=value) %>%
                    rename("incompletes"="incomplete") %>%
                    select(scenario,period_id,months_waited_id,incompletes,reneges)) %>%
    left_join(period_look_up,by="period_id") %>%
    left_join(months_waited_look_up,by="months_waited_id")

  write.csv(resx,file=paste0(outdir,"res_",analysis_trust,"_",analysis_spec,".csv"),row.names=FALSE)
  
}
