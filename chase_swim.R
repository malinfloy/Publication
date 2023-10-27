
### Method validation on chase vs swim, used fish from the Random stock tank
# Each trial consist of two chambers from chase method and another two from swim chamber method
# Under normal conditions

# Import mr data and background data ####

t1 <- import_file("C:/Users/malin/OneDrive - University of Bergen/MASTER  i Utvifysio/Master Thesis/Respirometry_coding/Respirometry_data/Raw data/Swim vs Chase/2022-10-26_100134_Chase_swim_T1/2022-10-26_100134_Chase_swim_T1.txt")

t2 <- import_file("C:/Users/malin/OneDrive - University of Bergen/MASTER  i Utvifysio/Master Thesis/Respirometry_coding/Respirometry_data/Raw data/Swim vs Chase/2022-10-26_103437_Chase_swim_T2/2022-10-26_103437_Chase_swim_T2.txt")

t3 <- import_file("C:/Users/malin/OneDrive - University of Bergen/MASTER  i Utvifysio/Master Thesis/Respirometry_coding/Respirometry_data/Raw data/Swim vs Chase/2022-10-26_110209_Chase_swim_T3/2022-10-26_110209_Chase_swim_T3.txt")

t4 <- import_file("C:/Users/malin/OneDrive - University of Bergen/MASTER  i Utvifysio/Master Thesis/Respirometry_coding/Respirometry_data/Raw data/Swim vs Chase/2022-10-26_113309_Chase_swim_T4/2022-10-26_113309_Chase_swim_T4.txt")

t5 <- import_file("C:/Users/malin/OneDrive - University of Bergen/MASTER  i Utvifysio/Master Thesis/Respirometry_coding/Respirometry_data/Raw data/Swim vs Chase/2022-10-26_120256_Chase_swim_T5/2022-10-26_120256_Chase_swim_T5.txt")

t6 <- import_file("C:/Users/malin/OneDrive - University of Bergen/MASTER  i Utvifysio/Master Thesis/Respirometry_coding/Respirometry_data/Raw data/Swim vs Chase/2022-10-26_123247_Chase_swim_T6/2022-10-26_123247_Chase_swim_T6.txt")

t7 <- import_file("C:/Users/malin/OneDrive - University of Bergen/MASTER  i Utvifysio/Master Thesis/Respirometry_coding/Respirometry_data/Raw data/Swim vs Chase/2022-10-26_130338_Chase_swim_T7/2022-10-26_130338_Chase_swim_T7.txt")

t8 <- import_file("C:/Users/malin/OneDrive - University of Bergen/MASTER  i Utvifysio/Master Thesis/Respirometry_coding/Respirometry_data/Raw data/Swim vs Chase/2022-10-26_133337_Chase_swim_T8/2022-10-26_133337_Chase_swim_T8.txt")

bg_start <- import_file("C:/Users/malin/OneDrive - University of Bergen/MASTER  i Utvifysio/Master Thesis/Respirometry_coding/Respirometry_data/Raw data/Swim vs Chase/2022-10-26_094240_Chase_swim_bg_start/2022-10-26_094240_Chase_swim_bg_start.txt")

bg_end <- import_file("C:/Users/malin/OneDrive - University of Bergen/MASTER  i Utvifysio/Master Thesis/Respirometry_coding/Respirometry_data/Raw data/Swim vs Chase/2022-10-26_140148_Chase_swim_bg_end/2022-10-26_140148_Chase_swim_bg_end.txt")


### calculate background data ####

# Before trial, chamber 1-4

bg_pre1 <- subset_data(bg_start, 150, 540, "row" ) |> 
  inspect(3,4) |> 
  calc_rate.bg()

bg_pre2 <- subset_data(bg_start, 150, 540, "row" ) |> 
  inspect(3,22) |> 
  calc_rate.bg()

bg_pre3 <- subset_data(bg_start, 150, 540, "row" ) |> 
  inspect(3,40) |> 
  calc_rate.bg()

bg_pre4 <- subset_data(bg_start, 150, 540, "row" ) |> 
  inspect(3,58) |> 
  calc_rate.bg()

# After trials - almost 4 hours after

bg_post1 <- subset_data(bg_end, 150, 650, "row" ) |> 
  inspect(3,4) |> 
  calc_rate.bg()

bg_post2 <- subset_data(bg_end, 150, 650, "row" ) |> 
  inspect(3,22) |> 
  calc_rate.bg()

bg_post3 <- subset_data(bg_end, 150, 650, "row" ) |> 
  inspect(3,40) |> 
  calc_rate.bg()

bg_post4 <- subset_data(bg_end, 150, 650, "row" ) |> 
  inspect(3,58) |> 
  calc_rate.bg()

#adj1 <- mean(c(-8.682354e-05,-0.0002819921))-224%
#adj2 <-mean(c(-8.526847e-05,-0.0002475783)) -190 %
#adj3 <-mean(c(8.580901e-05,-0.0003473932))-505 %
#adj4 <-mean(c(4.881113e-05,-0.0003639125)) 646% endring noe feil

### Calculate MMR from all trials ####
#make function

mmrfunc <-function(data,start,end,oxygen,plot_width,id, 
                      rate_width,adj,v,t,m, ...)

  {rep_1 <- subset_data(data,
                      from = start,
                      to = end, 
                      by = "row") |> 
  inspect(time= 3, oxygen = oxygen) |> 
  plot(width = plot_width) 

id <- rep_1 |>                
  # subset again to apply a 'wait' period
  subset_data(from = 30, 
              by = "row") |>                
  # use auto_rate to get most linear regions
  auto_rate(width = rate_width) |>
  # adjust
  adjust_rate(by = adj, method = "value") |>
  # convert
  convert_rate(oxy.unit = "%Air", 
               time.unit = "secs", 
               output.unit = "mg/h/g", 
               volume = v, 
               t =t, S = 0, 
               mass = m) |>   # in kg          
  # select highest rate
  select_rate(method = "highest", 
              n = 1) |>                      
  summary(export = TRUE)
}

#ch 1
t1mc <- mmrfunc(t1,310,800,4,0.15,t1mc,0.15,bg_post1,0.05419,25.2,0.00014)

#ch 2
t1fc <- mmrfunc(t1,310,800,22,plot_width = 0.2,t1fc,rate_width=0.2,bg_post2,0.05469,25.2,0.00021)

#ch 3
t1fs <-  mmrfunc(t1,420,800,40,plot_width = 0.15,t1fs,rate_width=0.2,bg_post3,0.05573,25.2,0.00024)

#ch 4
t1ms <-mmrfunc(t1,370,800,58,plot_width = 0.15,t1ms,rate_width=0.15,bg_post4,0.05707,25.2,0.0002)
  
#ch 1 died after
t2mc <- mmrfunc(t2,180,720,4,plot_width = 0.15,t2mc,rate_width=0.15,bg_post1,0.05419,25.3,0.00014)

#ch 2
t2ms <- mmrfunc(t2,325,720,22,plot_width = 0.15,t2ms,rate_width=0.15,bg_post2,0.05474,25.3,0.00016)

#ch 3
t2fc <- mmrfunc(t2,180,720,40,plot_width = 0.2,t2fc,rate_width=0.2,bg_post3,0.05558,25.3,0.00039)

#ch 4
t2fs <- mmrfunc(t2,220,720,58,plot_width = 0.2,t2fs,rate_width=0.2,bg_post4,0.0568,25.3,0.00047)

#ch 1
t3fc <- mmrfunc(t3,280,720,4,plot_width = 0.2,t3fc,rate_width=0.2,bg_post1,0.05413,25.2,0.0002)

#ch 2
t3mc <- mmrfunc(t3,280,720,22,plot_width = 0.15,t3mc,rate_width=0.15,bg_post2,0.05469,25.2,0.00021)

#ch 3
t3fs <- mmrfunc(t3,400,720,40,plot_width = 0.2,t3fs,rate_width=0.2,bg_post3,0.0557,25.2,0.00027)

#ch 4
t3ms <- mmrfunc(t3,340,720,58,plot_width = 0.15,t3ms,rate_width=0.15,bg_post4,0.05704,25.2,0.00023)

#ch 1
t4fc <- mmrfunc(t4,220,740,4,plot_width = 0.2,t4fc,rate_width=0.2,bg_post1,0.05411,25.2,0.00022)

#ch 2
t4mc <- mmrfunc(t4,220,740,22,plot_width = 0.15,t4mc,rate_width=0.15,bg_post2,0.05472,25.2,0.00018)

#ch 3
t4fs <- mmrfunc(t4,380,740,40,plot_width = 0.2,t4fs,rate_width=0.2,bg_post3,0.05578,25.2,0.00019)

#ch 4
t4ms <- mmrfunc(t4,320,740,58,plot_width = 0.15,t4ms,rate_width=0.15,bg_post4,0.05681,25.2,0.00046)

#ch 1
t5ms <- mmrfunc(t5,370,760,4,plot_width = 0.15,t5ms,rate_width=0.15,bg_post1,0.0542,25.1,0.00013)

#ch 2
t5mc <- mmrfunc(t5,250,760,22,plot_width = 0.15,t5mc,rate_width=0.15,bg_post2,0.05471,25.1,0.00019)

#ch 3
t5fs <- mmrfunc(t5,305,760,40,plot_width = 0.2,t5fs,rate_width=0.2,bg_post3,0.05534,25.1,0.00063)

#ch 4
t5fc <- mmrfunc(t5,250,760,58,plot_width = 0.2,t5fc,rate_width=0.2,bg_post4,0.05653,25.1,0.00074)

#ch 1
t6mc <- mmrfunc(t6,260,760,4,plot_width = 0.15,t6mc,rate_width=0.15,bg_post1,0.05417,25,0.00016)

#ch  2
t6ms <- mmrfunc(t6,80,760,22,plot_width = 0.15,t6ms,rate_width=0.15,bg_post2,0.05475,25,0.00015)

#ch 3
t6fs <- mmrfunc(t6,320,760,40,plot_width = 0.2,t6fs,rate_width=0.2,bg_post3,0.05537,25,0.0006)

#ch 4
t6fc <- mmrfunc(t6,230,760,58,plot_width = 0.2,t6fc,rate_width=0.2,bg_post4,0.05648,25,0.00079)

#ch 1
t7mc <- mmrfunc(t7,230,760,4,plot_width = 0.15,t7mc,rate_width=0.15,bg_post1,0.05419,25,0.00014)

#ch 2
t7ms <- mmrfunc(t7,380,760,22,plot_width = 0.15,t7ms,rate_width=0.15,bg_post2,0.05471,25,0.00019)

#ch 3
t7fc <- mmrfunc(t7,230,760,40,plot_width = 0.2,t7fc,rate_width=0.2,bg_post3,0.05542,25,0.00055)

#ch 4
t7fs <- mmrfunc(t7,320,760,58,plot_width = 0.2,t7fs,rate_width=0.2,bg_post4,0.0569,25,0.00037)

#ch 1
t8mc <- mmrfunc(t8,200,730,4,plot_width = 0.15,t8mc,rate_width=0.15,bg_post1,0.05418,25,0.00015)

#ch 2
t8ms <- mmrfunc(t8,380,730,22,plot_width = 0.15,t8ms,rate_width=0.15,bg_post2,0.05476,25,0.00014)

#ch 3
t8fc <- mmrfunc(t8,200,730,40,plot_width = 0.2,t8fc,rate_width=0.2,bg_post3,0.05567,25,0.0003)

#ch 4
t8fs <- mmrfunc(t8,290,730,58,plot_width = 0.2,t8fs,rate_width=0.2,bg_post4,0.05705,25,0.00022)


################################################################################

# Gather all data into table Hmmr_2fl "^[NH]mmr_")
method_df <- ls(pattern = "^(t[1-8][fmsc])") |> set_names() |> 
  map(get) |> 
  map(~pluck(.x,"rate.output") |> round(digits = 3) |> abs()) |> 
  unlist() |> 
  enframe( name = "ID", value = "MMR")|> 
  mutate(Method = if_else(str_detect(ID, "c"), "chase", "swim"),
         Sex = if_else(str_detect(ID, "f"), "female", "male"))

# Plot to see the difference, looks like chase has higher MMR

ggplot(method_df, aes(x = Method, y = MMR, fill = Sex))+
       geom_boxplot()+
  geom_point(position=position_dodge(width=0.75), aes(group=Sex), alpha=0.2)


# Quick analysis to test for significance


hist(method_df$MMR) 
method_df<- subset(method_df, MMR<0.9)

lm_method <-lme(MMR ~ Method+Sex, random = ~1|ID, na.action=na.omit, data = method_df) #Lowest AIC without interaction
summary(lm_method)


# No significant difference in method, sex did have an effect.
# Only looking at method and no sex, also showed no significance in method chase vs swim.
# chase also aerated the water up to <70%, would not be equal conditions for all fish. 
# Hence swim method was the most appropriate method to use


  



