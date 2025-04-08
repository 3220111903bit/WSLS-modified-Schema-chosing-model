rm(list = ls())
library(dplyr)
options(warn = -1)
setwd('D:/Users/Xin/Downloads/CMML_mini1')
source('D:/Users/Xin/Downloads/CMML_mini1/model_functions_cmml_lr_modified_v4.R')

typelist = c("H","Hc","HL","L","Lc","LH")
exp_typelist = c("painting", "quote")

subjectnum = 20
Param.df <- data.frame(Subject = 1:subjectnum,
                       a_schema = rep(0.2,subjectnum), # change from 0.2 to 0.1
                       h_schema = rep(1000,subjectnum),
                       Beta_N=rep(0.1,subjectnum),
                       Beta_Var= rep(0.3,subjectnum),
                       a_generic  = rep(0.1,subjectnum),
                       h_generic = rep(1500,subjectnum),
                       Beta_gN = rep(0.1,subjectnum), #change from 0.2 to 0,1
                       Beta_gVar = rep(0.2,subjectnum),
                       w = rep(0.7,subjectnum), #change from 0.3 to 0.6
                       Phi = rep(20,subjectnum),
                       decay_speed = rep(0.999,subjectnum),
                       decay_speed_thres = rep(0.999,subjectnum),
                       thres_item_inter  = rep(6,subjectnum),
                       thres_item_final = rep(13.75,subjectnum),
                       thres_schema = rep(50,subjectnum),
                       theta_shift = rep(3, subjectnum),
                       timevar = rep(0.0001,subjectnum),
                       modeltimestep = rep(0.061 ,subjectnum))

res <- simulation(Param.df, "L", "painting", save = T,
                  savepath = "D:/Users/Xin/Downloads/CMML_mini1/modify_v5_confichange", sim.mode="before",
                  before.path = file.path("D:/Users/Xin/Downloads/CMML_mini1/", "L"),
                  scale.confi.init = T)
res <- simulation(Param.df, "H", "painting", save = T,
                  savepath = "D:/Users/Xin/Downloads/CMML_mini1/modify_v5_confi", sim.mode="before",
                  before.path = file.path("D:/Users/Xin/Downloads/CMML_mini1/", "H"),
                  scale.confi.init = T)

#res <- simulation(Param.df, "L", "painting", save = T,
#                  savepath = "D:/Users/Xin/Downloads/CMML_mini1/sim_try1/after", sim.mode="after",
#                  before.path = file.path("D:/Users/Xin/Downloads/CMML_mini1/sim_try1", "L"),
#                  scale.confi.init = T)

params <- res$param
allresult <- res$allresult_processed
