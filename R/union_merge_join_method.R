

#### Union method
PR_MERGED1 <- union(SIM_PR_BDcompleto, APAC_PR_BDcompleto)

PR_MERGED2 <- union(BPAI_PR_BDcompleto, AIH_PR_BDcompleto)

PR_MERGED_BDCOMPLETO <- union(PR_MERGED1, PR_MERGED2)


###### Merge method
PR_M_MERGED1 <- merge.data.frame (SIM_PR_BDcompleto, BPAI_PR_BDcompleto, by = "ID_PACIENTE")

PR_M_MERGED2 <- merge.data.frame(APAC_PR_BDcompleto, AIH_PR_BDcompleto, by = "ID_PACIENTE")

PR_M_MERGED_BDcompleto2 <- merge.data.frame(PR_M_MERGED1, PR_M_MERGED2, by = "ID_PACIENTE")

PR_MERGE_BDCOMPLETO <- Reduce(function(x, y) merge(x, y,by =  "ID_PACIENTE", all=TRUE), list(BPAI_PR_BDcompleto, SIM_PR_BDcompleto, AIH_PR_BDcompleto,APAC_PR_BDcompleto))

PR_MERGE_BDCOMPLETO_list <- list(AIH_PR_BDcompleto, APAC_PR_BDcompleto, SIM_PR_BDcompleto, BPAI_PR_BDcompleto)

PR_MERGED_BD_COMPLETO <- PR_MERGE_BDCOMPLETO_list %>% reduce(left_join, by = "ID_PACIENTE")

data_table_AIH_PR = data.table(AIH_PR_BDcompleto, key="ID_PACIENTE")
data_table_BPAI_PR = data.table(BPAI_PR_BDcompleto, key="ID_PACIENTE")
data_table_SIM_PR = data.table(SIM_PR_BDcompleto, key="ID_PACIENTE")
data_table_APAC_PR = data.table(APAC_PR_BDcompleto, key="ID_PACIENTE")

data_table_list_PR1 <- list(data_table_AIH_PR,  data_table_APAC_PR)
data_table_list_PR2 <- list(data_table_BPAI_PR, data_table_SIM_PR)
data_table_list_PR <- list(data_table_AIH_PR,  data_table_APAC_PR , data_table_BPAI_PR, data_table_SIM_PR)
data_table_list_PR_inner <- list(PR_MERGED_BD_COMPLETO_dt1_inner, PR_MERGED_BD_COMPLETO_dt2_inner)
data_table_list_PR_full <- list(PR_MERGED_BD_COMPLETO_full1, PR_MERGED_BD_COMPLETO_full2)


PR_MERGED_BD_COMPLETO_dt1_inner <- data_table_list_PR1 %>% reduce(inner_join, by = "ID_PACIENTE")
PR_MERGED_BD_COMPLETO_dt2_inner <- data_table_list_PR2 %>% reduce(inner_join, by = "ID_PACIENTE")
PR_MERGED_BD_COMPLETO_inner <- data_table_list_PR_inner %>% reduce(inner_join, by = "ID_PACIENTE")

PR_MERGED_BD_COMPLETO_full1 <- data_table_list_PR1 %>% reduce(full_join, by = "ID_PACIENTE")
PR_MERGED_BD_COMPLETO_full2 <- data_table_list_PR2 %>% reduce(full_join, by = "ID_PACIENTE")
PR_MERGED_BD_COMPLETO_LJ <- data_table_list_PR_full %>% reduce(left_join, by = "ID_PACIENTE")

PR_MERGED_BD_COMPLETO <- data_table_list_PR %>% reduce(left_join, by = "ID_PACIENTE")


write.csv(PR_MERGED_BD_COMPLETO_dt1, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED_BD_COMPLETO_dt1.csv', row.names=FALSE)
write.csv(PR_MERGED_BD_COMPLETO_dt1_inner, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED_BD_COMPLETO_dt1_inner.csv', row.names=FALSE)
write.csv(PR_MERGED_BD_COMPLETO_dt2_inner, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED_BD_COMPLETO_dt2_inner.csv', row.names=FALSE)

write.csv(PR_MERGED_BD_COMPLETO_full1, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED_BD_COMPLETO_full1.csv', row.names=FALSE)
write.csv(PR_MERGED_BD_COMPLETO_full2, file = '/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED_BD_COMPLETO_full2.csv', row.names=FALSE)

PR_MERGED_BD_COMPLETO_full1 <- read.csv('/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED_BD_COMPLETO_full1.csv')
PR_MERGED_BD_COMPLETO_full2 <- read.csv('/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED_BD_COMPLETO_full2.csv')



PR_MERGED_BD_COMPLETO_dt1_inner <- read.csv("/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED/PR_MERGED_BD_COMPLETO_dt1_inner.csv")
PR_MERGED_BD_COMPLETO_dt2_inner <- read.csv("/Volumes/Mikael_backup3/PROJETO_BDBM/PR_MERGED/PR_MERGED_BD_COMPLETO_dt2_inner.csv")
