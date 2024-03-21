A = left_join(outcome_table_1_participant, outcome_table_2_participant, by = "Participant ID")
B = left_join(A, outcome_table_3_participant, by = "Participant ID")
C = left_join(B, outcomes_table4_participant, by = "Participant ID")
D = left_join(C, outcomes_table5_participant, by = "Participant ID")
E = left_join(D, cancer_outcomes_participant, by = "Participant ID")
F = left_join(E, cancer_outcomes2_participant, by = "Participant ID")
G = left_join(F, cancer_outcomes3_participant, by = "Participant ID") #####
H = left_join(G, data_participant, by = "Participant ID")
I = left_join(H, death_outcomes2_participant, by = "Participant ID")

print(colnames(B))
B  <- B  %>%
  select(-"Source of report of I51 (complications and ill-defined descriptions of heart disease)",
         -"Date I51 first reported (complications and ill-defined descriptions of heart disease)",
         -"Source of report of I25 (chronic ischaemic heart disease)",
         -"Date I25 first reported (chronic ischaemic heart disease)",
         -"Source of report of I24 (other acute ischaemic heart diseases)",
         -"Date I24 first reported (other acute ischaemic heart diseases)",
         -"Source of report of I23 (certain current complications following acute myocardial infarction)",
         -"Date I23 first reported (certain current complications following acute myocardial infarction)",
         -"Source of report of I22 (subsequent myocardial infarction)",
         -"Date I22 first reported (subsequent myocardial infarction)",
         -"Source of report of I21 (acute myocardial infarction)",
         -"Date I21 first reported (acute myocardial infarction)",
         -"Source of report of I20 (angina pectoris)",
         -"Date I20 first reported (angina pectoris)")
write.csv(G, file= 'Key_outcomes_table.csv')
write.csv(I, file= 'Key_outcomes_table_withdeath.csv')
#dx upload Key_outcomes_table_withdeath.csv

