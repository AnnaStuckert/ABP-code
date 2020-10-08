#N.B. mfi is not scored correctly due to errors
get_scores <- function(data){
  
  #create dataframe
  if ("be_id" %in% colnames(data) == TRUE) {
    scores <- select(data, record_id, gender, age, be_id)
  }  else {
    scores <- select(data, record_id, gender, age)
    scores$be_id <- NA
  }
  
  ####data prep
  
  if ("asra_a_1" %in% colnames(data) == TRUE) {
    #ASRA A
    asra_a_score <- data %>% 
      select(num_range("asra_a_", 1:6), "record_id", "gender", "age") %>%
      mutate(asra_a = rowSums(.[1:6]))
    scores$asra_a <- asra_a_score$asra_a #sum score
  } else {
    scores$asra_a <- NA
  }
  
  if ("asrs_b_1" %in% colnames(data) == TRUE) {
    #ASRS B
    asrs_b_score <- data %>% 
      select(num_range("asrs_b_", 1:12), "record_id", "gender", "age") %>%
      mutate(asrs_b = rowSums(.[1:12]))
    scores$asrs_b <- asrs_b_score$asrs_b # sum score
  } else {
    scores$asra_b <- NA
  }
  
  if ("asra_a_1" %in% colnames(data) == TRUE & "asrs_b_1" %in% colnames(data) == TRUE) {
    #combined
    asra_asrs_combined <- data %>% 
      select(num_range("asra_a_", 1:6), num_range("asrs_b_", 1:12), "record_id", "gender", "age") %>%
      mutate(asras_ab_combined = rowSums(.[1:18]))
    scores$asras_ab_combined <- asra_asrs_combined$asras_ab_combined# sum score
  } else {
    scores$asra_ab_combined <- NA
  }
  
  #CUSADOS
  if ("cusados_1" %in% colnames(data) == TRUE) {
  cusados_score <- data %>% 
    select(num_range("cusados_", 1:12), "record_id", "gender", "age") %>%
    mutate(cusados = rowSums(.[1:12]))
  scores$cusados <- cusados_score$cusados # sum score
  } else {
    scores$cusados <- NA
  }
  
  #Sias
  if ("sias_1" %in% colnames(data) == TRUE) {
    columnsToReverse <- c('sias_5', 'sias_9', 'sias_11')
    data[ ,columnsToReverse] = 4 - data[ ,columnsToReverse]
    sias_score <- data %>% 
      select(num_range("sias_", 1:20)) %>%
      mutate(sias = rowSums(.[1:20]))
    scores$sias <- sias_score$sias #reverse sum score
  } else {
    scores$sias <- NA
  }
  
  #DQ5
  if ("dq_1" %in% colnames(data) == TRUE) {
    dq5_score <- data %>% 
      select(num_range("dq_", 1:5), "record_id", "gender", "age") %>%
      mutate(dq5 = rowSums(.[1:5]))
    scores$DQ5 <- dq5_score$dq5 #sum score
  } else {
    scores$DQ5 <- NA
  }
  
  #MDQ - 1 == yes, 2 == no - missing data
  if ("mdq_1_1" %in% colnames(data) == TRUE) {
    mdq_score <- data %>%
      select(num_range("mdq_1_", 1:13))
    mdq_score$mdq_1_1 <- ifelse(mdq_score$mdq_1_1 == 2, 0, mdq_score$mdq_1_1)
    mdq_score$mdq_1_2 <- ifelse(mdq_score$mdq_1_2 == 2, 0, mdq_score$mdq_1_2)
    mdq_score$mdq_1_3 <- ifelse(mdq_score$mdq_1_3 == 2, 0, mdq_score$mdq_1_3)
    mdq_score$mdq_1_4 <- ifelse(mdq_score$mdq_1_4 == 2, 0, mdq_score$mdq_1_4)
    mdq_score$mdq_1_5 <- ifelse(mdq_score$mdq_1_5 == 2, 0, mdq_score$mdq_1_5)
    mdq_score$mdq_1_6 <- ifelse(mdq_score$mdq_1_6 == 2, 0, mdq_score$mdq_1_6)
    mdq_score$mdq_1_7 <- ifelse(mdq_score$mdq_1_7 == 2, 0, mdq_score$mdq_1_7)
    mdq_score$mdq_1_8 <- ifelse(mdq_score$mdq_1_8 == 2, 0, mdq_score$mdq_1_8)
    mdq_score$mdq_1_9 <- ifelse(mdq_score$mdq_1_9 == 2, 0, mdq_score$mdq_1_9)
    mdq_score$mdq_1_10 <- ifelse(mdq_score$mdq_1_10 == 2, 0, mdq_score$mdq_1_10)
    mdq_score$mdq_1_11 <- ifelse(mdq_score$mdq_1_11 == 2, 0, mdq_score$mdq_1_11)
    mdq_score$mdq_1_12 <- ifelse(mdq_score$mdq_1_12 == 2, 0, mdq_score$mdq_1_12)
    mdq_score$mdq_1_13 <- ifelse(mdq_score$mdq_1_13 == 2, 0, mdq_score$mdq_1_13)
    
    # to create sum scores with yes'es
    scores$MDQ_pt1_sum_of_yes <- rowSums(mdq_score) #sum score
    # to create threshold for when scores are problematic (1 = problemativ)
    #scores$MDQ_pt1_threshold <- ifelse(scores$MDP_pt1_sum_of_yes > 6, 1, 0)
  } else {
    scores$MDQ_pt1_sum_of_yes <- NA
  }
  
  #PCL
  if ("pcl_1" %in% colnames(data) == TRUE) {
    pcl_score <- data %>% 
      select(num_range("pcl_", 1:17), "record_id", "gender", "age") %>%
      mutate(pcl = rowSums(.[1:17]))
    scores$PCL <- pcl_score$pcl #sum score
  } else {
    scores$PLC <- NA
  }
  
  #PSS
  if ("pss_1" %in% colnames(data) == TRUE) {
    columnsToReverse <- c('pss_4', 'pss_5', 'pss_7', 'pss_8')
    data[ ,columnsToReverse] = 5 - data[ ,columnsToReverse]
    PSS_score <- data %>% 
      select(num_range("pss_", 1:10)) %>%
      mutate(pss = rowSums(.[1:10]))
    scores$PSS <- PSS_score$pss #reverse sum
  } else {
    scores$PSS <- NA
  }
  
  #IRI
  if ("iri_1" %in% colnames(data) == TRUE) {
    columnsToReverse <- c('iri_3', 'iri_12', 'iri_13', 'iri_14', 'iri_15', 'iri_18', 'iri_19')
    data[ ,columnsToReverse] = 4 - data[ ,columnsToReverse]
    iri_score <- data %>% 
      select(num_range("iri_", 1:28)) %>%
      mutate(iri_FS = rowSums(.[, c(1, 5, 7, 12, 16, 23, 26)])) %>%     
      mutate(iri_EC = rowSums(.[, c(2, 4, 9, 14, 18, 20, 22)])) %>%
      mutate(iri_PT = rowSums(.[, c(3, 8, 11, 15, 21, 25, 28)])) %>%    
      mutate(iri_PD = rowSums(.[, c(6, 10, 13, 17, 19, 24, 27)])) %>%
      mutate(iri_full = rowSums(.[1:28]))
    scores$iri_FS <- iri_score$iri_FS
    scores$iri_EC <- iri_score$iri_EC
    scores$iri_PT <- iri_score$iri_PT
    scores$iri_PD <- iri_score$iri_PD
    scores$iri_full <- iri_score$iri_full
  } else{
    scores$iri_FS <- NA
    scores$iri_EC <- NA
    scores$iri_PT <- NA
    scores$iri_PD <- NA
    scores$iri_full <- NA
  }
  
  #MFI20
  if ("mfi20_1" %in% colnames(data) == TRUE) {
    columnsToReverse <- c('mfi20_2', 'mfi20_5', 'mfi20_9', 'mfi20_10', 'mfi20_13', 'mfi20_14', 'mfi20_14', 'mfi20_16', 'mfi20_17', 'mfi20_18', 'mfi20_19')
    # data[ ,columnsToReverse] = 5 - data[ ,columnsToReverse]
    
    # data <- data %>% 
    #   mutate(across(columnsToReverse, ~5-.x))
    #change into 5 subscales for MFI20
    mfi_general_fatigue <- select(data, mfi20_1, mfi20_5, mfi20_12, mfi20_16, record_id)
    mfi_physical_fatigue <- select(data, mfi20_2, mfi20_8, mfi20_14, mfi20_20, record_id)
    mfi_reduced_activity <- select(data, mfi20_7, mfi20_11, mfi20_13, mfi20_19, record_id)
    mfi_reduced_motivation <- select(data, mfi20_3, mfi20_6, mfi20_10, mfi20_17, record_id)
    mfi_mental_fatigue <- select(data, mfi20_4, mfi20_9, mfi20_15, mfi20_18, record_id)
    mfi_general_fatigue <- mfi_general_fatigue %>%
      mutate(sum = rowSums(.[1:4]))
    mfi_physical_fatigue <- mfi_physical_fatigue %>%
      mutate(sum = rowSums(.[1:4]))
    mfi_reduced_activity <- mfi_reduced_activity %>%
      mutate(sum = rowSums(.[1:4]))
    mfi_reduced_motivation <- mfi_reduced_motivation %>%
      mutate(sum = rowSums(.[1:4]))
    mfi_mental_fatigue <- mfi_mental_fatigue %>%
      mutate(sum = rowSums(.[1:4]))
    scores$mfi_physical_fatigue <-  mfi_physical_fatigue$sum
    scores$mfi_general_fatigue <- mfi_general_fatigue$sum
    scores$mfi_reduced_activity <- mfi_reduced_activity$sum
    scores$mfi_reduced_motivation <- mfi_reduced_motivation$sum
    scores$mfi_mental_fatigue <- mfi_mental_fatigue$sum
    scores <- scores %>%
      mutate(mfi_sum = rowSums(scores[,c("mfi_physical_fatigue", "mfi_general_fatigue", "mfi_reduced_activity", "mfi_reduced_motivation", "mfi_mental_fatigue")]))
  } else {
    scores$mfi_physical_fatigue <-  NA
    scores$mfi_general_fatigue <- NA
    scores$mfi_reduced_activity <- NA
    scores$mfi_reduced_motivation <- NA
    scores$mfi_mental_fatigue <- NA
    scores$mfi_sum <- NA
  }
  
  #pas
  if ("pas_1" %in% colnames(data) == TRUE) {
    Pas_score <- data %>%
      select(num_range("pas_", 1:9))
    
    library(stringr)
    pas_1_split <- str_split_fixed(Pas_score$pas_1, ":", 2)
    Pas_score$pas_1_hour <- pas_1_split[,1]
    Pas_score$pas_1_min <- pas_1_split[,2]
    Pas_score$pas_1_hour <- as.integer(Pas_score$pas_1_hour)
    Pas_score$pas_1_min <- as.integer(Pas_score$pas_1_min)
    Pas_score$pas_1_min <- Pas_score$pas_1_min/60
    Pas_score$pas_1_min <- Pas_score$pas_1_min*100
    Pas_score$pas_1_min <- round(Pas_score$pas_1_min, 0)
    Pas_score$pas_1_hour <- Pas_score$pas_1_hour*100
    Pas_score$pas_1_min_clean <- Pas_score$pas_1_hour+Pas_score$pas_1_min
    Pas_score$pas_1_min_clean <- Pas_score$pas_1_min_clean/100
    
    pas_2_split <- str_split_fixed(Pas_score$pas_2, ":", 2)
    Pas_score$pas_2_hour <- pas_2_split[,1]
    Pas_score$pas_2_min <- pas_2_split[,2]
    Pas_score$pas_2_hour <- as.integer(Pas_score$pas_2_hour)
    Pas_score$pas_2_min <- as.integer(Pas_score$pas_2_min)
    Pas_score$pas_2_min <- Pas_score$pas_2_min/60
    Pas_score$pas_2_min <- Pas_score$pas_2_min*100
    Pas_score$pas_2_min <- round(Pas_score$pas_2_min, 0)
    Pas_score$pas_2_hour <- Pas_score$pas_2_hour*100
    Pas_score$pas_2_min_clean <- Pas_score$pas_2_hour+Pas_score$pas_2_min
    Pas_score$pas_2_min_clean <- Pas_score$pas_2_min_clean/100
    
    pas_3_split <- str_split_fixed(Pas_score$pas_3, ":", 2)
    Pas_score$pas_3_hour <- pas_3_split[,1]
    Pas_score$pas_3_min <- pas_3_split[,2]
    Pas_score$pas_3_hour <- as.integer(Pas_score$pas_3_hour)
    Pas_score$pas_3_min <- as.integer(Pas_score$pas_3_min)
    Pas_score$pas_3_min <- Pas_score$pas_3_min/60
    Pas_score$pas_3_min <- Pas_score$pas_3_min*100
    Pas_score$pas_3_min <- round(Pas_score$pas_3_min, 0)
    Pas_score$pas_3_hour <- Pas_score$pas_3_hour*100
    Pas_score$pas_3_min_clean <- Pas_score$pas_3_hour+Pas_score$pas_3_min
    Pas_score$pas_3_min_clean <- Pas_score$pas_3_min_clean/100
    
    pas_4_split <- str_split_fixed(Pas_score$pas_4, ":", 2)
    Pas_score$pas_4_hour <- pas_4_split[,1]
    Pas_score$pas_4_min <- pas_4_split[,2]
    Pas_score$pas_4_hour <- as.integer(Pas_score$pas_4_hour)
    Pas_score$pas_4_min <- as.integer(Pas_score$pas_4_min)
    Pas_score$pas_4_min <- Pas_score$pas_4_min/60
    Pas_score$pas_4_min <- Pas_score$pas_4_min*100
    Pas_score$pas_4_min <- round(Pas_score$pas_4_min, 0)
    Pas_score$pas_4_hour <- Pas_score$pas_4_hour*100
    Pas_score$pas_4_min_clean <- Pas_score$pas_4_hour+Pas_score$pas_4_min
    Pas_score$pas_4_min_clean <- Pas_score$pas_4_min_clean/100
    
    pas_5_split <- str_split_fixed(Pas_score$pas_5, ":", 2)
    Pas_score$pas_5_hour <- pas_5_split[,1]
    Pas_score$pas_5_min <- pas_5_split[,2]
    Pas_score$pas_5_hour <- as.integer(Pas_score$pas_5_hour)
    Pas_score$pas_5_min <- as.integer(Pas_score$pas_5_min)
    Pas_score$pas_5_min <- Pas_score$pas_5_min/60
    Pas_score$pas_5_min <- Pas_score$pas_5_min*100
    Pas_score$pas_5_min <- round(Pas_score$pas_5_min, 0)
    Pas_score$pas_5_hour <- Pas_score$pas_5_hour*100
    Pas_score$pas_5_min_clean <- Pas_score$pas_5_hour+Pas_score$pas_5_min
    Pas_score$pas_5_min_clean <- Pas_score$pas_5_min_clean/100
    
    pas_6_split <- str_split_fixed(Pas_score$pas_6, ":", 2)
    Pas_score$pas_6_hour <- pas_6_split[,1]
    Pas_score$pas_6_min <- pas_6_split[,2]
    Pas_score$pas_6_hour <- as.integer(Pas_score$pas_6_hour)
    Pas_score$pas_6_min <- as.integer(Pas_score$pas_6_min)
    Pas_score$pas_6_min <- Pas_score$pas_6_min/60
    Pas_score$pas_6_min <- Pas_score$pas_6_min*100
    Pas_score$pas_6_min <- round(Pas_score$pas_6_min, 0)
    Pas_score$pas_6_hour <- Pas_score$pas_6_hour*100
    Pas_score$pas_6_min_clean <- Pas_score$pas_6_hour+Pas_score$pas_6_min
    Pas_score$pas_6_min_clean <- Pas_score$pas_6_min_clean/100
    
    pas_7_split <- str_split_fixed(Pas_score$pas_7, ":", 2)
    Pas_score$pas_7_hour <- pas_7_split[,1]
    Pas_score$pas_7_min <- pas_7_split[,2]
    Pas_score$pas_7_hour <- as.integer(Pas_score$pas_7_hour)
    Pas_score$pas_7_min <- as.integer(Pas_score$pas_7_min)
    Pas_score$pas_7_min <- Pas_score$pas_7_min/60
    Pas_score$pas_7_min <- Pas_score$pas_7_min*100
    Pas_score$pas_7_min <- round(Pas_score$pas_7_min, 0)
    Pas_score$pas_7_hour <- Pas_score$pas_7_hour*100
    Pas_score$pas_7_min_clean <- Pas_score$pas_7_hour+Pas_score$pas_7_min
    Pas_score$pas_7_min_clean <- Pas_score$pas_7_min_clean/100
    
    pas_8_split <- str_split_fixed(Pas_score$pas_8, ":", 2)
    Pas_score$pas_8_hour <- pas_8_split[,1]
    Pas_score$pas_8_min <- pas_8_split[,2]
    Pas_score$pas_8_hour <- as.integer(Pas_score$pas_8_hour)
    Pas_score$pas_8_min <- as.integer(Pas_score$pas_8_min)
    Pas_score$pas_8_min <- Pas_score$pas_8_min/60
    Pas_score$pas_8_min <- Pas_score$pas_8_min*100
    Pas_score$pas_8_min <- round(Pas_score$pas_8_min, 0)
    Pas_score$pas_8_hour <- Pas_score$pas_8_hour*100
    Pas_score$pas_8_min_clean <- Pas_score$pas_8_hour+Pas_score$pas_8_min
    Pas_score$pas_8_min_clean <- Pas_score$pas_8_min_clean/100
    
    pas_9_split <- str_split_fixed(Pas_score$pas_9, ":", 2)
    Pas_score$pas_9_hour <- pas_9_split[,1]
    Pas_score$pas_9_min <- pas_9_split[,2]
    Pas_score$pas_9_hour <- as.integer(Pas_score$pas_9_hour)
    Pas_score$pas_9_min <- as.integer(Pas_score$pas_9_min)
    Pas_score$pas_9_min <- Pas_score$pas_9_min/60
    Pas_score$pas_9_min <- Pas_score$pas_9_min*100
    Pas_score$pas_9_min <- round(Pas_score$pas_9_min, 0)
    Pas_score$pas_9_hour <- Pas_score$pas_9_hour*100
    Pas_score$pas_9_min_clean <- Pas_score$pas_9_hour+Pas_score$pas_9_min
    Pas_score$pas_9_min_clean <- Pas_score$pas_9_min_clean/100
    
    Pas_score <- select(Pas_score, pas_1_min_clean, pas_2_min_clean, pas_3_min_clean, pas_4_min_clean, pas_5_min_clean, pas_6_min_clean, pas_7_min_clean, pas_8_min_clean,pas_9_min_clean)
    #sources: https://golf.procon.org/met-values-for-800-activities/
    #sleep
    Pas_score$pas_1_met <- Pas_score$pas_1_min_clean*0.95
    #watching tv sitting
    Pas_score$pas_2_met <- Pas_score$pas_2_min_clean*1.3
    #desk work
    Pas_score$pas_3_met <- Pas_score$pas_3_min_clean*1.3
    #cooking
    Pas_score$pas_4_met <- Pas_score$pas_4_min_clean*3.3
    #cleaning, sweeping, slow, moderateeffort
    Pas_score$pas_5_met <- Pas_score$pas_5_min_clean*3.8
    #bicycling, <10 mph, leisure, to work or for pleasure	
    Pas_score$pas_6_met <- Pas_score$pas_6_min_clean*4
    #carrying, loading or stacking wood, loading/unloading or carrying lumber
    Pas_score$pas_7_met <- Pas_score$pas_7_min_clean*5.5
    #shoveling snow, by hand
    Pas_score$pas_8_met <- Pas_score$pas_8_min_clean*6
    #jogging, general or soccer, casual, general
    Pas_score$pas_9_met <- Pas_score$pas_9_min_clean*7
    
    #find 24-hour met sum score
    Pas_score <- Pas_score %>%
      mutate(pas_sum = rowSums(Pas_score[,c("pas_1_met", "pas_2_met", "pas_3_met", "pas_4_met", "pas_5_met", "pas_6_met", "pas_7_met", "pas_8_met", "pas_9_met")]))
    scores$pas <- Pas_score$pas_sum
  }
  
  #STAI
  if ("stai_1" %in% colnames(data) == TRUE) {
    columnsToReverse <- c('stai_1', 'stai_2', 'stai_5', 'stai_8', 'stai_11', 'stai_14', 'stai_15', 'stai_16', 'stai_19', 'stai_20', 'stai_21', 'stai_23', 'stai_26', 'stai_27', 'stai_30', 'stai_33', 'stai_36', 'stai_39')
    data[ ,columnsToReverse] = 4 - data[ ,columnsToReverse]
    STAI_score <- data %>%
      select(num_range("stai_", 1:40)) %>%
      mutate(stai = rowSums(.[1:40]))
    scores$stai <- STAI_score$stai
  } else {
    scores$STAI <- NA
  }
  
  
  # #MDQ 
  # data$mdq_1_1 <- data$mdq_1_1 - 1
  # data$mdq_1_2 <- data$mdq_1_2 - 1
  # data$mdq_1_3 <- data$mdq_1_3 - 1
  # data$mdq_1_4 <- data$mdq_1_4 - 1
  # data$mdq_1_5 <- data$mdq_1_5 - 1
  # data$mdq_1_6 <- data$mdq_1_6 - 1
  # data$mdq_1_7 <- data$mdq_1_7 - 1
  # data$mdq_1_8 <- data$mdq_1_8 - 1
  # data$mdq_1_9 <- data$mdq_1_9 - 1
  # data$mdq_1_10 <- data$mdq_1_10 - 1
  # data$mdq_1_11 <- data$mdq_1_11 - 1
  # data$mdq_1_12 <- data$mdq_1_12 - 1
  # data$mdq_1_13 <- data$mdq_1_13 - 1
  # #if mdq_1 er højere end 7 yes = postive, if under 7 yes, then negative 
  # mdq_score <- data %>% 
  #   select(num_range("mdq_1_", 1:13))
  # mdq_score$score_pt1<- ifelse(rowSums(mdq_score[1:13]) <= 6, 1, 0)
  # 
  #PHQ9
  if ("phq_9_1" %in% colnames(data) == TRUE) {
    data$phq_9_1 <- data$phq_9_1 - 1
    data$phq_9_2 <- data$phq_9_2 - 1
    data$phq_9_3 <- data$phq_9_3 - 1
    data$phq_9_4 <- data$phq_9_4 - 1
    data$phq_9_5 <- data$phq_9_5 - 1
    data$phq_9_6 <- data$phq_9_6 - 1
    data$phq_9_7 <- data$phq_9_7 - 1
    data$phq_9_8 <- data$phq_9_8 - 1
    data$phq_9_9 <- data$phq_9_9 - 1
    PHQ9_score <- data %>% 
      select(num_range("phq_9_", 1:10), "record_id", "gender", "age") %>%
      mutate(phq9_depression_score = rowSums(.[1:9])) #only up until question 9, as number 10 is "if you have selected one of the previously mentioned" - check if it should be included in sum score
    #spytter kun 66 ud, evt slet "drop_na"? - når drop na slettes passer det.
    scores$PHQ9 <- PHQ9_score$phq9_depression_score #sum score
  } else {
    scores$PHQ <- NA
  }
  
  # Oci_r
  if ("oci_r_1" %in% colnames(data) == TRUE) {
    data$oci_r_1 <- data$oci_r_1 - 1
    data$oci_r_2 <- data$oci_r_2 - 1
    data$oci_r_3 <- data$oci_r_3 - 1
    data$oci_r_4 <- data$oci_r_4 - 1
    data$oci_r_5 <- data$oci_r_5 - 1
    data$oci_r_6 <- data$oci_r_6 - 1
    data$oci_r_7 <- data$oci_r_7 - 1
    data$oci_r_8 <- data$oci_r_8 - 1
    data$oci_r_9 <- data$oci_r_9 - 1
    data$oci_r_10 <- data$oci_r_10 - 1
    data$oci_r_11 <- data$oci_r_11 - 1
    data$oci_r_12 <- data$oci_r_12 - 1
    data$oci_r_13 <- data$oci_r_13 - 1
    data$oci_r_14 <- data$oci_r_14 - 1
    data$oci_r_15 <- data$oci_r_15 - 1
    data$oci_r_16 <- data$oci_r_16 - 1
    data$oci_r_17 <- data$oci_r_17 - 1
    data$oci_r_18 <- data$oci_r_18 - 1
    oci_r_score <- data %>% 
      select(num_range("oci_r_", 1:18), "record_id", "gender", "age") %>%
      mutate(oci_r = rowSums(.[1:18]))
    scores$oci_r <- oci_r_score$oci_r
  } else {
    scores$OCI_R <- NA
  }
  
  # isi
  if ("isi_1" %in% colnames(data) == TRUE) {
    isi_score <- data %>% 
      select(num_range("isi_", 1:7), "record_id", "gender", "age") %>%
      mutate(isi = rowSums(.[1:7]))
    scores$isi <- isi_score$isi
  } else {
    scores$ISI <- NA
  }
  
  #maia
  if ("maia_1" %in% colnames(data) == TRUE) {
    maia_notice_score <- data %>% 
      select(num_range("maia_", 1:4), "record_id", "gender", "age") %>%
      mutate(maia_notice = rowSums(.[1:4])/4)
    columnsToReverse <- c('maia_5', 'maia_6', 'maia_7')
    data[ ,columnsToReverse] = 6 - data[ ,columnsToReverse]
    maia_ndistract_score <- data %>%
      select(num_range('maia_', 5:7), "record_id", "gender", "age") %>%
      mutate(maia_ndistract= rowSums(.[1:3])/6)
    
    columnsToReverse <- c('maia_8', 'maia_9')
    data[ ,columnsToReverse] = 6 - data[ ,columnsToReverse]
    maia_nworry_score <- data %>% 
      select(num_range('maia_', 8:10), "record_id", "gender", "age") %>%
      mutate(maia_nworry = rowSums(.[1:3])/5)
    maia_attnReg_score <- data %>% 
      select(num_range('maia_', 11:17), "record_id", "gender", "age") %>%
      mutate(maia_attnReg = rowSums(.[1:7])/7)
    maia_EmoAware_score <- data %>% 
      select(num_range('maia_', 18:22), "record_id", "gender", "age") %>%
      mutate(maia_EmoAware = rowSums(.[1:5])/5)
    maia_SelfReg_score <- data %>% 
      select(num_range('maia_', 23:26), "record_id", "gender", "age") %>%
      mutate(maia_SelfRef = rowSums(.[1:4])/4)
    maia_listen_score <- data %>% 
      select(num_range('maia_', 27:29), "record_id", "gender", "age") %>%
      mutate(maia_listen = rowSums(.[1:3])/3)
    maia_trust_score <- data %>% 
      select(num_range('maia_', 30:32), "record_id", "gender", "age") %>%
      mutate(maia_trust = rowSums(.[1:3])/3)
    scores$maia_notice <- maia_notice_score$maia_notice #sum score
    scores$maia_ndistract <- maia_ndistract_score$maia_ndistract #reverse sum
    scores$maia_nworry <- maia_nworry_score$maia_nworry #reverse sum
    scores$maia_attnReg <- maia_attnReg_score$maia_attnReg #sum score
    scores$maia_EmoAware <- maia_EmoAware_score$maia_EmoAware #sum score
    scores$maia_SelfRef <- maia_SelfReg_score$maia_SelfRef #sum score
    scores$maia_listen <- maia_listen_score$maia_listen #sum score
    scores$maia_trust <- maia_trust_score$maia_trust #sum score
  } else {
    scores$maia_notice <- NA
    scores$maia_ndistract <- NA
    scores$maia_nworry <- NA
    scores$maia_attnReg <- NA
    scores$maia_EmoAware <- NA
    scores$maia_SelfRef <- NA
    scores$maia_listen <- NA
    scores$maia_trust <- NA
  }
  
  #MDI
  if ("mdi_1" %in% colnames(data) == TRUE) {
    mdi_score <- data %>% 
      select(num_range("mdi_", 1:20), "mdi_8a", "mdi_8b", "mdi_9a", "mdi_9b", "mdi_10a", "mdi_10b") %>%
      mutate(mdi_8 = pmax(mdi_8a, mdi_8b)) %>%  #take only the highest value of the two columns 8a and 8b
      mutate(mdi_10 = pmax(mdi_10a, mdi_10b)) %>% #take only the highest value of the two columns 10a and 10b
      mutate(mdi_4_5 = pmax(mdi_4, mdi_5)) #take only the highest value of the two columns 4 and 5
    mdi_score[, c(4:5, 8:9, 12:13)] <- 0 #replace values in the "duelling values" with 0 to facilitate sum-scoring
    mdi_score <- mdi_score %>% mutate(mdi = rowSums(.[1:16]))
    scores$mdi <- mdi_score$mdi
  } else {
    scores$mdi <- NA
  }
  
  #MSPSS
  if ("mpsss_1" %in% colnames(data) == TRUE) {
    MSPSS_score <- data %>% 
      select(num_range("mpsss_", 1:12)) %>%
      mutate(mpsss_SO = rowSums(.[, c(1, 2, 5, 10)]/4)) %>% 
      mutate(mpsss_Fam = rowSums(.[, c(3, 4, 8, 11)]/4)) %>%
      mutate(mpsss_Friends = rowSums(.[, c(6, 7, 9, 12)]/4)) %>%
      mutate(mpsss_full = rowSums(.[1:12]/12))
    scores$mpsss_SO <- MSPSS_score$mpsss_SO
    scores$mpsss_Fam <- MSPSS_score$mpsss_Fam
    scores$mpsss_Friends <- MSPSS_score$mpsss_Friends
    scores$mpsss_full <- MSPSS_score$mpsss_full
  }else {
    scores$mpsss_SO <-  NA
    scores$mpsss_Fam <- NA
    scores$mpsss_Friends <- NA
    scores$mpsss_full <- NA
  }
  
  #SPEQ
  if ("speq_p1" %in% colnames(data) == TRUE) {
    #Assuming all subscales are sum-scores
    #SPEQ_P - paranoia
    speq_p_score <- data %>% 
      select(num_range("speq_p", 1:15), "record_id", "gender", "age") %>%
      -1 %>% 
      mutate(speq_p = rowSums(.[1:15]))
    #SPEQ_c - hallucinations
    speq_c_score <- data %>% 
      select(num_range("speq_c", 1:9), "record_id", "gender", "age") %>%
      -1 %>% 
      mutate(speq_c = rowSums(.[1:9]))
    #SPEQ_cog - cognitive disorganization
    speq_cog_score <- data %>% 
      select(num_range("speq_cog", 1:11), "record_id", "gender", "age") %>%
      -1 %>% 
      mutate(speq_cog = rowSums(.[1:11]))
    #SPEQ_m - grandiosity
    speq_m_score <- data %>% 
      select(num_range("speq_m", 1:8), "record_id", "gender", "age") %>%
      -1 %>% 
      mutate(speq_m = rowSums(.[1:8]))
    #SPEQ_teps - hedonia (reversed it for ANhedonia values (delete first 2 lines of code for non-reversed scores))
    columnsToReverse <- c('speq_teps1', 'speq_teps2', 'speq_teps3', 'speq_teps4', 'speq_teps5','speq_teps6',  'speq_teps7', 'speq_teps8', 'speq_teps9', 'speq_teps10')
    data[ ,columnsToReverse] = 6 - data[ ,columnsToReverse]
    speq_teps_score <- data %>% 
      select(num_range("speq_teps", 1:10), "record_id", "gender", "age") %>%
      -1 %>% 
      mutate(speq_teps = rowSums(.[1:10]))
    scores$speq_p <- speq_p_score$speq_p
    scores$speq_c <- speq_c_score$speq_c
    scores$speq_cog <- speq_cog_score$speq_cog
    scores$speq_m <- speq_m_score$speq_m
    scores$speq_teps <- speq_teps_score$speq_teps
  } else {
    scores$speq_p <- NA
    scores$speq_c <- NA
    scores$speq_cog <- NA
    scores$speq_m <- NA
    scores$speq_teps <- NA
  }
  
  #MAAS
  if ("maas_1" %in% colnames(data) == TRUE) {
    maas_score <- data %>% 
      select(num_range("maas_", 1:15), "record_id", "gender", "age") %>%
      mutate(maas = rowSums(.[1:15]/15))
    scores$maas <- maas_score$maas
  } else {
    scores$MAAS <- NA
  }
  
  # WHO
  if ("who5_1" %in% colnames(data) == TRUE) {
    data$who5_1 <- data$who5_1 - 1
    data$who5_2 <- data$who5_2 - 1
    data$who5_3 <- data$who5_3 - 1
    data$who5_4 <- data$who5_4 - 1
    data$who5_5 <- data$who5_5 - 1
    WHO_score <- data %>% 
      select(num_range("who5_", 1:5), "record_id", "gender", "age") %>%
      mutate(who = rowSums(.[1:5])*4)
    scores$who <- WHO_score$who
    scores$who_riskzone_yes <- ifelse(scores$who <= 50, 1, 0)
  } else {
    scores$who <- NA
    scores$who_riskzone_yes <- NA
  }
  
  #AQ10
  if ("aq10_1" %in% colnames(data) == TRUE) {
    columnsToReverse <- c('aq10_2', 'aq10_3', 'aq10_4', 'aq10_5', 'aq10_6', 'aq10_9')
    data[ ,columnsToReverse] = 4 - data[ ,columnsToReverse]
    aq10_score <- data %>% 
      select(num_range("aq10_", 1:10))
    aq10_score$aq10_1<- ifelse(aq10_score$aq10_1 <= 2, 1, 0)
    aq10_score$aq10_2<- ifelse(aq10_score$aq10_2 <= 2, 1, 0)
    aq10_score$aq10_3<- ifelse(aq10_score$aq10_3 <= 2, 1, 0)
    aq10_score$aq10_4<- ifelse(aq10_score$aq10_4 <= 2, 1, 0)
    aq10_score$aq10_5<- ifelse(aq10_score$aq10_5 <= 2, 1, 0)
    aq10_score$aq10_6<- ifelse(aq10_score$aq10_6 <= 2, 1, 0)
    aq10_score$aq10_7<- ifelse(aq10_score$aq10_7 <= 2, 1, 0)
    aq10_score$aq10_8<- ifelse(aq10_score$aq10_8 <= 2, 1, 0)
    aq10_score$aq10_9<- ifelse(aq10_score$aq10_9 <= 2, 1, 0)
    aq10_score$aq10_10<- ifelse(aq10_score$aq10_10 <= 2, 1, 0)
    aq10_score$aq10_sum <- rowSums(aq10_score[1:10])
    scores$aq10 <- aq10_score$aq10_sum
    scores$aq10_autism_yes <- ifelse(scores$aq10 >= 6, 1, 0)
  } else {
    scores$aq10 <- NA
    scores$aq10_autism_yes <- NA
  }
  
  #COVID
  if ("cov_1" %in% colnames(data) == TRUE) {
    covid_data <- select(data, record_id, cov_1, cov_2, cov_3, cov_4, cov_5)
    covid_data$cov_5_1 <- covid_data$cov_5 /20
    covid_sum <- select(covid_data, record_id, cov_1, cov_2, cov_3, cov_4, cov_5_1)
    covid_sum$sum <- rowSums(covid_sum[2:6])
    scores$covid <- covid_sum$sum
  }else {
    scores$covid <- NA
  }
  
  # WHOQOL
  if ("whoqol_1" %in% colnames(data) == TRUE) {
    columnsToReverse <- c('whoqol_3', 'whoqol_4', 'whoqol_26')
    data[ ,columnsToReverse] = 5 - data[ ,columnsToReverse]
    Quality_life <- select(data, record_id, whoqol_1, whoqol_2)
    Domain_1_physical <- select(data, record_id, whoqol_3, whoqol_4, whoqol_10, whoqol_15, whoqol_16, whoqol_17, whoqol_18)
    Domain_2_phychological <- select(data, record_id, whoqol_5, whoqol_6, whoqol_7, whoqol_11, whoqol_19, whoqol_26) 
    Domain_3_social_relationships <- select(data, record_id, whoqol_20, whoqol_21, whoqol_22)
    Domain_4_Enviroment <- select(data, record_id, whoqol_8, whoqol_9, whoqol_12, whoqol_13, whoqol_14, whoqol_23, whoqol_24, whoqol_25)
    Quality_life <- Quality_life %>% 
      mutate(whoqol_quality_life= rowSums(.[2:3]))
    Domain_1_physical <- Domain_1_physical %>% 
      mutate(whoqol_physical= rowSums(.[2:8]))
    Domain_2_phychological <- Domain_2_phychological %>% 
      mutate(whoqol_phychological= rowSums(.[2:7]))
    Domain_3_social_relationships <- Domain_3_social_relationships %>% 
      mutate(whoqol_social_relationships= rowSums(.[2:4]))
    Domain_4_Enviroment <- Domain_4_Enviroment %>% 
      mutate(whoqol_enviroment = rowSums(.[2:9]))
    scores$whoqol_quality_life <- Quality_life$whoqol_quality_life
    scores$whoqol_physical <- Domain_1_physical$whoqol_physical
    scores$whoqol_phychological <- Domain_2_phychological$whoqol_phychological
    scores$whoqol_social_relationships <- Domain_3_social_relationships$whoqol_social_relationships
    scores$whoqol_enviroment <- Domain_4_Enviroment$whoqol_enviroment
    scores <- scores %>%
      mutate(whoqol_sum = rowSums(scores[,c("whoqol_quality_life", "whoqol_physical", "whoqol_phychological", "whoqol_social_relationships", "whoqol_enviroment")]))
  } else {
    scores$whoqol_quality_life <- NA
    scores$whoqol_physical <- NA
    scores$whoqol_phychological <- NA
    scores$whoqol_social_relationships <- NA
    scores$whoqol_enviroment <- NA
    scores$whoqol_sum <- NA }
  # Fejl i `$<-.data.frame`(`*tmp*`, whoqol_physical, value = c(28, 20,  : replacement has 112 rows, data has 113 
  #insert NAs? PROBLEM: domain1 has 112 rows, all others have 113 - find an alternative to drop.na
  
  
  #PHQ15
  if ("phq15_1" %in% colnames(data) == TRUE) {
  PHQ15_score <- data %>% 
    select(num_range("phq15_", 1:15)) 
  
  PHQ15_score <- PHQ15_score - 1
  PHQ15_score$phq15_14 <- ifelse(PHQ15_score$phq15_14 == 3, 2, PHQ15_score$phq15_14)
  PHQ15_score$phq15_15 <- ifelse(PHQ15_score$phq15_15 == 3, 2, PHQ15_score$phq15_15)
  
  PHQ15_score <- PHQ15_score %>% mutate(PHQ15 = rowSums(.[1:15], na.rm = TRUE))
  scores$phq15 <-PHQ15_score$PHQ15
  #For PHQ15, normally all 15 questions are scored either as 0, 1 or 2. However, for the questionnaire in redcap, questions are scored 1,2 or 3 (should be handled by subtracting 1 from all questions for scoring) and question 14 and 15 have 4 options instead of 3, and they are scored as either 1, 2, 3 or 4. I cannot find ANYTHING online about why the last 2 questions should be scored with 4 options instead of the usual 3, and whether I can sum-score it the same way is PHQ15 normally would (in case values extend the normal max of 30). What should I do about this?
  } else {
    scores$phq15 <- NA
  }
  
  data <- data %>% 
    rename(
      wemwsb_8 = wemwsb_15,
      wemwsb_9 = wemwsb_16
    )
  #WEMWBS
  if ("wemwsb_1" %in% colnames(data) == TRUE) {
    WEMWBS_score <- select(data, num_range("wemwsb_", 1:14))
    WEMWBS_score <- mutate(WEMWBS_score, WEMWBS = rowSums(WEMWBS_score[1:14]))
    scores$wemwes <- WEMWBS_score$WEMWBS
  } else {
    scores$wemwbs <- NA
  }
  
  return(scores)
}

scores <- get_scores(data)

#Write out the data
write_csv(scores, here("data","data_prepd.csv"))
