# Biomass of the tree stem (in tons) = (cubic foot volume * wood density) / 2000
# BA = basal area
# CF4 = Cubic form factor
# TERM = Equation coefficient
# CV4: Cubic foot volume above stump with four inch top
# CVTS: Cubic foot volume including top and stump

biomass <- function(species, DBH_cm, HT_m) {
  DBH_in = DBH_cm / 2.54
  HT_ft = HT_m / 0.3048
  
  BA = DBH_in ** 2 * 0.005454154
  TERM = (1.033 * (1.0 + 1.382937 * exp(-4.015292 * (DBH_in / 10.0)))) * (BA + 0.087266) - 0.174533
  
  if (is.na(DBH_cm) | is.na(HT_m) | is.na(species)) {
    
    TotalBiomass = NA
    
  } else if (species == 'ARCMAN') {
    
    TotalBiomass = NA

  } else if (species == 'HETARB') {
    
    TotalBiomass = NA
    
  } else if (species == 'MYRCAL') {
    
    TotalBiomass = NA
    
  } else {
    
    if (species == 'ABICON' | species == 'ABIGRA') {
      # --------------------------- Volume Equation 23 ---------------------------
      
      if (DBH_in >= 6) {
        
        CF4 = 0.299039 + 1.91272 * (1 / HT_ft) + 0.0000367217 * (HT_ft ** 2 / DBH_in)
        if (CF4 < 0.3) {
          CF4 = 0.3
        }
        if (CF4 > 0.4) {
          CF4 = 0.4
        }
        
        CV4 = CF4 * BA * HT_ft
        CVTS = (CV4 * TERM) / (BA - 0.087266)
        
      } else {
        
        TMP_DBH = 6
        BA_TMP = TMP_DBH ** 2 * 0.005454154
        
        CF4_TMP = 0.299039 + 1.91272 * (1 / HT_ft) + 0.0000367217 * (HT_ft ** 2 / TMP_DBH)
        if (CF4_TMP < 0.3) {CF4_TMP = 0.3}
        if (CF4_TMP > 0.4) {CF4_TMP = 0.4}
        
        CV4_TMP = CF4_TMP * BA_TMP * HT_ft
        TARIF_TMP = (CV4_TMP * 0.912733) / (BA_TMP - 0.087266)
        
        if (TARIF_TMP <= 0) {TARIF_TMP = 0.01}
        TARIF = TARIF_TMP * (0.5 * (TMP_DBH - DBH_in) ** 2 + (1 + 0.063 * (TMP_DBH - DBH_in) ** 2))
        if (TARIF <= 0) {TARIF = 0.01}
        
        CVTS = TARIF * TERM
        
      }
      
      
      # -------------------------------- Biomass ---------------------------------
      
      if (species == 'ABICON') {
        WoodDensity = 23.09
        
        # Bark biomass equation 1 (kg)
        BB = exp(2.1069 + 2.7271 * log(DBH_cm)) / 1000
        
      } else if (species == 'ABIGRA') {
        WoodDensity = 21.84
        
        # Bark biomass equation 2 (kg)
        BB = 0.6 + 16.4 * (DBH_cm / 100) ** 2 * HT_m
        
      }
      
      # Live branch biomass equation 1 (kg)
      BLB = 13.0 + 12.4 * (DBH_cm / 100) ** 2 * HT_m
      
    } else if (species == 'ABIMAG') {
      # --------------------------- Volume Equation 18 ---------------------------
      
      if (DBH_in >= 6) {
        
        CF4 = 0.231237 + 0.028176 * (HT_ft / DBH_in)
        if (CF4 < 0.3) {CF4 = 0.3}
        if (CF4 > 0.4) {CF4 = 0.4}
        
        CV4 = CF4 * BA * HT_ft
        CVTS = (CV4 * TERM) / (BA - 0.087266)
        
      } else {
        
        TMP_DBH = 6
        BA_TMP = TMP_DBH ** 2 * 0.005454154
        
        CF4_TMP = 0.231237 + 0.028176 * (HT_ft / TMP_DBH)
        if (CF4_TMP < 0.3) {CF4_TMP = 0.3}
        if (CF4_TMP > 0.4) {CF4_TMP = 0.4}
        
        CV4_TMP = CF4_TMP * BA_TMP * HT_ft
        
        TARIF_TMP = (CV4_TMP * 0.912733) / (BA_TMP - 0.087266)
        if (TARIF_TMP <= 0) {TARIF_TMP = 0.01}
        
        TARIF = TARIF_TMP * (0.5 * (TMP_DBH - DBH_in) ** 2 + (1 + 0.063 * (TMP_DBH - DBH_in) ** 2))
        if (TARIF <= 0) {TARIF = 0.01}
        
        CVTS = TARIF * TERM
        
      }
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 22.46
      
      # Bark biomass equation 4 (kg)
      BB = exp(1.47146 + 2.8421 * log(DBH_cm)) / 1000
      
      # Live branch biomass equation 3 (kg)
      BLB = exp(-4.1817 + 2.3324 * log(DBH_cm))
      
    } else if (species == 'AESCAL') {
      # --------------------------- Volume Equation 43 ---------------------------
      
      CVTS = 0.0065261029 * (DBH_in ** 2.31958) * (HT_ft ** 0.62528)
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 20.59
      
      # Bark biomass equation missing
      BB = 0
      
      # Live branch biomass equation missing
      BLB = 0
      
    } else if (species == 'ARBMEN') {
      # --------------------------- Volume Equation 40 ---------------------------
      
      if (HT_ft > 120)
        HT_ft = 120
      
      CVTS = 0.0067322665 * (DBH_in ** 1.96628) * (HT_ft ** 0.83458)
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 36.19
      
      # Bark biomass equation missing
      BB = 0
      
      # Live branch biomass equation missing
      BLB = 0
      
    } else if (species == 'CALDEC' | species == 'CALLIT') {
      
      # --------------------------- Volume Equation 19 ---------------------------
      
      if (DBH_in >= 6) {
        
        CF4 = 0.225786 + 4.44236 * (1 / HT_ft)
        if (CF4 < 0.27) {CF4 = 0.27}
        
        CV4 = CF4 * BA * HT_ft
        CVTS = (CV4 * TERM) / (BA - 0.087266)
        
      } else {
        
        TMP_DBH = 6
        BA_TMP = TMP_DBH ** 2 * 0.005454154
        
        CF4_TMP = 0.225786 + 4.44236 * (1 / HT_ft)
        if (CF4_TMP < 0.27) {CF4_TMP = 0.27}
        
        CV4_TMP = CF4_TMP * BA_TMP * HT_ft
        
        TARIF_TMP = (CV4_TMP * 0.912733) / (BA_TMP - 0.087266)
        if (TARIF_TMP <= 0) {TARIF_TMP = 0.01}
        
        TARIF = TARIF_TMP * (0.5 * (TMP_DBH - DBH_in) ** 2 + (1 + 0.063 * (TMP_DBH - DBH_in) ** 2))
        if (TARIF <= 0) {TARIF = 0.01}
        
        CVTS = TARIF * TERM
        
      }
      
      
      # -------------------------------- Biomass ---------------------------------
      
      if (species == 'CALDEC') {
        WoodDensity = 21.84
        
        # Bark biomass equation 12 (kg)
        BB = exp(-13.3146 + 2.8594 * log(DBH_cm)) * 1000
        
      } else if (species == 'CALLIT') {
        WoodDensity = 25.58
        
        # Bark biomass equation 13 (kg)
        BB = 0.336 + 0.00058 * DBH_cm ** 2 * HT_m
        
      }
      
      # Live branch biomass equation 10 (kg)
      BLB = 0.199 + 0.00381 * DBH_cm ** 2 * HT_m
      
    } else if (species == 'CHRCHR') {
      # --------------------------- Volume Equation 32 ---------------------------
      
      CVTS = 0.0120372263 * DBH_in ** 2.02232 * HT_ft ** 0.68638
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 26.21
      
      # Bark biomass equation missing
      BB = 0
      
      # Live branch biomass equation missing
      BLB = 0
      
    } else if (species == 'CONIFE') {
      # --------------------------- Volume Equation 17 ---------------------------
      
      CVTS = 0.001106485 * DBH_in ** 1.8140497 * HT_ft ** 1.2744923
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 25.58
      
      # Bark biomass equation 21 (kg)
      BB = 0.9 + 27.4 * (DBH_cm / 100) ** 2 * HT_m
      
      # Live branch biomass equation 17 (kg)
      BLB = exp(-5.2581 + 2.6045 * log(DBH_cm))
      
    } else if (species == 'EUCALY') {
      # --------------------------- Volume Equation 31 ---------------------------
      
      CVTS = 0.0016144 * DBH_in ** 2 * HT_ft
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 32.45
      
      # Bark biomass equation missing
      BB = 0
      
      # Live branch biomass equation 28 (kg)
      BLB = exp(3.0136553 + 2.4839 * log(DBH_cm)) * (1 - 1 / (1.6013 + 0.1060 * DBH_cm **
                                                                1.309)) / 1000
      
    } else if (species == 'NOTDEN') {
      # --------------------------- Volume Equation 34 ---------------------------
      
      if (HT_ft > 120)
        HT_ft = 120
      
      CVTS = 0.0058870024 * DBH_in ** 1.94165 * HT_ft ** 0.86562
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 36.19
      
      # Bark biomass equation missing
      BB = 0
      
      # Live branch biomass equation missing
      BLB = 0
      
    } else if (species == 'PINJEF' | species == 'PINPON') {
      # --------------------------- Volume Equation 5 ---------------------------
      
      if (DBH_in >= 6) {
        
        CF4 = 0.402060 - 0.899914 * (1 / DBH_in)
        if (CF4 < 0.3) {CF4 = 0.3}
        if (CF4 > 0.4) {CF4 = 0.4}
        
        CV4 = CF4 * BA * HT_ft
        CVTS = (CV4 * TERM) / (BA - 0.087266)
        
      } else {
        
        TMP_DBH = 6
        BA_TMP = TMP_DBH ** 2 * 0.005454154
        
        CF4_TMP = 0.402060 - 0.899914 * (1 / TMP_DBH)
        if (CF4_TMP < 0.3) {CF4_TMP = 0.3}
        if (CF4_TMP > 0.4) {CF4_TMP = 0.4}
        
        CV4_TMP = CF4_TMP * BA_TMP * HT_ft
        
        TARIF_TMP = (CV4_TMP * 0.912733) / (BA_TMP - 0.087266)
        if (TARIF_TMP <= 0) {TARIF_TMP = 0.01}
        
        TARIF = TARIF_TMP * (0.5 * (TMP_DBH - DBH_in) ** 2 + (1 + 0.063 * (TMP_DBH - DBH_in) ** 2))
        if (TARIF <= 0) {TARIF = 0.01}
        
        CVTS = TARIF * TERM
        
      }
      
      # -------------------------------- Biomass ---------------------------------
      
      if (species == 'PINJEF') {
        WoodDensity = 23.09
        
      } else if (species == 'PINPON') {
        WoodDensity = 23.71
        
      }
      
      # Bark biomass equation 9 (kg)
      BB = exp(-3.6263 + 1.34077 * log(DBH_cm) + 0.8567 * log(HT_m))
      
      # Live branch biomass equation 7 (kg)
      BLB = exp(-4.1068 + 1.5177 * log(DBH_cm) + 1.0424 * log(HT_m))
      
    } else if (species == 'PINLAM') {
      # --------------------------- Volume Equation 20 ---------------------------
      
      if (DBH_in >= 6) {
        
        CF4 = 0.358550 - 0.488134 * (1 / DBH_in)
        if (CF4 < 0.3) {CF4 = 0.3}
        if (CF4 > 0.4) {CF4 = 0.4}
        
        CV4 = CF4 * BA * HT_ft
        CVTS = (CV4 * TERM) / (BA - 0.087266)
        
      } else {
        
        TMP_DBH = 6
        BA_TMP = TMP_DBH ** 2 * 0.005454154
        
        CF4_TMP = 0.358550 - 0.488134 * (1 / TMP_DBH)
        if (CF4_TMP < 0.3) {CF4_TMP = 0.3}
        if (CF4_TMP > 0.4) {CF4_TMP = 0.4}
        
        CV4_TMP = CF4_TMP * BA_TMP * HT_ft
        
        TARIF_TMP = (CV4_TMP * 0.912733) / (BA_TMP - 0.087266)
        if (TARIF_TMP <= 0) {TARIF_TMP = 0.01}
        
        TARIF = TARIF_TMP * (0.5 * (TMP_DBH - DBH_in) ** 2 + (1 + 0.063 * (TMP_DBH - DBH_in) ** 2))
        if (TARIF <= 0) {TARIF = 0.01}
        
        CVTS = TARIF * TERM
        
      }
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 21.22
      
      # Bark biomass equation 10 (kg)
      BB = exp(2.183174 + 2.6610 * log(DBH_cm)) / 1000
      
      # Live branch biomass equation 8 (kg)
      BLB = exp(-7.637 + 3.3648 * log(DBH_cm))
      
    } else if (species == 'PINMUR') {
      # --------------------------- Volume Equation 16 ---------------------------
      
      if (DBH_in >= 6) {
        
        CF4 = 0.422709 - 0.0000612236 * (HT_ft ** 2 / DBH_in)
        if (CF4 < 0.3) {CF4 = 0.3}
        if (CF4 > 0.4) {CF4 = 0.4}
        
        CV4 = CF4 * BA * HT_ft
        CVTS = (CV4 * TERM) / (BA - 0.087266)
        
      } else {
        
        TMP_DBH = 6
        BA_TMP = TMP_DBH ** 2 * 0.005454154
        
        CF4_TMP = 0.422709 - 0.0000612236 * (HT_ft ** 2 / TMP_DBH)
        if (CF4_TMP < 0.3) {CF4_TMP = 0.3}
        if (CF4_TMP > 0.4) {CF4_TMP = 0.4}
        
        CV4_TMP = CF4_TMP * BA_TMP * HT_ft
        
        TARIF_TMP = (CV4_TMP * 0.912733) / (BA_TMP - 0.087266)
        if (TARIF_TMP <= 0) {TARIF_TMP = 0.01}
        
        TARIF = TARIF_TMP * (0.5 * (TMP_DBH - DBH_in) ** 2 + (1 + 0.063 * (TMP_DBH - DBH_in) ** 2))
        if (TARIF <= 0) {TARIF = 0.01}
        
        CVTS = TARIF * TERM
        
      }
      
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 28.08
      
      # Bark biomass equation 14 (kg)
      BB = 3.2 + 9.1 * (DBH_cm / 100) ** 2 * HT_m
      
      # Live branch biomass equation 11 (kg)
      BLB = 7.8 + 12.3 * (DBH_cm / 100) ** 2 * HT_m
      
    } else if (species == 'PSEMEN') {
      # --------------------------- Volume Equation 3 ----------------------------
      
      if (DBH_in >= 6) {
        
        CF4 = 0.248569 + 0.0253524 * (HT_ft / DBH_in) - 0.0000560175 * (HT_ft ** 2 / DBH_in)
        if (CF4 < 0.3) {CF4 = 0.3}
        if (CF4 > 0.4) {CF4 = 0.4}
        
        CV4 = CF4 * BA * HT_ft
        CVTS = (CV4 * TERM) / (BA - 0.087266)
        
      } else {
        
        TMP_DBH = 6
        BA_TMP = TMP_DBH ** 2 * 0.005454154
        
        CF4_TMP = 0.248569 + 0.0253524 * (HT_ft / TMP_DBH) - 0.0000560175 * (HT_ft ** 2 / TMP_DBH)
        if (CF4_TMP < 0.3) {CF4_TMP = 0.3}
        if (CF4_TMP > 0.4) {CF4_TMP = 0.4}
        
        CV4_TMP = CF4_TMP * BA_TMP * HT_ft
        
        TARIF_TMP = (CV4_TMP * 0.912733) / (BA_TMP - 0.087266)
        if (TARIF_TMP <= 0) {TARIF_TMP = 0.01}
        
        TARIF = TARIF_TMP * (0.5 * (TMP_DBH - DBH_in) ** 2 + (1 + 0.063 * (TMP_DBH - DBH_in) ** 2))
        if (TARIF <= 0) {TARIF = 0.01}
        
        CVTS = TARIF * TERM
        
      }
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 28.08
      
      # Bark biomass equation 8 (kg)
      BB = exp(-4.3103 + 2.43 * log(DBH_cm))
      
      # Live branch biomass equation 6 (kg)
      BLB = exp(-3.6941 + 2.1382 * log(DBH_cm))
      
    } else if (species == 'QUEAGR') {
      # --------------------------- Volume Equation 43 ----------------------------
      
      CVTS = 0.0065261029 * DBH_in ** 2.31958 * HT_ft ** 0.62528
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 36.82
      
      # Bark biomass equation missing
      BB = 0
      
      # Live branch biomass equation missing
      BLB = 0
      
    } else if (species == 'QUEDOU') {
      # --------------------------- Volume Equation 39 ----------------------------
      
      CVTS = 0.0125103008 * DBH_in ** 2.33089 * HT_ft ** 0.46100
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 36.82
      
      # Bark biomass equation missing
      BB = 0
      
      # Live branch biomass equation missing
      BLB = 0
      
    } else if (species == 'QUEGAR' | species == 'QUELOB') {
      # --------------------------- Volume Equation 41 ----------------------------
      
      CVTS = 0.0072695058 * DBH_in ** 2.14321 * HT_ft ** 0.74220
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 39.94
      
      # Bark biomass equation missing
      BB = 0
      
      # Live branch biomass equation missing
      BLB = 0
      
    } else if (species == 'QUEKEL') {
      # --------------------------- Volume Equation 38 ----------------------------
      
      CVTS = 0.0070538108 * DBH_in ** 1.97437 * HT_ft ** 0.85034
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 31.82
      
      # Bark biomass equation missing
      BB = 0
      
      # Live branch biomass equation missing
      BLB = 0
      
    } else if (species == 'SEQSEM') {
      # --------------------------- Volume Equation 24 ----------------------------
      
      CVTS = exp(-6.2597 + 1.9967 * log(DBH_in) + 0.9642 * log(HT_ft))
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 22.46
      
      # Bark biomass equation 17 if DBH > 39.37 in, equation 13 if less
      
      if (DBH_in > 39.37) {
        BB = exp(7.189689 + 1.5837 * log(DBH_cm)) / 1000
      } else {
        BB = 0.336 + 0.00058 * DBH_cm ** 2 * HT_m
      }
      
      # Live branch biomass equation 10
      BLB = 0.199 + 0.00381 * DBH_cm ** 2 * HT_m
      
    } else if (species == 'TSUHET') {
      # --------------------------- Volume Equation 6 ---------------------------
      
      CVTS = (-2.72170 + 2.00857 * log(DBH_in) + 1.08620 * log(HT_ft) - 0.00568 * DBH_in)
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 26.21
      
      # Bark biomass equation 15 (kg)
      BB = exp(-4.371 + 2.259 * log(DBH_cm))
      
      # Live branch biomass equation 12 (kg)
      BLB = exp(-4.570 + 2.271 * log(DBH_cm))
      
    } else if (species == 'UMBCAL') {
      # --------------------------- Volume Equation 33 ----------------------------
      
      CVTS = 0.0057821322 * DBH_in ** 1.94553 * HT_ft ** 0.88389
      
      # -------------------------------- Biomass ---------------------------------
      
      WoodDensity = 31.82
      
      # Bark biomass equation missing
      BB = 0
      
      # Live branch biomass equation missing
      BLB = 0
      
    } 
    
    # Stem biomass
    BSTEM = ((CVTS * WoodDensity) / 2000) * 0.90718474 # Mg
    
    # Total Biomass (Mg)
    TotalBiomass = BSTEM + (BB / 1000) + (BLB / 1000)
    
  }
  
}