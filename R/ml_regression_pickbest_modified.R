# PURPOSE:
# Performs a Random Forest, Support Vector Machine and ordinary least-squares regression analysis and a 
# 5-fold cross validation with 10 repeasts used to calculate RMSE and r-squared.

# Methods generally follow:
# de Almeida, C. T., Galv?o, L. S., Ometto, J. P. H. B., Jacon, A. D., de Souza Pereira, F. R., Sato, L. Y., ... & Longo, M. 
# (2019). Combining LiDAR and hyperspectral data for aboveground biomass modeling in the Brazilian Amazon using different 
# regression algorithms. Remote Sensing of Environment, 232, 111323.


# AUTHOR:
# Dr. Matthew Clark
# Dept of Geography and Global Studies
# Sonoma State University
# 1801 E. Cotati Ave
# Rohnert Park, CA 94928 USA
# matthew.clark@sonoma.edu
# +1 707-664-2558

# Version: September 17, 2019; R 3.5.3
# Update: November 26, 2019; R 3.5.3; Changed predictor inputs for lidR standard canopy metrics

# load necessary libraries
library(doParallel) # used for parallel processing
library(caret) # used for SVM tuning (takes advantage of doParallel)


# remove any existing objects
rm(list=ls(all=TRUE))


# *******************************************************************************************************
# set these parameters before running

# set working directory
workspace<-"D:/clark/manuscript_publications/fop_biomass_machine_learning/data"
setwd(workspace)

# files to process
files<-c(
        "fop_QS_adjusted_plots_biomass_den0_pulse_stdmetrics_LAD_z.csv", # full pulse density
        "fop_QS_adjusted_plots_biomass_den1_pulse_stdmetrics_LAD_z.csv",
        "fop_QS_adjusted_plots_biomass_den2_pulse_stdmetrics_LAD_z.csv",
        "fop_QS_adjusted_plots_biomass_den5_pulse_stdmetrics_LAD_z.csv",
        "fop_QS_adjusted_plots_biomass_den10_pulse_stdmetrics_LAD_z.csv",
        "fop_QS_adjusted_plots_biomass_den15_pulse_stdmetrics_LAD_z.csv",
        "fop_QS_adjusted_plots_biomass_den20_pulse_stdmetrics_LAD_z.csv",
        "fop_QS_adjusted_plots_biomass_den25_pulse_stdmetrics_LAD_z.csv",
        "fop_QS_adjusted_plots_biomass_den30_pulse_stdmetrics_LAD_z.csv"
)

# Output base names
basefiles <- c(
        "mlregress_lidar_den0_pulse_K10",
        "mlregress_lidar_den1_pulse_K10",
        "mlregress_lidar_den2_pulse_K10",
        "mlregress_lidar_den5_pulse_K10",
        "mlregress_lidar_den10_pulse_K10",
        "mlregress_lidar_den15_pulse_K10",
        "mlregress_lidar_den20_pulse_K10",
        "mlregress_lidar_den25_pulse_K10",
        "mlregress_lidar_den30_pulse_K10"
)

# response variable
responses <- c(
        "LiveBiomassMgHa",
        "LiveStandingDeadBiomassMgHa"
#        "LiveAllDeadBiomassMgHa"
)


# LasTools predictors
#predictors<-c("min","max","avg","std","ske","kur","p01","p05","p10","p25","p50","p75","p90","p95","p99","cov","dns")

# lidR standard metrics predictors
predictors<-c("zmax","zmean","zsd","zskew","zkurt","zentropy","pzabovezmean",
              "zq5","zq10","zq15","zq20","zq25","zq30","zq35","zq40","zq45","zq50","zq55","zq60","zq65","zq70","zq75","zq80","zq85","zq90","zq95",
              "zpcum1","zpcum2","zpcum3","zpcum4","zpcum5","zpcum6","zpcum7","zpcum8","zpcum9","LADCV")
                # not used: "pzabove2",

                # zmax : maximum height
                # zmean : mean height
                # zsd : standard deviation of height distribution
                # zskew : skewness of height distribution
                # zkurt : kurtosis of height distribution
                # zentropy : entropy of height distribution (see function entropy)
                # pzabovezmean : percentage of returns above zmean
                # zqx : x percentile (quantile) of height distribution
                # zpcumx : cumulative percentage of return in the x layer according to Wood et al. 2008
                # Woods, M., Lim, K., & Treitz, P. (2008). Predicting forest stand variables from LIDAR data in the Great Lakes St. Lawrence Forest of Ontario. The Forestry Chronicle, 84(6), 827-839.

# set seed
set.seed(111)

# number of folds
numfolds <- 10

# repeats
repeats <- 10
repeatsfinal <- 100

# ***********************************************
for (r in c(1:length(responses))) {
        for (i in c(1:length(files))) {
                
        # read in CSV file
        data <- read.csv(files[i], header = TRUE)
        
        # base file name
        response<-responses[r]
        base <- paste(basefiles[i], "_", response, sep = '')
        
        # output R workspace
        outRobj <- paste(base, "_pickbest.RData", sep = '')
        
        
        # Start log file
        logfile <- paste(base, "_pickbest_log.txt", sep = "")
        timestamp<-format(Sys.time(), "%Y%m%d_%H%M")	
        write(paste(
                "--- ",
                format(Sys.time(), "%Y-%m-%d %H:%M"),
                "  Begin",
                sep =
                        ''
        ),
        file = logfile,
        append = FALSE)	# erase prior file, if any
        write(paste("----------", sep = ''),
              file = logfile,
              append = TRUE)
        write(
                paste("Working directory: ", workspace, sep = ''),
                file = logfile,
                append = TRUE
        )
        write(
                paste("Input training data: ", files[i], sep = ''),
                file = logfile,
                append = TRUE
        )
        write(
                paste("R training workspace output file: ", outRobj, sep = ''),
                file = logfile,
                append = TRUE
        )
        write(
                paste("Response: ", response, sep = ''),
                file = logfile,
                append = TRUE
        )
        write("", file = paste(logfile), append = TRUE)
        
        # Perform RFE
        
        prednum <- length(predictors)
        
        # Register do parallel (uses all detected cores except 2)
        cl <- makeCluster(detectCores() - 2, outfile = "c:/temp/Log.txt")
        registerDoParallel(cl)
        
        # training control for internal model parameters
        ctrl <- trainControl(method = "cv",
                             number = numfolds,
                             allowParallel = TRUE)
        
        # Random Forests RFE
        rfProfile <- rfe(
                data[, predictors],
                data[, response],
                sizes = c(2:prednum),
                rfeControl = rfeControl(
                        functions = rfFuncs,
                        method = "repeatedcv",
                        number = numfolds,
                        repeats = repeats,
                        allowParallel = TRUE
                ),
                #preProcess = c("center", "scale"),
                ntree = 1000,
                tuneLength = 10,
                metric = "RMSE",
                trControl = ctrl
        )
        
        # Support Vector Regression RFE
        svmProfile <- rfe(
                data[, predictors],
                data[, response],
                sizes = c(2:prednum),
                rfeControl = rfeControl(
                        functions = caretFuncs,
                        method = "repeatedcv",
                        number = numfolds,
                        repeats = repeats,
                        allowParallel = TRUE
                ),
                method = "svmRadial",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                metric = "RMSE",
                trControl = ctrl
        )
        
        # Ordinary least-squares regression RFE
        lmProfile <- rfe(
                data[, predictors],
                data[, response],
                sizes = c(2:prednum),
                rfeControl = rfeControl(
                        functions = lmFuncs,
                        method = "repeatedcv",
                        number = numfolds,
                        repeats = repeats,
                        allowParallel = TRUE
                ),
                metric = "RMSE",
                trControl = ctrl
        )
        
        # pick best variables
        rfSize <-
                pickSizeBest(
                        rfProfile$results,
                        metric = "RMSE",
                        maximize = FALSE
                )
        svmSize <-
                pickSizeBest(
                        svmProfile$results,
                        metric = "RMSE",
                        maximize = FALSE
                )
        lmSize <-
                pickSizeBest(
                        lmProfile$results,
                        metric = "RMSE",
                        maximize = FALSE
                )
        
        rfVariables <- rfProfile$optVariables[1:rfSize]
        svmVariables <- svmProfile$optVariables[1:svmSize]
        lmVariables <- lmProfile$optVariables[1:lmSize]
        
        # train final models
        
        # Training control for model parameters
        ctrl <- trainControl(
                method = "repeatedcv",
                number = numfolds,
                repeats = repeatsfinal,
                allowParallel = TRUE
        )
        
        # Random Forests
        rfTrain <- train(
                data[, rfVariables],
                data[, response],
                method = "rf",
                #preProcess = c("center", "scale"),
                trControl = ctrl,
                ntree = 1000,
                tuneLength = 10,
                metric = "RMSE",
                allowParallel = TRUE
        )
        
        mtry <- rfTrain$bestTune[1]
        rfStats <- rfTrain$results[which(rfTrain$results$mtry == as.numeric(mtry)), ]
        rfRMSECV <- rfStats$RMSE
        rfRsquaredCV <- rfStats$Rsquared
        
        # SVR
        svmTrain <- train(
                data[, svmVariables],
                data[, response],
                method = "svmRadial",
                preProcess = c("center", "scale"),
                trControl = ctrl,
                tuneLength = 10,
                metric = "RMSE",
                allowParallel = TRUE
        )
        sigma <- svmTrain$bestTune[1]
        cost <- svmTrain$bestTune[2]
        svmStats <- svmTrain$results[which(svmTrain$results$C == as.numeric(cost)), ]
        svmRMSECV <- svmStats$RMSE
        svmRsquaredCV <- svmStats$Rsquared
        
        # OLS regression
        lmTrain <- train(data[, lmVariables], data[, response],
                         method = "lm",
                         #preProcess = c("center", "scale"),
                         trControl = ctrl)
        lmStats <- lmTrain$results
        lmRMSECV = lmStats$RMSE
        lmRsquaredCV = lmStats$Rsquared
        
        # stop parallel processing cluster
        stopCluster(cl)
        
        # Write out results to log file
        write(paste("---------------------"),
              file = logfile,
              append = TRUE)
        write(paste("Random Forests"),
              file = logfile,
              append = TRUE)
        write(paste("RMSE-CV: ", rfRMSECV, sep = ''),
              file = logfile,
              append = TRUE)
        write(paste("Rsquared-CV: ", rfRsquaredCV, sep = ''),
                file = logfile,
                append = TRUE)
        write(paste("mtry: ", mtry, sep = ''),
              file = logfile,
              append = TRUE
        )
        write("", file = paste(logfile), append = TRUE)
        write(paste("---------------------"),
              file = logfile,
              append = TRUE)
        write(paste("Support Vector Regression"),
              file = logfile,
              append = TRUE)
        write(paste("RMSE-CV: ", svmRMSECV, sep = ''),
              file = logfile,
              append = TRUE)
        write(paste("Rsquared-CV: ", svmRsquaredCV, sep = ''),
                file = logfile,
                append = TRUE)
        write(paste("Cost: ", cost, sep = ''),
                file = logfile,
                append = TRUE)
        write(paste("Sigma: ", sigma, sep = ''),
                file = logfile,
                append = TRUE
        )
        write("", file = paste(logfile), append = TRUE)
        write(paste("---------------------"),
              file = logfile,
              append = TRUE)
        write(paste("OLS Regression"),
              file = logfile,
              append = TRUE)
        write(paste("RMSE-CV: ", lmRMSECV, sep = ''),
              file = logfile,
              append = TRUE)
        write(
                paste("Rsquared-CV: ", lmRsquaredCV, sep = ''),
                file = logfile,
                append = TRUE
        )
        write("", file = paste(logfile), append = TRUE)
        
        # Write out statistics files
        statsfile <- paste(base, "_pickbest_rf_stats.csv", sep = "")
        write.csv(rfStats, file=statsfile,row.names=FALSE)
        statsfile <- paste(base, "_pickbest_svm_stats.csv", sep = "")
        write.csv(svmStats, file=statsfile,row.names=FALSE)
        statsfile <- paste(base, "_pickbest_lm_stats.csv", sep = "")
        write.csv(lmStats, file=statsfile,row.names=FALSE)
        
        # Save RF workspace and close
        write("", file = logfile, append = TRUE)
        write(
                paste(
                        "--- ",
                        format(Sys.time(), "%Y-%m-%d %H:%M"),
                        "  Writing R workspace",
                        sep = ''
                ),
                file = logfile,
                append = TRUE
        )
        save.image(file = outRobj) # Saves the entire R workspace, which includes all variables
        write(
                paste(
                        "--- ",
                        format(Sys.time(), "%Y-%m-%d %H:%M"),
                        "  Finished",
                        sep =
                                ''
                ),
                file = logfile,
                append = TRUE
        )
        rm(logfile)
        
        } # end files loop
} # end responses loop

#
#