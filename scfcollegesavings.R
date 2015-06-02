#### Set up the file ####
      
      #set working directory
      setwd( "I:/User/Williams/Publications/529Data/rstuff" )

      #Load external packages
      library(mitools)	# allows analysis of multiply-imputed survey data
      library(survey)		# load survey package (analyzes complex design surveys)
      library(downloader)	# downloads and then runs the source() function on scripts from github
      library(foreign) 	# load foreign package (converts data files into R)
      library(Hmisc) 		# load Hmisc package (loads a simple wtd.quantile function)
      library(dplyr)    #always needed
      
      # turn off scientific notation in most output
      options( scipen = 20 )
      
      # load two svyttest functions (one to conduct a df-adjusted t-test and one to conduct a multiply-imputed t-test)
      source("scf.survey.R")
      
      # load the 201 survey of consumer finances into memory
      load( "scf2013.rda" )
      
      #create variable with list of implicates
      implicates <- c("imp1","imp2","imp3","imp4","imp5")
      
#### Delete unneeded variables ####
      
      #list of variables to keep.
      vars.to.keep <- c( 'y1' , 'yy1' , 'wgt' , 'one' , 'networth', 'income', 'five',
                         'x3727', 'x3728','x6755','x3730', 'x3736', 'x3742', 'x3748','x3754','x3760',
                         'x3732','x3738','x3744','x3750','x3756','x3762')
      
      #keep only the variables we care about
      imp1 <- imp1[ , vars.to.keep ]
      imp2 <- imp2[ , vars.to.keep ]
      imp3 <- imp3[ , vars.to.keep ]
      imp4 <- imp4[ , vars.to.keep ]
      imp5 <- imp5[ , vars.to.keep ]
      
      #rename coded variables
      for (implicate in implicates) {
            
            temp <- get(implicate)
            
            names(temp)[8:22] <- c(
                                    "haveaccts",
                                    "numb.accts",
                                    "original.numb.accts",
                                    "acct.1.balance",
                                    "acct.2.balance",
                                    "acct.3.balance",
                                    "acct.4.balance",
                                    "acct.5.balance",
                                    "acct.6.balance",
                                    "acct.1.type",
                                    "acct.2.type",
                                    "acct.3.type",
                                    "acct.4.type",
                                    "acct.5.type",
                                    "acct.6.type"
                                    
            )
            
            assign(implicate, temp)
            
            remove(temp)
      }
      
      # clear up RAM
      gc()

#### Double check everything lines up with FRB data ####

      #generate mean income for whole sample (compare to FRB numbers--should equal 86.6k)
      meanincomes <- mean(
            c(
                  weighted.mean( imp1$income , imp1$wgt ) ,
                  weighted.mean( imp2$income , imp2$wgt ) ,
                  weighted.mean( imp3$income , imp3$wgt ) ,
                  weighted.mean( imp4$income , imp4$wgt ) ,
                  weighted.mean( imp5$income , imp5$wgt )
            )
      )
      meanincomes # <- should equal 86.6k
      
      #generate median income for whole sample (compare to FRB numbers--should equal 46.7k)
      medianincomes <- mean(
            c(
                  wtd.quantile( imp1$income , imp1$wgt , 0.5 ) ,
                  wtd.quantile( imp2$income , imp2$wgt , 0.5 ) ,
                  wtd.quantile( imp3$income , imp3$wgt , 0.5 ) ,
                  wtd.quantile( imp4$income , imp4$wgt , 0.5 ) ,
                  wtd.quantile( imp5$income , imp5$wgt , 0.5 )            
            )
      )
      
      medianincomes # <- should equal 46.7k
      
      #### Try running it using anthony damico's code #### 
      
      # # construct an imputed replicate-weighted survey design object
      # # build a new replicate-weighted survey design object,
      # # but unlike most replicate-weighted designs, this object includes the
      # # five multiply-imputed data tables - imp1 through imp5
      scf.design <- 
            svrepdesign( 
                  
                  # use the main weight within each of the imp# objects
                  weights = ~wgt , 
                  
                  # use the 999 replicate weights stored in the separate replicate weights file
                  repweights = rw[ , -1 ] , 
                  
                  # read the data directly from the five implicates
                  data = imputationList( list( imp1 , imp2 , imp3 , imp4 , imp5 ) ) , 
                  
                  scale = 1 ,
                  
                  rscales = rep( 1 / 998 , 999 ) ,
                  
                  # use the mean of the replicate statistics as the center
                  # when calculating the variance, as opposed to the main weight's statistic
                  mse = TRUE ,
                  
                  type = "other" ,
                  
                  combined.weights = TRUE
            )
      
      # Check mean income
      ( mean_income <- scf.MIcombine( with( scf.design , svymean( ~income ) ) ) )  # <- should equal 86.6k
      
      # Check median income 
      ( median_income <- scf.MIcombine( with( scf.design , svyquantile( ~income, quantiles = .5)))) # <- should equal 46.7k
      
#### Group each record in to an income quantile ####   

      #calculate income quantiles
      for (quantile in c(.2,.4,.6,.8,.9)) {
            temp <- scf.MIcombine( with( scf.design , svyquantile( ~income, quantiles = quantile)))
            temp <- coef(temp)
            
            if (!exists("quantile_table")) {
                  quantile_table <- data.frame(quantile = c(quantile), income = temp)
            } else {
                  quantile_table <- rbind(quantile_table, c(quantile, temp))
            }
            remove(temp)
      }
      
      # Categorize each record as belonging to an income group.
      for (implicate in implicates) {
            
            #grab implicate
            temp <- get(implicate)
            
            #assign incomes below .1 percentile to group 1
            temp[temp$income < quantile_table[1,2],"inc.group"] <- 1
            
            #assign incomes at each threshold to respective groups
            for (i in 2:5) {
                  temp[temp$income < quantile_table[i,2] & temp$income >= quantile_table[(i-1),2] ,"inc.group"] <- i
            }
            
            #assign incomes above .9 percentile to group 6
            temp[temp$income >= quantile_table[5,2],"inc.group"] <- 6
            
            #assign to data set as a whole
            assign(implicate,temp)
            
            #remove temporary data set.
            remove(temp)
      }
                  
 #### Calculate value of 529 and Coverdell Accounts ####
      
      #run the calculation
      for (implicate in implicates) {
            
            temp <- get(implicate)
            
            temp <- mutate(temp, acct.1.edu = ifelse(acct.1.type == 3 | acct.1.type == 2, acct.1.balance, 0)) %>%
                  mutate(acct.2.edu = ifelse(acct.2.type == 3 | acct.2.type == 2, acct.2.balance, 0)) %>%
                  mutate(acct.3.edu = ifelse(acct.3.type == 3 | acct.3.type == 2, acct.3.balance, 0)) %>%
                  mutate(acct.4.edu = ifelse(acct.4.type == 3 | acct.4.type == 2, acct.4.balance, 0)) %>%
                  mutate(acct.5.edu = ifelse(acct.5.type == 3 | acct.5.type == 2, acct.5.balance, 0)) %>%
                  mutate(acct.6.edu = ifelse(acct.6.type == 3 | acct.6.type == 2, acct.6.balance, 0)) %>%
                  mutate(edu_savings = acct.1.edu + acct.2.edu + acct.3.edu + acct.4.edu + acct.5.edu + acct.6.edu)
            
            assign(implicate, temp)
            
      }
      
      #create variable that is equal to the "five" column if individual has edu savings
      for (implicate in implicates) {
            temp <- get(implicate)
            temp <- mutate(temp, has.edu = ifelse(edu_savings>0, five, 0))
            assign(implicate,temp)
      }
      
      # recreate scf.design variable but include the edu_savings numbers.
      scf.design <- 
            svrepdesign( 
                  
                  # use the main weight within each of the imp# objects
                  weights = ~wgt , 
                  
                  # use the 999 replicate weights stored in the separate replicate weights file
                  repweights = rw[ , -1 ] , 
                  
                  # read the data directly from the five implicates
                  data = imputationList( list( imp1 , imp2 , imp3 , imp4 , imp5 ) ) , 
                  
                  scale = 1 ,
                  
                  rscales = rep( 1 / 998 , 999 ) ,
                  
                  # use the mean of the replicate statistics as the center
                  # when calculating the variance, as opposed to the main weight's statistic
                  mse = TRUE ,
                  
                  type = "other" ,
                  
                  combined.weights = TRUE
            )
      
##########################################
## Analysis is below; above is cleaning ##
##########################################            
     
# Get mean incomes for income group
            
      #create data frame for results
      group.income.stats <- data.frame(group = c(1:6))
      
      for (i in 1:6) {
            
            #get weighted median for income group in each implicate
            temp_median <- c(
                  wtd.quantile( filter(imp1,inc.group == i)$income, filter(imp1,inc.group == i)$wgt, .5),
                  wtd.quantile( filter(imp2,inc.group == i)$income, filter(imp2,inc.group == i)$wgt, .5),
                  wtd.quantile( filter(imp3,inc.group == i)$income, filter(imp3,inc.group == i)$wgt, .5),
                  wtd.quantile( filter(imp4,inc.group == i)$income, filter(imp4,inc.group == i)$wgt, .5),
                  wtd.quantile( filter(imp5,inc.group == i)$income, filter(imp5,inc.group == i)$wgt, .5)
            )
            
            #add median to table
            group.income.stats[group.income.stats$group==i,"median.income"] <- mean(temp_median)
            remove(temp_median)
            
            #get weighted mean for income group in each implicate
            temp_means <- c(
                        weighted.mean( filter(imp1,inc.group == i)$income, filter(imp1,inc.group == i)$wgt),
                        weighted.mean( filter(imp2,inc.group == i)$income, filter(imp2,inc.group == i)$wgt),
                        weighted.mean( filter(imp3,inc.group == i)$income, filter(imp3,inc.group == i)$wgt),
                        weighted.mean( filter(imp4,inc.group == i)$income, filter(imp4,inc.group == i)$wgt),
                        weighted.mean( filter(imp5,inc.group == i)$income, filter(imp5,inc.group == i)$wgt)
                  )
            
            #add mean to table.
            group.income.stats[group.income.stats$group==i,"mean.income"] <- mean(temp_means)
            remove(temp_means)
            
      }

#calculate mean education savings by income group

      #create data frame to store results.
      mean_savings <- data.frame(income_group = c(1:6), 
                                 imp1 = NA,
                                 imp2 = NA,
                                 imp3 = NA,
                                 imp4 = NA,
                                 imp5 = NA)
      
      #calculate mean savings for each income group and in each implicate
      for (implicate in implicates) {
            
            temp <- get(implicate)
            
            
            for (i in 1:6) {
                  temp2 <- filter(temp, inc.group == i)
                  mean_savings[i,implicate] = weighted.mean( temp2$edu_savings , temp2$wgt )
                  remove(temp2)
            }

            remove(temp)
      }
      
      #average across data frames 
      mean_savings <- mutate(mean_savings, average_savings = ((imp1+imp2+imp3+imp4+imp5)/5))
      
      
      #Compare with anthony damico's script
      mean_savings_2 <- scf.MIcombine( with( scf.design , svyby( ~edu_savings , ~inc.group , svymean ) ) )
      
      #merge in to results
      group.income.stats[c("mean.edu.savings", "mean.savings.se")] <- c(coef(mean_savings_2), SE(mean_savings_2))
      
      
#calculate total education savings by income group
      
      #calculate using damico's script
      total.edu.savings <- scf.MIcombine( with( scf.design , svyby( ~edu_savings , ~inc.group , svytotal ) ) )
      
      #add to table 
      group.income.stats[c("total.edu.savings")] <- c(coef(total.edu.savings))


#calculate number of households with assets

      
      #create variable for "total households"
      total.households <- scf.MIcombine( with( scf.design , svyby( ~five , ~inc.group , svytotal ) ) )
      group.income.stats$total.households <- coef(total.households)
      
      #create variable for total households with edu savings 
      hh.with.edu.sav <- scf.MIcombine( with( scf.design , svyby( ~has.edu , ~inc.group , svytotal ) ) )
      group.income.stats$hh.with.edu <- coef(hh.with.edu.sav)
      
#calculate percentages
      
      #calculate share of total savings
      group.income.stats[c("share.of.edu.savings")] <- c(group.income.stats$total.edu.savings)/sum(group.income.stats$total.edu.savings)
      
      #calculate percent with a savings acct.
      group.income.stats[c("percent.with.edu.savings")] <- c(group.income.stats$hh.with.edu)/sum(group.income.stats$total.households)
      
      