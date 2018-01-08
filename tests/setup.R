if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
my_username <- Sys.getenv( "my_username" )
my_password <- Sys.getenv( "my_password" )
library(lodown)
lodown( "share" , output_dir = file.path( getwd() ) , 
	your_username = my_username , 
	your_password = my_password )
library(lodown)
# examine all available SHARE microdata files
share_cat <-
	get_catalog( "share" ,
		output_dir = file.path( getwd() ) , 
		your_username = my_username , 
		your_password = my_password )

# wave 1, wave 6, and longitudinal weights only
share_cat <- subset( share_cat , grepl( "ave 1|ave 6|ongitudinal" , output_folder ) )
# download the microdata to your local computer




options( survey.lonely.psu = "adjust" )

library(survey)

available_files <-
	list.files( 
		file.path( getwd() ) , 
		recursive = TRUE , 
		full.names = TRUE 
	)

# wave six demographics file
share_dn6_df <-
	readRDS( grep( "6\\.0\\.0(.*)sharew6(.*)dn\\.rds" , available_files , value = TRUE ) )

share_dn6_df <-
	share_dn6_df[ c( "mergeid" , "country" , "dn042_" , "dn004_" ) ]
	
# wave six physical health file
share_ph1_df <-
	readRDS( grep( "sharew1(.*)ph\\.rds" , available_files , value = TRUE ) )

share_ph1_df$weight_in_2004 <-
		ifelse( share_ph1_df$ph012_ < 0 , NA , share_ph1_df$ph012_ )
		
share_ph1_df <-
	share_ph1_df[ c( "mergeid" , "weight_in_2004" , "ph005_" ) ]
	
# wave six physical health file
share_ph6_df <-
	readRDS( grep( "6\\.0\\.0(.*)sharew6(.*)ph\\.rds" , available_files , value = TRUE ) )

share_ph6_df$weight_in_2015 <-
		ifelse( share_ph6_df$ph012_ < 0 , NA , share_ph6_df$ph012_ )
		
share_ph6_df <-
	share_ph6_df[ c( "mergeid" , "weight_in_2015" , "ph003_" ) ]
	

# longitudinal weights file
share_longwt_df <-
	readRDS( grep( "longitudinal_weights_w1\\-(.*)\\.rds" , available_files , value = TRUE ) )

# france only longitudinal weights
france_df <- subset( share_longwt_df , country == 17 & ( cliw_a > 0 ) )

nrow_check <- nrow( france_df )

# merge on each of the tables
france_df <- merge( france_df , share_dn6_df )
france_df <- merge( france_df , share_ph1_df )
france_df <- merge( france_df , share_ph6_df )

# confirm no change in records
stopifnot( nrow( france_df ) == nrow_check )

share_design <- 
	svydesign( 
		~ psu + ssu , 
		strata = ~ stratum1 + stratum2 , 
		data = france_df , 
		weights = ~ cliw_a , 
		nest = TRUE 
	)
share_design <- 
	update( 
		share_design , 
		
		one = 1 ,
		
		sexe = factor( dn042_ , levels = 1:2 , labels = c( 'masculin' , 'feminin' ) ) ,
		
		health_in_general_2015 =
			factor( ph003_ , levels = 1:5 , labels =
				c( "excellente" , "tres bonne" , "bonne" , "acceptable" , "mediocre" )
			) ,
			
		fortemente_limite_2004 = ifelse( ph005_ %in% 1:3 , as.numeric( ph005_ == 1 ) , NA )

	)
sum( weights( share_design , "sampling" ) != 0 )

svyby( ~ one , ~ sexe , share_design , unwtd.count )
svytotal( ~ one , share_design )

svyby( ~ one , ~ sexe , share_design , svytotal )
svymean( ~ weight_in_2015 , share_design , na.rm = TRUE )

svyby( ~ weight_in_2015 , ~ sexe , share_design , svymean , na.rm = TRUE )
svymean( ~ health_in_general_2015 , share_design , na.rm = TRUE )

svyby( ~ health_in_general_2015 , ~ sexe , share_design , svymean , na.rm = TRUE )
svytotal( ~ weight_in_2015 , share_design , na.rm = TRUE )

svyby( ~ weight_in_2015 , ~ sexe , share_design , svytotal , na.rm = TRUE )
svytotal( ~ health_in_general_2015 , share_design , na.rm = TRUE )

svyby( ~ health_in_general_2015 , ~ sexe , share_design , svytotal , na.rm = TRUE )
svyquantile( ~ weight_in_2015 , share_design , 0.5 , na.rm = TRUE )

svyby( 
	~ weight_in_2015 , 
	~ sexe , 
	share_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ weight_in_2015 , 
	denominator = ~ weight_in_2004 , 
	share_design ,
	na.rm = TRUE
)
sub_share_design <- subset( share_design , dn004_ == 1 )
svymean( ~ weight_in_2015 , sub_share_design , na.rm = TRUE )
this_result <- svymean( ~ weight_in_2015 , share_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ weight_in_2015 , 
		~ sexe , 
		share_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( share_design )
svyvar( ~ weight_in_2015 , share_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ weight_in_2015 , share_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ weight_in_2015 , share_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ fortemente_limite_2004 , share_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( weight_in_2015 ~ fortemente_limite_2004 , share_design )
svychisq( 
	~ fortemente_limite_2004 + health_in_general_2015 , 
	share_design 
)
glm_result <- 
	svyglm( 
		weight_in_2015 ~ fortemente_limite_2004 + health_in_general_2015 , 
		share_design 
	)

summary( glm_result )
library(srvyr)
share_srvyr_design <- as_survey( share_design )
share_srvyr_design %>%
	summarize( mean = survey_mean( weight_in_2015 , na.rm = TRUE ) )

share_srvyr_design %>%
	group_by( sexe ) %>%
	summarize( mean = survey_mean( weight_in_2015 , na.rm = TRUE ) )

