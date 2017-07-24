if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )
my_username <- Sys.getenv( "my_username" )
my_password <- Sys.getenv( "my_password" )
library(lodown)
# examine all available SHARE microdata files
share_cat <-
	get_catalog( "share" ,
		output_dir = file.path( getwd() ) , 
		your_username = my_username , 
		your_password = my_password )

# 2015 only
share_cat <- subset( share_cat , year == 2015 )
# download the microdata to your local computer
stopifnot( nrow( share_cat ) > 0 )



library(survey)

share_df <- readRDS( file.path( getwd() , "2015 main.rds" ) )

share_design <- 
	svydesign( 
		~ psu , 
		strata = ~ stratum , 
		data = share_df , 
		weights = ~ weight , 
		nest = TRUE 
	)
share_design <- 
	update( 
		share_design , 
		q2 = q2 ,
		never_rarely_wore_bike_helmet = as.numeric( qn8 == 1 ) ,
		ever_smoked_marijuana = as.numeric( qn47 == 1 ) ,
		ever_tried_to_quit_cigarettes = as.numeric( q36 > 2 ) ,
		smoked_cigarettes_past_year = as.numeric( q36 > 1 )
	)
sum( weights( share_design , "sampling" ) != 0 )

svyby( ~ one , ~ ever_smoked_marijuana , share_design , unwtd.count )
svytotal( ~ one , share_design )

svyby( ~ one , ~ ever_smoked_marijuana , share_design , svytotal )
svymean( ~ bmipct , share_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , share_design , svymean , na.rm = TRUE )
svymean( ~ q2 , share_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , share_design , svymean , na.rm = TRUE )
svytotal( ~ bmipct , share_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , share_design , svytotal , na.rm = TRUE )
svytotal( ~ q2 , share_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , share_design , svytotal , na.rm = TRUE )
svyquantile( ~ bmipct , share_design , 0.5 , na.rm = TRUE )

svyby( 
	~ bmipct , 
	~ ever_smoked_marijuana , 
	share_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ ever_tried_to_quit_cigarettes , 
	denominator = ~ smoked_cigarettes_past_year , 
	share_design ,
	na.rm = TRUE
)
sub_share_design <- subset( share_design , qn41 == 1 )
svymean( ~ bmipct , sub_share_design , na.rm = TRUE )
this_result <- svymean( ~ bmipct , share_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ bmipct , 
		~ ever_smoked_marijuana , 
		share_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( share_design )
svyvar( ~ bmipct , share_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ bmipct , share_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ bmipct , share_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ never_rarely_wore_bike_helmet , share_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( bmipct ~ never_rarely_wore_bike_helmet , share_design )
svychisq( 
	~ never_rarely_wore_bike_helmet + q2 , 
	share_design 
)
glm_result <- 
	svyglm( 
		bmipct ~ never_rarely_wore_bike_helmet + q2 , 
		share_design 
	)

summary( glm_result )
library(srvyr)
share_srvyr_design <- as_survey( share_design )
share_srvyr_design %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

share_srvyr_design %>%
	group_by( ever_smoked_marijuana ) %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

unwtd.count( ~ never_rarely_wore_bike_helmet , yrbss_design )

svytotal( ~ one , subset( yrbss_design , !is.na( never_rarely_wore_bike_helmet ) ) )
 
svymean( ~ never_rarely_wore_bike_helmet , yrbss_design , na.rm = TRUE )

svyciprop( ~ never_rarely_wore_bike_helmet , yrbss_design , na.rm = TRUE , method = "beta" )

