## MFL ADP connection

library(httr)
library(jsonlite)


## API URL for 14 day old ADP in PPR and non-keepers leagues
## see here for adp api doc: https://www71.myfantasyleague.com/2018/api_info?STATE=test&CMD=export&TYPE=adp
### you can make some variables/objects to make the URL a bit more parameterized 
adp_url <-
	'https://www71.myfantasyleague.com/2018/export?TYPE=adp&DAYS=14&TIME=&FRANCHISES=&IS_PPR=1&IS_KEEPER=0&IS_MOCK=&INJURED=&CUTOFF=&DETAILS=0&JSON=1'
## run the GET
r <-
	GET(adp_url)
## extract content
adp_content <-
	content(r)
## api_content is a list of lists -->
names(adp_content)

## pluck player adp data -- this is another list of lists
player_adp <- 
	adp_content$adp$player

## rbind the list of lists into df -- thanks a ton do.call!! 
player_adp_df <- 
	data.frame(do.call(rbind, player_adp))

names(player_adp_df)

## gonna format df so that values aren't all strings
int_names <- 
	c('minPick', 'maxPick')

float_names <- 
	c('averagePick')
	
char_names <- 
	c('id', 'draftsSelectedIn') ## yes.. id has to be a char bc MFL has id values like 0501, 0532, etc (why!?) 


## probably can do an lapply or something but it's late.... and for loops don't hurt my brain as much
for(i in int_names){
	player_adp_df[, i] <-
		as.integer(player_adp_df[, i])
}

for(i in float_names){
	player_adp_df[, i] <-
		as.numeric(player_adp_df[, i])
}

for(i in char_names){
	player_adp_df[, i] <-
		as.character(player_adp_df[, i])
}


## let's checkout the DF!!
head(player_adp_df)

########## ok, the above is mildly useless b/c we don't have the player names associated to each player ID... 
########## so let's pull the player info so we have some useable stuff

## we're gonna build out the api call b/c the player api allows us to grab specific players instead of it 
## just returning every possible player. See player API doc here: https://www71.myfantasyleague.com/2018/api_info?STATE=test&CMD=export&TYPE=players

player_url_pref <- 
	'https://www71.myfantasyleague.com/2018/export?TYPE=players&DETAILS=0&SINCE=&'

## create comma delim string of player ids to pass to API
player_ids <-
	paste(unique(player_adp_df$id), collapse = ',')

player_url <-
	paste0(player_url_pref, 'PLAYERS=', player_ids, '&JSON=1')

## GET it! 
r <- 
	GET(player_url)

## extract content
player_content <-
	content(r)

## extract player data info -- list of lists 
player_data <- 
	player_content$players$player
	
## have to do some quick processing to make sure each list element has the same attributes on it...
## for some reason MFL designates some players as rookies and then leaves it out for others..
## we'll make the NR (non-rookie) parameter for those who aren't rookies (ie status attribute is missing)
## this is gonna help w/ the do.call rbind.

name_vec <- ## gonna use this to perserve list order
	c('status', 'position', 'name', 'id', 'team')
	
player_data <- lapply(player_data, 
							 function(x) {
							 	if('status' %nin% names(x)) {
							 		x[['status']] <- 'NR'
							 	} 
							 	return(x[name_vec])
							 })

## rbind list of lists into df
player_data_df <- 
	data.frame(do.call(rbind, player_data))


names(player_data_df)


## ok, player data is formated, here's a sneak peak...
head(player_data_df)


#### mereg player data w/ ADP! 

df <- 
	merge(player_adp_df, player_data_df, on = 'id', all.x = TRUE)


## this is the result you can play with
names(df)
head(df)

