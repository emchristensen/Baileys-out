# what are Bailey's doing??
library(ggplot2)
library(dplyr)


# ==================================================================
# Functions

#' @title Add treatment codes
#' 
#' @description adds treatment codes for each plot in the rodent dataframe
#' 
#' @param data dataframe must have a column named 'plot'
#' 
#' @return dataframe with columns:
#'            plot
#'            censusdate
#'            species
#'            abundance
#'            numericdate
#'            before_after (two letters representing before/after switch: C = control, E = krat exclosure, X = total rodent removal)
add_treatment = function(data){
  treatment = data.frame(before_after = c('CX','CE','EE','CC',
                                          'XC','EC','XC','CE',
                                          'CX','XX','CC','CX',
                                          'EC','CC','EE','XX',
                                          'CC','EC','EE','EE',
                                          'EE','CE','XX','XC'),plot=seq(1,24))
  data = merge(data,treatment,by='plot')
  
  return(data)
}
# =======================================================================
# first PB was recorded on plot 1 (spectab excl) in Sept 1995
startdate = '1995-06-01'
treatments = c('CC','EE','CE','EC')
treatments = c('EC','CE','EE','CC','XC','CX','XX')


data = portalr::abundance(path='repo', level = 'Plot', type='Granivores',
                          length="All", unknowns=FALSE, min_plots = 1,
                          shape="flat", time='date',clean=F,na_drop=T)

data$species = as.character(data$species)
rdat = add_treatment(data) %>% arrange(censusdate)
pb = rdat %>% dplyr::filter(species == 'PB', 
                                       before_after %in% treatments, 
                                       censusdate >= startdate)


# plot PB - timeseries
ggplot(pb, aes(x = censusdate, y = abundance, colour = treatment)) +
  geom_jitter(height=.1,width=.3) +
  geom_smooth(method='auto',se = TRUE) +
  scale_colour_brewer(type = 'qual', palette = 'Dark2') +
  theme(legend.position = 'top') 


# focus on switch 
pbswitch = dplyr::filter(pb,censusdate>='2012-01-01')
ggplot(pbswitch, aes(x = censusdate, y = abundance, colour = before_after)) +
  geom_jitter(height=.1,width=.3) +
  geom_smooth(method='auto',se = TRUE) +
  scale_colour_brewer(type = 'qual', palette = 'Dark2') +
  theme(legend.position = 'top') +
  geom_vline(xintercept=as.Date('2015-04-10'))

# averages before/after switch by treatment type
pb_before = dplyr::filter(pbswitch,censusdate < as.Date('2015-04-01'))
pb_after = dplyr::filter(pbswitch,censusdate > as.Date('2015-04-01'))
aggregate(pb_before$abundance,by=list(treatment=pb_before$before_after),FUN=mean)
aggregate(pb_after$abundance,by=list(treatment=pb_after$before_after),FUN=mean)
# decreased everywhere; decreased most on exclosures that became controls