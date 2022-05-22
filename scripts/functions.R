# color-blind palette (source: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442", "#999999", "#000000")

#' Latex labeller for ggplot that will interpret latex equations correctly (i.e. anything between $$). 
#' Works for both the \code{labels} parameter of discrete ggplot2 scales as well as the \code{labeller} of facets.
latex_labeller <- function(labels, ...) {
  
  require("dplyr")
  require("tidyr")
  require("purrr")
  require("latex2exp")
  require("rlang")
  
  # figure out if we're in a scale or facet labeller
  facet_labels <- methods::is(labels, "data.frame")
  if (!facet_labels) labels <- data_frame(..x.. = as.character(labels))
  
  # gather labels
  labels <- labels %>% 
    # add position info
    mutate(pos = row_number()) %>% 
    # gather labels
    mutate_if(is.factor, as.character) %>% 
    gather(var, val, -pos) %>% as_data_frame() 
  
  # convert latex to expression
  labels <- labels %>% 
    mutate(
      val = map(val, ~latex2exp::TeX(.x))
    )
  
  # spread data frame again
  labels <- labels %>% 
    filter(!is.na(pos)) %>% 
    spread(var, val) %>% 
    select(-pos)
  
  # return appropriate value for different labellers
  if (!facet_labels)
    labels <- quo(c(!!!map(labels$..x.., unname))) %>% eval_tidy()
  
  return(labels)
}
class(latex_labeller) <- c("function", "labeller")

# figure themes
theme_figure <- function(legend = TRUE, grid = TRUE, text_size = 20, axis_text_size = NULL) {
	the_theme <- theme_bw() + 
		theme(text = element_text(size = text_size),
					plot.background = element_blank(), panel.background = element_blank(),
					panel.border = element_rect(color="black", size=1), 
					strip.background = element_rect(color="black", linetype = 1),
					plot.margin = unit(c(0.1, 0.3, 0.1, 0.1), "cm")
		)
	# adjust grid
	if(!grid)
		the_theme <- the_theme + theme(panel.grid = element_blank())
	else
		the_theme <- the_theme + theme(panel.grid.minor = element_blank())
	# adjust legend
	if (!legend)
		the_theme <- the_theme + theme(legend.position = "none")
	# overwrite axis text size if provided
	if (!is.null(axis_text_size))
		the_theme <- the_theme + 
			theme(axis.text = element_text(size = axis_text_size)) 
	return(the_theme)
}

#isotopic mass balance
iso_mass_balance <- function (a, da, b, db) (a*da + b*db)/(a+b)
iso_mass_balance_error <- function (a, da, b, db, a.err = 0, da.err = 0, b.err= 0, db.err = 0) {
	abs(iso_mass_balance(a, da, b, db)) * 
		sqrt(
			( (da * a)^2 * ( (da.err/da)^2 + (a.err/a)^2 ) +
					(db * b)^2 * ( (db.err/db)^2 + (b.err/b)^2  ) ) /
				( da * a + db * b )^2 +
				(a.err^2 + b.err^2) / (a+b)^2
		)
}

#O18 branching isotope effect of nitrate reduction (25 or as high as 30)
eps_bnar <- 25


#calculate concentration and isotopic mass balance
#Nitrite and Norg isotopic compositions are only calculated for samples that have concentrations > 0 within the 1 sigma error range for the calculated concentration. 
calculate_mass_balance	<- function(year, well, conc, conc_err, d15N, d15N_err, d18O, d18O_err, compound)
{	group_by(year, well) %>%
		
		do({
			
			# out
			ret_val <- .
			
			# pull out NOx, NO3, and Ntot
			NOx <- filter(., compound == "NO3+NO2")
			NO3 <- filter(., compound == "NO3")
			Ntot <- filter(., compound == "NO3+Norg")
			
			
			if (nrow(NO3) > 1) {
				# average multiple acid stored NO3 analyses
				NO3 <- NO3 %>% group_by(year, well, compound) %>% summarize_all(funs(mean(.))) %>% ungroup()
			}
			if (nrow(NOx) > 1) {
				# average multiple NO3+NO2 analyses
				NOx <- NOx %>% group_by(year, well, compound) %>% summarize_all(funs(mean(.))) %>% ungroup()
			}
			if (nrow(Ntot) > 1) {
				# average multiple Ntot analyses
				Ntot <- Ntot %>% group_by(year, well, compound) %>% summarize_all(funs(mean(.))) %>% ungroup()
			}
			
			# Norg entry
			if (nrow(Ntot) == 1 && nrow(NO3) == 1) {
				Norg <- 
					data_frame(
						# metatdata
						compound = "Norg",
						well = Ntot$well[1],
						year = Ntot$year[1],
						# calculate Norg by mass balance
						conc = Ntot$conc - NO3$conc,
						conc_err = sqrt(Ntot$conc_err^2 + NO3$conc_err^2)
					)
				
				# exclude measurements that can't be ascertained to be > 0 within 1 sigma
				if (Norg$conc - Norg$conc_err > 0) {
					Norg <- Norg %>%
						# isotope composition and error propagation
						mutate(
							d15N = iso_mass_balance(Ntot$conc, Ntot$d15N, -1 * NO3$conc, NO3$d15N),
							d15N_err = iso_mass_balance_error(
								Ntot$conc, Ntot$d15N, -1 * NO3$conc, NO3$d15N, 
								Ntot$conc_err, Ntot$d15N_err, NO3$conc_err, NO3$d15N_err)
						)
				}
				
				ret_val <- bind_rows(ret_val, Norg)
			}
			
			# NO2 entry
			
			if (nrow(NOx) == 1 && nrow(NO3) == 1) {
				
				NO2 <- 
					data_frame(
						# metatdata
						compound = "NO2",
						well = NOx$well[1],
						year = NOx$year[1],
						# calculate nitrite by mass balance
						conc = NOx$conc - NO3$conc,
						conc_err = sqrt(NOx$conc_err^2 + NO3$conc_err^2)
					)
				
				# exclude measurements that can't be ascertained to be > 0 within 1 sigma
				if (NO2$conc - NO2$conc_err > 0) {
					NO2 <- NO2 %>%
						# isotope composition and error propagation
						mutate(
							d15N = iso_mass_balance(NOx$conc, NOx$d15N, -1 * NO3$conc, NO3$d15N),
							d15N_err = iso_mass_balance_error(
								NOx$conc, NOx$d15N, -1 * NO3$conc, NO3$d15N, 
								NOx$conc_err, NOx$d15N_err, NO3$conc_err, NO3$d15N_err),
							d18O = iso_mass_balance(NOx$conc, NOx$d18O, -1 * NO3$conc, NO3$d18O) + eps_bnar,
							d18O_err = iso_mass_balance_error(
								NOx$conc, NOx$d18O, -1 * NO3$conc, NO3$d18O, 
								NOx$conc_err, NOx$d18O_err, NO3$conc_err, NO3$d18O_err)
						) 
				}
				
				ret_val <- bind_rows(ret_val, NO2)
			}
			ret_val})
	}


