library(ggprism)
library("ggplot2")
library(ggpubr)
library(gridExtra)
library(RColorBrewer)
library(rstatix)
library(ggmosaic)
library(ggstatsplot)
library(e1071)

###########################################################################
##################              Colors                   ##################
###########################################################################

color_varaibles <- function(Variable = ""){

	if(Variable == "Enterotype"){
		groupsOrder <- c("Bacteroides 1","Bacteroides 2","Prevotella","Ruminococcus")	
		colores <- c( "#f28118", "#ba1015", "#059571", "#4b59a1" )
		names(colores) <- groupsOrder
	}else if(Variable == "Bacteroides_2"){

		groupsOrder <- c("Bacteroides 2","Other")	
		colores <- c( "#ba1015", "gray" )
		names(colores) <- groupsOrder
			
	}		
	
	return(colores)
}

###########################################################################
##################       Detect outliers functions       ##################
###########################################################################
skewness_distribution_function <- function(x_var = c()){

	x_var <- x_var[!is.na(x_var)]
	  	
	skewness_value <- e1071::skewness( x_var , na.rm = TRUE)
	Shapirotest <- shapiro.test(x_var)
	
	W <- Shapirotest$statistic
	p.value <- Shapirotest$p.value
	ret_vector <- c(W,p.value,skewness_value)
	names(ret_vector) <- c("W","p.value","skewness")
	return(ret_vector)
	
}


outliers_rare_values_detection <- function(x = c(), detenction_method = "non_parametric",  # detenction_method non-parametric  parametric both
				N_SD = 3,  N_IQR = 3, min_percent_cat = 1){

	if(is.numeric(x)){
		if(detenction_method == "non_parametric"){
			outliers <- detect_outliers_continuous_variables_iqr(x_var = x, multiplier = N_IQR)		
		}else if(detenction_method == "parametric"){
			outliers <- detect_outliers_continuous_variables_sd(x_var = x , n_sd = N_SD)
		}else{
			parametric_res <- detect_outliers_continuous_variables_sd(x_var = x , n_sd = N_SD)
			non_parametric_res <- detect_outliers_continuous_variables_iqr(x_var = x , multiplier = N_IQR)			
			outliers <- list(parametric_res,non_parametric_res)
			names(outliers) <- c("parametric","non_parametric")	
		}	
	}
	if(is.character(x)){ outliers <- detect_outliers_continuous_discrete(x_var = x, percentage = min_percent_cat )  }
	
	if(is.factor(x)){	outliers <- detect_outliers_continuous_discrete(x_var = x, percentage = min_percent_cat )	}	

	if(is.logical(x)){	outliers <- detect_outliers_continuous_discrete(x_var = x, percentage = min_percent_cat )	}
		
	return(outliers)
}


detect_outliers_continuous_variables_sd <- function(x_var, n_sd = 3) {
	# Ensure the input is numeric
	if (!is.numeric(x_var)) stop("The input must be numeric.")
  	x_var <- x_var[!is.na(x_var)]
  
	# Calculate mean and standard deviation
	mean_x <- mean(x_var, na.rm = TRUE)
	sd_x <- sd(x_var, na.rm = TRUE)
  
	# Identify outliers
	outliers <- abs(x_var - mean_x) > n_sd * sd_x
  
	# Return vector indicating outliers
	return(x_var[outliers])
}

detect_outliers_continuous_variables_iqr <- function(x_var, multiplier = 3) { #Multiplier: Typically set to 1.5 for detecting mild outliers or 3 for extreme outliers.

	# Ensure the input is numeric
	if (!is.numeric(x_var)) stop("The input must be numeric.")
   	x_var <- x_var[!is.na(x_var)] 
	# Calculate the first (Q1) and third quartiles (Q3)
	q1 <- quantile(x_var, 0.25, na.rm = TRUE)
	q3 <- quantile(x_var, 0.75, na.rm = TRUE)
  
	# Compute the interquartile range (IQR)
	iqr <- q3 - q1
  
	# Define lower and upper bounds for outliers
	lower_bound <- q1 - multiplier * iqr
	upper_bound <- q3 + multiplier * iqr
  
	# Identify outliers
	outliers <- (x_var < lower_bound) | (x_var > upper_bound)
  
	# Return vector indicating outliers
	return(x_var[outliers])
}

detect_outliers_continuous_discrete <- function(x_var, percentage = 1 ){

   	x_var <- x_var[!is.na(x_var)] 
   	x_var <- as.character(x_var)
	percentage_df <-  data.frame(table(x_var)/length(x_var) * 100)
	LowAbundFeatures <- as.character(percentage_df[percentage_df$Freq < percentage,]$x_var)
	return(LowAbundFeatures)
} 

is.a.numeric <- function(var){
	var_unique <- unique(var[ !is.na(var) ])
	is_numeric <- sapply( var_unique, function(x){
		x <- gsub(" ","",x); x <- gsub("-","",x); x <- gsub("[.]","",x)
		xvec <- unlist(strsplit(x,""))
		all_num <- all(grepl("[0-9]", xvec ))
		return(all_num)
	}  )
	return(all(is_numeric))
}
chek_unusual_values <- function(Metadata_all, VAR2Chehck){
	for(i in colnames(Metadata_all) ){
		Var <- Metadata_all[,i]
		Var <- Var[!is.na(Var)]
		if( any( Var == VAR2Chehck) ){
			print(i); print(Metadata_all[,i]); cat("\n\n\n")
		}
	}
}


###################################################################
##################       Statistical tests       ##################
###################################################################


cont_res_kw <- function(subDF = data.frame(),discrete = "",continuous = ""){
	subDF <- subDF[,match(c(discrete,continuous), colnames(subDF))]
	colnames(subDF) <-c("discrete","continuous")
	subDF <- subDF[complete.cases(subDF),]
	subDF$discrete <- factor(subDF$discrete)
	
	KT <- kruskal.test(continuous ~ discrete, data = subDF)
	Mean <- c(by(subDF$continuous , subDF$discrete,mean))
	SD <- c(by(subDF$continuous , subDF$discrete,sd))
	N <- c(by(subDF$continuous , subDF$discrete,length))

	names(Mean) <- paste0("Mean.",names(Mean))
	names(SD) <- paste0("SD.",names(SD))
	names(N) <- paste0("N.",names(N))
	KT.statistic = KT$statistic
	KT.p.value = KT$p.value; names(KT.p.value) <- "KT.p.value"
	ret <-c(Mean,SD,N, KT.statistic,KT.p.value)		
	
	ret.df <- data.frame(t(ret))
	retDF <- cbind(data.frame(Discrete=discrete,Continuous=continuous) , ret.df)
	
	return(retDF)
}

N_categories_NA_values_function <- function(in.meta){
	N_NA <-  apply(in.meta,2,function(x){ NA_vec <- is.na(x); N_NAs<- length( NA_vec[NA_vec == T]); return(N_NAs) })  
	NA_info_Table <- data.frame(Variable = names(N_NA), N_NAs = N_NA, NA_percentage =  (N_NA / dim(in.meta)[1]) * 100 )
	# NA_info_Table[NA_info_Table$NA_percentage < 20,]

	###############################
	###  N values per varaible  ###
	Var_Ncategories <- apply(in.meta,2,function(x){ NA_vec <- is.na(x); x <- x[NA_vec != T]; N <- length(unique(x)); return(N ) } )  
	Ncategories_info_Table <- data.frame(Variable = names(Var_Ncategories), N_values = Var_Ncategories )
	NA_info_Table$N_values <- Ncategories_info_Table[match( rownames(NA_info_Table) , rownames(Ncategories_info_Table) ),]$N_values

	############################	
	#### Unbalance features ####
	Var_balance_cat <- apply(in.meta,2,function(x){ NA_vec <- is.na(x); x <- x[NA_vec != T]; min_cat <- min(table(x)); return(min_cat ) } )  
	NA_info_Table$N_min_categories <- Var_balance_cat[match( rownames(NA_info_Table) , names(Var_balance_cat) )]
	
	#######################
	###  Varaible type  ###
	Var_type <- c()
	for(i in colnames(in.meta)){ Var_type <- c( Var_type ,  class(in.meta[,i]) ) }
	names(Var_type) <- colnames(in.meta)
	NA_info_Table$Var_type <- Var_type[match( rownames(NA_info_Table) , names(Var_type) )]	

	NA_info_Table <- NA_info_Table[order(NA_info_Table$NA_percentage),]
	return(NA_info_Table)
}




cont_plot <- function(subDF = data.frame(),discrete = "",continuous = "",p.val = c(), q.val = c(),colors = c(), dunnTest = T){
	subDF <- subDF[,match(c(discrete,continuous), colnames(subDF))]
	colnames(subDF) <-c("discrete","continuous")
	subDF <- subDF[complete.cases(subDF),]
	subDF$discrete <- factor(subDF$discrete)
	subDF
	Pvals <- round(p.val,digits=3)
	Qvals <- round(q.val,digits=3)
	subT <- paste0("p-val = ",Pvals,", q-val = ",Qvals)	

	dfCombn <-t(combn(levels(subDF$discrete),2))
	list_vec <- list()
	for(i in 1:nrow(dfCombn)){list_vec[[i]] <- dfCombn[i,]}

	Mean <- sort(c(by(subDF$continuous , subDF$discrete,mean)))	
	subDF$discrete <- factor( as.character(subDF$discrete) , names(Mean) )

	#vector <- levels( subDF$discrete )
	#combinaciones <- combn(vector, 2, simplify = FALSE)	
	sig_comp <- data.frame(dunn_test(continuous ~ discrete, data = subDF, p.adjust.method = "BH"))
	sig_comp <- subset(sig_comp, p.adj < 0.05)
	combinaciones <- list()
	if(nrow(sig_comp) != 0){
		for(i in 1:nrow(sig_comp)){combinaciones[[i]] <- unlist(c(sig_comp[i,c("group1","group2")]))  }
	}

	#subDF$discrete <- factor(subDF$discrete, levels=c("Superdonor" , "Autologous"))	
	p <- ggplot(subDF, aes(x=discrete, y=continuous, fill = discrete )) + geom_boxplot() + ylab(continuous) + xlab(discrete) + 
		#geom_jitter(shape=16, position=position_jitter(0.2) ) + 
		theme_bw() +   
		#scale_color_manual(values = c( c("gray") )) + 		
		#geom_signif(comparisons = combinaciones,map_signif_level = TRUE,step_increase = 0.1) +
		labs(title =  continuous,subtitle = subT)+
		theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
	if( (dunnTest ==  T) &   (nrow(sig_comp) != 0) ){
		p  <- p + geom_signif(comparisons = combinaciones,map_signif_level = TRUE,step_increase = 0.1) 
	}

	if(length(colors) != 0){
		p <- p + scale_fill_manual(values = colors[levels(subDF$discrete)]) 
	}
	
	return(p)
}

disc_plot <- function(subDF = data.frame(),discrete1 = "",discrete2 = "",p.val = c(), q.val = c(),colors=c(), order_discrete2 = c(), order_discrete1 = c(),
		method = "ggbarstats"){ # method = "ggbarstats" "geom_mosaic"
	
	subDF <- subDF[,match(c(discrete1,discrete2), colnames(subDF))]
	colnames(subDF) <-c("discrete1","discrete2")
	subDF <- subDF[complete.cases(subDF),]
	#subDF$discrete <- factor(subDF$discrete)
	subDF
	Pvals <- round(p.val,digits=3)
	Qvals <- round(q.val,digits=3)
	subT <- paste0("p-val = ",Pvals,", q-val = ",Qvals)	
	
	if(length(order_discrete2) != 0){
		subDF$discrete2 <- factor(subDF$discrete2,order_discrete2)
	}
	if(length(order_discrete1) != 0){
		subDF$discrete1 <- factor(subDF$discrete1,order_discrete1)
	}	

	TitleName <- paste0(discrete1," | ",discrete2)
	if(method == "ggbarstats" ){
		retPlot <- ggbarstats(data  = subDF, x = discrete1, y = discrete2, label = "both" ) + labs(title =  TitleName,subtitle = subT)
		retPlot <- retPlot + guides(fill=guide_legend(title=discrete1)) + xlab(discrete2) + ylab("%") 
	}
	if(method == "geom_mosaic" ){
		freq_table <- data.frame(table(subDF))
		retPlot <- ggplot(data = freq_table) +
		  geom_mosaic(aes(weight = Freq, x = product(discrete2), fill = discrete1), na.rm = TRUE) +  theme_classic(base_size = 15)+
		  guides(fill=guide_legend(title=discrete1)) +
		  theme(axis.text.x = element_text(angle = 25, hjust = 1)) + ylab(discrete1) + xlab(discrete2) + 
		  labs(title =  TitleName,subtitle = subT)
		  retPlot
	  #theme(axis.text.x = element_text(angle = -25, hjust = .1)) #+  theme(axis.title.x = element_blank()) 		
	}
	
	if(length(colors) != 0){
		retPlot <- retPlot + scale_fill_manual(values = colors ) 
	}
		
	return(retPlot)	
}


cont_res_wt <- function(subDF = data.frame(),discrete = "",continuous = ""){
	subDF <- subDF[,match(c(discrete,continuous), colnames(subDF))]
	colnames(subDF) <-c("discrete","continuous")
	subDF <- subDF[complete.cases(subDF),]
	subDF$discrete <- factor(subDF$discrete)
	
	WT <- wilcox.test(continuous ~ discrete, data = subDF)
	level1 = levels(subDF$discrete)[1]
	level2 = levels(subDF$discrete)[2]
	
	mean.level1 <-mean(subDF[subDF$discrete == level1,"continuous"])
	median.level1 <-median(subDF[subDF$discrete == level1,"continuous"])
	sd.level1 <-sd(subDF[subDF$discrete == level1,"continuous"])

	DFlevel1 <- subDF[subDF$discrete == level1,]
	DFlevel1 <- DFlevel1[complete.cases(DFlevel1),]
        N_level1 <- nrow(DFlevel1)
        
	mean.level2 <-mean(subDF[subDF$discrete == level2,"continuous"])
	median.level2 <-median(subDF[subDF$discrete == level2,"continuous"])
	sd.level2 <-sd(subDF[subDF$discrete == level2,"continuous"])  	

	DFlevel2 <- subDF[subDF$discrete == level2,]
	DFlevel2 <- DFlevel2[complete.cases(DFlevel2),]
        N_level2 <- nrow(DFlevel2)
        
	
	Dominant <- ifelse(median.level1 - median.level2 > 0, as.character(level1),as.character(level2))
	
	retDF<-data.frame(Discrete=discrete,Continuous=continuous,Level1=level1, N_level1,Level2=level2,N_level2,
		mean.level1,mean.level2,median.level1,median.level2,sd.level1,sd.level2,
		WT$statistic,WT$p.value,Dominant)
	return(retDF)
}

table_wt_test_function <- function(in.df=data.frame(), continuous_features = c(), Disc_var = "", Cont_var = "", Feature = ""  ){
	in.df$FEATURE = in.df[,Feature]
	ret_df <- data.frame()
	for(i in continuous_features){	
		sub_df <- subset(in.df, FEATURE == i)
		res_temp <- cont_res_wt(subDF = sub_df,discrete =Disc_var,continuous = Cont_var)
		res_temp <- data.frame(Feature = i, res_temp)
		ret_df <- rbind(ret_df,res_temp )
		rm(sub_df,res_temp)
	}

	ret_df$q.val <- p.adjust(ret_df$WT.p.value, method="BH")
	rownames(ret_df) <- NULL
	ret_df<-ret_df[order(ret_df$q.val),]
	return(ret_df)
}

# report = "p" "p.adj" 
plot_features_wt <- function(subDF = data.frame(), discrete = "", continuous = "", 
		Feature = "", colors, subTitle, p_val_vec = NULL, q_val_vec = NULL, 
		report = "p"){ 
	subDF <- subDF[,match(c(discrete,continuous,Feature), colnames(subDF))]
	colnames(subDF) <-c("discrete","continuous","feature")	
	
	subDF$feature<- factor( subDF$feature , names( sort(c(by(subDF$continuous, subDF$feature,mean)), decreasing=T)  ) )

	df_p_val <- subDF %>%
	  rstatix::group_by(feature) %>%
	  rstatix::wilcox_test(continuous ~ discrete) %>%
	  rstatix::adjust_pvalue(p.col = "p", method = "BH") %>%
	  rstatix::add_significance(p.col = "p.adj") %>% 
	  rstatix::add_xy_position(x = "feature", dodge = 0.8) # important for positioning!

	df_p_val$p <-  round(df_p_val$p,digits=2)

	if( !is.null(p_val_vec) ){
		df_p_val$p <- round( p_val_vec[ match(as.character(df_p_val$feature) , names(p_val_vec) )] ,digits=2)
	
	}
	if( !is.null(q_val_vec) ){
		df_p_val$p.adj <- round( q_val_vec[ match(as.character(df_p_val$feature) , names(q_val_vec) )] ,digits=2)
	
	}

	rPlot <- ggplot(subDF, aes(x=feature, y=continuous, fill = discrete )) + geom_boxplot() + ylab(continuous) + xlab(Feature) + 
		#geom_jitter(shape=16, position=position_jitter(0.2) ) + 
		theme_bw() +   
		scale_fill_manual(values = colors[levels(factor(subDF$discrete))]) + 
		labs(title =  paste(unique(subDF$discrete),collapse=" / "),subtitle = subTitle) +
		theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) + 
		theme(legend.position="bottom") 
	
	if(report == "p"){	rPlot <- rPlot + add_pvalue(df_p_val, xmin = "xmin", xmax = "xmax",label = "p = {p}",tip.length = 0) }
	if(report == "p.adj"){	rPlot <- rPlot + add_pvalue(df_p_val, xmin = "xmin", xmax = "xmax",label = "p.adj = {p.adj}",tip.length = 0) }	

	return(rPlot)
}


####################################################################
##################       model functions          ##################
####################################################################

nb_glm_function <- function(Y.var = "", X.var = c(""), in.df=data.frame()  ){
	formula_model <- paste(Y.var, "~" , paste(X.var,collapse = " + ") )
	#formula_model <- "Abundance ~ STUDY_GROUP + moisture + DEMOGRAPHIC_GENDER"
	formula_model <- as.formula(formula_model)

	N <- nrow( in.df[complete.cases(in.df[,Y.var]), ]  )
	if( sum(in.df[,Y.var] == 0) / nrow(in.df) < 0.2 ){
		model_fit <- glmmTMB(formula = formula_model, data = in.df,family =nbinom2 )    
	}else{
		model_fit <- glmmTMB(formula = formula_model, data = in.df,family =nbinom2, ziformula=~1 )    
	}	
	
	Res_summ <- summary(model_fit)	
	Res_summ <- data.frame(Res_summ$coefficients$cond)
	categories = rownames(Res_summ) 
	var = rownames(Res_summ) 
	for(nam in X.var){ categories <- gsub(nam,"",categories) ; var[grepl(nam,var)] <- nam }
	categories[categories == ""] <- var[categories == ""]
	Res_summ <- data.frame( var, categories, Res_summ )
	rownames(Res_summ) <- NULL
	colnames(Res_summ)  <- gsub( "Pr...z.." , "p.value" , colnames(Res_summ) )
	colnames(Res_summ)  <- gsub( "Pr...t.." , "p.value" , colnames(Res_summ) )
	
	Res_anova <- data.frame(Anova(model_fit))	
	Res_anova <- data.frame( var = rownames(Res_anova) , Res_anova )	
	rownames(Res_anova) <- NULL

	list_ret <- list( Res_summ , Res_anova )
	names(list_ret) <- c("summary","anova")
	return(list_ret)
}

glm_function <- function(Y.var = "", X.var = c(""), in.df=data.frame()  ){
	formula_model <- paste(Y.var, "~" , paste(X.var,collapse = " + ") )
	#formula_model <- "Abundance ~ STUDY_GROUP + moisture + DEMOGRAPHIC_GENDER"
	formula_model <- as.formula(formula_model)
	
	model_fit <- glm(formula = formula_model, data = in.df )    
	
	Res_summ <- summary(model_fit)	
	Res_summ <- data.frame(Res_summ$coefficients)
	categories = rownames(Res_summ) 
	var = rownames(Res_summ) 
	for(nam in X.var){ categories <- gsub(nam,"",categories) ; var[grepl(nam,var)] <- nam }
	categories[categories == ""] <- var[categories == ""]
	Res_summ <- data.frame( var, categories, Res_summ )
	rownames(Res_summ) <- NULL
	colnames(Res_summ)  <- gsub( "Pr...z.." , "p.value" , colnames(Res_summ) )
	colnames(Res_summ)  <- gsub( "Pr...t.." , "p.value" , colnames(Res_summ) )


	Res_anova <- data.frame(Anova(model_fit))	
	Res_anova <- data.frame( var = rownames(Res_anova) , Res_anova )	
	rownames(Res_anova) <- NULL

	list_ret <- list( Res_summ , Res_anova )
	names(list_ret) <- c("summary","anova")
	return(list_ret)
}


### method = "lm"  method = "neg_binomial"
table_glm_test_function <- function(in.df=data.frame(), continuous_features = c(), Y.VAR = "", X.VAR = c() , 
			Feature = "OTU", Categorie2Explore = "", method = "lm" ){

	in.df$FEATURE = in.df[,Feature]
	ret_df <- data.frame()
	for(i in continuous_features){	
		sub_df <- subset(in.df, FEATURE == i)
		
		if(method == "neg_binomial"){	
			res_temp <- data.frame(Feature = i, nb_glm_function( Y.var = Y.VAR , X.var = X.VAR , in.df = sub_df )$summary )
		}
		if(method == "lm"){	
			res_temp <- data.frame(Feature = i, glm_function( Y.var = Y.VAR , X.var = X.VAR , in.df = sub_df )$summary )
		}			
		ret_df <- rbind(ret_df,res_temp )
		rm(sub_df,res_temp)
	}

	ret_df <- ret_df[ret_df$var == Categorie2Explore,]
	ret_df$q.val <- p.adjust(ret_df$p.value, method="BH") 
	
	if( any(colnames(ret_df) %in% "Pr...t..") ){ ret_df$q.val <- p.adjust(ret_df$Pr...t.., method="BH") }
	if( any(colnames(ret_df) %in% "Pr...z..") ){ ret_df$q.val <- p.adjust(ret_df$Pr...z.., method="BH")  }
		
	rownames(ret_df) <- NULL
	ret_df<-ret_df[order(ret_df$q.val),]
	return(ret_df)
}





