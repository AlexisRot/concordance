#############################################################################################
# ================================ New Comments (11/19/2020) ================================
# QUALIFICATION_2.4.1.R is a modified reversion from QUALIFICATION_2.4.R.
# QUALIFICATION.Wrap() function wraps QUALIFICATOIN() function for multiple group variables,
#                                                     in order to output and integrate into one single csv file.
# The following content lists the changes of arguments of QUALIFICATION.Wrap() derived from QUALIFICATOIN().
# New Arguments:
# Group.Var.Value.Vector  : A character vector, the value vector of the Group variable.
# output.csv.file.name    : A character, Name of output csv file, do NOT add ".csv". Default to Data.File.Name with postfix ".result"
# Removed Argument:
# Group.Var.Value         : The value vector of the Group variable
#
# The following contect summarizes the changes in the code.
# 5.
# 1. Add Symbol "%" to percent difference, when combining Estimates with CIs into one cell for
#    the specific parameter Average Diff. (% diff.)
#
# 2. Output table name and data size for each table, the return of the original QUALIFICATION function is modified.
# 3. Add split columns, in order to split tables obviously.
# 4. Combine Estimate with CIs into one cell in the csv file.

# ============================= End of New Comments (11/19/2020) ============================
#FILE NAME 		QUALIFICATION_1.3.R


#DESCRIPTION		Compute the concordance analysis (plot, Curve parameters, GMTs, GMT Ratio
#			and Constant bias and the confidence intervals associated
#                       Compute the Full Provide anlysis
#                       Compute Altman Bland Analysis

#METHOD 		Concordance computation (Linear Regression with constraints in the model)
#Reference:		Measurement methods comparisons and linear statistical relationship,
#			Tan C-Y, Iglewicz B, Technometrics 1999, Vol 41(1), p 192-201, ISSN 0040-1706

#EXT FUNCTION PARAM		WRITE.HTLM.TABLEN_1.0.R function
#				DEBUG: to print the dataset put "ON", else "OFF"
#				VERY.SMALL.VALUE: value for helping the computation
#				Round.To: number of decimal for the number output
#				GMT.Round.To: number of decimal for the GMT output
#	                        LOCATION: where the dataset is and where will be written the output files

#INT FUNCTION PARAM 	[REQUIRED]

#	Data.File.Name   : The name of the csv file, do NOT add ?.csv?
#	X.Var.Name       : The Name of the X-Variable, must be the same shown as the title of in the csv file.
#	Y.Var.Name       : The Name of the X-Variable, must be the same shown as the title of in the csv file.
#	logbase          : log base, typically is 2 or 10,

#	Concordance.X.Label  : Label at X-Axis,
#	Concordance.Y.Label  : Label at Y-Axis,


#     AltmanBland.X.Label  : Label at X-Axis,
#     AltmanBland.Y.Label  : Label at Y-Axis,


#	X.Min               : Always give the minimal value of X here,(Same for X and Y axis for Concordance And X axis of AB)
#	X.Max               : Always give the maximum value of X here,

#	AltmanBland.Y.Min   : Always give the minimal value of Y here,(Specific to Altman Bland)
#	AltmanBland.Y.Max   : Always give the maximum value of Y here,(Specific to Altman Bland)

#INT FUNCTION PARAM    [OPTIONAL]

#	Group.Var.Name   : The variable used to subset the dataset, e.g. ?BS?,
#	Group.Var.Value  : The value of the Group variable,
#	Alpha            : 0.05, usually use 0.05 (default)
#	Title            : Additional title,
#	Postfix          : A string attach to the name of output file,

#	LLOQ             : If ?X and ?Y have common LLOQ, specified here, (LLOQ removed from concordance and AB and Agreement)
#	X.LLOQ           : If ?X and ?Y have difference LLOQ, specify the LLOQ of X,
#	Y.LLOQ           : If ?X and ?Y have difference LLOQ, specify the LLOQ of Y,
#	Upper.Bound      : If ?X and ?Y have common upper bound, specified here, default is NO upper bound.
#	X.Upper.Bound    : If ?X and ?Y have difference upper bound, specify the LLOQ of X, default is NO upper bound.
#	Y.Upper.Bound    : If ?X and ?Y have difference upper bound, specify the LLOQ of Y, default is NO upper bound

#	X.Ticks                      : Specify the X-Tickmarks default is determined by system,
#	AltmanBland.Y.Ticks          : Specify the Y-Tickmarks default is determined by system,

#     Concordance.Y.Axis.Residuals  : give the max value of the y-axis, if you want to change the default plotting y-axis (default = 0)
#     Concordance.Y.Ticks.Residuals : give the ticks marks of the y-axis, (e.g.: -4,-3,-2,-1,0,1,2,3,4) had to be symetric.


#	Concordance.Need.95CI.Band   : True if want to show 95% confidence interval band, default is FALSE.
#	Concordance.Range.Band       : TRUE if you want to add 2 parallel lines to the Concordance curve in the plot. Default is FALSE.
#     Concordance.Range.Band.value : Value for the gap between the curve and the parallel line of the Range. Default is 1.
#     Concordance.Sizable.Graph    : TRUE To have dot of a proportional Size to the nb of points
#	Concordance.Assumptions	     : TRUE if the plot of the resisuals and the Normality plot and test are needed. Default is FALSE.
#     Concordance.X.Annotation     : X Position for the text. Default is -1.
#     Concordance.Y.Annotation     : Y Position for the text.Default is -1.
#     Concordance.Intercept.Value.plot : True, if you want to print the value of the intercept on the plot. Default is FALSE
#     Concordance.Perfect.Band     : TRUE To draw the perfect band around the perfect line
#	Concordance.Assumptions.Label.X  : Name of the Study or Antigen, by default / Constant "Predicted Values" and "Theoritical Quantiles"


#     AltmanBland.Need.95CI.Band : To Draw the 95CI Band around the curve, default is FALSE,
#     AltmanBland.Show.95CI      : To Show the 95CI Band around the curve, default is FALSE,
#     AltmanBland.Plot.Log.Diff  : To plot the Log diff instead of the Ratio, default is FALSE,
#     AltmanBland.Draw.Mean.STD  : To Draw the +/- 2 Std around the Mean, default is TRUE,
#     AltmanBland.Draw.Perfect.Line : To Draw the Perfect Line around the curve, default is FALSE,
#      AltmanBland.Draw.Regression.Line : To Draw the Regression Line, default is FALSE,
#       AltmanBland.Plot.Reference.X.Axis : to plot the Ratio by the reference lab (X. Variable) by default FALSE

#	X.Cutoff         : For serostatus agreement, specify the X cutoff value here, default is NOT to estimate serostatus agreement
#	Y.Cutoff         : For serostatus agreement, specify the Y cutoff value here, default is NOT to estimate serostatus agreement

#       Test.Type        : Default "ELISA"
#       Agreement.Cutoff : Default 1, set of the limit for the absolute difference in log2 of the data, only compute when Test.type is not ELISA.


#INPUT FILE		csv file with the data to analyze

#OUTPUT FILE		html file for the computation
#			pdf file for the plot

#REQUIREMENTS

#NOTES

#WRITTEN BY		Sandrine CAYEZ

#CREATION DATE		14-Jul-06



#MODIFICATION		1 parameter added

#REASON FOR MODIFICATION Name of the Antigen / Study for the Asumption graphs

#MODIFY BY		Sandrine CAYEZ

#MODIFICATION DATE	31-Jul-06



#MODIFICATION		1 parameter added

#REASON FOR MODIFICATION graph Altman Bland with the reference variable in x-axis

#MODIFY BY		Sandrine CAYEZ

#MODIFICATION DATE	07-Aug-06


#MODIFICATION		Computation of the 95% CI for the pairwise agreement + Add one parameter
#                       to control the x and y-axis for the residuals plot / and Y-tick mark for this axis

#REASON FOR MODIFICATION Altman Bland does not allow an assessment of the pairwise agreement
#			 Reference: Assessing the concordance of two measurement methods
#				    Jason J.Z. Liao, Robert C. Capen, Timothy L. Schofield
#		 		    Merck Research Laboratories, P.O.Box4, West Point, PA 19486

#                       Additional parameter to have the same output

#MODIFY BY		Sandrine CAYEZ

#MODIFICATION DATE	22-Aug-06



#MODIFY BY		Lingyi Zheng
#Reason                 Switch x,y axis in the serostatus agreement table

#MODIFY BY		Lingyi Zheng
#Reason                 Fix the CI of logbase

#MODIFY BY		Lingyi Zheng on 11/12/2013
#Reason                 Added 90%CI to slope and percent difference


#MODIFICATION DATE	11/27/2017

#MODIFY BY		Pauline Jurvilliers
#Reason                 Fold difference modified to logbase^(...) instead of 2^(...)





############################################################################################

# PJU 11/07/2017: I put in comment
#source( "\\\\Usswtaps167\\groupb\\Groups\\RD Med Affrs\\Clinical Serology\\Unit_BIOSTATISTICS\\4.Programs\\R SOURCE CODE\\UTILITIES\\WRITE.HTML.TABLEN_1.0.R");

## Size of the Dot when they are a lot of observations with the same results

Size.Increment   <- 0.5;
Size.Max         <- 15;


DEBUG            <- "ON";
VERY.SMALL.VALUE <- 0.000001;

## Rounding of the Output

Round.To         <- 3.0;
GMT.Round.To     <- 3.0;

Axis.Text.Size <- 1.0;
Axis.Line.Size <- 1.0;

## Altman Bland external parameters

Mean.STD.Style         <- 1;
Mean.STD.Style2        <- 4;

Reference.Center.Line.Color     <- "Red";
Reference.Line.Color            <- "#206040";
Reference.Line.Thickness        <- 2;

Perfect.Line.Style     <- 2;
Perfect.Line.Color     <- 4;
Perfect.Line.Thickness <- 2;

Text.Size <- 0.75;

Profile.Color <- c(1,2,4,8);
Profile.Style <- c(21, 24, 22, 23);
Profile.Size  <- c(1.1, 0.7, 1.0, 1.0);

Show.Slope.95CI <- TRUE;
COORDINATE.AT.ORIGIN <- TRUE;

Points.Line.Width    <- 1.5;

######################################################################################
####### Output to html  #######################################
######################################################################################
write.html.tablen <- function(

  x,
  file= "",
  capt= "The Default Title",
  append= append
)
{
  x1 <- paste(colnames(x))
  n <- length(x1)
  x1[n] <- paste( x1[n],  "</thead><tbody>" )
  x <- rbind(x1,x)

  head <- paste("<table border=1 cellpadding= \"3\" cellspacing= \"0\" align= \"center\" rules=\"groups\">\n<caption>", capt, "</caption>\n <thead>\n")
  cat(head, file= file, append= append)
  write.table(x, sep= "</td><td align=\"center\">", file= file,
              eol= "</td></tr>\n", row.names= rep("<tr>", nrow(x)),
              col.names= FALSE, append= TRUE, quote= FALSE)
  cat("</tr> </tbody>\n</table>\n\n<br>\n", file= file, append= TRUE)
}

######################################################################################
####### Loading and Treating Raw Data Function #######################################
######################################################################################

LOADING.TREATING.RAWDATA <- function (

  Data.File.Name = Data.File.Name,
  X.Var.Name     = X.Var.Name,
  Y.Var.Name     = y.Var.Name,
  Profile.Var.Name = "",
  Group.Var.Name = "",
  Group.Var.Value = "",

  Remove.LLOQ    = FALSE,
  LLOQ           = 0.0,
  X.LLOQ         = 0.0,
  Y.LLOQ         = 0.0,

  Upper.Bound    = 9999999999,
  X.Upper.Bound  = 9999999999,
  Y.Upper.Bound  = 9999999999,
  Remove.Above.Upper.Bound = Remove.Above.Upper.Bound

)

{

  Rawdata.From.File <- read.csv2(file = paste(LOCATION, Data.File.Name, ".csv", sep=""), sep = separator_excel);
  Rawdata.From.File[is.na(Rawdata.From.File)] <- ""
  print(head(Rawdata.From.File))

  X.Var.Pos <- 0; Y.Var.Pos <- 0; Group.Var.Pos <- 0; Profile.Var.Pos <- -1;

  for ( i in 1:length(Rawdata.From.File) )
  {
    if ( X.Var.Name == names(Rawdata.From.File)[i] ) { X.Var.Pos <- i; }
    if ( Y.Var.Name == names(Rawdata.From.File)[i] ) { Y.Var.Pos <- i; }
    if ( Profile.Var.Name == names(Rawdata.From.File)[i] ) { Profile.Var.Pos <- i; }
    if ( Group.Var.Name == names(Rawdata.From.File)[i] ) { Group.Var.Pos <- i; }
  }

  if ( Group.Var.Pos >= 1 ) {
    Rawdata.From.File <- Rawdata.From.File[Rawdata.From.File[Group.Var.Pos] == Group.Var.Value, ];
  }

  if ( LLOQ > 0.0 & X.LLOQ < VERY.SMALL.VALUE & Y.LLOQ < VERY.SMALL.VALUE ) {
    X.LLOQ <- LLOQ; Y.LLOQ <- LLOQ;
  }

  if ( Upper.Bound < 999999 & X.Upper.Bound > 999999 & Y.Upper.Bound > 999999 ) {
    X.Upper.Bound <- Upper.Bound; Y.Upper.Bound <- Upper.Bound;
  }



  Size.Of.Data <- length(Rawdata.From.File[, 1]);
  print("Size.Of.Data")
  print(Size.Of.Data)

  if ( Profile.Var.Pos > 0 ) {
    print("Profile.Var.Pos > 0")
    Rawdata <- data.frame ( X.Var.Res = Rawdata.From.File[X.Var.Pos],
                            X.Var     = seq(1, Size.Of.Data),
                            X.Var.GMT = seq(1, Size.Of.Data),
                            Y.Var.Res = Rawdata.From.File[Y.Var.Pos],
                            Y.Var     = seq(1, Size.Of.Data),
                            Y.Var.GMT = seq(1, Size.Of.Data),
                            Profile   = Rawdata.From.File[Profile.Var.Pos],
                            Remove.LLOQ.Flag = seq(1, Size.Of.Data),
                            Freq  = seq(1, Size.Of.Data)
    );
  } else {
    print("Profile.Var.Pos <= 0")
    Rawdata <- data.frame ( X.Var.Res = Rawdata.From.File[X.Var.Pos],
                            X.Var     = seq(1, Size.Of.Data),
                            X.Var.GMT = seq(1, Size.Of.Data),
                            Y.Var.Res = Rawdata.From.File[Y.Var.Pos],
                            Y.Var     = seq(1, Size.Of.Data),
                            Y.Var.GMT = seq(1, Size.Of.Data),
                            Profile   = seq(1, Size.Of.Data),
                            Remove.LLOQ.Flag = seq(1, Size.Of.Data),
                            Freq  = seq(1, Size.Of.Data)
    );
  }



  names(Rawdata) <- c("X.Var.Res", "X.Var", "X.Var.GMT",
                      "Y.Var.Res", "Y.Var", "Y.Var.GMT",
                      "Profile", "Remove.LLOQ.Flag"  , "Freq"
  );

  # print(paste(X.Var.Name, "@", X.Var.Pos, ", ", Y.Var.Name, "@", Y.Var.Pos, ", ",
  #             Profile.Var.Name, "@", Profile.Var.Pos, ", ", Group.Var.Name, "@", Group.Var.Pos, sep=""));



  for ( i in 1:Size.Of.Data )
  {
    #print(i)
    # Edited by Lingyi Zheng on Jan. 30, 2013
    if ( is.na(Rawdata$X.Var.Res[i]) ) { Rawdata$Remove.LLOQ.Flag[i] <- 9999; Rawdata$X.Var[i] = NA; } else {

      if ( substr(as.character(Rawdata$X.Var.Res[i]),1,1) == "<" )
      {
        Rawdata$X.Var[i] <- as.numeric(substr(as.character(Rawdata$X.Var.Res[i]),2,10));
        Rawdata$X.Var.GMT[i] <- Rawdata$X.Var[i] / 2.0;
      }
      else if ( substr(as.character(Rawdata$X.Var.Res[i]),1,1) == ">" )
      {
        Rawdata$X.Var[i] <- as.numeric(substr(as.character(Rawdata$X.Var.Res[i]),2,10));
        Rawdata$X.Var.GMT[i] <- Rawdata$X.Var[i] / 1.0;
      }
      else
      {
        Rawdata$X.Var[i] <- as.numeric(as.character(Rawdata$X.Var.Res[i]));
        Rawdata$X.Var.GMT[i] <- Rawdata$X.Var[i];
      }
    }



    if ( is.na(Rawdata$Y.Var.Res[i]) ) { Rawdata$Remove.LLOQ.Flag[i] <- 9999; Rawdata$Y.Var[i] = NA; } else {

      if ( substr(as.character(Rawdata$Y.Var.Res[i]),1,1) == "<" )
      {
        Rawdata$Y.Var[i] <- as.numeric(substr(as.character(Rawdata$Y.Var.Res[i]),2,10));
        Rawdata$Y.Var.GMT[i] <- Rawdata$Y.Var[i] / 2.0;
      }
      else if ( substr(as.character(Rawdata$Y.Var.Res[i]),1,1) == ">" )
      {
        Rawdata$Y.Var[i] <- as.numeric(substr(as.character(Rawdata$Y.Var.Res[i]),2,10));
        Rawdata$Y.Var.GMT[i] <- Rawdata$Y.Var[i] / 1.0;
      }
      else
      {
        Rawdata$Y.Var[i] <- as.numeric(as.character(Rawdata$Y.Var.Res[i]));
        Rawdata$Y.Var.GMT[i] <- Rawdata$Y.Var[i];
      }
    }



    if ( is.na(Rawdata$X.Var[i]) | is.na(Rawdata$Y.Var[i]) | Rawdata$Remove.LLOQ.Flag[i] >= 9998 | # Edited by Lingyi
         ( Rawdata$X.Var.GMT[i] < X.LLOQ) | (Rawdata$Y.Var.GMT[i] < Y.LLOQ ) )
    {
      Rawdata$Remove.LLOQ.Flag[i] <- 9999;
    }
    else
    {
      Rawdata$Remove.LLOQ.Flag[i] <- 0;
    }

    if ( !is.na(Rawdata$X.Var[i]) & Rawdata$X.Var[i] > X.Upper.Bound )
    {
      Rawdata$X.Var[i] <- X.Upper.Bound;
      Rawdata$X.Var.GMT[i] <- X.Upper.Bound;
      if (Remove.Above.Upper.Bound) { Rawdata$Remove.LLOQ.Flag[i] <- 9998; }
    }

    if ( !is.na(Rawdata$Y.Var[i]) & Rawdata$Y.Var[i] > Y.Upper.Bound )
    {
      Rawdata$Y.Var[i] <- Y.Upper.Bound;
      Rawdata$Y.Var.GMT[i] <- Y.Upper.Bound;
      if (Remove.Above.Upper.Bound) { Rawdata$Remove.LLOQ.Flag[i] <- 9998; }
    }


    Rawdata$Freq[i]    <-  1;
    if ( Profile.Var.Pos < 0 ) { Rawdata$Profile[i] <- -1; }
  }



  Rawdata <- Rawdata[ !is.na(Rawdata$X.Var), ];
  Rawdata <- Rawdata[ !is.na(Rawdata$Y.Var), ];

  if ( Remove.LLOQ == "REMOVE.1.LAB")
  {
    Rawdata <- Rawdata[ Rawdata$X.Var.GMT >= ( X.LLOQ - VERY.SMALL.VALUE ) &
                          Rawdata$Y.Var.GMT >= ( Y.LLOQ - VERY.SMALL.VALUE ),];
  }

  else if ( Remove.LLOQ == "REMOVE.2.LAB")
  {
    Rawdata <- Rawdata[ Rawdata$X.Var.GMT >= ( X.LLOQ - VERY.SMALL.VALUE ) |
                          Rawdata$Y.Var.GMT >= ( Y.LLOQ - VERY.SMALL.VALUE ),];
  }

  if ( Remove.Above.Upper.Bound ) Rawdata <- Rawdata[ Rawdata$Remove.LLOQ.Flag != 9998, ];

  print(head(Rawdata,15));
        print("----------------------- End of Loading Data .... ");

  # To have access to the RawData After Preparation


  list( RawData = Rawdata );

}

###############################################################################
######################GMT Ratio and 95% CI when paired Data ###################
###############################################################################

GMT.GMTRatio <- function ( Rawdata        = RawData,
                           Data.File.Name = Data.File.Name,
                           X.Var          = X.Var,
                           Y.Var          = Y.Var,
                           X.Var.GMT      = X.Var.GMT,
                           Y.Var.GMT      = Y.Var.GMT,
                           Paired         = TRUE,
                           Alpha          = 0.05
)

{
  Size.Of.Valid.Data <- length(Rawdata$X.Var.GMT);


  GMT.Ratio.Data <- data.frame( X.Var = log10( Rawdata$X.Var.GMT ),
                                Y.Var = log10( Rawdata$Y.Var.GMT ));
  # print(paste("2 Num of Valid Data (", Data.File.Name, ") : ", Size.Of.Valid.Data, sep = "" ));

  Covariance.Matrix <- cor(GMT.Ratio.Data);
  Rho <- Covariance.Matrix[1,2];

  Mean.X <- mean(log10(Rawdata$X.Var.GMT));
  Mean.Y <- mean(log10(Rawdata$Y.Var.GMT));
  Mean.Diff <- Mean.Y - Mean.X;

  GMT.X <- 10 ^ Mean.X;
  GMT.Y <- 10 ^ Mean.Y;
  GMT.Ratio <- GMT.Y / GMT.X;


  SD.X  <- sd(log10(Rawdata$X.Var.GMT));
  SE.X  <- SD.X / sqrt(Size.Of.Valid.Data);
  LCL.X <- round( 10 ^ ( Mean.X + qt(Alpha/2.0, Size.Of.Valid.Data - 1) * SE.X), GMT.Round.To);
  UCL.X <- round( 10 ^ ( Mean.X + qt(1-Alpha/2.0, Size.Of.Valid.Data - 1) * SE.X), GMT.Round.To);

  SD.Y  <- sd(log10(Rawdata$Y.Var.GMT));
  SE.Y  <- SD.Y / sqrt(Size.Of.Valid.Data);
  LCL.Y <- round( 10 ^ ( Mean.Y + qt(Alpha/2.0, Size.Of.Valid.Data - 1) * SE.Y), GMT.Round.To);
  UCL.Y <- round( 10 ^ ( Mean.Y + qt(1-Alpha/2.0, Size.Of.Valid.Data - 1) * SE.Y), GMT.Round.To);

  if (Paired == TRUE) {SE.GMT.Ratio <- sqrt(SE.X ^ 2 + SE.Y ^ 2 - 2 * Rho * SE.X * SE.Y);}
  else {SE.GMT.Ratio <- sqrt(SE.X ^ 2 + SE.Y ^ 2 );}

  LCL.GMT.Ratio.P95 <- round( 10 ^ ( Mean.Diff + qt(   Alpha / 2.0, Size.Of.Valid.Data - 1) * SE.GMT.Ratio), GMT.Round.To);
  UCL.GMT.Ratio.P95 <- round( 10 ^ ( Mean.Diff + qt( 1-Alpha / 2.0, Size.Of.Valid.Data - 1) * SE.GMT.Ratio), GMT.Round.To);

  LCL.GMT.Ratio.P90 <- round( 10 ^ ( Mean.Diff + qt(   Alpha, Size.Of.Valid.Data - 1) * SE.GMT.Ratio), GMT.Round.To);
  UCL.GMT.Ratio.P90 <- round( 10 ^ ( Mean.Diff + qt( 1-Alpha, Size.Of.Valid.Data - 1) * SE.GMT.Ratio), GMT.Round.To);

  #LCL.GMT.Ratio <- round( 10 ^ ( Mean.Diff + qnorm( Alpha / 2.0 )   * SE.GMT.Ratio), GMT.Round.To);
  #UCL.GMT.Ratio <- round( 10 ^ ( Mean.Diff + qnorm( 1-Alpha / 2.0 ) * SE.GMT.Ratio), GMT.Round.To);


  restable5 <- matrix(0,1,4)
  restable6 <- matrix(0,1,4)
  restable7 <- matrix(0,1,4)
  restable8 <- matrix(0,1,4)
  restable9 <- matrix(0,1,4)



  restable5[1,] <- cbind("GMT Reference Lab",  round(GMT.X, GMT.Round.To),         LCL.X, 	      UCL.X);
  restable6[1,] <- cbind("GMT Destination Lab",round(GMT.Y, GMT.Round.To), 	       LCL.Y, 	      UCL.Y);
  restable7[1,] <- cbind("GMT Ratio (90%CI)",  round(GMT.Ratio, GMT.Round.To), LCL.GMT.Ratio.P90, UCL.GMT.Ratio.P90);
  restable8[1,] <- cbind("GMT Ratio (95%CI)",  round(GMT.Ratio, GMT.Round.To), LCL.GMT.Ratio.P95, UCL.GMT.Ratio.P95);
  restable9[1,] <- cbind("Rho", 		   round(Rho, GMT.Round.To + 1),       "-",           "-" );


  GMT.Ratio.Table <- rbind(restable5[1,], restable6[1,], restable7[1,], restable8[1,], restable9[1,])

  list ( 	GMT.Ratio.Table = GMT.Ratio.Table );

}


###############################################################################
####### Serostatus agreement and sensitivity ##################################
###############################################################################

Serostatus.Agreement <- function ( Rawdata   = Rawdata,
                                   X.Var.GMT = X.Var.GMT,
                                   Y.Var.GMT = Y.Var.GMT,
                                   X.Cutoff  = X.Cutoff,
                                   Y.Cutoff  = Y.Cutoff,
                                   Alpha     = Alpha
)
{

  restable9  <- matrix(0,1,4)
  restable10 <- matrix(0,1,4)
  restable11 <- matrix(0,1,4)
  restable12 <- matrix(0,1,4)
  restable13 <- matrix(0,1,4)
  restable14 <- matrix(0,1,4)
  restable15 <- matrix(0,1,4)

  True.Negative <- Rawdata[ Rawdata$X.Var.GMT < X.Cutoff  & Rawdata$Y.Var.GMT < Y.Cutoff, ];
  True.Negative.Count <- length(True.Negative$X.Var);

  True.Positive <- Rawdata[ Rawdata$X.Var.GMT >= X.Cutoff & Rawdata$Y.Var.GMT >= Y.Cutoff, ];
  True.Positive.Count <- length(True.Positive$X.Var);

  False.Negative <- Rawdata[ Rawdata$X.Var.GMT >= X.Cutoff & Rawdata$Y.Var.GMT < Y.Cutoff, ];
  False.Negative.Count <- length(False.Negative$X.Var);

  False.Positive <- Rawdata[ Rawdata$X.Var.GMT < X.Cutoff  & Rawdata$Y.Var.GMT >= Y.Cutoff, ];
  False.Positive.Count <- length(False.Positive$X.Var);

  Sub.Total.X.Neg <- True.Negative.Count + False.Negative.Count
  Sub.Total.Y.Neg <- True.Negative.Count + False.Positive.Count
  Sub.Total.X.Pos <- True.Positive.Count + False.Positive.Count
  Sub.Total.Y.Pos <- False.Negative.Count + True.Positive.Count
  Total <- length(Rawdata$X.Var.GMT)

  Percent.Sub.Total.Y.Neg <- paste( Sub.Total.Y.Neg," (", round( Sub.Total.Y.Neg/Total*100,Round.To),"%)",sep="")
  Percent.Sub.Total.X.Neg <- paste( Sub.Total.X.Neg," (", round( Sub.Total.X.Neg/Total*100,Round.To),"%)",sep="")
  Percent.Sub.Total.Y.Pos <- paste( Sub.Total.Y.Pos," (", round( Sub.Total.Y.Pos/Total*100,Round.To),"%)",sep="")
  Percent.Sub.Total.X.Pos <- paste( Sub.Total.X.Pos," (", round( Sub.Total.X.Pos/Total*100,Round.To),"%)",sep="")

  Agreement <- paste(round((True.Negative.Count + True.Positive.Count)/Total*100,Round.To),
                     " (", True.Negative.Count + True.Positive.Count, "/", Total, ")",sep="");

  restable9 [1,] <- cbind("Ref Lab Negative", True.Negative.Count, False.Positive.Count, Percent.Sub.Total.Y.Neg);
  restable10[1,] <- cbind("Ref Lab Positive", False.Negative.Count, True.Positive.Count, Percent.Sub.Total.Y.Pos);
  restable11[1,] <- cbind("Sub Total", Percent.Sub.Total.X.Neg, Percent.Sub.Total.X.Pos, Total);
  restable12[1,] <- cbind("# of Sample in Agreement", "-", "-", Agreement);

  Serostatus.Agreement.Table <- rbind (restable9[1,], restable10[1,],restable11[1,], restable12[1,]);



  ### Computation of the Difference and its 95% CI between th proportion of protected (Complementary table)


  Percent.Protected.In.X <- CI.Proportion (
    Nb.Success   = Sub.Total.X.Pos,
    Nb.Total     = Total,
    Method       = "Exact CI",
    Percent.Name = "% Protected in Dest Lab",
    Alpha        = Alpha
  )

  Percent.Protected.In.Y <- CI.Proportion (
    Nb.Success   = Sub.Total.Y.Pos,
    Nb.Total     = Total,
    Method       = "Exact CI",
    Percent.Name = "% Protected in Ref Lab",
    Alpha        = Alpha
  )


  Percent.Difference.Protected <- CI.Difference.Proportion ( Nb.Total.X  = Total,
                                                             Nb.Total.Y  = Total,
                                                             Nb.X        = Sub.Total.Y.Pos,
                                                             Nb.Y        = Sub.Total.X.Pos,
                                                             Alpha       = Alpha
  )

  Percent.Diff.Protected.Table <- rbind( Percent.Protected.In.Y$Percent.Proportion.CI.Table,
                                         Percent.Protected.In.X$Percent.Proportion.CI.Table,
                                         Percent.Difference.Protected$Diff.Proportion.Table)

  list (Serostatus.Agreement.Table = Serostatus.Agreement.Table,
        Percent.Diff.Protected.Table = Percent.Diff.Protected.Table);


}

###############################################################################
####### Confidence Interval for a difference of proportion #####################
###############################################################################

CI.Difference.Proportion <- function ( Nb.Total.X  = Nb.Total.X,
                                       Nb.Total.Y  = Nb.Total.Y,
                                       Nb.X        = Nb.X,
                                       Nb.Y        = Nb.Y,
                                       Alpha       = Alpha
)
{

  p.X <- Nb.X / Nb.Total.X
  p.Y <- Nb.Y / Nb.Total.Y

  Diff.p <- p.Y - p.X

  if (Nb.Total.X == Nb.Total.Y)
  {
    Percent.Diff.p <- paste( round(Diff.p*100,Round.To), " (", Nb.Y - Nb.X, "/", Nb.Total.X, ")", sep="")
  }
  else {  Percent.Diff.p <- round(Diff.p*100,Round.To)}

  STD <- sqrt ((p.X*(1-p.X)/Nb.Total.X) + (p.Y*(1-p.Y)/Nb.Total.Y))

  LCL.Diff.p <-round (100*(Diff.p - qnorm( 1-Alpha / 2.0 )*STD ),Round.To)
  UCL.Diff.p <-round (100*(Diff.p + qnorm( 1-Alpha / 2.0 )*STD ),Round.To)

  Diff.Proportion.Table <- cbind("Diff protected (%)",Percent.Diff.p, LCL.Diff.p, UCL.Diff.p)

  list (Diff.Proportion.Table = Diff.Proportion.Table)

}


###############################################################################
####### Confidence Interval for a proportion ##################################
###############################################################################

CI.Proportion <- function (
    Nb.Success   = Nb.Success,
    Nb.Total     = Nb.Total,
    Method       = "Exact CI",
    Percent.Name = "Agreement",
    Alpha        = Alpha
)

{

  if (Nb.Success == Nb.Total)

  {

    if (Method == "Exact CI")

    {
      p <- Nb.Success/Nb.Total
      LCI.inter<- 1+(Nb.Total - Nb.Success+1)/(Nb.Success*qf(Alpha, 2*Nb.Success,2*(Nb.Total-Nb.Success+1)));
      if (p < 1) {UCI.inter <- 1+(Nb.Total-Nb.Success)/((Nb.Success+1)*qf(1-Alpha,2*(Nb.Success+1),2*(Nb.Total-Nb.Success)))}
      else { UCI.inter <- 1;}
      LowCI = round((1/LCI.inter)*100,Round.To);
      UppCI = round((1/UCI.inter)*100,Round.To);
    }

    else if (Method == "Wald")

    {

      p <- (Nb.Success+2)/(Nb.Total+4)
      LowCI <- round((p - qnorm(1-Alpha)*sqrt(p*(1-p)/(Nb.Total +4)))*100,Round.To)
      UppCI <- round((p + qnorm(1-Alpha)*sqrt(p*(1-p)/(Nb.Totala +4)))*100,Round.To)
      if (UppCI >100) {UppCI <- 100}

    }
  }

  else

  {
    if (Method == "Exact CI")

    {

      p <- Nb.Success/Nb.Total
      LCI.inter<- 1+(Nb.Total - Nb.Success+1)/(Nb.Success*qf(Alpha/2, 2*Nb.Success,2*(Nb.Total-Nb.Success+1)));
      if (p < 1) {UCI.inter <- 1+(Nb.Total-Nb.Success)/((Nb.Success+1)*qf(1-Alpha/2,2*(Nb.Success+1),2*(Nb.Total-Nb.Success)))}
      else { UCI.inter <- 1;}
      LowCI = round((1/LCI.inter)*100,Round.To);
      UppCI = round((1/UCI.inter)*100,Round.To);
    }

    else if (Method == "Wald")

    {

      p <- (Nb.Success+2)/(Nb.Total+4);
      LowCI <- round((p - qnorm(1-Alpha/2)*sqrt(p*(1-p)/(Nb.Total+4)))*100,Round.To)
      UppCI <- round((p + qnorm(1-Alpha/2)*sqrt(p*(1-p)/(Nb.Total+4)))*100,Round.To)
      if (UppCI >100) {UppCI <- 100}

    }
  }

  Success.Rate <- round(Nb.Success / Nb.Total*100, Round.To)

  restable13 <- matrix(0,1,4)
  restable13[1,] <- cbind(paste(Percent.Name, "(%)",sep=""),paste(Success.Rate, " (",Nb.Success, "/",Nb.Total,")", sep = ""),LowCI,UppCI)

  list (Percent.Proportion.CI.Table = restable13[1,])
}


###############################################################################
######################### Agreement Analysis ##################################
###############################################################################


Agreement.Analysis.FAP <- function (
    Rawdata 	= RawData,
    X.Var   	= X.Var,
    Y.Var   	= Y.Var,
    Percent.Name 	= "Agreement",
    Alpha   	= Alpha,
    Agreement.Cutoff=Agreement.Cutoff
)


{

  X.Agree <- log(Rawdata$X.Var)/log(2) #comparison in log2
  Y.Agree <- log(Rawdata$Y.Var)/log(2)

  diff.agreement <- round(abs(X.Agree - Y.Agree),2);
  diff.agreement.Coding <- 0;
  Nb.Total <- length (Rawdata$X.Var)

  for (i in 1:Nb.Total)
  {

    if (diff.agreement[i] > (Agreement.Cutoff+VERY.SMALL.VALUE)) {diff.agreement.Coding[i] <- 0}
    else {diff.agreement.Coding[i] <- 1}

  }

  Agreement <- round(sum(diff.agreement.Coding)/Nb.Total*100,Round.To)

  Nb.Success <- sum(diff.agreement.Coding)

  Agreement.FAP <- CI.Proportion (
    Nb.Success   = Nb.Success,
    Nb.Total     = Nb.Total,
    Method       = "Exact CI",
    Percent.Name = "Agreement",
    Alpha        = Alpha
  )

  list(Agreement.FAP.Table = Agreement.FAP$Percent.Proportion.CI.Table)

}


###############################################################################
######################### Concordance##########################################
###############################################################################


Concordance.Computation <- function (
    Rawdata        = Rawdata,
    Freq           = Freq,
    X.Var          = X.Var,
    Y.Var          = Y.Var,
    logbase        = logbase,
    Concordance.xmin = X.Min,
    Concordance.xmax = X.Max,
    Alpha          = 0.05
)
{

  # computation of the parameters
  # print("---------------- ## CONCORDANCE Qualification ## ----------------");
  # print(Rawdata);
  X <- log(Rawdata$X.Var)/log(logbase)
  Y <- log(Rawdata$Y.Var)/log(logbase)


  xx <- Rawdata$X.Var
  yy <- Rawdata$Y.Var

  n <- length(X)

  Xbar <- mean(X)
  Ybar <- mean(Y)
  Sxx  <- sum((X-Xbar)^2)
  Syy  <- sum((Y-Ybar)^2)
  Sxy  <- sum((X-Xbar)%*%(Y-Ybar))


  beta <- (Syy - Sxx + sqrt((Syy - Sxx)^2 + 4*Sxy^2)) / (2*Sxy)

  rsquared <- round(Sxy^2/(Sxx*Syy),4); #Round.To)

  aa <- Ybar - beta * Xbar
  dat <- data.frame (X=X,Y=Y)

  Yhat <- aa + beta * X

  varYerror <- sum((Yhat-Y)%*%(Yhat-Y))/(n-2)

  Residuals <-(Yhat-Y)/sqrt(varYerror)

  covmat <- cov(dat)
  cormat <- cor(dat)

  eigen.res   <- eigen(covmat)
  eigen.vect  <- eigen.res$vectors[1,]
  eigen.vect2 <- eigen.res$vectors[2,]
  eigen.val   <- eigen.res$values

  lambda1 <- eigen.val[1]
  lambda2 <- eigen.val[2]

  const   <- (lambda1*lambda2)/((n-1)*(lambda2 - lambda1)^2)
  covbeta <- const*(eigen.vect2%*%t(eigen.vect2))

  varlogbeta <-  (1.0/log(logbase))^2 *
    ( (covbeta[1,1]/eigen.vect[1]^2) + (covbeta[2,2]/eigen.vect[2]^2) -
        2*(covbeta[1,2]/(eigen.vect[1]*eigen.vect[2])));

  varbeta  <- beta^2*varlogbeta*log(logbase)^2
  varXbar  <- varYerror/n
  delta    <- varbeta/(beta)^2 + varXbar/(Xbar)^2-2*cormat[1,2]*(sqrt(varbeta*varXbar)/(beta*Xbar))
  varalpha <- varYerror/n + (beta*Xbar)^2*delta

  diff <- Y-X
  diffmean <- mean(diff)

  sum.diff <- sum((diff - diffmean)^2)
  vardiff <- sum.diff/(n*(n - 1))

  delta3 <- varbeta/(beta)^2 + varXbar/(X-Xbar)^2-2*cormat[1,2]*(sqrt(varbeta*varXbar)/(beta*(X-Xbar)));

  # print(paste("Var (X-Y) = ", vardiff));

  #Confidence interval of the predicted value and Band Range
  print(Concordance.xmin)
  print(Concordance.xmax)
  X.Seq <- seq ( Concordance.xmin, Concordance.xmax, length = 250);
  print(X.Seq)
  Y.Hat <- aa + beta * X.Seq;
  ubounds.95CI  <- Y.Hat + qt(1-Alpha/2, n-2) * sqrt((varYerror/n) + (X.Seq - Xbar)^2 * varbeta);
  lbounds.95CI  <- Y.Hat - qt(1-Alpha/2, n-2) * sqrt((varYerror/n) + (X.Seq - Xbar)^2 * varbeta);


  #Slope

  Slope <- round(beta,Round.To)
  lcl.Slope.P95 <- round(exp(log(logbase) * (log(beta)/log(logbase) - qt(1-Alpha/2,n-2)*sqrt(varlogbeta))),Round.To)
  ucl.Slope.P95 <- round(exp(log(logbase) * (log(beta)/log(logbase) + qt(1-Alpha/2,n-2)*sqrt(varlogbeta))),Round.To)

  Slope <- round(beta,Round.To)
  lcl.Slope.P90 <- round(exp(log(logbase) * (log(beta)/log(logbase) - qt(1-Alpha,  n-2)*sqrt(varlogbeta))),Round.To)
  ucl.Slope.P90 <- round(exp(log(logbase) * (log(beta)/log(logbase) + qt(1-Alpha,  n-2)*sqrt(varlogbeta))),Round.To)

  #Intercept

  Intercept <- round(aa,Round.To)
  lcl.Intercept.P95 <- round(aa - qt(1-Alpha/2,n-2)*sqrt(varalpha),Round.To)
  ucl.Intercept.P95 <- round(aa + qt(1-Alpha/2,n-2)*sqrt(varalpha),Round.To)

  lcl.Intercept.P90 <- round(aa - qt(1-Alpha  ,n-2)*sqrt(varalpha),Round.To)
  ucl.Intercept.P90 <- round(aa + qt(1-Alpha  ,n-2)*sqrt(varalpha),Round.To)


  #fold Rise

  Fold.Rise <- round(4^beta,Round.To)
  lcl.Fold.Rise<- round(4^(exp(log(logbase) * (log(beta)/log(logbase) - qt(1-Alpha/2,n-2)*sqrt(varlogbeta)))),Round.To)
  ucl.Fold.Rise<- round(4^(exp(log(logbase) * (log(beta)/log(logbase) + qt(1-Alpha/2,n-2)*sqrt(varlogbeta)))),Round.To)

  # Average Difference

  Avg.Diff <- round(exp(log(logbase)*diffmean)-1,Round.To+2)*100
  lcl.Avg.Diff.P95 <- round(exp(log(logbase) * (diffmean - qt(1-Alpha/2,n-1)*sqrt(vardiff)))-1,Round.To+2)*100
  ucl.Avg.Diff.P95 <- round(exp(log(logbase) * (diffmean + qt(1-Alpha/2,n-1)*sqrt(vardiff)))-1,Round.To+2)*100

  Avg.Diff <- round(exp(log(logbase)*diffmean)-1,Round.To+2)*100
  lcl.Avg.Diff.P90 <- round(exp(log(logbase) * (diffmean - qt(1-Alpha,  n-1)*sqrt(vardiff)))-1,Round.To+2)*100
  ucl.Avg.Diff.P90 <- round(exp(log(logbase) * (diffmean + qt(1-Alpha,  n-1)*sqrt(vardiff)))-1,Round.To+2)*100


  # Fold Difference

  # Pauline Jurvilliers 11/27/2017: fold difference logbase^diffmean instead of 2^diffmean

  Fold.diff <- round(logbase^diffmean,Round.To)
  lcl.Fold.diff.P95 <- round(logbase ^ (diffmean - qt(1-Alpha/2,n-1)*sqrt(vardiff)), Round.To )
  ucl.Fold.diff.P95 <- round(logbase ^ (diffmean + qt(1-Alpha/2,n-1)*sqrt(vardiff)), Round.To )

  Fold.diff <- round(logbase^diffmean,Round.To)
  lcl.Fold.diff.P90 <- round(logbase ^ (diffmean - qt(1-Alpha,  n-1)*sqrt(vardiff)), Round.To )
  ucl.Fold.diff.P90 <- round(logbase ^ (diffmean + qt(1-Alpha,  n-1)*sqrt(vardiff)), Round.To )

  ###### Update 95%CI for the pairwise comparison


  Xbar.Ori <- mean(xx)
  Ybar.Ori <- mean(yy)
  Sxx.Ori  <- sum((xx-Xbar.Ori)^2)
  Syy.Ori  <- sum((yy-Ybar.Ori)^2)
  Sxy.Ori  <- sum((xx-Xbar.Ori)%*%(yy-Ybar.Ori))
  # print(paste("X.Bar, Y.Bar, Sxx, Syy, Sxy, Beta = ", Xbar.Ori, Ybar.Ori, Sxx.Ori, Syy.Ori, Sxy.Ori, beta, sep = ", "));

  Pairwise.Agreement     <- 0;
  Var.Agreement          <- (Sxx - Sxy/beta)/n
  lcl.Pairwise.Agreement <- round(Pairwise.Agreement - qt(1-Alpha/2,n-1)*sqrt(2)*sqrt(Var.Agreement), Round.To);
  ucl.Pairwise.Agreement <- round(Pairwise.Agreement + qt(1-Alpha/2,n-1)*sqrt(2)*sqrt(Var.Agreement), Round.To);


  #Information in the Table


  Slope.Table.P90   	  <- cbind("Conc. Slope (90%CI)", Slope,           lcl.Slope.P90,          ucl.Slope.P90)
  Slope.Table.P95   	  <- cbind("Conc. Slope (95%CI)", Slope,           lcl.Slope.P95,          ucl.Slope.P95)
  Intercept.Table.P90  	  <- cbind("Intercept (90%CI)",  Intercept,        lcl.Intercept.P90,      ucl.Intercept.P90)
  Intercept.Table.P95   	  <- cbind("Intercept (95%CI)",  Intercept,        lcl.Intercept.P95,      ucl.Intercept.P95)
  Fold.Rise.Table    	  <- cbind("Fold Rise",          Fold.Rise,          lcl.Fold.Rise,          ucl.Fold.Rise)
  Average.Diff.Table.P90   <- cbind("Average Diff. (90%CI)", Avg.Diff,       lcl.Avg.Diff.P90,       ucl.Avg.Diff.P90)
  Average.Diff.Table.P95	  <- cbind("Average Diff. (95%CI)", Avg.Diff,       lcl.Avg.Diff.P95,       ucl.Avg.Diff.P95)
  Fold.Diff.Table.P90      <- cbind("Fold Difference (90%CI)",    Fold.diff,      lcl.Fold.diff.P90,      ucl.Fold.diff.P90)
  Fold.Diff.Table.P95      <- cbind("Fold Difference (95%CI)",    Fold.diff,      lcl.Fold.diff.P95,      ucl.Fold.diff.P95)
  Pairwise.Agreement.Table <- cbind( "Pairwise Agreement",    paste(Pairwise.Agreement, "/", round(Var.Agreement, 4), sep = ""), lcl.Pairwise.Agreement, ucl.Pairwise.Agreement)
  Rsquare.Table      	  <- cbind("R square",               rsquared,           "-",                    "-")


  list   (Slope.Table.P90     	 = Slope.Table.P90,
          Slope.Table.P95     	 = Slope.Table.P95,
          Intercept.Table.P90 	 = Intercept.Table.P90,
          Intercept.Table.P95 	 = Intercept.Table.P95,
          Fold.Rise.Table 	 = Fold.Rise.Table,
          Average.Diff.Table.P90 	 = Average.Diff.Table.P90,
          Average.Diff.Table.P95 	 = Average.Diff.Table.P95,
          Fold.Diff.Table.P90  	 = Fold.Diff.Table.P90,
          Fold.Diff.Table.P95  	 = Fold.Diff.Table.P95,
          Pairwise.Agreement.Table = Pairwise.Agreement.Table,
          Rsquare.Table 		 = Rsquare.Table,
          Slope                    = beta,
          Intercept                = aa,
          Residuals                = Residuals,
          ubounds.95CI             = ubounds.95CI,
          lbounds.95CI             = lbounds.95CI)

}



###############################################################################
######################### Graphs     ##########################################
###############################################################################


Concordance.AltmanBland.Graph <- function (

  #Loading Table
  Data.File.Name = Data.File.Name,
  Rawdata        = Rawdata,
  Freq           = Freq,
  Profile.Legend = Profile.Legend,

  X.Var          = X.Var,
  Y.Var          = Y.Var,

  X.Concordance.Profile.Legend     = -999,
  Y.Concordance.Profile.Legend     = -999,

  X.AltmanBland.Profile.Legend     = -999,
  Y.AltmanBland.Profile.Legend     = -999,



  # General parameters

  Alpha          = 0.05,
  logbase        = logbase,
  Title          = "Default Title",
  Postfix        = "0",

  #Concordance parameters
  Concordance.X.Label          = "Default x label",
  Concordance.Y.Label          = "Default y label",

  Concordance.Draw.Conc.Line   = TRUE,
  Concordance.Need.Profile.Legend = FALSE,
  Concordance.Need.95CI.Band   = FALSE,
  Concordance.Range.Band	     = FALSE,
  Concordance.Perfect.Band     = FALSE,
  Concordance.Range.Band.Value = 1,
  Concordance.Sizable.Graph    = FALSE,
  Concordance.Assumptions	   	 = FALSE,
  Concordance.Assumptions.Label.X  = "",
  Concordance.X.Annotation   	 = -1,
  Concordance.Y.Annotation   	 = -1,
  Concordance.Intercept.Value.plot = FALSE,
  Title.concordance = Title.concordance,

  # Axis Parameters (Concordance and AB share the same X.Ticks)


  X.Ticks                            = c(),
  X.Tick.Labels                      = c(),
  X.Min                              = 0,
  X.Max          		                 = 5,
  AltmanBland.Y.Ticks                = c(0, 1, 2, 3, 4),
  AltmanBland.Y.Min                  = 0,
  AltmanBland.Y.Max                  = 4,
  Concordance.Y.Axis.Residuals       = 0,
  Concordance.Y.Ticks.Residuals      = c(),
  #Altman Bland Parameters



  AltmanBland.X.Label              	 = "Default x label",
  AltmanBland.Y.Label             	 = "Default y label",

  AltmanBland.Need.95CI.Band      	 = FALSE,
  AltmanBland.Show.95CI           	 = FALSE,
  AltmanBland.Plot.Log.Diff        	 = FALSE,
  AltmanBland.Draw.Mean.STD  	       = FALSE,
  AltmanBland.Draw.Perfect.Line      = FALSE,
  AltmanBland.Draw.Regression.Line   = TRUE,
  AltmanBland.Plot.Reference.X.Axis  = FALSE,
  Title.altman                       = Title.altman

)

{
  ###Part 0: Dataset

  X <- Rawdata$X.Var
  Y <- Rawdata$Y.Var

  Log.X <- log(X)/log(logbase)
  Log.Y <- log(Y)/log(logbase)

  Size.Of.Valid.Data <- length (X)


  ###PART I: CONCORDANCE


  Concordance <- Concordance.Computation ( Rawdata          = Rawdata,
                                           Freq             = Freq,
                                           X.Var            = X.Var,
                                           Y.Var            = Y.Var,
                                           logbase          = logbase,
                                           Concordance.xmin = X.Min,
                                           Concordance.xmax = X.Max,
                                           Alpha            = Alpha
  )

  #
  #       pdf(file = paste(LOCATIONout, Postfix, ".pdf", sep = ""),
  #       paper = "Letter", width = 8, height = 8);
  #
  #       par(oma = c(4, 3, 7, 4), font = 1.5, font.axis = 1.5, font.main = 1.5,
  #           font.lab = 1, cex.axis = 1, cex.lab = 1, cex.main = 1,cex.sub=1);

  ## ALexis ROTUREAU 07JUL2023
  png(paste0(output.folder,"/", concordance.folder,"/Concordance_plot",Postfix,".png"));

  #layout(matrix(c(1,2,3,4),2));

  Concordance.xlab <- Concordance.X.Label;
  Concordance.ylab <- Concordance.Y.Label;


  Concordance.xmin <- X.Min;
  Concordance.ymin <- X.Min;
  Concordance.xmax <- X.Max;
  Concordance.ymax <- X.Max;


  X.Seq <- seq ( Concordance.xmin, Concordance.xmax, length = 250);

  plot(-9999999999, -9999999999, type = "n",
       xlim = c(Concordance.xmin,Concordance.xmax), ylim = c(Concordance.ymin,Concordance.ymax),
       xlab = Concordance.xlab, ylab = Concordance.ylab, cex.axis = 0.90, axes = F, main = "");


  # Pauline Jurvilliers 11/17/2017: we add title to the plot
  #title(main = Title.concordance)




  for ( i in 1:Size.Of.Valid.Data )
  {
    Profile <- as.numeric(Rawdata$Profile[i]);

    if ( Profile < 0 ) { points(Log.X[i], Log.Y[i]);
    }
    else
    {

      points(Log.X[i], Log.Y[i], lwd = Points.Line.Width,
             cex = Profile.Size [Profile],
             pch = Profile.Style[Profile],
             col = Profile.Color[Profile]
      );
    }
  }




  # Sizable graph / Size of the points function of the weight

  Marked <- FALSE;
  for ( i in 2:Size.Of.Valid.Data )
  {
    Marked <- FALSE;
    for ( j in 1:(i - 1) )
    {

      # print(paste("n,i,j = ", Size.Of.Valid.Data,i,j));

      if (( Rawdata$X.Var[i] == Rawdata$X.Var[j] ) &
          ( Rawdata$Y.Var[i] == Rawdata$Y.Var[j] ) & !Marked)
      {
        Rawdata$Freq[j]  <- Rawdata$Freq[j] + 1;
        Rawdata$Freq[i]  <- -1; Marked = TRUE;
      }
    }
  }


  if ( Concordance.Sizable.Graph )
  {
    for ( i in 1:Size.Of.Valid.Data )
    {
      if ( Rawdata$Freq[i] > 0 )
      {
        points(log(Rawdata$X.Var[i])/log(logbase), log(Rawdata$Y.Var[i])/log(logbase),
               cex = 1.0 + min(Rawdata$Freq[i]-1, Size.Max) * Size.Increment);
      }
    }
  }

  #else {
  #       points(Log.X, Log.Y);
  #}

  # creation of the Axis

  if ( Concordance.xmin < 0 && COORDINATE.AT.ORIGIN )
  {
    if ( length(X.Ticks) == 0 )
    {
      axis(1, cex=.75, pos = 0, lty = 1, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
      axis(2, cex=.75, pos = 0, lty = 1, las = 2, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
    }

    else if ( length(X.Tick.Labels) == 0 )
    {
      axis(1, cex=.75, pos = 0, lty = 1, at = X.Ticks, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
      axis(2, cex=.75, pos = 0, lty = 1, at = X.Ticks, las = 2, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
    }
    else
    {
      axis(1, cex=.75, pos = 0, lty = 1, at = X.Ticks, labels = X.Tick.Labels, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
      axis(2, cex=.75, pos = 0, lty = 1, at = X.Ticks, labels = X.Tick.Labels, las = 2, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
    }
  }

  else
  {
    if ( length(X.Ticks) == "" )
    {
      axis(1, cex=.75, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
      axis(2, cex=.75, las = 2, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
    }


    else if ( length(X.Tick.Labels) == 0 )
    {
      axis(1, cex=.75, at = X.Ticks, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
      axis(2, cex=.75, at = X.Ticks, las = 2, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
    }


    else
    { axis(1, cex=.75, at = X.Ticks, labels = X.Tick.Labels, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
      axis(2, cex=.75, at = X.Ticks, labels = X.Tick.Labels, las = 2, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
    }
  }

  #Text on the graph

  Text.X.Pos  = Concordance.xmin + ( Concordance.xmax - Concordance.xmin ) / 1.9;
  Text.Height = ( Concordance.ymax - Concordance.ymin ) / 18;
  Text.Y.Pos  = Concordance.ymin + ( Concordance.xmax - Concordance.xmin ) / 2.7;

  if ( Concordance.X.Annotation > 0 ) Text.X.Pos <- Concordance.X.Annotation;
  if ( Concordance.Y.Annotation > 0 ) Text.Y.Pos <- Concordance.Y.Annotation;



  if ( Concordance.Need.95CI.Band )
  {
    Concordance.String <- paste("Concordance Slope =", Concordance$Slope.Table.P90[2],    " (", Concordance$Slope.Table[3],     ", ", Concordance$Slope.Table[4],     ")", sep = "");
    Intercept.String   <- paste("Intercept =",         Concordance$Intercept.Table[2]," (", Concordance$Intercept.Table[3], ", ", Concordance$Intercept.Table[4], ")", sep = "");
  }
  else
  {
    Concordance.String <- paste("Concordance Slope =", Concordance$Slope.Table.P90[2]);
    Intercept.String   <- paste("Intercept =",         Concordance$Intercept.Table[2]);
  }

  if ( Concordance.Draw.Conc.Line ) {
    if ( Show.Slope.95CI ) { text(Text.X.Pos, Text.Y.Pos, Concordance.String, adj=0, cex=0.8); }

    if ( Concordance.Intercept.Value.plot )
    {
      text(Text.X.Pos, Text.Y.Pos - Text.Height, Intercept.String, adj=0, cex=0.8)
    }
  }
  box(bty= "o")

  # Add line and 95% CI etc to the graph

  legtxt <- c();

  if ( Concordance.Draw.Conc.Line )
  {
    abline(Concordance$Intercept, Concordance$Slope, col = 2, lwd = 2)
    abline(a=0, b=1, lty=2, col= 4, lwd = 2)
    legtxt   <- c("Observed Concordance Line", "Perfect Concordance Line");
  }

  lty.list <- c(1,2)
  pch.list <- c(-1, -1)
  col.list <- c(2,4)
  lwd.list <- c(2,2)

  if ( Concordance.Range.Band )   { Band.Legend.Text <- "Observed Conc. Line +/- 2-Fold"; }
  if ( Concordance.Perfect.Band ) { Band.Legend.Text <- "Perfect Concordance Line +/- 2-Fold"; }


  if (Concordance.Need.95CI.Band)
  {
    lines(X.Seq, Concordance$ubounds.95CI, col = Reference.Line.Color, lty = 4, lwd = Reference.Line.Thickness, cex=0.7);
    lines(X.Seq, Concordance$lbounds.95CI, col = Reference.Line.Color, lty = 4, lwd = Reference.Line.Thickness, cex=0.7);
    legtxt   <- c(legtxt, "95% Confidence Bound")
    pch.list <- c(pch.list, -1);
    lty.list <- c(lty.list, 4)
    col.list <- c(col.list, Reference.Line.Color)
    lwd.list <- c(lwd.list, Reference.Line.Thickness)
  }


  if (Concordance.Range.Band)
  {
    Y.Hat <- Concordance$Intercept + Concordance$Slope * X.Seq;
    range.ubounds <- Y.Hat + Concordance.Range.Band.Value;
    range.lbounds <- Y.Hat - Concordance.Range.Band.Value;
    lines(X.Seq, range.ubounds, col = Reference.Line.Color, lty = 4, lwd = Reference.Line.Thickness,  cex=0.7);
    lines(X.Seq, range.lbounds, col = Reference.Line.Color, lty = 4, lwd = Reference.Line.Thickness,  cex=0.7);
    legtxt   <- c(legtxt, Band.Legend.Text)
    lty.list <- c(lty.list,4)
    pch.list <- c(pch.list, -1);
    col.list <- c(col.list, Reference.Line.Color)
    lwd.list <- c(lwd.list, Reference.Line.Thickness)
  }

  if (Concordance.Perfect.Band)
  {
    abline( 1.0, 1.0, col = Reference.Line.Color, lty = 4, lwd = Reference.Line.Thickness, cex=0.7);
    abline(-1.0, 1.0, col = Reference.Line.Color, lty = 4, lwd = Reference.Line.Thickness, cex=0.7);
    legtxt   <- c(legtxt, Band.Legend.Text)
    lty.list <- c(lty.list, 4)
    pch.list <- c(pch.list, -1);
    col.list <- c(col.list, Reference.Line.Color)
    lwd.list <- c(lwd.list, Reference.Line.Thickness)
  }

  if ( Concordance.Need.Profile.Legend ) {

    legend( X.Concordance.Profile.Legend, Y.Concordance.Profile.Legend,
            legend = Profile.Legend,
            col    = Profile.Color,
            pch    = Profile.Style,
            #lwd    = Profile.Size,
            cex    = 0.7
    );
  }

  #     if ( Concordance.Draw.Conc.Line ) {
  #          legtxt   <- c(legtxt, Profile.Legend);
  #          pch.list <- c(pch.list, Profile.Style);
  #          col.list <- c(col.list, Profile.Color);
  #	        lwd.list <- c(lwd.list, Profile.Size);
  #     } else {
  #          legtxt   <- c(Profile.Legend);
  #          pch.list <- c(Profile.Style);
  #          col.list <- c(Profile.Color);
  #	        lwd.list <- c(Profile.Size);
  #          lty.list <- c(1,1,1,1,1,1,1);
  #     }
  #}

  if ( Concordance.Draw.Conc.Line ) {
    if ( Concordance.xmin > -0.01 | abs( Concordance.xmin + Concordance.xmax ) < 0.01 | Concordance.xmax < VERY.SMALL.VALUE )
    {

      if ( Concordance.xmax < VERY.SMALL.VALUE )
      {
        offset = ( Concordance.xmax - Concordance.xmin ) / 8.0;

      }    else { offset = 0.0; }

      legend(Concordance.xmax / 20 + Concordance.xmin, Concordance.ymax, legend = legtxt, col = col.list, pch = pch.list, lty=lty.list, lwd=lwd.list, cex=0.7);

    }
    else
    {
      legend(Concordance.xmax / 20, Concordance.ymax, legend = legtxt, col = col.list, lty=lty.list, pch = pch.list, lwd=lwd.list, cex=0.7);
    }
  }
  ## ALexis ROTUREAU 07JUL2023
  dev.off()


  ### PART II : VERIFICATION OF THE HYPOTHESIS FOR THE CONCORDANCE


  Yhat <- Concordance$Intercept + Concordance$Slope * Log.X;


  if (Concordance.Assumptions)
  {

    # RESIDUALS

    max.y.residuals.axis <- max(max(abs(Concordance$Residuals)),3)
    if (Concordance.Y.Axis.Residuals > max.y.residuals.axis) { max.yres <- Concordance.Y.Axis.Residuals }
    else {max.yres<- max.y.residuals.axis}

    ## ALexis ROTUREAU 07JUL2023
    # png("~/biostat-dashboard/RPackage/Graph2.png");

    plot(Yhat,Concordance$Residuals,xlab=paste(Concordance.Assumptions.Label.X," Predicted Values",sep="") ,ylab="Residuals" ,
         xlim = c(Concordance.xmin,Concordance.xmax), ylim=c(-max.yres,max.yres),axes = F);

    ## ALexis ROTUREAU 07JUL2023
    # dev.off()

    # creation of the Axis

    if ( Concordance.xmin < 0 )
    {
      if ( length(Concordance.Y.Ticks.Residuals) == "" )
      {
        if ( length(X.Ticks) == "" )
        {
          axis(1, cex=.75, pos = 0, lty = 1, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
          axis(2, cex=.75, pos = 0, lty = 1, las = 2, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
        }
        else
        {
          axis(1, cex=.75, pos = 0, lty = 1, at = X.Ticks, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
          axis(2, cex=.75, pos = 0, lty = 1, las = 2, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
        }
      }
      else
      {
        if ( length(X.Ticks) == "" )
        {
          axis(1, cex=.75, pos = 0, lty = 1, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
          axis(2, cex=.75, pos = 0, lty = 1, at = Concordance.Y.Ticks.Residuals, las = 2, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
        }
        else
        {
          axis(1, cex=.75, pos = 0, lty = 1, at = X.Ticks, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
          axis(2, cex=.75, pos = 0, lty = 1, at = Concordance.Y.Ticks.Residuals, las = 2, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
        }
      }
    }
    else
    {
      if ( length(Concordance.Y.Ticks.Residuals) == "" )
      {
        if ( length(X.Ticks) == "" )
        {
          axis(1, cex=.75, lty = 1, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
          axis(2, cex=.75, lty = 1, las = 2, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
        }
        else
        {
          axis(1, cex=.75, lty = 1, at = X.Ticks, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
          axis(2, cex=.75, lty = 1, las = 2, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
        }
      }
      else
      {
        if ( length(X.Ticks) == "" )
        {
          axis(1, cex=.75, lty = 1, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
          axis(2, cex=.75, lty = 1, at = Concordance.Y.Ticks.Residuals, las = 2, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
        }
        else
        {
          axis(1, cex=.75, lty = 1, at = X.Ticks, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
          axis(2, cex=.75, lty = 1, at = Concordance.Y.Ticks.Residuals, las = 2, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
        }
      }
    }


    abline(h=0,col="green",lty=2);
    abline(h=-3,col="red",lty=2);
    abline(h=3,col="red",lty=2);
    # To find the outliers;

    box(bty= "o");

    ## ALexis ROTUREAU 07JUL2023
    # dev.off();


    #print(Concordance$Residuals);



    #NORMALITY

    qqnorm(Concordance$Residuals,main = "", xlab = paste(Concordance.Assumptions.Label.X," Theoritical Quantiles",sep=""));
    qqline(Concordance$Residuals,col="red");

  }

  pvalue.normality<-round(shapiro.test(Concordance$Residuals)[[2]],5);
  pvalue.normality1 <-ifelse(pvalue.normality <0.0001,"<0.0001",pvalue.normality);
  #print (paste("p-value of the normality test = ", pvalue.normality1 , sep=""))



  ### PART III: ALTMAN BLAND GRAPH

  #GMT, GMT Ratio and their confidence Intervals

  AB <- ALTMAN.BLAND (
    Rawdata	 	 = Rawdata,
    X.Var     		 = X.Var,
    Y.Var     		 = Y.Var,
    X.Var.GMT     	 = X.Var.GMT,
    Y.Var.GMT     	 = Y.Var.GMT,

    logbase        	 = logbase,
    Alpha          	 = Alpha,
    Plot.Log.Diff  	 = AltmanBland.Plot.Log.Diff,
    Plot.Reference.X.Axis = AltmanBland.Plot.Reference.X.Axis


  )


  #Plot of Altman Bland

  Regressor     <- AB$Regressor;
  Size.Of.Valid.Data <- length(Regressor);

  if (AltmanBland.Plot.Reference.X.Axis) {X.Axis.Variable <- AB$X.Log.Var.Ref}
  else                                   {X.Axis.Variable <- AB$X.Y.Individual.Log.Mean}


  X.Y.Individual.Log.Diff.Mean <- AB$X.Y.Individual.Log.Diff.Mean
  X.Y.Individual.Log.Diff.STD  <- AB$X.Y.Individual.Log.Diff.STD
  AB.Slope     <- AB$Slope
  AB.Intercept <- AB$Intercept

  # ALexis ROTUREAU 07JUL2023
  png(paste0(output.folder,"/", concordance.folder,"/AltmanBland_plot",Postfix,".png"));

  plot(-9999999999, -9999999999, type = "p",
       xlim = c(X.Min, X.Max), ylim = c(AltmanBland.Y.Min, AltmanBland.Y.Max),
       xlab = AltmanBland.X.Label, ylab = AltmanBland.Y.Label, axes = F, main = "");


  # Pauline Jurvilliers 11/17/2017: we add title to the plot
  #title(main = Title.altman)


  for ( i in 1:Size.Of.Valid.Data )
  {
    Profile <- as.numeric(Rawdata$Profile[i]);

    if ( Profile < 0 ) { points(X.Axis.Variable[i], Regressor[i]);
    }
    else
    {

      points(X.Axis.Variable[i], Regressor[i], lwd = Points.Line.Width,
             cex = Profile.Size [Profile],
             pch = Profile.Style[Profile],
             col = Profile.Color[Profile]
      );
    }
  }


  box(bty= "o");

  if ( AltmanBland.Plot.Log.Diff )
  {
    Legend.Text.1 <- "Avg. log2 Difference";
    Legend.Text.2 <- "Avg. log2 Difference +/- 2STD";
  }
  else
  {
    Legend.Text.1 <- "Geometric Mean Ratio (GMR)";
    Legend.Text.2 <- "GMR +/- 2STD";
  }


  if ( AltmanBland.Draw.Regression.Line ) {

    if ( AltmanBland.Draw.Mean.STD & AltmanBland.Draw.Perfect.Line ) {
      legend(X.Min, AltmanBland.Y.Max, cex=0.7, legend = c("Regression Line", Legend.Text.1, Legend.Text.2, "Perfect Agreement"),
             col = c(2, Reference.Center.Line.Color, Reference.Line.Color, Perfect.Line.Color),
             lty = c(1, Mean.STD.Style, Mean.STD.Style2, Perfect.Line.Style), lwd = c(2,2,2,2));
    }

    if ( AltmanBland.Draw.Mean.STD & !AltmanBland.Draw.Perfect.Line ) {
      legend(X.Min, AltmanBland.Y.Max, cex=0.7, legend = c("Regression Line", "Mean", "Mean +/- 2STD"),lwd = c(2,2,2),
             col = c(2, Reference.Center.Line.Color, Reference.Line.Color), lty=c(1, Mean.STD.Style, Mean.STD.Style2));
    }

    if ( !AltmanBland.Draw.Mean.STD & AltmanBland.Draw.Perfect.Line ) {
      legend(X.Min, AltmanBland.Y.Max, cex=0.7, legend = c("Regression Line", "Perfect Agreement"),
             col = c(2, Perfect.Line.Color), lty=c(1, Perfect.Line.Style), lwd = c(2,2));
    }

    if ( !AltmanBland.Draw.Mean.STD & !AltmanBland.Draw.Perfect.Line ) {
      legend(X.Min, AltmanBland.Y.Max, legend = c("Regression Line"), col = c(2), lty=c(1), lwd=2, cex=0.7);
    }

  }


  else {

    if ( AltmanBland.Draw.Mean.STD & AltmanBland.Draw.Perfect.Line ) {
      legend(X.Min, AltmanBland.Y.Max, cex=0.7, legend = c(Legend.Text.1, Legend.Text.2, "Perfect Agreement"), lwd = c(2,2,2,2),
             col = c(Reference.Center.Line.Color, Reference.Line.Color, Perfect.Line.Color), lty=c(Mean.STD.Style, Mean.STD.Style2, Perfect.Line.Style));
    }

    if ( AltmanBland.Draw.Mean.STD & !AltmanBland.Draw.Perfect.Line ) {
      legend(X.Min, AltmanBland.Y.Max, cex=0.7, legend = c("Mean", "Mean +/- 2STD"), lwd = c(2,2,2),
             col = c(Reference.Center.Line.Color, Reference.Line.Color), lty=c(Mean.STD.Style, Mean.STD.Style2));
    }

    if ( !AltmanBland.Draw.Mean.STD & AltmanBland.Draw.Perfect.Line ) {
      legend(X.Min, AltmanBland.Y.Max, cex=0.7, legend = c("Perfect Agreement"),
             col = c(Perfect.Line.Color), lty=c(Perfect.Line.Style), lwd = 2);
    }
  }


  axis(1, cex=.75, lty = 1, at = X.Ticks, labels = X.Tick.Labels, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);
  axis(2, cex=.75, lty = 1, las = 2, at = AltmanBland.Y.Ticks, cex.axis = Axis.Text.Size, lwd = Axis.Line.Size);


  if ( AltmanBland.Draw.Regression.Line ) abline(AB.Intercept, AB.Slope, col = 2, lwd = 2);

  if ( AltmanBland.Draw.Mean.STD ) {
    Text.Hight = ( AltmanBland.Y.Max - AltmanBland.Y.Min ) / 50.0;

    if ( AltmanBland.Plot.Log.Diff ) {
      abline(X.Y.Individual.Log.Diff.Mean, 0, col = Reference.Center.Line.Color, lty = Mean.STD.Style, lwd = Reference.Line.Thickness);
      abline(X.Y.Individual.Log.Diff.Mean - 2 * X.Y.Individual.Log.Diff.STD, 0, lty = Mean.STD.Style2, col = Reference.Line.Color, lwd = Reference.Line.Thickness);
      abline(X.Y.Individual.Log.Diff.Mean + 2 * X.Y.Individual.Log.Diff.STD, 0, lty = Mean.STD.Style2, col = Reference.Line.Color, lwd = Reference.Line.Thickness);
    }
    else {
      abline(2** X.Y.Individual.Log.Diff.Mean, 0, col = Reference.Center.Line.Color, lty = Mean.STD.Style, lwd = Reference.Line.Thickness);
      abline(2**(X.Y.Individual.Log.Diff.Mean - 2 * X.Y.Individual.Log.Diff.STD), 0, lty = Mean.STD.Style2, col = Reference.Line.Color, lwd = Reference.Line.Thickness);
      abline(2**(X.Y.Individual.Log.Diff.Mean + 2 * X.Y.Individual.Log.Diff.STD), 0, lty = Mean.STD.Style2, col = Reference.Line.Color, lwd = Reference.Line.Thickness);
    }
  }

  if ( AltmanBland.Draw.Perfect.Line ) {
    if ( AltmanBland.Plot.Log.Diff ) { abline(0, 0, lty = Perfect.Line.Style, col = Perfect.Line.Color, lwd = Perfect.Line.Thickness); }
    else { abline(1, 0, lty = Perfect.Line.Style, col = Perfect.Line.Color, lwd = Perfect.Line.Thickness); }
  }

  if ( Concordance.Need.Profile.Legend ) {

    legend( X.AltmanBland.Profile.Legend, Y.AltmanBland.Profile.Legend,
            legend = Profile.Legend,
            col    = Profile.Color,
            pch    = Profile.Style,
            #lwd    = Profile.Size,
            cex    = 0.7
    );
  }



  # ALexis ROTUREAU 07JUL2023
  # dev.off()
  # dev.off()

  ###PART IV: RESULTS



  Concordance.Normality.Residuals <- cbind("P-value Normality",pvalue.normality1, "-","-")


  Concordance.Table <- rbind ( Concordance$Slope.Table.P90,
                               Concordance$Slope.Table.P95,
                               Concordance$Intercept.Table.P90,
                               Concordance$Intercept.Table.P95,
                               Concordance$Average.Diff.Table.P90,
                               Concordance$Average.Diff.Table.P95,
                               Concordance$Fold.Rise.Table,
                               Concordance$Fold.Diff.Table.P90,
                               Concordance$Fold.Diff.Table.P95,
                               Concordance$Pairwise.Agreement.Table,
                               Concordance$Rsquare.Table,
                               Concordance.Normality.Residuals)

  ## ALexis ROTUREAU 07JUL2023
  dev.off()


  list(Concordance.Table    = Concordance.Table,
       AltmanBland.Table = AB$Altman.Bland.Table)

}






###############################################################################
######################### Altman Bland ########################################
###############################################################################


ALTMAN.BLAND <- function (
    Rawdata	 	 = Rawdata,
    X.Var     		 = X.Var,
    Y.Var     		 = Y.Var,
    X.Var.GMT     	 = X.Var.GMT,
    Y.Var.GMT     	 = Y.Var.GMT,

    logbase        	 = logbase,
    Alpha          	 = 0.05,
    Plot.Log.Diff  	 = FALSE,
    Plot.Reference.X.Axis = AltmanBland.Plot.Reference.X.Axis


)

{


  Size.Of.Valid.Data <- length(Rawdata$X.Var)
  GMT.Ratio.Data <- data.frame( X.Var = log10( Rawdata$X.Var.GMT ),
                                Y.Var = log10( Rawdata$Y.Var.GMT ));

  Covariance.Matrix <- cor(GMT.Ratio.Data);
  Rho <- Covariance.Matrix[1,2];

  Mean.X <- mean(log10(Rawdata$X.Var.GMT));
  Mean.Y <- mean(log10(Rawdata$Y.Var.GMT));
  Mean.Diff <- Mean.Y - Mean.X;

  GMT.X <- 10 ^ Mean.X;
  GMT.Y <- 10 ^ Mean.Y;
  GMT.Ratio <- GMT.Y / GMT.X;

  Y.To.X.Ratio                 <- Rawdata$Y.Var / Rawdata$X.Var;
  X.Y.Individual.GMT           <- sqrt(Rawdata$Y.Var * Rawdata$X.Var);
  X.Y.Individual.Log.Diff      <- log(Y.To.X.Ratio) / log(2);
  X.Y.Individual.Log.Mean      <- log(X.Y.Individual.GMT) / log(logbase);
  X.Log.Var.Ref		   <- log(Rawdata$X.Var) / log(logbase);

  X.Y.Individual.Log.Diff.Mean <- mean(X.Y.Individual.Log.Diff);
  X.Y.Individual.Log.Diff.STD  <- sd(X.Y.Individual.Log.Diff);
  X.Y.Individual.Log.Diff.SE   <- X.Y.Individual.Log.Diff.STD/sqrt(Size.Of.Valid.Data);

  # if ( DEBUG == "ON" ) print(Y.To.X.Ratio);
  # if ( DEBUG == "ON" ) print(X.Y.Individual.GMT);
  # if ( DEBUG == "ON" ) print(X.Y.Individual.Log.Mean);


  if ( Plot.Log.Diff ) { Regressor     <- X.Y.Individual.Log.Diff;}
  else                 { Regressor     <- Y.To.X.Ratio; }

  if(Plot.Reference.X.Axis) {X.Variable <- X.Log.Var.Ref}
  else 			{X.Variable <- X.Y.Individual.Log.Mean}

  Mean.Regressor <- mean(Regressor);
  STD.Regressor  <- sd(Regressor);


  Linear.Relation <- lm(Regressor ~ X.Variable);
  Linear.Relation.Res <- unlist (summary(Linear.Relation));
  Linear.Relation.Hyp <- summary(Linear.Relation)[[4]]

  LowCI95.Slope <- round(confint(Linear.Relation)[2,1],Round.To)
  UppCI95.Slope <- round(confint(Linear.Relation)[2,2],Round.To)

  LowCI95.Intercept <- round(confint(Linear.Relation)[1,1],Round.To)
  UppCI95.Intercept <- round(confint(Linear.Relation)[1,2],Round.To)

  PValue.Intercept.Rawdata <- Linear.Relation.Hyp[1,4];
  PValue.Intercept.String.0  <- ifelse(PValue.Intercept.Rawdata <0.0001, "<0.0001", round(PValue.Intercept.Rawdata, 4));

  PValue.Slope.Rawdata <- Linear.Relation.Hyp[2,4]
  PValue.Slope.String.0  <- ifelse(PValue.Slope.Rawdata <0.0001,"<0.0001",round(PValue.Slope.Rawdata, 4));

  RSquare   <- round(Linear.Relation.Res$r.squared, 4); #Round.To)
  Intercept <- round(Linear.Relation.Res$coefficients1, Round.To)
  Slope     <- round(Linear.Relation.Res$coefficients2, Round.To)

  YHat <- Slope * X.Variable + Intercept
  MSE  <- sum( ( Y.To.X.Ratio - YHat ) ^ 2 ) / ( Size.Of.Valid.Data - 2 )

  # TEST INTERCEPT = 1

  Linear.Relation.1 <- lm(Regressor -1 ~ X.Variable);
  Linear.Relation.Hyp.1 <- summary(Linear.Relation.1)[[4]]
  PValue.Intercept.Rawdata.1 <- Linear.Relation.Hyp.1[1,4];
  PValue.Intercept.String.1  <- ifelse(PValue.Intercept.Rawdata.1 <0.0001, "<0.0001", round(PValue.Intercept.Rawdata.1, 4));

  # CREATION OF THE FINAL TABLE

  restable1 <- matrix(0,1,6)
  restable2 <- matrix(0,1,6)
  restable3 <- matrix(0,1,6)
  restable4 <- matrix(0,1,6)
  restable5 <- matrix(0,1,6)

  if ( Plot.Log.Diff )
  {
    restable1[1,] <- cbind("Log Diff +/- 2Std", round(X.Y.Individual.Log.Diff.Mean, Round.To),
                           round((X.Y.Individual.Log.Diff.Mean - 2 * X.Y.Individual.Log.Diff.STD), Round.To),
                           round((X.Y.Individual.Log.Diff.Mean + 2 * X.Y.Individual.Log.Diff.STD), Round.To),"-","-");
    restable5[1,] <- cbind("Log Diff (95%CI)", round(X.Y.Individual.Log.Diff.Mean, Round.To),
                           round((X.Y.Individual.Log.Diff.Mean + qt(Alpha/2, Size.Of.Valid.Data-1) * X.Y.Individual.Log.Diff.SE), Round.To),
                           round((X.Y.Individual.Log.Diff.Mean - qt(Alpha/2, Size.Of.Valid.Data-1) * X.Y.Individual.Log.Diff.SE), Round.To),"-","-");
  }
  else
  {
    restable1[1,] <- cbind("GMR +/- 2Std", round(2** X.Y.Individual.Log.Diff.Mean, Round.To),
                           round(2^(X.Y.Individual.Log.Diff.Mean - 2 * X.Y.Individual.Log.Diff.STD), Round.To),
                           round(2^(X.Y.Individual.Log.Diff.Mean + 2 * X.Y.Individual.Log.Diff.STD), Round.To),"-","-");
    restable5[1,] <- cbind("GMR (95%CI)", round(2** X.Y.Individual.Log.Diff.Mean, Round.To),
                           round(2^(X.Y.Individual.Log.Diff.Mean + qt(Alpha/2, Size.Of.Valid.Data-1) * X.Y.Individual.Log.Diff.SE), Round.To),
                           round(2^(X.Y.Individual.Log.Diff.Mean - qt(Alpha/2, Size.Of.Valid.Data-1) * X.Y.Individual.Log.Diff.SE), Round.To),"-","-");
  }


  restable2[1,] <- cbind("Slope",     Slope,     LowCI95.Slope,    UppCI95.Slope,     PValue.Slope.String.0,     "-")
  restable3[1,] <- cbind("Intercept", Intercept, LowCI95.Intercept,UppCI95.Intercept, PValue.Intercept.String.0, PValue.Intercept.String.1)
  restable4[1,] <- cbind("R square",  RSquare ,  "-",              "-",               "-",                       "-")


  Altman.Bland.Table <- rbind (restable1, restable5, restable2,  restable3, restable4)

  list (Altman.Bland.Table           = Altman.Bland.Table,
        Regressor 		   = Regressor,
        X.Y.Individual.Log.Diff.Mean = X.Y.Individual.Log.Diff.Mean,
        X.Y.Individual.Log.Diff.STD  = X.Y.Individual.Log.Diff.STD,
        X.Y.Individual.Log.Mean      = X.Y.Individual.Log.Mean,
        X.Log.Var.Ref                = X.Log.Var.Ref,
        Slope			   = Linear.Relation.Res$coefficients2,
        Intercept                    = Linear.Relation.Res$coefficients1 )



}

#*****************************************************************************#
#************************ Qualification **************************************#
#*****************************************************************************#



QUALIFICATION <- function (


  #Loading dataset parameters
  Data.File.Name 	= Data.File.Name,
  X.Var.Name     	= X.Var.Name,
  Y.Var.Name     	= Y.Var.Name,
  Profile.Var.Name    	= "",
  Profile.Legend    	= c(""),
  Group.Var.Name 	= "",
  Group.Var.Value 	= "",


  X.Concordance.Profile.Legend     = -999,
  Y.Concordance.Profile.Legend     = -999,

  X.AltmanBland.Profile.Legend     = -999,
  Y.AltmanBland.Profile.Legend     = -999,


  #General parameters
  Alpha          	= 0.05,
  logbase        	= logbase,
  Title          	= "Default Title",
  Postfix        	= "0",

  # Preparation of RawData Parameters

  LLOQ           = 0.0,
  X.LLOQ         = 0.0,
  Y.LLOQ         = 0.0,
  Upper.Bound    = 9999999999,
  X.Upper.Bound  = 9999999999,
  Y.Upper.Bound  = 9999999999,
  Remove.Above.Upper.Bound = TRUE,

  #Concordance parameters
  Concordance.X.Label        		= "Default x label",
  Concordance.Y.Label        		= "Default y label",


  Concordance.Draw.Conc.Line           = TRUE,
  Concordance.Need.Profile.Legend      = FALSE,
  Concordance.Need.95CI.Band 		= FALSE,
  Concordance.Range.Band 		= FALSE,
  Concordance.Perfect.Band		= FALSE,
  Concordance.Sizable.Graph  		= FALSE,
  Concordance.Range.Band.Value 	= 1,
  Concordance.Assumptions		= FALSE,
  Concordance.Assumptions.Label.X      = "",
  Concordance.X.Annotation   		= -1,
  Concordance.Y.Annotation   		= -1,
  Concordance.Intercept.Value.plot 	= FALSE,
  Title.concordance = Title.concordance,


  #graph parameters (AB and Concordance shared some parameters)

  X.Ticks                          = c(),
  X.Tick.Label                     = c(),
  X.Min                            = 0,
  X.Max          		    = 5,
  AltmanBland.Y.Ticks              = c(0, 1, 2, 3, 4),
  AltmanBland.Y.Min                = 0,
  AltmanBland.Y.Max                = 4,
  Concordance.Y.Axis.Residuals     = 0,
  Concordance.Y.Ticks.Residuals    = c(),



  #Altman Bland Parameters


  AltmanBland.X.Label        	 = "Default x label",
  AltmanBland.Y.Label        	 = "Default y label",

  AltmanBland.Need.95CI.Band 	     = FALSE,
  AltmanBland.Show.95CI      	     = FALSE,
  AltmanBland.Plot.Log.Diff  	     = FALSE,
  AltmanBland.Draw.Mean.STD  	     = TRUE,
  AltmanBland.Draw.Perfect.Line     = FALSE,
  AltmanBland.Draw.Regression.Line  = FALSE,
  AltmanBland.Plot.Reference.X.Axis = FALSE,
  Title.altman = Title.altman,


  #Serostatus agreement parameters

  X.Cutoff       = -1,
  Y.Cutoff       = -1,

  #Agreement parameters

  Test.Type      = "ELISA",
  Agreement.Cutoff = 1



)
{

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #              Load Data and Clean data                                       #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

  ### Raw data for Concordance no Sample <LLOQ

  Concordance <- LOADING.TREATING.RAWDATA ( Data.File.Name = Data.File.Name,
                                            X.Var.Name     = X.Var.Name,
                                            Y.Var.Name     = Y.Var.Name,
                                            Profile.Var.Name = Profile.Var.Name,
                                            Group.Var.Name = Group.Var.Name,
                                            Group.Var.Value = Group.Var.Value,

                                            Remove.LLOQ    = "REMOVE.1.LAB",
                                            LLOQ           = LLOQ,
                                            X.LLOQ         = X.LLOQ,
                                            Y.LLOQ         = Y.LLOQ,

                                            Upper.Bound    = Upper.Bound,
                                            X.Upper.Bound  = X.Upper.Bound,
                                            Y.Upper.Bound  = Y.Upper.Bound,
                                            Remove.Above.Upper.Bound = Remove.Above.Upper.Bound

  )

  Concordance.RawData.No.LLOQ <- Concordance$RawData;

  ### Rawdata for Agreement (FAP)

  Agreement.RawData <- Concordance$RawData;

  ### RawData for the serostatus agreement (all data used)

  Serostatus <- LOADING.TREATING.RAWDATA ( Data.File.Name = Data.File.Name,
                                           X.Var.Name     = X.Var.Name,
                                           Y.Var.Name     = Y.Var.Name,
                                           Profile.Var.Name = Profile.Var.Name,
                                           Group.Var.Name  = Group.Var.Name,
                                           Group.Var.Value = Group.Var.Value,

                                           Remove.LLOQ    = FALSE,
                                           LLOQ           = LLOQ,
                                           X.LLOQ         = X.LLOQ,
                                           Y.LLOQ         = Y.LLOQ,

                                           Upper.Bound    = Upper.Bound,
                                           X.Upper.Bound  = X.Upper.Bound,
                                           Y.Upper.Bound  = Y.Upper.Bound,
                                           Remove.Above.Upper.Bound = Remove.Above.Upper.Bound

  )

  Serostatus.RawData <- Serostatus$RawData;


  ### Size of the RawData for the Concordance

  Size.Of.Valid.Data.Concordance <- length(Concordance.RawData.No.LLOQ$X.Var);

  ### Size of the RawData for the Agreement

  Size.Of.Valid.Data.Agreement <- length(Agreement.RawData$X.Var);

  ### Size of the RawData for the Concordance

  Size.Of.Valid.Data.Serostatus <- length(Serostatus.RawData$X.Var);


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #              GMT And GMT Ratio + 95% CI                                     #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

  GMT <- GMT.GMTRatio ( Rawdata        = Concordance.RawData.No.LLOQ,
                        Data.File.Name = Data.File.Name,
                        X.Var          = X.Var,
                        Y.Var          = Y.Var,
                        X.Var.GMT      = X.Var.GMT,
                        Y.Var.GMT      = Y.Var.GMT,
                        Paired         = TRUE,
                        Alpha          = Alpha
  )



  GMT.Ratio.Table <- GMT$GMT.Ratio.Table;


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #              Sensitivity And Serostatus agreement                           #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

  if ( X.Cutoff >= 0 | Y.Cutoff >= 0 ) #Antoine, 15Nov2023 : Temporary modification of >= instead of >
  {



    Serostatus <- Serostatus.Agreement  ( 	Rawdata   = Serostatus.RawData,
                                           X.Var.GMT = X.Var.GMT,
                                           Y.Var.GMT = Y.Var.GMT,
                                           X.Cutoff  = X.Cutoff,
                                           Y.Cutoff  = Y.Cutoff,
                                           Alpha     = Alpha
    )

    Serostatus.Agreement.Table <- rbind(Serostatus$Serostatus.Agreement.Table,
                                        Serostatus$Percent.Diff.Protected.Table);


  }

  else {Serostatus.Agreement.Table <- cbind("Serostatus Agreement","N/A","N/A","N/A")}

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #                            Agreement Analysis                               #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


  if (Test.Type == "ELISA") {Agreement.Table <- cbind("Agreement","N/A","N/A","N/A")}

  else
  {
    Agreement <- Agreement.Analysis.FAP  (
      Rawdata 	 = Agreement.RawData,
      X.Var   	 = X.Var,
      Y.Var   	 = Y.Var,
      Percent.Name 	 = "Agreement",
      Alpha   	 = Alpha,
      Agreement.Cutoff =Agreement.Cutoff
    )

    Agreement.Table <- Agreement$Agreement.FAP.Table;

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #                            Concordance analysis                             #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

  # Part IV: Concordance Analysis

  Concordance.and.AB <- Concordance.AltmanBland.Graph  (

    #Loading Table
    Data.File.Name = Data.File.Name,
    Rawdata        = Concordance.RawData.No.LLOQ,
    Freq           = Freq,
    X.Var          = X.Var,
    Y.Var          = Y.Var,

    X.Concordance.Profile.Legend     = X.Concordance.Profile.Legend,
    Y.Concordance.Profile.Legend     = Y.Concordance.Profile.Legend,

    X.AltmanBland.Profile.Legend     = X.AltmanBland.Profile.Legend,
    Y.AltmanBland.Profile.Legend     = Y.AltmanBland.Profile.Legend,

    Profile.Legend = c(Profile.Legend),

    # General parameters
    Alpha          = Alpha,
    logbase        = logbase,
    Title          = Title,
    Postfix        = Postfix,

    #Concordance parameters
    Concordance.X.Label              = Concordance.X.Label,
    Concordance.Y.Label              = Concordance.Y.Label,


    Concordance.Draw.Conc.Line       = Concordance.Draw.Conc.Line,
    Concordance.Need.95CI.Band       = Concordance.Need.95CI.Band,
    Concordance.Need.Profile.Legend  = Concordance.Need.Profile.Legend,
    Concordance.Range.Band	         = Concordance.Range.Band,
    Concordance.Perfect.Band         = Concordance.Perfect.Band,
    Concordance.Range.Band.Value     = Concordance.Range.Band.Value,
    Concordance.Sizable.Graph        = Concordance.Sizable.Graph,
    Concordance.Assumptions	      	 = Concordance.Assumptions,
    Concordance.Assumptions.Label.X  = Concordance.Assumptions.Label.X,
    Concordance.X.Annotation   	     = Concordance.X.Annotation,
    Concordance.Y.Annotation   	     = Concordance.Y.Annotation,
    Concordance.Intercept.Value.plot = Concordance.Intercept.Value.plot,
    Title.concordance                = Title.concordance,


    # Axis Parameters (Concordance and AB share the same X.Ticks)
    X.Ticks                           = X.Ticks,
    X.Tick.Label                      = X.Tick.Label,
    X.Min                             = X.Min,
    X.Max          		                = X.Max,
    AltmanBland.Y.Ticks               = AltmanBland.Y.Ticks,
    AltmanBland.Y.Min                 = AltmanBland.Y.Min ,
    AltmanBland.Y.Max                 = AltmanBland.Y.Max ,
    Concordance.Y.Axis.Residuals      = Concordance.Y.Axis.Residuals,
    Concordance.Y.Ticks.Residuals     = Concordance.Y.Ticks.Residuals,

    #Altman Bland Parameters
    AltmanBland.X.Label           	  = AltmanBland.X.Label,
    AltmanBland.Y.Label           	  = AltmanBland.Y.Label,

    AltmanBland.Need.95CI.Band 	      = AltmanBland.Need.95CI.Band,
    AltmanBland.Show.95CI         	  = AltmanBland.Show.95CI,
    AltmanBland.Plot.Log.Diff  	      = AltmanBland.Plot.Log.Diff,
    AltmanBland.Draw.Mean.STD  	      = AltmanBland.Draw.Mean.STD,
    AltmanBland.Draw.Perfect.Line     = AltmanBland.Draw.Perfect.Line,
    AltmanBland.Draw.Regression.Line  = AltmanBland.Draw.Regression.Line,
    AltmanBland.Plot.Reference.X.Axis = AltmanBland.Plot.Reference.X.Axis,
    Title.altman                      = Title.altman

  )




  Concordance.Table <- Concordance.and.AB$Concordance.Table
  AltmanBland.Table <- Concordance.and.AB$AltmanBland.Table

  Qualification.Table <- rbind (Concordance.Table, Agreement.Table)
  #Qualification.Table <- rbind (Concordance.Table, GMT.Ratio.Table, Agreement.Table)



  AlphaCI.Low<- paste(100-Alpha*100,"%LCL",sep="")
  AlphaCI.Upp<- paste(100-Alpha*100,"%UCL",sep="")

  colnames(Qualification.Table) <- c("Parameter","Estimates",AlphaCI.Low, AlphaCI.Upp)
  write.html.tablen(file= paste(output.folder,"/", concordance.folder,"/",Postfix, ".html",sep=""), Qualification.Table,
                    capt= as.character(paste("Concordance Parameter estimates: Size of Data ", Size.Of.Valid.Data.Concordance)),
                    append=FALSE)
  Qualification.Table.Info <- data.frame("Table" = "Concordance", "Size of Data" = Size.Of.Valid.Data.Concordance)  # added by Xiaoyu

  # change colnames for GM.Ratio.Table -- modified by Xiaoyu
  # based on the following lines, it should focus on GMT.Ratio.Table, not Serostatus.Agreement.Table
  # colnames( Serostatus.Agreement.Table) <- c("Parameter","Estimates",AlphaCI.Low, AlphaCI.Upp)
  colnames(GMT.Ratio.Table) <- c("Parameter","Estimates",AlphaCI.Low, AlphaCI.Upp)
  write.html.tablen(file= paste(output.folder,"/", concordance.folder,"/",Postfix, ".html",sep=""), GMT.Ratio.Table,
                    capt= as.character(paste("GMT and GMT Ratio: Size of Data ", Size.Of.Valid.Data.Concordance)),
                    append=TRUE)
  GMT.Ratio.Table.Info <- data.frame("Splitter" = " || ", "Table" = "GMT and GMT Ratio", "Size of Data" = Size.Of.Valid.Data.Concordance)  # added by Xiaoyu

  if (X.Cutoff >= 0 | Y.Cutoff >= 0 ) #Antoine, 15Nov2023 : Temporary modification of >= instead of >
  {
    colnames( Serostatus.Agreement.Table) <- c("Parameter","Estimates",AlphaCI.Low, AlphaCI.Upp)
    write.html.tablen(file= paste(output.folder,"/", concordance.folder,"/",Postfix, ".html",sep=""),  Serostatus.Agreement.Table,
                      capt= as.character(paste("Serostatus Agreement: Size of Data ", Size.Of.Valid.Data.Serostatus)),
                      append=TRUE)
    Serostatus.Agreement.Table.Info <- data.frame("Splitter" = " || ", "Table" = "Serostatus Agreement", "Size of Data" = Size.Of.Valid.Data.Serostatus)  # added by Xiaoyu
  }else{
    Serostatus.Agreement.Table.Info <- data.frame("Splitter" = " || ", "Table" = "Serostatus Agreement", "Size of Data" = "NA")  # added by Xiaoyu
  }

  colnames(AltmanBland.Table) <- c("Parameter","Estimates",AlphaCI.Low, AlphaCI.Upp, "p-value Test = 0", "p-value Test = 1")
  write.html.tablen(file= paste(output.folder,"/", concordance.folder,"/",Postfix, ".html",sep=""), AltmanBland.Table,
                    capt= as.character(paste("Altman Bland estimates: Size of Data ", Size.Of.Valid.Data.Concordance)),
                    append=TRUE)
  AltmanBland.Table.Info <- data.frame("Splitter" = " || ", "Table" = "Altman Bland", "Size of Data" = Size.Of.Valid.Data.Concordance)  # added by Xiaoyu

  # Pauline 11/15/2017: in order to merge all the results in one big table
  final <- list(Qualification.Table.Info, Qualification.Table, GMT.Ratio.Table.Info, GMT.Ratio.Table, Serostatus.Agreement.Table.Info,Serostatus.Agreement.Table, AltmanBland.Table.Info,AltmanBland.Table)
  names(final) <- c("Qualification.Table.Info","Qualification.Table", "GMT.Ratio.Table.Info","GMT.Ratio.Table", "Serostatus.Agreement.Table.Info","Serostatus.Agreement.Table", "AltmanBland.Table.Info","AltmanBland.Table")
  return(final)

}

#*****************************************************************************#
#********************** Qualification Wrap ***********************************#
#*****************************************************************************#
# New Argument:
# Group.Var.Value.Vector  : A character vector, the value vector of the Group variable.
# output.csv.file.name    : A character, Name of output csv file, do NOT add ".csv". Default to Data.File.Name with postfix ".result"
# Removed Argument:
# Group.Var.Value         : The value vector of the Group variable

QUALIFICATION.Wrap <- function(
    #Loading dataset parameters
  Data.File.Name 	= Data.File.Name,
  output.csv.file.name = paste0(Data.File.Name,".result"),
  X.Var.Name     	= X.Var.Name,
  Y.Var.Name     	= Y.Var.Name,
  Profile.Var.Name    	= "",
  Profile.Legend    	= c(""),
  Group.Var.Name 	= "",
  # Group.Var.Value 	= "",
  Group.Var.Value.Vector = "",


  X.Concordance.Profile.Legend     = -999,
  Y.Concordance.Profile.Legend     = -999,

  X.AltmanBland.Profile.Legend     = -999,
  Y.AltmanBland.Profile.Legend     = -999,


  #General parameters
  Alpha          	= 0.05,
  logbase        	= logbase,
  Title          	= "Default Title",
  Postfix        	= "0",

  # Preparation of RawData Parameters

  LLOQ           = 0.0,
  X.LLOQ         = 0.0,
  Y.LLOQ         = 0.0,
  Upper.Bound    = 9999999999,
  X.Upper.Bound  = 9999999999,
  Y.Upper.Bound  = 9999999999,
  Remove.Above.Upper.Bound = TRUE,

  #Concordance parameters
  Concordance.X.Label        		= "Default x label",
  Concordance.Y.Label        		= "Default y label",


  Concordance.Draw.Conc.Line           = TRUE,
  Concordance.Need.Profile.Legend      = FALSE,
  Concordance.Need.95CI.Band 		= FALSE,
  Concordance.Range.Band 		= FALSE,
  Concordance.Perfect.Band		= FALSE,
  Concordance.Sizable.Graph  		= FALSE,
  Concordance.Range.Band.Value 	= 1,
  Concordance.Assumptions		= FALSE,
  Concordance.Assumptions.Label.X      = "",
  Concordance.X.Annotation   		= -1,
  Concordance.Y.Annotation   		= -1,
  Concordance.Intercept.Value.plot 	= FALSE,


  #graph parameters (AB and Concordance shared some parameters)

  X.Ticks                          = c(),
  X.Tick.Label                     = c(),
  X.Min                            = 0,
  X.Max          		    = 5,
  AltmanBland.Y.Ticks              = c(0,1,2,3,4),
  AltmanBland.Y.Min                = 0,
  AltmanBland.Y.Max                = 4,
  Concordance.Y.Axis.Residuals     = 0,
  Concordance.Y.Ticks.Residuals    = c(),



  #Altman Bland Parameters


  AltmanBland.X.Label        	 = "Default x label",
  AltmanBland.Y.Label        	 = "Default y label",

  AltmanBland.Need.95CI.Band 	     = FALSE,
  AltmanBland.Show.95CI      	     = FALSE,
  AltmanBland.Plot.Log.Diff  	     = FALSE,
  AltmanBland.Draw.Mean.STD  	     = TRUE,
  AltmanBland.Draw.Perfect.Line     = FALSE,
  AltmanBland.Draw.Regression.Line  = FALSE,
  AltmanBland.Plot.Reference.X.Axis = FALSE,

  #Serostatus agreement parameters

  X.Cutoff       = -1,
  Y.Cutoff       = -1,

  #Agreement parameters

  Test.Type      = "ELISA",
  Agreement.Cutoff = 1
)
{
  data.df <- data.frame()

  for (j in 1:length(Group.Var.Value.Vector))   # Serotype.short
  {
    res <- QUALIFICATION(#Loading dataset parameters
      Data.File.Name 	= Data.File.Name,
      X.Var.Name     	= X.Var.Name,
      Y.Var.Name     	= Y.Var.Name,
      Profile.Var.Name    	= Profile.Var.Name,
      Profile.Legend    	= Profile.Legend,
      Group.Var.Name 	= Group.Var.Name,
      Group.Var.Value 	= as.character(Group.Var.Value.Vector[j]),


      X.Concordance.Profile.Legend     = X.Concordance.Profile.Legend,
      Y.Concordance.Profile.Legend     = Y.Concordance.Profile.Legend,

      X.AltmanBland.Profile.Legend     = X.AltmanBland.Profile.Legend,
      Y.AltmanBland.Profile.Legend     = Y.AltmanBland.Profile.Legend,


      #General parameters
      Alpha          	= Alpha,
      logbase        	= logbase,
      Title          	= Title,
      Postfix        	= paste(Postfix, Group.Var.Value.Vector[j], sep = "."),

      # Preparation of RawData Parameters

      LLOQ           = LLOQ,
      X.LLOQ         = X.LLOQ,
      Y.LLOQ         = Y.LLOQ,
      Upper.Bound    = Upper.Bound,
      X.Upper.Bound  = X.Upper.Bound,
      Y.Upper.Bound  = Y.Upper.Bound,
      Remove.Above.Upper.Bound = Remove.Above.Upper.Bound,

      #Concordance parameters
      Concordance.X.Label        		= paste(Group.Var.Value.Vector[j], Concordance.X.Label, sep = " "),  # default to Group.var + a specific title
      Concordance.Y.Label        		= paste(Group.Var.Value.Vector[j], Concordance.Y.Label, sep = " "),  # default to Group.var + a specific title


      Concordance.Draw.Conc.Line           = Concordance.Draw.Conc.Line,
      Concordance.Need.Profile.Legend      = Concordance.Need.Profile.Legend,
      Concordance.Need.95CI.Band 		= Concordance.Need.95CI.Band,
      Concordance.Range.Band 		= Concordance.Range.Band,
      Concordance.Perfect.Band		= Concordance.Perfect.Band,
      Concordance.Sizable.Graph  		= Concordance.Sizable.Graph,
      Concordance.Range.Band.Value 	= Concordance.Range.Band.Value,
      Concordance.Assumptions		= Concordance.Assumptions,
      Concordance.Assumptions.Label.X      = Concordance.Assumptions.Label.X,
      Concordance.X.Annotation   		= Concordance.X.Annotation,
      Concordance.Y.Annotation   		= Concordance.Y.Annotation,
      Concordance.Intercept.Value.plot 	= Concordance.Intercept.Value.plot,


      #graph parameters (AB and Concordance shared some parameters)

      X.Ticks                          = X.Ticks,
      X.Tick.Label                     = X.Tick.Label,
      X.Min                            = X.Min,
      X.Max          		    = X.Max,
      AltmanBland.Y.Ticks              = AltmanBland.Y.Ticks,
      AltmanBland.Y.Min                = AltmanBland.Y.Min,
      AltmanBland.Y.Max                = AltmanBland.Y.Max,
      Concordance.Y.Axis.Residuals     = Concordance.Y.Axis.Residuals,
      Concordance.Y.Ticks.Residuals    = Concordance.Y.Ticks.Residuals,



      #Altman Bland Parameters


      AltmanBland.X.Label        	 = paste(Group.Var.Value.Vector[j], AltmanBland.X.Label, sep = " "),
      AltmanBland.Y.Label        	 = AltmanBland.Y.Label,

      AltmanBland.Need.95CI.Band 	     = AltmanBland.Need.95CI.Band,
      AltmanBland.Show.95CI      	     = AltmanBland.Show.95CI,
      AltmanBland.Plot.Log.Diff  	     = AltmanBland.Plot.Log.Diff,
      AltmanBland.Draw.Mean.STD  	     = AltmanBland.Draw.Mean.STD,
      AltmanBland.Draw.Perfect.Line     = AltmanBland.Draw.Perfect.Line,
      AltmanBland.Draw.Regression.Line  = AltmanBland.Draw.Regression.Line,
      AltmanBland.Plot.Reference.X.Axis = AltmanBland.Plot.Reference.X.Axis,

      #Serostatus agreement parameters

      X.Cutoff       = X.Cutoff,
      Y.Cutoff       = Y.Cutoff,

      #Agreement parameters

      Test.Type      = Test.Type,
      Agreement.Cutoff = Agreement.Cutoff
    )

    n_table <- length(res)
    for(j in 1:n_table){
      raw.data <- as.data.frame(res[[j]])

      n_row = dim(raw.data)[1]
      n_col = dim(raw.data)[2]

      for (i in 1:n_row){
        if(j == 1 & i == 1){  # corresponding to first table and first row
          res.df.temp <- raw.data[i,]
          res.df <- res.df.temp
        }else{
          res.df.temp <- raw.data[i,]
          if(j %% 2 == 0){  # if not X.Table.Info
            if(raw.data[i,1] %in% c("Average Diff. (90%CI)","Average Diff. (95%CI)")){  # add % symbol for percent difference (% diff.), when combine Estimates and CIs into one cell.
              res.df.temp.Estimate.CI <- data.frame("Estimate.CI" = paste0(raw.data[i,2],"% (",raw.data[i,3],"% , ",raw.data[i,4],"%)"))  # combined Estimates and CIs into one cell.
            }else{
              res.df.temp.Estimate.CI <- data.frame("Estimate.CI" = paste0(raw.data[i,2]," (",raw.data[i,3]," , ",raw.data[i,4],")"))  # combined Estimates and CIs into one cell.
            }

            res.df.temp <- cbind(res.df.temp, res.df.temp.Estimate.CI)
          }

          res.df <- cbind(res.df, res.df.temp)
        }

      }  # End for
    }  # End for

    data.df <- rbind(data.df, res.df)
  }
  row.names(data.df) <- Group.Var.Value.Vector

  output.csv.file.name <- paste0(output.csv.file.name,".csv")
  write.csv(data.df, file = output.csv.file.name)
}
