#############################################################################################

#FILE NAME 		WRITE.HTML.TABLEN_1.0.R

#FUNCTION NAME 		write.html.tablen

#DESCRIPTION		Create a html file with the information given to the program

#METHOD 			

#EXT FUNCTION PARAM		

#INT FUNCTION PARAM [REQUIRED] 	x	: the text/dataset to print in the html format
#				file	: the name of the html output file
#				capt	: The title to give to the html table
#				append	: TRUE means adding to the previous information in the htmll file
#				the new informations, FALSE means that the html will erase the previous html file

#  		    [OPTIONAL]	

#INPUT FILE		

#OUTPUT FILE		html file 

#REQUIREMENTS		

#NOTES			

#WRITTEN BY		Eloi Kpamegan

#CREATION DATE		

#MODIFICATION		{Nature of the modification}

#REASON FOR MODIFICATION	{reason for the modification}

#MODIFY BY		{Name of the biostatisticien who modified the function}

#MODIFICATION DATE	{Modification Date}

############################################################################################




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