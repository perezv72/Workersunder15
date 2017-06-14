# Estimating number of workers earning less than $15 per hour. 
This R script calculates the number of workers that earn less than $15 per hour for various demographic groups the state and national using Current Population Survey Merged Outgoing Rotation Groups (CPS MORG) microdata. The methodology was adapted from the methodology described in [*The Growing Movement for $15*](http://www.nelp.org/content/uploads/Growing-Movement-for-15-Dollars.pdf') report by the National Employment Law Project. 

## NELP Methodology Summary
*	Uses CPS MORG data from NBER, 2012 to 2014
*	Adjusts 2012 and 2013 dollars to 2014 dollars. 
*	Used three years of data to have samples big enough for generating estimates by occupation.
*	Sample includes private and public sector hourly and salaried workers and excludes self-employed.
*	Calculated wages are calculated from hourly wages that do not include tips, commissions, unless otherwise noted.  
*	For calculations involving wages, they only include respondents for whom wage data are available.
