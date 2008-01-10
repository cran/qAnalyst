`switchFun` <-
function(type,argument)
{
#swiccha gli argomenti a default se l'utente sbaglia a inserire#
#studia le distributioni
	if (argument=="chart")
	{
		#converte i codici delle distribuzioni nel suffisso interno ad r
		out=switch(type,"XBAR"="xbar", 
		"X-BAR"="xbar", 
		"x-bar"="xbar",
		"range"="r",
		"sd"="s",
		"individuals"="i",
		"individual"="i",
		"moving range"="mr")
		#se l'argomento nn è macciato allora restituisce il default
		if (is.null(out)) out=type
	}
	#se sono previsti altri argomenti ci possono aggiuntgere
invisible(out)
}

