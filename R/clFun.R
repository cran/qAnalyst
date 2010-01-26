`clFun` <-
function(x, sg, nSigma, cl, type = "xbar", mu=NA, sigma=NA) {

  #if not "u" or "l" stops
  if(!is.element(cl, c("u", "l"))) {stop("Error! cl must be either u or l")}


  # xbar Chart
  if(type == "xbar") {

    # first case: mu and sigma are both not provided (as in release 0.6.0)
    if(is.na(mu) && is.na(sigma)) {
      xbar = tapply(x, sg, mean, na.rm = TRUE)
      #considers missing value
      xbar[is.nan(xbar)]=NA
      xbarbar = mean(x, na.rm = TRUE)
      #waring is sg<2
      sgSize = as.numeric(tapply(x, sg, countFun))
      #care confidence limits
      #sg below 7 R else sg
      sgMean=mean(sgSize,na.rm=TRUE)
      #sg below 8
      if (sgMean<7) {
        r = tapply(x, sg, rFun)
        rbar = mean(r,na.rm=TRUE)
        d2 = getCoeffFun(sgSize, "d2")
        #returns limits vecotr
        if(cl == "u") clout = xbarbar + (nSigma*rbar)/(d2*sqrt(sgSize))
        if(cl == "l") clout = xbarbar - (nSigma*rbar)/(d2*sqrt(sgSize))
      }
      #caso due: uso S per la  variabilita
      else {
        sbar=centerFun(x=x,sg=sg,type="s")
        A3 = getCoeffFun(sgSize, "A3")
        if(cl == "u") clout = xbarbar + (nSigma*(A3/3))*sbar
        if(cl == "l") clout = xbarbar - (nSigma*(A3/3))*sbar
      }
    }

    # second case: mu and sigma are both provided (release 0.6.1, Nicola)
    if(!is.na(mu) && !is.na(sigma)) {
      sgSize = as.numeric(tapply(x, sg, countFun))            # size of each subgroups
      if(cl == "u") clout = mu + nSigma*(sigma/sqrt(sgSize))  # upper confidence limit
      if(cl == "l") clout = mu - nSigma*(sigma/sqrt(sgSize))  # lower confidence limit
    }

    # third case: mu is provided, sigma not
    if(!is.na(mu) && is.na(sigma)) {
      print('TO BE IMPLEMENTED') # TO BE IMPLEMENTED
    }

    # fourth case: sigma is provided, mu not
    if(is.na(mu) && !is.na(sigma)) {
      print('TO BE IMPLEMENTED') # TO BE IMPLEMENTED
    }

  }


  # r Chart
  if(type=="r") {

    # first case: sigma is not provided (as in release 0.6.0)
    if(is.na(mu) && is.na(sigma)) {
        xbar = tapply(x, sg, mean, na.rm = TRUE)
        xbar[is.nan(xbar)]=NA
        r = tapply(x, sg, rFun)
        #check estimator r bar variable sample dim
        r[is.nan(r)]=NA
        rbar = mean(r,na.rm=TRUE)
        xbarbar = mean(x, na.rm = TRUE)
        sgSize = tapply(x, sg, countFun)
        #da Montgomery pp 157  where 3 = nSigma
        d3 = getCoeffFun(sgSize, "d3")
        d2 = getCoeffFun(sgSize, "d2")
        D4 = (1+nSigma*d3/d2)
        D3 = (1-nSigma*d3/d2)
        #limits
        UCL=D4*rbar
        LCL=ifelse(D3*rbar>0,D3*rbar,0)
        if(cl == "u") clout = UCL
        if(cl == "l") clout = LCL
    }

    # second case: sigma is provided (release 0.6.1, Nicola)
    if(!is.na(mu) && !is.na(sigma)) {
	sgSize = table(sg)
        center = centerFun(x, sg, type = "r", mu=mu, sigma=sigma)                  # compute center line
        if(cl == "u") {
          clout = center + nSigma*getParameterFun(sgSize, "d3")*sigma            # upper confidence limit (Montgomery, page 201)
          clout[clout < 0] = 0                                                   # confidence limit cannot be less than 0
        }
        if(cl == "l") {
          clout = center - nSigma*getParameterFun(sgSize, "d3")*sigma            # lower confidence limit (Montgomery, page 201)
          clout[clout < 0] = 0                                                   # confidence limit cannot be less than 0
        }
    }

  }


  # s chart
  if(type=="s") {
    #SBAR estimation
    sdCampionari= tapply(x,sg,sd,na.rm=TRUE)
    sdCampionari[is.na(sdCampionari)]=NA
    sgSize = tapply(x,sg,countFun)
    #formula montgomery pp 189
    numFormula=sum(sdCampionari^2*(sgSize-1),na.rm=TRUE)
    m=length(unique(sg))
    denFormula=sum(sgSize,na.rm=TRUE)-m
    sbar=(numFormula/denFormula)^0.5
    #Montgomery uses 3 sigma
    # here we use B3 and B4
    c4 = getCoeffFun(sgSize, "c4")
    costSup=(1+(nSigma/c4)*(sqrt(1-c4^2)))
    costInf=(1-(nSigma/c4)*(sqrt(1-c4^2)))
    #limits
    if(cl == "u") { clout=sbar*costSup; clout=ifelse(clout>0,clout,0)}
    if(cl == "l") { clout=sbar*costInf; clout=ifelse(clout>0,clout,0)}
  }


  # i chart (xbar chart for individual measurements)
  if(type=="i") {

    # first case: mu and sigma are both not provided (as in release 0.6.0)
    if(is.na(mu) && is.na(sigma)) {
      center = mean(na.omit(x))
      MR = centerFun(x = x, sg = sg, type="mr", sigma = sigma)
      d2 = getCoeffFun(sg+1, "d2")                  # release 0.6.1, Nicola
      UCL = center + nSigma * MR / d2               # release 0.6.1, Nicola
      LCL = center - nSigma * MR / d2               # release 0.6.1, Nicola
    }

    # second case: mu and sigma are both provided (release 0.6.1, Nicola)
    if(!is.na(mu) && !is.na(sigma)) {
      MR = centerFun(x = x, sg = sg, type="mr", sigma = sigma)
      d2 = getCoeffFun(sg+1, "d2")                  # release 0.6.1, Nicola
      d3 = getCoeffFun(sg+1, "d3")                  # release 0.6.1, Nicola
      UCL = mu + nSigma * MR * (d3/d2)              # release 0.6.1, Nicola
      LCL = mu - nSigma * MR * (d3/d2)              # release 0.6.1, Nicola
    }
    
    # third case: mu is provided, sigma not
    if(!is.na(mu) && is.na(sigma)) {
        print('TO BE IMPLEMENTED') # TO BE IMPLEMENTED
    }

    # fourth case: sigma is provided, mu not
    if(is.na(mu) && !is.na(sigma)) {
        print('TO BE IMPLEMENTED') # TO BE IMPLEMENTED
    }

    # Return output
    if(cl == "u") clout=rep(UCL,length(x))
    if(cl == "l") clout=rep(LCL,length(x))
    
  }


  # mr chart
  if(type=="mr") {
    #recalculates center kline
    #see when interval is used
    MR = centerFun(x = x, sg = sg, type = "mr", sigma = sigma)
    #calcola i limiti centrali
    d2 = getCoeffFun(sg+1, "d2")
    d3 = getCoeffFun(sg+1, "d3")

    UCL = MR + nSigma * (d3/d2) * MR         # release 0.6.1, Nicola
    LCL = MR - nSigma * (d3/d2) * MR         # release 0.6.1, Nicola
    LCL = ifelse(LCL < 0, 0, LCL)

    if(cl == "u") clout=rep(UCL,length(x))
    if(cl == "l") clout=rep(LCL,length(x))
  }


  # p chart
  if(type=="p") {
    # compute center line
    pbar = centerFun(x = x, sg = sg, type = "p", mu = mu)
    # compute control limits
    UCL = pbar + nSigma*sqrt((pbar*(1-pbar))/sg)
    LCL = pbar - nSigma*sqrt((pbar*(1-pbar))/sg)
    LCL = ifelse(LCL < 0, 0, LCL)   # non negativity
    if(cl == "u") clout=UCL
    if(cl == "l") clout=LCL
  }


  # np chart
  if(type=="np")  {
    #calculates pbar
    #for npbar
    pbar  = centerFun(x = x, sg = sg, type = "p",  mu = mu/sg)
    npbar = centerFun(x = x, sg = sg, type = "np", mu = mu)
    #calculates UCL
    UCL = npbar + nSigma*sqrt(sg*pbar*(1-pbar))
    LCL = npbar - nSigma*sqrt(sg*pbar*(1-pbar))
    LCL = ifelse(LCL < 0, 0, LCL)   # non negativity
    if(cl == "u") clout=UCL
    if(cl == "l") clout=LCL
  }


  # c chart
  if(type=="c") {
    cline=centerFun(x = x, sg = sg, type = "c", mu = mu)
    UCL = cline + nSigma*sqrt(cline)
    LCL = cline - nSigma*sqrt(cline)
    LCL = ifelse(LCL < 0, 0, LCL)   # non negativity
    if(cl == "u") clout=rep(UCL,length(x))
    if(cl == "l") clout=rep(LCL,length(x))
  }


  # u chart
  if(type=="u") {
    cline=centerFun(x=x, sg=sg, type="u", mu = mu)
    UCL = cline + nSigma*sqrt(cline/sg)
    LCL = cline - nSigma*sqrt(cline/sg)
    LCL = ifelse(LCL < 0, 0, LCL)   # non negativity
    if(cl == "u") clout=UCL
    if(cl == "l") clout=LCL
  }

  # Return output
  return(clout)

}

