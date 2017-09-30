;read in corn yeild 
a=read_csv('Corn_metrics.csv',header=header)
obj = LIST()
FOR i=0,N_TAGS(a)-1 DO BEGIN
  ; this excludes columns that have string type data
  IF SIZE(a.(i),/TYPE) EQ 7 THEN CONTINUE
  obj.add,FLOAT(a.(i))
ENDFOR
;data set that will resemble csv file layout
xdata=TRANSPOSE(obj.toarray())

;get number of rows and columns
ath=size(xdata) ;ath[1]=1055 rows
yrs=35  ;going from 1980 to 2014 

;array for bu/acre
yld=FltArr(ath[1],yrs)
FOR i=0,ath[1]-1 DO BEGIN
  FOR j=0,yrs-1 DO BEGIN
    yld[i,j]=xdata[i,j+6]
  ENDFOR
ENDFOR

;Find columns with  missing data and get rid of them
try = INTARR(ath[1],yrs)
try[WHERE(yld LE 0)] = 1
ind = INTARR(ath[1])
FOR i=0,ath[1]-1 DO ind[i]=total(try[i,*])
iok = WHERE(IND LT 1)
xx = yld[iok,*]

;**************************************************************************************************************************
;PCA - Raw
xxmean = mean(xx,dim=1, /NAN)
FOR i=0,yrs-1 DO xx[*,i] = xx[*,i]-xxmean[i]
covar = correlate(transpose(xx),/COVAR)
eval = EIGENQL(covar,eigenvector=ev)
ev = transpose(ev)
IF determ(ev) LT 0 then ev[-1,*] = -1 * ev[-1,*]
xxrot = transpose(ev)##xx
s= MAKE_ARRAY(ath[1],yrs,value=NAN)
s[iok,*]=xxrot
write_csv, 'Corn_PCA_02.csv',transpose(s)

;***************************************************************************************************************************
;PCA - Detrended
gmo_pct = [0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0.044,  0.119,  0.281,  0.339,  0.25,   0.26,   0.34,   0.4,  0.47,   0.52,   0.61,   0.73,   0.8,  0.85,   0.86,   0.88,   0.88,   0.9,  0.93]
gmo_pct_std = (gmo_pct-mean(gmo_pct))/sqrt(variance(gmo_pct))
yld_mean = xxmean
yld_mean_std = (xxmean-mean(xxmean))/sqrt(variance(xxmean))
reg1 = regress(gmo_pct,yld_mean, const=const1, yfit=yfit1, corr=corr1, ftest=ftest1) ;corr1=0.792

gm_2016_std = [-0.910041930160683 , -0.910041930160683 ,  -0.910041930160683 ,  -0.910041930160683 ,  -0.910041930160683 ,  -0.910041930160683 ,  -0.910041930160683 ,  -0.910041930160683 ,  -0.910041930160683 ,  -0.910041930160683 ,  -0.910041930160683 ,  -0.910041930160683 ,  -0.910041930160683 ,  -0.910041930160683 ,  -0.910041930160683 ,  -0.910041930160683 ,  -0.789620223072851 ,  -0.584355949627684 ,  -0.140985118986122 ,  0.0177525858114739 ,  -0.225827685343458 ,  -0.198459115550769 ,  0.020489442790743 , 0.184700861546877 , 0.3762808500957 , 0.513123699059145 , 0.759440827193346 , 1.08786366470561 ,  1.27944365325444 ,  1.41628650221788 ,  1.44365507201057 ,  1.49839221159595 ,  1.49839221159595 ,  1.55312935118133 ,  1.63523506055939 ,  1.6078664907667 , 1.6078664907667 ]
yld_2016_us_std = [-1.7187868299434 , -0.955969355259011 ,  -0.772722699329241 ,  -2.14068029359566 , -1.04972345829285 , -0.568168292709964 ,  -0.508506590779341 ,  -0.491460390227735 ,  -1.9915260387691 ,  -0.640614645054292 ,  -0.546860542020456 ,  -0.968754005672716 ,  0.00714097590675309 , -1.30541646656694 , 0.309711035697767 , -0.759938048915537 ,  -0.180367230160918 ,  -0.197413430712524 ,  0.1307259299059 , 0.105156629078491 , 0.23726468335344 ,  0.29266483514616 ,  -0.0866131271270818 , 0.463126840662225 , 1.23446741562242 ,  0.706035198522617 , 0.757173800177436 , 0.825358602383862 , 0.936158905969305 , 1.40919097127638 ,  0.906328055003992 , 0.6591581470057 , -0.350829235676982 ,  1.14071331258858 ,  1.69045328037789 ,  1.57965297679245 ,  1.84386908534235 ]
reg_2016 = regress(gm_2016_std,yld_2016_us_std, const=const16, yfit=yfit16, corr=corr16, ftest=ftest16) ;corr16=0.8203


;take out trend
xx2 = yld[iok,*]
ath2 = size(xx2)
yy_t = transpose(xx2)
yfit_gmo = FltArr(ath2[1],yrs)
FOR i=0, ath2[1]-1 DO BEGIN
  y_gmo = regress(gmo_pct, yy_t[*,i], const=const3, yfit=yfit2, correlation=corr2, ftest=ftest2, chisq=chi2)
  yfit_gmo[i,*] = yfit2
ENDFOR
xx2 = xx2 - yfit_gmo
xx2_save = yld[iok,*]

;take out mean for PCA
xx2mean = mean(xx2,dim=1, /NAN)
FOR i=0,yrs-1 DO xx2[*,i] = xx2[*,i]-xx2mean[i]
covar2 = correlate(transpose(xx2),/COVAR)
eval2 = EIGENQL(covar2,eigenvector=ev2)
ev2 = transpose(ev2)
IF determ(ev2) LT 0 then ev2[-1,*] = -1 * ev2[-1,*]
xxrot2 = transpose(ev2)##xx2
s2= MAKE_ARRAY(ath[1],yrs,value=NAN)
s2[iok,*]=xxrot2
write_csv, 'Corn_PCA2_02.csv',transpose(s2)

END