# test to see how this line is shown
library(ncdf4)
library(maps) 
library(mapdata) 
library(maptools) 

names=c("AS","Johnston","MH","Mariana","PRIA","Wake")
for (ii in 1:length(names)) {
  print(names[ii])
  fln=paste("2023/chl-",names[ii],"-1998-2022.nc",sep='') 
  print(fln)
  nc=nc_open(fln)
  
  #time-series
  v1=nc$var[[1]]
  chl=ncvar_get(nc,v1)
  dates=as.POSIXlt(v1$dim[[3]]$vals,origin='1970-01-01',tz='GMT')
  
  ts=rep(NA,dim(chl)[3])
  for (i in 1:dim(chl)[3]) ts[i]=mean(chl[,,i],na.rm=TRUE)
  
  #T=data.frame(dates, ts)
  #fln_out=paste("chl-",names[ii],".csv",sep='')
  #write.csv(T,fln_out,row.names=TRUE)


  n=length(ts)
  fig_fln=paste("figs/chl-",names[ii],"-ts.png",sep='') 
  png(file=fig_fln,width = 180, height = 100, units='mm', res = 300)
  #plot
  #x11(height=5.2, width=11.1)
  plot(1:(n-12),ts[1:(n-12)],type='l',axes=FALSE,xlab='',main=paste("Chl-a (",names[ii],")",sep=''),ylab='(mg/m^3)',pch=20,lwd=4, col=1,xlim=c(1,n))
  lines((n-12):n,ts[(n-12):n],col="#5dade2",lwd=4)
  #lines((n-12):n,ts[(n-12):n],col="#5dade2",lwd=3)
  axis(2)
  axis(1,seq(1,n,12),1998:2022)
  box()
  #plot done 
  dev.off()


  # anomaly
  m=rep(NA,12)
  for (i in 1:12) {
    ind=seq(i,n,12)
    m[i]=mean(ts[ind],na.rm=TRUE)
  }
  
  y=n/12  #  # of years
  
  mtot=rep(m,y)

  anom=ts-mtot
  T=data.frame(dates, ts,anom)
  fln_out=paste("chl-",names[ii],".csv",sep='')
  write.csv(T,fln_out,row.names=TRUE)
  
  fig_fln=paste("figs/chl-",names[ii],"-ts-anom.png",sep='') 
  png(file=fig_fln,width = 180, height = 100, units='mm', res = 300)
  
  #plot
  plot(1:(n-12),anom[1:(n-12)],type='l',axes=FALSE,xlab='',main=paste("Chl-a anomalies (",names[ii],")",sep=''),ylab='(mg/m^3)',pch=20,lwd=3,xlim=c(1,n))
  lines((n-12):n,anom[(n-12):n],col="#3498db",lwd=3)
  #points((n-12):n,anom[(n-12):n],col=4,pch=20)
  axis(2)
  axis(1,seq(1,n,12),1998:2022)
  box()
  lines(c(-10,n+10),c(0,0))
  #plot done 
  dev.off()

}



