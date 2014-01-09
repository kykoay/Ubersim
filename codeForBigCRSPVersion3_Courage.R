#code for big CRSP
#make sure workspace is cleared
#where is the CRSP file?
#setwd("C:\\Users\\Evan\\Documents\\R")
setwd("C:\\Users\\Evan\\Documents\\R\\test")
input=file("flippedFinalCRSP0.txt")
open(input)


#input is the file that contains permnos in first row, followed by discrete adjusted returns.

#write permnos to output file

line=scan(input,nlines=1)

n=length(line)

write(line,"sCRSP.txt",ncolumns=n)
s=rep(1,n)

repeat{
  line=scan(input,nlines=1,quiet=TRUE)
  line[line<(-1)]=NA
  if(length(line)==0){
    break
  }
  s=s*(1+line)
  write(s,"sCRSP.txt",ncolumns=n,append=TRUE)
  s[is.na(s)]=1
}

close(input)


#get indic
position=function(v){
  p=numeric(length(v))
  if(!is.numeric(v)){
    print("fuck you, the input is not fucking numeric! You must be a management student...")
    print("CHECK YOUR FUCKING DATA SOURCE!")
    return()
  }
  
  top=as.numeric(quantile(v,probs=.9,na.rm=TRUE))
  bottom=as.numeric(quantile(v,probs=.1,na.rm=TRUE))
  #top decile =1
  p[v>=top]=1
  #bottom decile=-1
  p[v<=bottom]=-1
  return (p)
}


#hpr from t-12 to t-1
#simulate an "sCRSP.txt" for testing, comment out before using for realz.
# n=4
# write(c(1000,10001,10002,10003),"sCRSP.txt",ncolumns=n)
# s=rep(1,n)
# i=0
# while (i<504){
#   s=s*(1+rnorm(n)/100)
#   write(s,"sCRSP.txt",ncolumns=n,append=TRUE)
#   print(i)
#   i=i+1
# }
# 
# 
closeAllConnections()

input=file("sCRSP.txt")
open(input)
line=scan(input,nlines=1,quiet=TRUE)
n=length(line)
write(line,"indic.txt",ncolumns=n)
closeAllConnections()

input=file("sCRSP.txt")
inputL1=file("sCRSP.txt")
inputL12=file("sCRSP.txt")
open(input)
open(inputL1)
open(inputL12)
#skip=1(headers)+lag
#so skip for inputL1 is 11*21=231, first line scanned to write in as headers to output
#skip for inputLagged=1 to skip the headers
lag1=11*21
lag12=0
current=12*21
#indic is available as of row 254 or day 253 (1 year plus 1 day+header)
#this row calls keat's function position() on the vector of holding period returns, calculated from row 232 over row 1
invisible(scan(inputL1,nlines=1,skip=lag1))
invisible(scan(inputL12,nlines=1,skip=lag12))
invisible(scan(input,nlines=1,skip=current))

i=0
while(i<current){
  print(i/current)
  write(rep(NA,n),"indic.txt",ncolumns=n,append=TRUE)
  i=i+1
}

repeat{
  line=scan(input,nlines=1,quiet=TRUE)
  if(length(line)==0){
    break
  }
  hpr=scan(inputL1,nlines=1,quiet=TRUE)/scan(inputL12,nlines=1,quiet=TRUE)
  hpr[is.na(line)]==NA
  write(position(hpr),
    "indic.txt",
    ncolumns=n,
    append=TRUE
    )
}
closeAllConnections()

write("rWML","rWML.txt",ncolumns=1)

ret=file("flippedFinalCRSP0.txt")
indic=file("indic.txt")
open(ret)
open(indic)

n=length(scan(ret,nlines=1,quiet=TRUE))
invisible(scan(indic,nlines=1,quiet=TRUE))
daysToUpdating=21
i=scan(indic,nlines=1,quiet=TRUE)
repeat{  
  r=scan(ret,nlines=1,quiet=TRUE)
  if(length(r)==0){
    break
  }
  if(daysToUpdating==0){
    i=scan(indic,nlines=1,quiet=TRUE,skip=20)
    daysToUpdating=21
  }
  daysToUpdating=daysToUpdating-1
  
  if(sum(is.na(i))>0){
    write(NA,"rWML.txt",ncolumns=1,append=TRUE)
  }else{
    rWML=r[i!=0&!is.na(r)]%*%i[i!=0&!is.na(r)]/sum(i==1)
    #print(rWML)
    write(rWML,"rWML.txt",ncolumns=1,append=TRUE)
  }
}
























