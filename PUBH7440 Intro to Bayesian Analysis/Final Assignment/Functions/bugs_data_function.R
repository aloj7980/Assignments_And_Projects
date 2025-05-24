#this function works as long as your data are either
# 1) scalars
# 2) vectors
# 3) 2-dimensional arrays
#I'm too lazy to update this function for multidimensional arrays... :(

bugs.data=function(vari=list(),file=NULL){
  
  ##categorize variable 
  ##to determine the way to print data
  nv=length(vari)
  vtype=rep(NA,nv)
  vname=names(vari)
  patho <- file
  
  ## I find it can use str() function instead
  for(i in 1:nv){
    if(length(vari[[i]])==1){ #scalar
      vtype[i]=1
    }else if(is.null(dim(vari[[i]]))){ #vector
      vtype[i]=2
    }else{ #matrix
      vtype[i]=3
    }
  }
  ##print data
  cat(paste('#',patho,'\n',sep=''),file=patho)
  cat('list(',file=patho,append=TRUE)
  
  for(j in 1:nv){
    if(vtype[j]==1){
      if(j==nv){cat(paste(vname[j],'=',vari[[j]],')',sep=''),file=patho,append=TRUE)}
      else{cat(paste(vname[j],'=',vari[[j]],',\n',sep=''),file=patho,append=TRUE)}
    }
    else if(vtype[j]==2){
      cat(paste(vname[j],'=c(',sep=''),file=patho,append=TRUE)
      tepv=vari[[j]]
      num=length(tepv)
      cat(tepv,file=patho,append=TRUE,sep=', ',fill=TRUE)
      cat(paste(')',sep=''),file=patho,append=TRUE)
      if(j!= nv){cat(',\n',file=patho,append=TRUE)}
      else{cat(')',file=patho,append=TRUE)}
    }
    else if(vtype[j]==3){
      #I need to recode this to be similar to the vtype[j]==2 above -- the for loop here is slow
      cat(paste(vname[j],'=structure(.Data=c(',sep=''),file=patho,append=TRUE)
      coln=ncol(vari[[j]]);rown=nrow(vari[[j]])
      tepv=t(vari[[j]])
      #tepv=as.vector(vari[[j]])
      #num=length(tepv)
      #for(nsim in 1:(num-1)){
      #  cat(paste(tepv[nsim],',',sep=''),file=patho,append=TRUE)
      #  if(nsim/20 == floor(nsim/20)){cat('\n',file=patho,append=TRUE)}
      #}
      cat(tepv,file=patho,append=TRUE,sep=', ',fill=TRUE)
      cat(paste('),.Dim=c(',rown,',',coln,'))',sep=''),file=patho,append=TRUE)
      if(j!= nv){cat(',\n',file=patho,append=TRUE)}
      else{cat(')',file=patho,append=TRUE)}
    }
    cat('Done with ',vname[j],'\n',sep='')
  }
  
}

#you don't want this code to be in the function file
#bugs.data(vari = list(N=N,Ns=Ns,Ny=Ny,Nr=Nr,obese=Y,R=R),file = 'data_try.txt')
