wsl.obs.filter=function(o.xy,a.xy=NULL,grid)
{
  # Check 'o.xy' input
  if(ncol(o.xy)!=2 || !all(colnames(o.xy)%in%c("x","y"))){
    stop("Supplied points should be a data.frame/matrix with two columns named x and y!")
  }
  
  # Check 'ras' input
  if(!(class(grid)%in%c("RasterBrick","RasterStack","RasterLayer"))) {
    stop("env.layer should be of class RasterBrick, RasterStack or RasterLayer!")
  }
  
  # Apply simple filtering
  if (!is.null(a.xy)) {
    
    # Check 'a.xy' input
    
    if(ncol(a.xy)!=2 || !all(colnames(a.xy)%in%c("x","y"))){
      stop("Supplied points should be a data.frame/matrix with two columns named x and y!")
    }
    
    # Position presences and absences
    posP=cellFromXY(grid,o.xy)
    posA=cellFromXY(grid,a.xy)
    
    # Remove absences where we find presences
    posA=posA[!(posA %in% posP)]
    
    # Extract new presences/absences and regroup
    new.pxy=coordinates(grid)[unique(posP),]
    new.axy=coordinates(grid)[unique(posA),]
    new.oxy=list(new.pxy,new.axy)
    names(new.oxy)=c("Presences","Absences")
    
  } else {
    # Apply simple filtering
    posCELL=cellFromXY(grid,o.xy)
    new.oxy=coordinates(grid)[unique(posCELL),]
  }
  
  if (class(new.oxy)%in%"numeric"){
    new.oxy=matrix(new.oxy,ncol=2)
    colnames(new.oxy)=c("x","y")
  }
  
  return(new.oxy)
}