#fist version! not thoroughly tested!
Clark.West<-function(y1,y2,y){
  #H0 is that y1 <= y2 and H1 y1>y2
  e1     = (y-y1)^2;
  e2     = (y-y2)^2;
  e3     = (y1-y2)^2;
  f_hat = (e1 - e2 + e3);
  
  P<-length(f_hat)
  f_mean<-mean(f_hat)
  t.stat<-sqrt(P)*f_mean/(var(f_hat-f_mean))^0.5
  p.val<-1-pt(t.stat,df = df)
  
  result<-data.frame(t.stat,p.val)
  colnames(result)<-c("t.stat","p.val")
  return(result)
}