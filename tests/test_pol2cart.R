test.polar.cartesian<-function(){
  tol<-1e-5
  for (r in runif(10)*10) {
    for (t in 1:360) {
      cart<-pol2cart(r,t)
      pol<-cart2pol(cart[1],cart[2])    
      if (t>180) {
        adjust<-270 
      } else {
        adjust<-90 
      }
      
      test_that("Conversions between polar and cartesian coordinates",expect_true(pol[1]-r<tol&round(pol[2])==adjust-t))
    }
  }
}