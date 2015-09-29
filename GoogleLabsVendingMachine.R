snacks<-c(917,134,1569,1649,1431,1622,233,2094,1072,915,1922,2437,2714,2491,1886,2812,426,1673,94,2139,2569,496,2249,1553,1580)
buttons<-c(16,23,61,7,7,7,13,13,13,19,19,21,27,56,56,73,77,97,11,37,41)

y<-length(snacks)
algo<-function(bSet){snackG<-((bSet[1])*(bSet[2])+(bSet[3])-(bSet[4])+(bSet[5]))}
r<-0
snack<-0
snackLog<-NULL

for (i in 1:y) {
  for (r in 1:100000) {
    
    bSet<-sample(buttons,5)
    snack<-algo(bSet)
    
    if (snack == snacks[i]) {
      snackLog[i]<-'Got it!'
      break
      
    }
    
    else {
      snackLog[i]<-'Hmmmm'
      
    }
    
    
  }
  
  
}

rm(bSet,buttons,i,r,snack,snacks,y)
snackLog<<-snackLog

print(snackLog)