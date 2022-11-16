#3 component
if (input$criteria_count == 3){
  for(i in 1:lnt){
    if (input$var_3_1 == constant[i,1] ){
      num_1_1 = constant[i,2]
      num_2_1 = constant[i,3]
      num_3_1 = constant[i,4]
      rowd_1 = constant[i,]
    }
  }
  for(i in 1:lnt){
    if (input$var_3_2 == constant[i,1] ){
      num_1_2 = constant[i,2]
      num_2_2 = constant[i,3]
      num_3_2 = constant[i,4]
      rowd_2 = constant[i,]
    }
  }
  for(i in 1:lnt){
    if (input$var_3_3 == constant[i,1] ){
      num_1_3 = constant[i,2]
      num_2_3 = constant[i,3]
      num_3_3 = constant[i,4]
      rowd_3 = constant[i,]
    }
  }
  daty <- rbind(rowd_1,rowd_2,rowd_3)
}
#4 component
if (input$criteria_count == 4){
  for(i in 1:lnt){
    if (input$var_4_1 == constant[i,1] ){
      num_1_1 = constant[i,2]
      num_2_1 = constant[i,3]
      num_3_1 = constant[i,4]
      rowd_1 = constant[i,]
    }
  }
  for(i in 1:lnt){
    if (input$var_4_2 == constant[i,1] ){
      num_1_2 = constant[i,2]
      num_2_2 = constant[i,3]
      num_3_2 = constant[i,4]
      rowd_2 = constant[i,]
    }
  }
  for(i in 1:lnt){
    if (input$var_4_3 == constant[i,1] ){
      num_1_3 = constant[i,2]
      num_2_3 = constant[i,3]
      num_3_3 = constant[i,4]
      rowd_3 = constant[i,]
    }
  }  
  for(i in 1:lnt){
    if (input$var_4_4 == constant[i,1] ){
      num_1_4 = constant[i,2]
      num_2_4 = constant[i,3]
      num_3_4 = constant[i,4]
      rowd_4 = constant[i,]
    }
  } 
  daty <- rbind(rowd_1,rowd_2,rowd_3,rowd_4)
  
}
#5 component
if (input$criteria_count == 5){
  for(i in 1:lnt){
    if (input$var_5_1 == constant[i,1] ){
      num_1_1 = constant[i,2]
      num_2_1 = constant[i,3]
      num_3_1 = constant[i,4]
      rowd_1 = constant[i,]
    }
  }
  for(i in 1:lnt){
    if (input$var_5_2 == constant[i,1] ){
      num_1_2 = constant[i,2]
      num_2_2 = constant[i,3]
      num_3_2 = constant[i,4]
      rowd_2 = constant[i,]
    }
  }
  for(i in 1:lnt){
    if (input$var_5_3 == constant[i,1] ){
      num_1_3 = constant[i,2]
      num_2_3 = constant[i,3]
      num_3_3 = constant[i,4]
      rowd_3 = constant[i,]
    }
  }  
  for(i in 1:lnt){
    if (input$var_5_4 == constant[i,1] ){
      num_1_4 = constant[i,2]
      num_2_4 = constant[i,3]
      num_3_4 = constant[i,4]
      rowd_4 = constant[i,]
    }
  }
  for(i in 1:lnt){
    if (input$var_5_5 == constant[i,1] ){
      num_1_5 = constant[i,2]
      num_2_5 = constant[i,3]
      num_3_5 = constant[i,4]
      rowd_5 = constant[i,]
    }
  } 
  daty <- rbind(rowd_1,rowd_2,rowd_3,rowd_4,rowd_5)
}
