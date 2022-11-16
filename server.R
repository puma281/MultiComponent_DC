# Define server logic ----
constant_1 <- read.csv("data/antconst.csv")
constant <- constant_1[,1:5]
server <- function(input, output) {
  
  
  output$tablu <- renderTable({
    lnt = dim(constant)[1]
    #2 component
    if (input$criteria_count == 2){
    for(i in 1:lnt){
      if (input$var_2_1 == constant[i,1] ){
        rowd_1 = constant[i,]
        rowd_1$perc = input$per_2_1
      }
    }
    for(i in 1:lnt){
      if (input$var_2_2 == constant[i,1] ){
        rowd_2 = constant[i,]
        rowd_2$perc = input$per_2_2
      }
    }  
      daty <- rbind(rowd_1,rowd_2)
      datyf <- daty[order(daty[,2]),]
    }
    #3 component
    if (input$criteria_count == 3){
      for(i in 1:lnt){
        if (input$var_3_1 == constant[i,1] ){
          rowd_1 = constant[i,]
        }
      }
      for(i in 1:lnt){
        if (input$var_3_2 == constant[i,1] ){
          rowd_2 = constant[i,]
        }
      }
      for(i in 1:lnt){
        if (input$var_3_3 == constant[i,1] ){
          rowd_3 = constant[i,]
        }
      }
      daty <- rbind(rowd_1,rowd_2,rowd_3)
      daty_f <- sort
      datyf <- daty[order(daty[,2]),]
      
    }
    #4 component
    if (input$criteria_count == 4){
      for(i in 1:lnt){
        if (input$var_4_1 == constant[i,1] ){
          rowd_1 = constant[i,]
        }
      }
      for(i in 1:lnt){
        if (input$var_4_2 == constant[i,1] ){
          rowd_2 = constant[i,]
        }
      }
      for(i in 1:lnt){
        if (input$var_4_3 == constant[i,1] ){
          rowd_3 = constant[i,]
        }
      }  
      for(i in 1:lnt){
        if (input$var_4_4 == constant[i,1] ){
          rowd_4 = constant[i,]
        }
      } 
      daty <- rbind(rowd_1,rowd_2,rowd_3,rowd_4)
      datyf <- daty[order(daty[,2]),]
      
    }
    #5 component
    if (input$criteria_count == 5){
      for(i in 1:lnt){
        if (input$var_5_1 == constant[i,1] ){
          rowd_1 = constant[i,]
        }
      }
      for(i in 1:lnt){
        if (input$var_5_2 == constant[i,1] ){
          rowd_2 = constant[i,]
        }
      }
      for(i in 1:lnt){
        if (input$var_5_3 == constant[i,1] ){
          rowd_3 = constant[i,]
        }
      }  
      for(i in 1:lnt){
        if (input$var_5_4 == constant[i,1] ){
          rowd_4 = constant[i,]
        }
      }
      for(i in 1:lnt){
        if (input$var_5_5 == constant[i,1] ){
          rowd_5 = constant[i,]
        }
      } 
      daty <- rbind(rowd_1,rowd_2,rowd_3,rowd_4,rowd_5)
      datyf <- daty[order(daty[,2]),]
      
    }
    datyf
    
    })
  #Calculation of relative voltage
  output$act_plat <- renderText({
    lnt = dim(constant)[1]
    #2 component
    if (input$criteria_count == 2){
      for(i in 1:lnt){
        if (input$var_2_1 == constant[i,1] ){
          rowd_1 = constant[i,]
          rowd_1$perc = input$per_2_1
        }
      }
      for(i in 1:lnt){
        if (input$var_2_2 == constant[i,1] ){
          rowd_2 = constant[i,]
          rowd_2$perc = input$per_2_2
        }
      }
      daty_2 <- rbind(rowd_1,rowd_2)
      datyf_2 <- daty_2[order(daty_2[,2]),]
      if (input$sel_1 == 1){
        comp_f = datyf_2[,-6]
        table_2 = matrix(nrow = 2,ncol = 5)
        per = c(input$per_2_1,input$per_2_2)
        recv = c(input$rec_1,input$rec_2)
        table_2[,1] = datyf_2[,"perc"]
        table_2[1,2]= recv[1]*table_2[1,1]/100
        table_2[2,3] = recv[2]*table_2[2,1]/100
        table_2[2,2] = table_2[2,1]- table_2[2,3]
        table_2[1,3] = table_2[1,1] - table_2[1,2]
        table_2[,4] = table_2[,2]/sum(table_2[,2])
        table_2[,5] = table_2[,3]/sum(table_2[,3])
        myfunction_temp_bub_2 <- function(tmp) {
          #input feed xi
          numen <- table_2[,5]
          #Ki = Psat/p
          ps = NULL
          func = NULL
          for (i in 1:2) {
            ps[i] <- exp(comp_f[i,3] - comp_f[i,4] / (tmp+comp_f[i,5]))
            func[i] = numen[i] * ps[i]/((input$press)*760)
          }
          return(abs(sum(func)-1))
        }
        myfunction_temp_dew_2 <- function(tmp) {
          #input feed xi
          numen <- table_2[,4]
          #Ki = Psat/p
          ps = NULL
          func = NULL
          for (i in 1:2) {
            ps[i] <- exp(comp_f[i,3] - comp_f[i,4] / (tmp+comp_f[i,5]))
            func[i] = numen[i] *(input$press)*760/ps[i]
          }
          return(abs(sum(func)-1))
        }
        tdew = optimize(myfunction_temp_dew_2,c(-1E6,1E6))$minimum
        tbub = optimize(myfunction_temp_bub_2,c(-1E6,1E6))$minimum
        comp_f[,6] = exp(comp_f[,3]-comp_f[,4]/(tdew+comp_f[,5]))/((input$press)*760)
        comp_f[,7] = exp(comp_f[,3]-comp_f[,4] /(tbub+comp_f[,5]))/((input$press)*760)
        comp_f[,8] = sqrt(comp_f[,6]*comp_f[,7])
        comp_f[,9] = comp_f[,8]/comp_f[2,8]
        rel_f_vol_f <- round(comp_f[1,9],digits = 2)
        min_plat = round(log((input$rec_1/100)*(input$rec_2/100)/((1-input$rec_1/100)*(1-input$rec_2/100)))/log(rel_f_vol_f))
        #calc of phi and rmin
        alpha <- comp_f[,9]
        myfunction_tenta <- function(phi){
          #func = alpha(i)*z(i)/alpha(i)-phi
          #numen = alpha(i)*z(i)
          zzz <- per/100
          qded <- input$qual
          #den = alpha(i)-phi
          func = NULL
          for(i in 1:2){
            func[i] = alpha[i]*zzz[i]/(alpha[i]-phi)
          }
          return(abs(sum(func)-1+qded))
        }
        DD = sum(table_2[,2])
        theta = optimize(myfunction_tenta,comp_f[,9])$minimum
        func_pp = NULL
        xid = table_2[,4]
        for(i in 1:2){
          func_pp[i] = alpha[i]*xid[i]/(alpha[i]-theta)
        }
        vmin = sum(func_pp)
        rmin = (vmin - 1)
        rel_f_vol_f <- round(comp_f[1,9],digits = 2)
        min_plat = round(log((input$rec_1/100)*(input$rec_2/100)/((1-input$rec_1/100)*(1-input$rec_2/100)))/log(rel_f_vol_f))
        r_org = (input$org_reflex)*rmin
        psi = (r_org - rmin)/(r_org + 1)
        ref_k = 1-exp(((1+54.4*psi)/(11+117.2*psi))*((psi-1)/(psi^0.5)))
        act_plat_or = (ref_k + min_plat)/(1+ref_k)
        act_plat_org =  round(act_plat_or)
        output$rel_volt <- renderText({
          rel_f_vol_f
        })
        output$min_platty <- renderText({
          min_plat
        })
        output$ref_ratio <- renderText({
          rmin
        })
      } else {
       act_plat_org = "Wrong Order of components selected"
      }
      
    }
    #3 component relative volatility
    if (input$criteria_count == 3){
      for(i in 1:lnt){
        if (input$var_3_1 == constant[i,1] ){
          rowd_1 = constant[i,]
          rowd_1$perc = input$per_3_1
        }
      }
      for(i in 1:lnt){
        if (input$var_3_2 == constant[i,1] ){
          rowd_2 = constant[i,]
          rowd_2$perc = input$per_3_2
        }
    
      }
      for(i in 1:lnt){
        if (input$var_3_3 == constant[i,1] ){
          rowd_3 = constant[i,]
          rowd_3$perc = input$per_3_3
        }
      }  
      daty_3 <- rbind(rowd_1,rowd_2,rowd_3)
      datyf_3 <- daty_3[order(daty_3[,2]),]
      if (input$sel_1 == 1 && input$sel_2 == 2){
        comp_f = datyf_3[,-6]
        table_2 = matrix(data = 0,nrow = 3,ncol = 5)
        per = c(input$per_3_1,input$per_3_2,input$per_3_3)
        recv = c(input$rec_1,input$rec_2)
        table_2[,1] = datyf_3[,"perc"]
        table_2[1,2]= recv[1]*table_2[1,1]/100
        table_2[2,3] = recv[2]*table_2[2,1]/100
        table_2[2,2] = table_2[2,1]- table_2[2,3]
        table_2[1,3] = table_2[1,1] - table_2[1,2]
        table_2[3,3] = table_2[3,1]
        table_2[,4] = table_2[,2]/sum(table_2[,2])
        table_2[,5] = table_2[,3]/sum(table_2[,3])
        myfunction_temp_bub_2 <- function(tmp) {
          #input feed xi
          numen <- table_2[,5]
          #Ki = Psat/p
          ps = NULL
          func = NULL
          for (i in 1:3) {
            ps[i] <- exp(comp_f[i,3] - comp_f[i,4] / (tmp+comp_f[i,5]))
            func[i] = numen[i] * ps[i]/((input$press)*760)
          }
          return(abs(sum(func)-1))
        }
        myfunction_temp_dew_2 <- function(tmp) {
          #input feed xi
          numen <- table_2[,4]
          #Ki = Psat/p
          ps = NULL
          func = NULL
          for (i in 1:3) {
            ps[i] <- exp(comp_f[i,3] - comp_f[i,4] / (tmp+comp_f[i,5]))
            func[i] = numen[i] *(input$press)*760/ps[i]
          }
          return(abs(sum(func)-1))
        }
        tdew = optimize(myfunction_temp_dew_2,c(-1E6,1E6))$minimum
        tbub = optimize(myfunction_temp_bub_2,c(-1E6,1E6))$minimum
        comp_f[,6] = exp(comp_f[,3]-comp_f[,4]/(tdew+comp_f[,5]))/((input$press)*760)
        comp_f[,7] = exp(comp_f[,3]-comp_f[,4] /(tbub+comp_f[,5]))/((input$press)*760)
        comp_f[,8] = sqrt(comp_f[,6]*comp_f[,7])
        comp_f[,9] = comp_f[,8]/comp_f[2,8]
        rel_f_vol_f <- round(comp_f[1,9],digits = 2)
        min_plat = round(log((input$rec_1/100)*(input$rec_2/100)/((1-input$rec_1/100)*(1-input$rec_2/100)))/log(rel_f_vol_f))
        #calc of phi and rmin
        alpha <- comp_f[,9]
        myfunction_tenta <- function(phi){
          #func = alpha(i)*z(i)/alpha(i)-phi
          #numen = alpha(i)*z(i)
          
          zzz <- per/100
          qded <- input$qual
          #den = alpha(i)-phi
          func = NULL
          for(i in 1:3){
            func[i] = alpha[i]*zzz[i]/(alpha[i]-phi)
          }
          return(abs(sum(func)-1+qded))
        }
        DD = sum(table_2[,2])
        theta = optimize(myfunction_tenta,comp_f[,9])$minimum
        func_pp = NULL
        xid = table_2[,4]
        for(i in 1:3){
          func_pp[i] = alpha[i]*xid[i]/(alpha[i]-theta)
        }
        vmin = sum(func_pp)
        rmin = (vmin - 1)
      } else if(input$sel_1 == 2 && input$sel_2 == 3) {
      comp_f = datyf_3[,-6]
      table_2 = matrix(data = 0,nrow = 3,ncol = 5)
      per = c(input$per_3_1,input$per_3_2,input$per_3_3)
      recv = c(input$rec_1,input$rec_2)
      table_2[,1] = datyf_3[,"perc"]
      table_2[1,2]= recv[1]*table_2[1,1]/100
      table_2[2,3] = recv[2]*table_2[2,1]/100
      table_2[2,2] = table_2[2,1]- table_2[2,3]
      table_2[1,3] = table_2[1,1] - table_2[1,2]
      table_2[3,3] = table_2[3,1]
      table_2[,4] = table_2[,2]/sum(table_2[,2])
      table_2[,5] = table_2[,3]/sum(table_2[,3])
      myfunction_temp_bub_2 <- function(tmp) {
        #input feed xi
        numen <- table_2[,5]
        #Ki = Psat/p
        ps = NULL
        func = NULL
        for (i in 1:3) {
          ps[i] <- exp(comp_f[i,3] - comp_f[i,4] / (tmp+comp_f[i,5]))
          func[i] = numen[i] * ps[i]/((input$press)*760)
        }
        return(abs(sum(func)-1))
      }
      myfunction_temp_dew_2 <- function(tmp) {
        #input feed xi
        numen <- table_2[,4]
        #Ki = Psat/p
        ps = NULL
        func = NULL
        for (i in 1:3) {
          ps[i] <- exp(comp_f[i,3] - comp_f[i,4] / (tmp+comp_f[i,5]))
          func[i] = numen[i] *(input$press)*760/ps[i]
        }
        return(abs(sum(func)-1))
      }
      tdew = optimize(myfunction_temp_dew_2,c(-1E6,1E6))$minimum
      tbub = optimize(myfunction_temp_bub_2,c(-1E6,1E6))$minimum
      comp_f[,6] = exp(comp_f[,3]-comp_f[,4]/(tdew+comp_f[,5]))/((input$press)*760)
      comp_f[,7] = exp(comp_f[,3]-comp_f[,4] /(tbub+comp_f[,5]))/((input$press)*760)
      comp_f[,8] = sqrt(comp_f[,6]*comp_f[,7])
      comp_f[,9] = comp_f[,8]/comp_f[3,8]
      rel_f_vol_f <- round(comp_f[2,9],digits = 2)
      min_plat = round(log((input$rec_1/100)*(input$rec_2/100)/((1-input$rec_1/100)*(1-input$rec_2/100)))/log(rel_f_vol_f))
      #calc of phi and rmin
      alpha <- comp_f[,9]
      myfunction_tenta <- function(phi){
        #func = alpha(i)*z(i)/alpha(i)-phi
        #numen = alpha(i)*z(i)
        
        zzz <- per/100
        qded <- input$qual
        #den = alpha(i)-phi
        func = NULL
        for(i in 1:3){
          func[i] = alpha[i]*zzz[i]/(alpha[i]-phi)
        }
        return(abs(sum(func)-1+qded))
      }
      DD = sum(table_2[,2])
      theta = optimize(myfunction_tenta,comp_f[,9])$minimum
      func_pp = NULL
      xid = table_2[,4]
      for(i in 1:3){
        func_pp[i] = alpha[i]*xid[i]/(alpha[i]-theta)
      }
      vmin = sum(func_pp)
      rmin = (vmin - 1)
      }else{
        act_plat_org = "Wrong Order of components selected"
      }

      rel_f_vol_f <- round(comp_f[1,9],digits = 2)
      min_plat = round(log((input$rec_1/100)*(input$rec_2/100)/((1-input$rec_1/100)*(1-input$rec_2/100)))/log(rel_f_vol_f))
      r_org = (input$org_reflex)*rmin
      psi = ((r_org) - rmin) /(r_org + 1)
      ref_k = 1-exp(((1+54.4*psi)/(11+117.2*psi))*((psi-1)/(psi^0.5)))
      act_plat_or = (ref_k + min_plat)/(1+ref_k)
      act_plat_org = round(act_plat_or)
      output$rel_volt <- renderText({
        rel_f_vol_f
      })
      output$min_platty <- renderText({
        min_plat
      })
      output$ref_ratio <- renderText({
        rmin
      })
    }
    act_plat_org
    
    
  })
  
  
  
}

