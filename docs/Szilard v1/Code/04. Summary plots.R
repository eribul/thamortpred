
fit_pred_red <- glm(death90day ~ Kon + Alder + ASA  +
                              heart_infarct    +
                              kidney           ,
                              data = anne_pred2,
                              family = 'binomial')  
                              
                              
probs <- data.frame(Status = fit_pred_red$model$death90day, Probability = fit_pred_red$fitted)   


probs <- dplyr::mutate(probs, Status  = factor(Status, 
                                    levels = c(0, 1),
                                    labels = c('Alive', 'Death')))


p <- ggplot(probs, aes(Status, Probability)) + 
          geom_boxplot()                     +
          geom_boxplot(aes(fill = Status))   +
          theme_bw()     +
        geom_blank() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())                       
p  
 ggsave(file="Probability.png", dpi = 600) 
fitAge <- glm(death90day ~  Alder   +
                            heart_infarct    +
                            kidney           ,
                            data = anne_pred2,
                          family = 'binomial') 

###
newdat00 <- data.frame(Alder = 65:95, heart_infarct = rep(0, 31), kindey = rep(0, 31))    
newdat10 <- data.frame(Alder = 65:95, heart_infarct = rep(1, 31), kindey = rep(0, 31))    
newdat01 <- data.frame(Alder = 65:95, heart_infarct = rep(0, 31), kindey = rep(1, 31))    
newdat11 <- data.frame(Alder = 65:95, heart_infarct = rep(1, 31), kindey = rep(1, 31))    
pred00 <-  predict(fitAge, newdat00)                       