
# Tema c.3 : Monte Carlo simulace a jejich vyuziti pri reseni zajimavych ukolu z pravdepodobnosti
# Iryna Didinova

rm(list = ls())
setwd('C:/iren/Statistika/DU')
set.seed(123)

##################################  Ukol 1: FInancni forecast  ##################################

# V tomto ukolu si predstavim, ze jsem marketingova manazerka jedne obchodni firmy a potrebuji predpovedet cisty zisk z prodeje noveho produktu v prvnim roce po jeho zavedeni. 
# Take reditel by chtel vedet pravdepodobnost toho, ze zavedeni produktu nebude uspesne a firma na tom prodela.
# Cisty zisk vypocteme nasledovne:
  
# net_profit = volume * (price - unit_cost) - fixed_cost

# Fixni naklady se rovnaji 120 tisic, ale ostatni parametry se vyviji s urcitou nejistotou. Se stejnou pravdepodobnosti se prodaji 100 tisic kusu, 75 tisic nebo 50 tisic 
# (zalezi na chovani konkurentu). Mezi objemem prodeje a cenou zbozi existuje neprima deterministicka zavislost (cim vetsi objem tim mensi je cena produktu). Takze v pristim roce 
# spolecnost proda 1 kus zbozi za 8, 10 nebo 11 dolaru. O nakladech na jednotku vime, ze se ridi trojuhelnikovou hustotou pravdepodobnosti s minimem v 5.5 $ a maximem v 7.5 $.
# (Pro podrobnejsi popis ulohy viz. dokument Word)

rm(list = ls())
set.seed(123)

# Ulozime objem a cenu do vektoru
volume.vec = as.vector(c(100000, 75000, 50000))
price.vec = as.vector(c(8, 10, 11))
# Vytvorime promenne
volume = 0
price = 0
unit_cost = 0
fixed_cost = 120000 
# Vytvorime tabulku pro ulozeni vysledku simulace
results_forecast = data.frame(scenario = 1:10000, volume  = 1:10000, price  = 1:10000, unit_cost = 1:10000, net_profit = 1:10000, fixed_cost = 1:10000,
                               fail_to_profit = 11, net_profit_check = 1:10000)

#install.packages("triangle")
library(triangle)
# Trojuhelnikova hustota pravdeposobnosti
data_for_hist = rtriangle(n = 10000000, 5.5, 7.5)
hist(data_for_hist, breaks = 100, col ="chartreuse", main="Triangle Distribution", xlab = "Unit cost") 

# Simulace 10 tisic pozorovani
for (i in 1:10000){
  # Nahodne vybereme scenario
  scenario = sample(1:3,1,prob = c(1/3, 1/3, 1/3))
  # Nahodne vybereme naklad na jednotku zbozi s trojuhelnikove hustoty  
  unit_cost = rtriangle(n = 1, 5.5, 7.5)
                      
  if (scenario == 1){
    volume = volume.vec[1]
    price = price.vec[1]
    
  }else if (scenario == 2){
    volume = volume.vec[2]
    price = price.vec[2]
    
  }else if(scenario == 3){
    volume = volume.vec[3]
    price = price.vec[3]
    
  }else {
    print(" CRITICAL ERROR!!!!!")
  }
  # Spocitame cisty zisk z prodeju noveho produktu
  net_profit = volume * (price - unit_cost) - fixed_cost
  net_profit_check = volume * (price - 6.5) - fixed_cost # pro kontrolu cisteho zisku se znamou unit_cost = 6.5

  # Pokud firma prodela, zapiseme do sloupce fail_to_profit jednicku
  if(net_profit <= 0){
    fail_to_profit = 1
  }else { fail_to_profit = 0
  }
  # Ulozime vysledky do datove tabulky
  results_forecast[i,1] <- scenario
  results_forecast[i,2] <- volume
  results_forecast[i,3] <- price
  results_forecast[i,4] <- unit_cost
  results_forecast[i,5] <- net_profit
  results_forecast[i,6] <- fixed_cost
  results_forecast[i,7] <- fail_to_profit
  results_forecast[i,8] <- net_profit_check
}
# Spocitame forecast cisteho zisku
mean_profit_forecast = mean(results_forecast$net_profit) 
mean_profit_forecast 
summary(results_forecast[,c(4,5)])
# Ocekavana hodnota cisteho zisku v pristim roce je 91 629.61 dolau

# Spocitame odhad pravdepodobnosti, ze firma prodela na prodeji noveho produktu
probability_of_fail = mean(results_forecast$fail_to_profit) #
probability_of_fail 
# Firma prodela na prodejii noveho produktu z pravdepodobnosti 8,67%

# Hostogram #1
hist_results = hist(results_forecast$net_profit,breaks = 100, col = "chartreuse", xlab = "Forecasted Net Profit", 
                    main = "Histogram of Forecasted Net Profit")
abline(v = 0, lty = 5, lwd = 2)
abline(v = mean(results_forecast$net_profit), lty = 5, lwd = 2)
text(x = 67000, y = 480,"Mean", cex = 1.3, col = "gray31")
# text(x = 160000, y = 480,"Mean = 91 629 $", cex = 1.1) # pro Word

# Histogram#2: Kvartily
hist_results = hist(results_forecast$net_profit,breaks = 100, col = "chartreuse", xlab = "Forecasted Net Profit", 
                    main = "Forecasted Net Profit: Quartiles")
abline(v = quantile(results_forecast$net_profit, prob= 0.25), lty = 5, lwd = 2)
abline(v = quantile(results_forecast$net_profit, prob= 0.50), lty = 5, lwd = 2)
abline(v = quantile(results_forecast$net_profit, prob= 0.75), lty = 5, lwd = 2)
text(x = 40000, y = 480, "25%", cex = 1.1)
text(x = 85000, y = 480, "50%", cex = 1.1)
text(x = 120000,y = 480, "75%", cex = 1.1)
#legend(x=-70000,y=440,c("1.Quartile = 57 910","2.Quartile = 100 961","3.Quartile = 130 592"))

# Rychla analyza pripadu ve kterych prodelame
fail = subset(results_forecast, net_profit <= 0 )
summary(fail[1:8])

hist(results_forecast$unit_cost, breaks = 50)
abline(v = 6.8)

##################################  Ukol 2: letadlo   ##################################
#100 people line up to board an airplane. Each has a boarding pass with assigned seat.
#However, the first person to board has lost his boarding pass and takes a random seat. After that, 
#each person takes the assigned seat if it is unoccupied, and one of unoccupied seats at random otherwise. 
#What is the probability that the last person to board gets to sit in his assigned seat?

rm(list = ls())
set.seed(123)
# Vytvorime tabulku, kam budou ulozene vysledky
results_airplane = data.frame(last_seats_in_assign_seat = 2:10001)

for( j in 1:10000){
  
  people = sample(1:100) # Pasazeri 
  seats_random = sample(1:100) # Randomni mista z kterych pasazeri budou vybirat
  seats_random_original = seats_random # Jsem pouzivala tento sloupec, kdyz jsem hledala chyby
  tickets = sample(1:100) # listky
  seats_taken = 0 # Sloupec mist, ktere pasazeri nakonec obsadi
  
  df = data.frame(people, tickets, seats_taken, seats_random,seats_random_original)
  
  df$tickets[1] = NA # Prvni pasazer ztratil svuj listek
  df$seats_taken[1]=df$seats_random[1] # Pasazer ztratil svuj listek a proto vybira misto nahodne
  df$seats_random[1]=NA # Toto misto uz zadny pasazer nebude moct obsadit 
  
  for(i in 2:99){ # Pasazeri 2 az 99
    
    # Zakonni misto pasazera i neni obsazeno 
    if(!(df$tickets[i] %in% df[1:(i-1),3])) {
      df$seats_taken[i] = df$tickets[i]
      # Toto misto uz zadny jiny pasazer nebude moct obsadit - prtoto toto cislo vymazeme z vektora randomnich mist
      df$seats_random[which(df$seats_random == df$tickets[i])]= NA
      
    # Zakonni misto pasazera i je obsazno 
    }else if (df$tickets[i] %in% df[1:(i-1),3]){
      # Proto si sedne na randomni volne misto - vybirame nahodne cislo z vektoru randomnich mist a nasledne toto cislo mazeme
      df$seats_taken[i] = sample(na.omit(df$seats_random),1)
      df$seats_random[ which(df$seats_random == df$seats_taken[i])]= NA
      
    }else { # pro kontrolu
      print("CRITICAL ERROR #1!!!!!")
    }
  }
  # kdyz zustava jen 1 hodnota ve sloupci "seats_random" "sample(na.omit(df$seats_random),1)" nevybira ji ale nejake randomni cislo od 1 do 100, 
  # proto pro posledniho cloveka pouziju "sum(na.omit(df$seats_random))"
  
  if(!(df$tickets[100] %in% df[1:100,3])) {
    
    df$seats_taken[100] = df$tickets[100]
    df$seats_random[which(df$seats_random == df$tickets[100])]= NA
    
  }else if (df$tickets[100] %in% df[1:100,3]){
    
    df$seats_taken[100] = sum(na.omit(df$seats_random))
    df$seats_random[ which(df$seats_random == df$seats_taken[100])]= NA
  }else {
    print("CRITICAL ERROR #1!!!!!")
  }
  
  ######################### Checks ########################
  # Tyto kontroly mi pomohly vsimnout si chyb v kodu a opravit je
  
  if (sum(duplicated(df$seats_taken)) >= 1){
    print("CRITICAL ERROR: seats taken are not unique!!!!!!!")
  } #else{ print("Check 1 : OK!!!")}
  
  if( sum(is.na(df$seats_taken))>= 1){
    print("CRITICAL ERROR: NA in seats taken!!!!!!!!")
  } #else{ print("Check 2 : OK!!!")}
  
  if(sum(df$seats_taken == 0) >=1){
    print("CRITICAL ERROR: seats taken is NULL!!!!!!!!")
  }#else{ print("Check 3 : OK!!!")}
  
  ### Vyplnime do tabulky results_airplane data pro odhad pravdepodobnosti ###
  
  if(df$tickets[100] == df$seats_taken[100]){
    
    results_airplane[j,1] <- 1 # Posledni pasazer sedi na svem zakonnim miste
  } else {
    results_airplane[j,1] <- 0 # Posledni pasazer nesedi na svem zakonnim miste
  }
}

# Finalni vypocet odhadu pravdepodobnosti
mean(results_airplane$last_seats_in_assign_seat)  # 0.4966
#1000 pozorovani: vysledek = 0.494
#10 000 pozorovani: vysledek = 0.5004
# S poctem pozorovani 10 tisic jsem dostala odhad pravdepodobnosti, ktery se skoro rovna teoreticke hodnote 0,5.

# Nakonec kod pro druhy ukol je celkem kratky, ale ztravila jsem s nim docela dost casu. Na zacatku jsem vubec netusila jak to 
# nasimulovat a jestli to vubec zvladnu, ale nakonec se mi to povedlo a byla to celkem stranda :)

