## Script for calibrating radiocarbon dates with Rice (single depths)
  
# Load library ----

source("scripts/functions.R")
library <- c("rice")

# Install missing packages
invisible(lapply(library, install_if_missing))

# Load the library
lapply(library, require, character.only = TRUE)


# 1) Arabian Peninsula ----

#Al Balid 1, Oman
calibrate(age = 300,error = 40,cc=1) # plot of the radiocarbon and calibrated distributions, together with their hpd ranges
calib.balid_1 <- caldist(300, 40)
points.balid_1 <- point.estimates(calib.balid_1)
points.balid_1 # midpoint = 380.0; weighted mean= 374.1; median =383.0      

#Boura, Syria
calibrate(age = 5730,error = 120,cc=1)
calib.Boura <- caldist(5730, 120)
points.Boura <- point.estimates(calib.Boura)
points.Boura # midpoint = 6540.0; weighted mean= 6535.1; median =6528.8               

#Mangrove_Suwayh_2_Oman
calibrate(age = 5605,error = 60,cc=2)
calib.Suwayh<- caldist(5605, 60,  deltaR =1.1
                       ,deltaSTD = 0, cc=2)
points.Suwayh <- point.estimates(calib.Suwayh)
points.Suwayh # midpoint = 5784.5 ; weighted mean= 5791.3  ;median= 5787.4     

# Wadi Sana, Yemen
#UGa- 30575
calibrate(age = 400,error = 20,cc=1)
calib.400<- caldist(400, 20)
points.400 <- point.estimates(calib.400)
points.400 # midpoint = 419.0 ; weighted mean= 463.1 ; median=479.5                            

#UGa-30576
calibrate(age = 670,error = 25,cc=1)
calib.670<- caldist(670, 25)
points.670 <- point.estimates(calib.670)
points.670 # midpoint = 616.0 ; weighted mean= 618.5  ; median= 636.8                          

#A-11779
calibrate(age = 690,error = 45,cc=1)
calib.690<- caldist(690, 45)
points.690 <- point.estimates(calib.690)
points.690 # midpoint = 621.0; weighted mean= 628.7; median= 644.8   

#UGa-30577
calibrate(age = 1930,error = 20,cc=1)
calib.1930<- caldist(1930, 20)
points.1930 <- point.estimates(calib.1930)
points.1930 # midpoint = 1836.0 ; weighted mean= 1849.8   ; median=1852.5                         

#UGa-30579
calibrate(age = 3740,error = 25,cc=1)
calib.3740<- caldist(3740, 25)
points.3740 <- point.estimates(calib.3740)
points.3740 # midpoint = 4103.5 ; weighted mean= 4087.3 ; median= 4094.9                  

#UGa-30580
calibrate(age = 4210,error = 25,cc=1)
calib.4210<- caldist(4210, 25)
points.4210 <- point.estimates(calib.4210)
points.4210 # midpoint = 4745.5 ; weighted mean= 4750.8; median=4739.9                 

#A-11052
calibrate(age = 4230,error = 50,cc=1)
calib.4230<- caldist(4230, 50)
points.4230 <- point.estimates(calib.4230)
points.4230 # midpoint = 4745.5 ; weighted mean= 4750.8; median=  4742.5         

#UGa-30585
calibrate(age = 4340,error = 25,cc=1)
calib.4340<- caldist(4340, 25)
points.4340 <- point.estimates(calib.4340)
points.4340 # midpoint = 4906.5 ; weighted mean= 4904.4; median= 4900.8             

#UGa-30583
calibrate(age = 4450,error = 25,cc=1)
calib.4450<- caldist(4450, 25)
points.4450 <- point.estimates(calib.4450)
points.4450 # midpoint = 5082.5; weighted mean= 5054.2; median= 4997.6                       

#A-11131
calibrate(age = 4490,error = 75,cc=1)
calib.4490<- caldist(4490, 75)
points.4490 <- point.estimates(calib.4490)
points.4490 # midpoint = 5094.5 ; weighted mean= 5056.3; median= 4999.8                 

#AA-39071
calibrate(age = 4490,error = 44,cc=1)
calib.4490<- caldist(4490, 44)
points.4490 <- point.estimates(calib.4490)
points.4490 # midpoint = 5098.0 ; weighted mean= 5108.8; median=  5108.4                

#A-11777
calibrate(age = 4555,error = 60,cc=1)
calib.4555<- caldist(4555, 60)
points.4555 <- point.estimates(calib.4555)
points.4555 # midpoint = 5217.0; weighted mean= 5178.4 ; median=  5163.4               

#GX-24614
calibrate(age = 4590,error = 90,cc=1)
calib.4590<- caldist(4590, 90)
points.4590 <- point.estimates(calib.4590)
points.4590 # midpoint = 5232.0; weighted mean= 5210.1 ; median=  5214.4                       

#AA-38421
calibrate(age = 4602,error = 45,cc=1)
calib.4602<- caldist(4602, 45)
points.4602 <- point.estimates(calib.4602)
points.4602 # midpoint = 5260.0; weighted mean= 5321.8; median= 5322.6    

#UGa-30584
calibrate(age = 4630,error = 25,cc=1) 
calib.4630<- caldist(4630, 25)
points.4630 <- point.estimates(calib.4630)
points.4630 # midpoint = 6057.5 ; weighted mean= 6062.0; median=  5411.8           

#UGa-30574
calibrate(age = 5170,error = 25,cc=1)
calib.5170<- caldist(5170, 25)
points.5170 <- point.estimates(calib.5170)
points.5170 # midpoint = 6057.5; weighted mean= 6062.0; median= 5925.5  

#UGa-30578
calibrate(age = 5270,error = 25,cc=1)
calib.5270<- caldist(5270, 25)
points.5270 <- point.estimates(calib.5270)
points.5270 # midpoint = 6057.5 ; weighted mean= 6062.0; median=  6071.7                 

#UGa-41448
calibrate(age = 5290,error = 20,cc=1)
calib.5290<- caldist(5290, 20)
points.5290 <- point.estimates(calib.5290)
points.5290 # midpoint = 6057.5 ; weighted mean= 6062.0 ; median= 6081.1  

## Dhofar, Oman

#ASOM 18 WP144-4
calibrate(age = 130,error = 20,cc=1, postbomb =2) #error
calib.1 <- caldist(130, 20,postbomb = TRUE)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 139.0 ; weighted mean=128.0 ; median= 106.1                           

#ASOM 18 WP 108-1b
calibrate(age = 110,error = 20,cc=1, postbomb =2)
calib.1 <- caldist(110, 20,postbomb = TRUE)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 144.0  ; median= 107.4, mean: 126.8            

#ASOM 18 WP 107-2b
calibrate(age = 140,error = 20,cc=1, postbomb =2)
calib.1 <- caldist(140, 20,postbomb = TRUE)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 137.0  ; median= 112.4, mean= 133.2                        

#ASOM 18 WP142-B
calibrate(age = 100,error = 20,cc=1, postbomb =2)
calib.1 <- caldist(100, 20,postbomb = TRUE)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 144.5 ; median= 113.6  , mean=  128.4       

#WP45-2
calibrate(age = 160,error = 25,cc=1, postbomb =2)
calib.1 <- caldist(160, 25,postbomb = TRUE)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 140.5   ; median= 170.3, mean=147.8        

#ASOM 18 WP153-2
calibrate(age = 340,error = 20,cc=1)
calib.1 <- caldist(340, 20)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 393.0  ; median= 382.3, mean= 389.9               

#ASOM 18 WP155-2B
calibrate(age = 510,error = 20,cc=1)
calib.1 <- caldist(510, 20)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 528.5  ; median= 528.0 , mean= 528.8        

#38-3a 4-6
calibrate(age = 710,error = 25,cc=1)
calib.1 <- caldist(710, 25)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 625.5 ; median= 663.3 , mean= 654.1                 

#ASOM 18 WP146
calibrate(age = 800,error = 20,cc=1)
calib.1 <- caldist(800, 20)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 705.0  ; median= 704.8 , mean=  705.2        

#WP50-2 0.5-2
calibrate(age = 970,error = 25,cc=1)
calib.1 <- caldist(970, 25)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 860.0 ; median= 851.2 , mean=858.2                  

#ASOM 18 WP155-2C
calibrate(age = 1540,error = 20,cc=1)
calib.1 <- caldist(1540, 20)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 1435.0  ; weighted mean= 1417.3 , median= 1401.3       

#WP38-2 B
calibrate(age = 1570,error = 25,cc=1)
calib.1 <- caldist(1570, 25)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 1455.0 ; median= 1459.8, mean= 1457.8             

#ASOM 18 WP147-1
calibrate(age = 1580,error = 20,cc=1,)
calib.1 <- caldist(1540, 20)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 1435.0  ; median= 1401.3 , mean=   1417.3             

#WP50-3a 7-9
calibrate(age = 1640,error = 25,cc=1)
calib.1 <- caldist(1640, 25)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 1498.5  ; median= 1520.2  , mean=  1503.0             

#ASOM 18 WP138
calibrate(age = 1680,error = 20,cc=1)
calib.1 <- caldist(1680, 20)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 1609.5  ; median= 1564.4, mean=1576.3        

#WP151
calibrate(age = 1690,error = 20,cc=1)
calib.1 <- caldist(1690, 20)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 1611.5  ; median= 1572.6  , mean=  1585.8           

#ASOM 18 WP145-4
calibrate(age = 1740,error = 20,cc=1)
calib.1 <- caldist(1740, 20)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 1626.5  ; median= 1627.8 , mean= 1635.9          

#ASOM 18 WP111-2E
calibrate(age = 1760,error = 20,cc=1)
calib.1 <- caldist(1760, 20)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 1651.5  ; median= 1649.2, mean= 1651.1           

#ASOM 18 WP155-D
calibrate(age = 2820,error = 20,cc=1)
calib.1 <- caldist(2820, 20)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 2926.5  ; median= 2917.7, mean=   2918.5        

#ASOM 18 WP103-2c
calibrate(age = 2940,error = 20,cc=1)
calib.1 <- caldist(2940, 20)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 3084.5  ; median= 3100.8, mean=3096.9       

#ASOM 18 WP149-2
calibrate(age = 3030,error = 20,cc=1)
calib.1 <- caldist(3030, 20)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 3250.0 ; median= 3230.9 , mean= 3242.4       

#155-F
calibrate(age = 3690,error = 20,cc=1)
calib.1 <- caldist(3690, 20)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 4035.0; weighted mean= 4031.4  ; median= 4035.1        


# 2) Mediterranean ----


# Abiare Tunisia
calibrate(age = 1050,error = 130,cc=1)
calib.Abiare <- caldist(1050, 130)
points.Abiare <- point.estimates(calib.Abiare)
points.Abiare # midpoint = 996.0; weighted mean= 967.5; median = 962.6                 


#Akotivika_1_Greece
calibrate(age = 2810,error = 60,cc=1)
calib.Akotivika<- caldist(2810, 60)
points.Akotivika <- point.estimates(calib.Akotivika)
points.Akotivika # midpoint = 2921.0; weighted mean= 2922.7  ; median =  2916.7               

#Akotivika_4_Greece
calibrate(age = 6132,error = 22,cc=1)
calib.Akotivika_4<- caldist(6132, 22)
points.Akotivika_4 <- point.estimates(calib.Akotivika_4)
points.Akotivika_4 # midpoint = 7050.0; weighted mean= 7038.6; median = 7007.9            

#Beni_Mtir_1_Tunisia
calibrate(age = 470,error = 50,cc=1)
calib.Beni_Mtir<- caldist(470, 50)
points.Beni_Mtir <- point.estimates(calib.Beni_Mtir)
points.Beni_Mtir # midpoint = 503.6 ; weighted mean= 478.0; median = 510.7         

#Beni_Mtir_2_Tunisia
calibrate(age = 740,error = 50,cc=1)
calib.Beni_Mtir_2<- caldist(740, 50)
points.Beni_Mtir_2 <- point.estimates(calib.Beni_Mtir_2)
points.Beni_Mtir_2 # midpoint = 647.5 ; weighted mean= 675.1 ; median =  677.7           

#Beysehir_Golu_1_Turkey
calibrate(age = 15390,error = 370,cc=1)
calib.Beysehir<- caldist(15390, 370)
points.Beysehir <- point.estimates(calib.Beysehir)
points.Beysehir # midpoint =  18715.0 ; weighted mean=  18679.2 ; median =  18675.4         

#Bozova_Lake_Turkey
calibrate(age = 2590,error = 70,cc=1)
calib.Bozova<- caldist(2590, 70)
points.Bozova <- point.estimates(calib.Bozova)
points.Bozova # midpoint =  8340.0 ; weighted mean=  8384.9  ; median =2678.7            

#El_Payo_Spain
calibrate(age = 3560,error = 40,cc=1)
calib.El_Payo<- caldist(3560, 40)
points.El_Payo <- point.estimates(calib.El_Payo)
points.El_Payo # midpoint = 3846.5  ; weighted mean=  3847.8 ; median = 3854.9                 

#El_Pirulejo_Spain
calibrate(age = 14250,error = 90,cc=1)
calib.El_Pirulejo<- caldist(14250, 90)
points.El_Pirulejo <- point.estimates(calib.El_Pirulejo)
points.El_Pirulejo # midpoint = 17385.0; weighted mean=  17317.5  ;median= 17304.0          

#Golbasi_Lake_Turkey
calibrate(age = 3080,error = 115,cc=1)
calib.Golbasi_Lake<- caldist(3080, 115)
points.Golbasi_Lake <- point.estimates(calib.Golbasi_Lake)
points.Golbasi_Lake # midpoint = 3257.5 ; weighted mean=  3255.4 ;median= 3264.1                  

#Gorgo_Pollicino_Italy
calibrate(age = 2325,error = 30,cc=1)
calib.Gorgo_Pollicino<- caldist(2325, 30)
points.Gorgo_Pollicino <- point.estimates(calib.Gorgo_Pollicino)
points.Gorgo_Pollicino # midpoint = 2298.0 ; weighted mean=  2335.4 ;median= 2342.8                

#Hoyran_Golu_Turkey
calibrate(age = 2470,error = 50,cc=1)
calib.Hoyran_Golu<- caldist(2470, 50)
points.Hoyran_Golu <- point.estimates(calib.Hoyran_Golu)
points.Hoyran_Golu # midpoint = 2542.0; weighted mean=  2553.5; median=2557.3       

#Turbera_Panera_Cabras_Spain
calibrate(age = 235,error = 35,cc=1)
calib.Turbera_Panera_Cabras<- caldist(235, 35)
points.Turbera_Panera_Cabras <- point.estimates(calib.Turbera_Panera_Cabras)
points.Turbera_Panera_Cabras # midpoint = 212.0; weighted mean= 227.7; median= 266.1                     

#Ouinet_Ennessours_Tunisia
calibrate(age = 470,error = 40,cc=1)
calib.Ouinet_Ennessours<- caldist(470, 40)
points.Ouinet_Ennessours <- point.estimates(calib.Ouinet_Ennessours)
points.Ouinet_Ennessours # midpoint = 447.5; weighted mean= 510.1  ;median= 512.6    

#Marcato_Cixe_Italy
calibrate(age = 935,error = 30,cc=1)
calib.Marcato_Cixe<- caldist(935, 30)
points.Marcato_Cixe <- point.estimates(calib.Marcato_Cixe)
points.Marcato_Cixe # midpoint = 835.5; weighted mean= 845.5 ;median=  846.8                 

#Melen_Golu_Turkey
calibrate(age = 2840,error = 100,cc=1)
calib.Melen_Golu<- caldist(2840, 100)
points.Melen_Golu <- point.estimates(calib.Melen_Golu)
points.Melen_Golu # midpoint = 2985.5 ; weighted mean= 2982.8  ;median= 2971.8 

#Navarres_1_Spain
calibrate(age = 5150,error = 50,cc=1)
calib.Navarres_1<- caldist(5150, 50)
points.Navarres_1 <- point.estimates(calib.Navarres_1)
points.Navarres_1 # midpoint = 5872.5; weighted mean= 5890.4 ;median= 5905.8 

# Demiryurt_Golu_Turkey
calibrate(age = 1940,error = 60,cc=1)
calib.1 <- caldist(1940, 60)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 1857.5 ; median= 1860.3 : weighted mean: 1860.6    

# 3) Sahara ----

#Amekni_Algeria
calibrate(age = 6800,error = 220,cc=1)
calib.Amekni<- caldist(6800, 220)
points.Amekni <- point.estimates(calib.Amekni)
points.Amekni # midpoint = 7645.0 ; weighted mean= 7664.2 ; median = 7659.3                    

#Bilgoy_Chad
calibrate(age = 7590,error = 120,cc=1)
calib.Bilgoy<- caldist(7590, 120)
points.Bilgoy <- point.estimates(calib.Bilgoy)
points.Bilgoy # midpoint =  8340.0 ; weighted mean=  8384.9; median =   8386.0        

#El_Atrun_Sudan
calibrate(age = 9180,error = 200,cc=1)
calib.El_Atrun<- caldist(9180, 200)
points.El_Atrun <- point.estimates(calib.El_Atrun)
points.El_Atrun # midpoint = 10397.5 ; weighted mean=  10369.1 ; median = 10368.7      

#Hassi_Meniet_Algeria
calibrate(age = 5410,error = 300,cc=1)
calib.Hassi_Meniet<- caldist(5410, 300)
points.Hassi_Meniet <- point.estimates(calib.Hassi_Meniet)
points.Hassi_Meniet # midpoint = 6185.0; weighted mean=  6185.3;median=6182.1                 

#Tjolumi_Chad
calibrate(age = 5845,error = 80,cc=1)
calib.Tjolumi<- caldist(5845, 80)
points.Tjolumi <- point.estimates(calib.Tjolumi)
points.Tjolumi # midpoint = 6650.0 ; weighted mean= 6651.0  ;median=6650.6                 

#Messak_Settafet_Lybia
calibrate(age = 4380,error = 145,cc=1)
calib.Messak_Settafet<- caldist(4380, 145)
points.Messak_Settafet <- point.estimates(calib.Messak_Settafet)
points.Messak_Settafet # midpoint = 5008.0 ; weighted mean= 4896.1   ;median=4901.1    

#Nemra_Chad
calibrate(age = 27000,error = 900,cc=1)
calib.Nemra<- caldist(27000, 900)
points.Nemra <- point.estimates(calib.Nemra)
points.Nemra # midpoint = 31240.0; weighted mean= 31243.4  ;median=  31161.3 

#Sai_island_Sudan
calibrate(age = 6060,error = 25,cc=1)
calib.Sai_island<- caldist(6060, 25)
points.Sai_island <- point.estimates(calib.Sai_island)
points.Sai_island # midpoint = 6892.5; weighted mean= 6910.2 ;median= 6910.0                  

#Tan_Tartait_Algeria
calibrate(age = 4470,error = 250,cc=1)
calib.Tan_Tartait<- caldist(4470, 250)
points.Tan_Tartait <- point.estimates(calib.Tan_Tartait)
points.Tan_Tartait # midpoint = 5062.0; weighted mean= 4892.0; median=  4886.3   

#Trou_au_Natron_Chad
calibrate(age = 14970,error = 400,cc=1)
calib.Trou_au_Natron<- caldist(14970, 400)
points.Trou_au_Natron <- point.estimates(calib.Trou_au_Natron)
points.Trou_au_Natron # midpoint = 18090.0; weighted mean= 18182.6  ;median= 18213.7             
# South-west Fourtchiak, Chad
calibrate(age = 21000,error = 2000,cc=1)
calib.1 <- caldist(21000, 2000)
points.1 <- point.estimates(calib.1)
points.1 # midpoint =24874.0  ; median= 24329.5  : weighted mean:24403.4   

#Amara_West_Sudan
#P1_17E
calibrate(age = 9365,error = 40,cc=1)
calib.9365 <- caldist(9365, 40)
points.9365 <- point.estimates(calib.9365)
points.9365 # midpoint = 10567.5 ; weighted mean= 10584.9 ; median=   10579.7         
#P2_17W
calibrate(age = 9323,error = 35,cc=1)
calib.9323 <- caldist(9323, 35)
points.9323 <- point.estimates(calib.9323)
points.9323 # midpoint = 10530.0; weighted mean= 10525.5  ; median=  10528.2              
#P2_17W a
calibrate(age = 9305,error = 40,cc=1)
calib.9305 <- caldist(9305, 40)
points.9305 <- point.estimates(calib.9305)
points.9305 # midpoint = 10475.0  ; weighted mean= 10495.6  ; median= 10504.1             
# P4_7
calibrate(age = 9305,error = 35,cc=1)
calib.9305_2 <- caldist(9305, 35)
points.9305_2 <- point.estimates(calib.9305_2)
points.9305_2 # midpoint = 10477.5; weighted mean= 10499.0; median=  10506.7                
#P5_12
calibrate(age = 9355,error = 35,cc=1)
calib.9355 <- caldist(9355, 35)
points.9355 <- point.estimates(calib.9355)
points.9355 # midpoint = 10560.0; weighted mean= 10571.3; median=10565.4   

# Taessa, Algeria

#GIF 8529 V
calibrate(age = 4630,error = 80,cc=1)
calib.4630<- caldist(4630, 80)
points.4630 <- point.estimates(calib.4630)
points.4630 # midpoint = 5285.0  ; weighted mean= 5321.0 ; median= 5352.6                

#GIF 8525 I
calibrate(age = 4910,error = 80,cc=1)
calib.4910<- caldist(4910, 80)
points.4910 <- point.estimates(calib.4910)
points.4910 # midpoint = 5682.5 ; weighted mean= 5660.3 ; median=5649.2                


# TH125_Libya
calibrate(age = 4960,error = 175,cc=1)
calib.1 <- caldist(4960, 175)
points.1 <- point.estimates(calib.1)
points.1 # midpoint =  5747.5   ; median=  5703.5 : weighted mean: 5701.7           

# TH120_Libya
#TH120-1
calibrate(age = 4820,error = 50,cc=1)
calib.1 <- caldist(4820, 50)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 5492.5  ; median= 5526.6  : weighted mean:5532.0 

#TH120-2
calibrate(age = 6480,error = 50,cc=1)
calib.1 <- caldist(6480, 50)
points.1 <- point.estimates(calib.1)
points.1 # midpoint =  7375.0   ; median=  7371.1  : weighted mean:7378.6

# TH94_Libya
calibrate(age = 5530,error = 220,cc=1)
calib.1 <- caldist(5530, 220)
points.1 <- point.estimates(calib.1)
points.1 # midpoint =  6295.0  ; median=  6318.9   : weighted mean: 6319.2 

## TH113_Libya

#3b
calibrate(age = 3915,error = 165,cc=1)
calib.1 <- caldist(3915, 165)
points.1 <- point.estimates(calib.1)
points.1 # midpoint =  4376.5  ; median=  4376.5   : weighted mean:4349.5 

#2b
calibrate(age = 4225,error = 175,cc=1)
calib.1 <- caldist(4225, 175)
points.1 <- point.estimates(calib.1)
points.1 # midpoint =  4799.5  ; median= 4714.6   : weighted mean:4707.7     

# GIK12329-6_Atlantic_Ocean
calibrate(age = 20050,error = 2325,cc=2) 
calib.balid_1 <- caldist(20050, 2325, cc=2)
points.balid_1 <- point.estimates(calib.balid_1)
points.balid_1 # midpoint = 22892.0 ; weighted mean= 22989.3       ; median =23055.5   

# 4) Sub-Sahara ----

# Gobero 1, Niger (Burials)

#P-540
calibrate(age = 4990,error = 40,cc=1)
calib.4990<- caldist(4990, 40)
points.4990 <- point.estimates(calib.4990)
points.4990 # midpoint = 5747.5 ; weighted mean= 5725.2 ; median=5708.1                

#P-594
calibrate(age = 4590,error = 40,cc=1)
calib.4590<- caldist(4590, 40)
points.4590 <- point.estimates(calib.4590)
points.4590 # midpoint = 5747.5 ; weighted mean= 5725.2  ; median=5306.9        

#P-589
calibrate(age = 4090,error = 40,cc=1)
calib.4090<- caldist(4090, 40)
points.4090 <- point.estimates(calib.4090)
points.4090 # midpoint = 4629.0 ; weighted mean= 4621.2  ; median= 4600.6               

#P-543
calibrate(age = 4710,error = 40,cc=1)
calib.4710<- caldist(4710, 40)
points.4710 <- point.estimates(calib.4710)
points.4710 # midpoint = 5450.0; weighted mean= 5431.9 ; median= 5414.9                

#P-545
calibrate(age = 4860,error = 40,cc=1)
calib.4860<- caldist(4860, 40)
points.4860 <- point.estimates(calib.4860)
points.4860 # midpoint = 5590.0; weighted mean= 5582.4  ; median=5588.7                

#P-583
calibrate(age = 8470,error = 40,cc=1)
calib.8470<- caldist(8470, 40)
points.8470 <- point.estimates(calib.8470)
points.8470 # midpoint = 9487.5; weighted mean= 9489.8 ; median= 9490.9                        

#P-584
calibrate(age = 8420,error = 40,cc=1)
calib.8420<- caldist(8420, 40)
points.8420 <- point.estimates(calib.8420)
points.8420 # midpoint = 9425.0; weighted mean= 9444.6  ; median= 9453.2                   

#P-582
calibrate(age = 8220,error = 40,cc=1)
calib.8220<- caldist(8220, 40)
points.8220 <- point.estimates(calib.8220)
points.8220 # midpoint = 9210.0 ; weighted mean= 9182.1  ; median=  9181.9               

#P-542
calibrate(age = 8570,error = 40,cc=1)
calib.8570<- caldist(8570, 40)
points.8570 <- point.estimates(calib.8570)
points.8570 # midpoint = 9567.5; weighted mean= 9535.5  ; median=9532.8                   

#P-590
calibrate(age = 8620,error = 40,cc=1)
calib.8620<- caldist(8620, 40)
points.8620 <- point.estimates(calib.8620)
points.8620 # midpoint = 9605.0 ; weighted mean= 9591.0 ; median= 9580.9                 

#Anambe_Senegal
calibrate(age = 4600,error = 125,cc=1)
calib.Anambe<- caldist(4600, 125)
points.Anambe <- point.estimates(calib.Anambe)
points.Anambe # midpoint = 5230.0 ; weighted mean= 5178.1; median = 5160.8                 

#Diogo_1_Senegal
calibrate(age = 8930,error = 180,cc=1)
calib.Diogo_1<- caldist(8930, 180)
points.Diogo_1 <- point.estimates(calib.Diogo_1)
points.Diogo_1 # midpoint =   10015.0 ; weighted mean=  9996.6, median =  9995.3        

#Kouka_Chad
calibrate(age = 9690,error = 210,cc=1)
calib.Kouka<- caldist(9690, 210)
points.Kouka <- point.estimates(calib.Kouka)
points.Kouka # midpoint = 11090.0  ; weighted mean=  11051.8 ;median=  11033.1                    

#Lake_Hardibo_Ethiopia
calibrate(age = 2460,error = 70,cc=1)
calib.Hardibo<- caldist(2460, 70)
points.Hardibo <- point.estimates(calib.Hardibo)
points.Hardibo # midpoint = 2539.0; weighted mean= 2542.2 ;median= 2541.1                

#Mandi_Chad
calibrate(age = 9835,error = 135,cc=1)
calib.Mandi<- caldist(9835, 135)
points.Mandi <- point.estimates(calib.Mandi)
points.Mandi # midpoint = 11290.0; weighted mean= 11291.9; median= 11279.4             

#Mare_de_Kissi_Burkina_Fasso
calibrate(age = 530,error = 30,cc=1)
calib.Mare_de_Kissi<- caldist(530, 30)
points.Mare_de_Kissi <- point.estimates(calib.Mare_de_Kissi)
points.Mare_de_Kissi # midpoint = 568.0 ; weighted mean= 548.8  ;median=537.8                      
#Mowo_Cameroon
calibrate(age = 515,error = 95,cc=1)
calib.Mowo_Cameroon<- caldist(515, 95)
points.Mowo_Cameroon <- point.estimates(calib.Mowo_Cameroon)
points.Mowo_Cameroon # midpoint = 493.5 ; weighted mean= 528.9  ;median=535.5                                   
#Tiguent_Mauritania
calibrate(age = 2868,error = 120,cc=1)
calib.TIguent<- caldist(2868, 120)
points.TIguent <- point.estimates(calib.TIguent)
points.TIguent # midpoint = 3045.0; weighted mean= 3021.9  ;median= 3013.1               

# Groumoui, Cameroon
# Orstom 307
calibrate(age = 940,error = 280,cc=1)
calib.Orstom_307<- caldist(940, 280)
points.Orstom_307 <- point.estimates(calib.Orstom_307)
points.Orstom_307 # midpoint = 862.0 ; weighted mean= 897.4; median=  887.0                   

# Orstom 313
calibrate(age = 1190,error = 300,cc=1)
calib.Orstom_313 <- caldist(1190, 300)
points.Orstom_313 <- point.estimates(calib.Orstom_313)
points.Orstom_313 # midpoint = 1128.5; weighted mean= 1114.2; median=1104.4             


# Danka_Valley_Ethiopia
calibrate(age = 7920,error = 80,cc=1)
calib.1 <- caldist(7920, 80)
points.1 <- point.estimates(calib.1)
points.1 # midpoint = 8776.0   ; median= 8768.1    : weighted mean: 8779.5  

