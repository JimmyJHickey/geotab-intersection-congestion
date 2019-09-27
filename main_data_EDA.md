Main Data EDA
================

Reading in the data
===================

``` r
# I will replace this ad-hoc method of reading in the data with an automatic one

# replace with correct directory. Can use file.choose()
train <- read.csv("/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/bigquery-geotab-intersection-congestion-data/train.csv") 

# replace with correct directory. Can use file.choose()
test <- read.csv("/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/bigquery-geotab-intersection-congestion-data/test.csv") 
```

``` r
# Uncomment the below lines to view all of the data in separate tabs

# View(train)

# View(test)
```

``` r
summary(train)
```

    ##      RowId         IntersectionId      Latitude       Longitude     
    ##  Min.   :1920335   Min.   :   0.0   Min.   :33.65   Min.   :-87.84  
    ##  1st Qu.:2134687   1st Qu.: 306.0   1st Qu.:39.94   1st Qu.:-84.39  
    ##  Median :2349039   Median : 685.0   Median :39.99   Median :-75.17  
    ##  Mean   :2349039   Mean   : 836.3   Mean   :39.66   Mean   :-77.90  
    ##  3rd Qu.:2563391   3rd Qu.:1254.0   3rd Qu.:41.92   3rd Qu.:-75.08  
    ##  Max.   :2777743   Max.   :2875.0   Max.   :42.38   Max.   :-71.02  
    ##                                                                     
    ##             EntryStreetName               ExitStreetName  
    ##  North Broad Street : 18938   North Broad Street : 19507  
    ##  Roosevelt Boulevard: 12490   Roosevelt Boulevard: 13517  
    ##  South Broad Street : 11906   South Broad Street : 12249  
    ##  Washington Street  : 10516   Washington Street  : 10758  
    ##  Market Street      :  9725   Market Street      :  9747  
    ##  Frankford Avenue   :  8482   Frankford Avenue   :  9057  
    ##  (Other)            :785352   (Other)            :782574  
    ##   EntryHeading     ExitHeading          Hour          Weekend      
    ##  E      :172814   W      :173419   Min.   : 0.00   Min.   :0.0000  
    ##  W      :169378   E      :166424   1st Qu.: 8.00   1st Qu.:0.0000  
    ##  S      :137736   S      :140157   Median :13.00   Median :0.0000  
    ##  N      :137585   N      :138488   Mean   :12.43   Mean   :0.2775  
    ##  SW     : 60753   NE     : 61821   3rd Qu.:17.00   3rd Qu.:1.0000  
    ##  NE     : 60629   NW     : 59689   Max.   :23.00   Max.   :1.0000  
    ##  (Other):118514   (Other):117411                                   
    ##      Month                                                 Path       
    ##  Min.   : 1.000   North Broad Street_N_North Broad Street_N  :  7032  
    ##  1st Qu.: 7.000   North Broad Street_S_North Broad Street_S  :  6935  
    ##  Median : 9.000   Walnut Street_W_Walnut Street_W            :  5310  
    ##  Mean   : 9.104   Chestnut Street_E_Chestnut Street_E        :  5009  
    ##  3rd Qu.:11.000   South Broad Street_N_South Broad Street_N  :  4557  
    ##  Max.   :12.000   Roosevelt Boulevard_W_Roosevelt Boulevard_W:  4361  
    ##                   (Other)                                    :824205  
    ##  TotalTimeStopped_p20 TotalTimeStopped_p40 TotalTimeStopped_p50
    ##  Min.   :  0.000      Min.   :  0.000      Min.   :  0.000     
    ##  1st Qu.:  0.000      1st Qu.:  0.000      1st Qu.:  0.000     
    ##  Median :  0.000      Median :  0.000      Median :  0.000     
    ##  Mean   :  1.731      Mean   :  5.356      Mean   :  7.682     
    ##  3rd Qu.:  0.000      3rd Qu.:  0.000      3rd Qu.: 10.000     
    ##  Max.   :273.000      Max.   :318.000      Max.   :343.000     
    ##                                                                
    ##  TotalTimeStopped_p60 TotalTimeStopped_p80 TimeFromFirstStop_p20
    ##  Min.   :  0.00       Min.   :  0.00       Min.   :  0.000      
    ##  1st Qu.:  0.00       1st Qu.:  0.00       1st Qu.:  0.000      
    ##  Median :  0.00       Median : 16.00       Median :  0.000      
    ##  Mean   : 11.91       Mean   : 22.95       Mean   :  3.127      
    ##  3rd Qu.: 19.00       3rd Qu.: 35.00       3rd Qu.:  0.000      
    ##  Max.   :368.00       Max.   :689.00       Max.   :334.000      
    ##                                                                 
    ##  TimeFromFirstStop_p40 TimeFromFirstStop_p50 TimeFromFirstStop_p60
    ##  Min.   :  0.000       Min.   :  0.00        Min.   :  0.0        
    ##  1st Qu.:  0.000       1st Qu.:  0.00        1st Qu.:  0.0        
    ##  Median :  0.000       Median :  0.00        Median :  0.0        
    ##  Mean   :  9.051       Mean   : 12.61        Mean   : 18.8        
    ##  3rd Qu.:  0.000       3rd Qu.: 22.00        3rd Qu.: 31.0        
    ##  Max.   :347.000       Max.   :355.00        Max.   :358.0        
    ##                                                                   
    ##  TimeFromFirstStop_p80 DistanceToFirstStop_p20 DistanceToFirstStop_p40
    ##  Min.   :  0.00        Min.   :   0.000        Min.   :   0.00        
    ##  1st Qu.:  0.00        1st Qu.:   0.000        1st Qu.:   0.00        
    ##  Median : 27.00        Median :   0.000        Median :   0.00        
    ##  Mean   : 34.04        Mean   :   6.564        Mean   :  19.87        
    ##  3rd Qu.: 49.00        3rd Qu.:   0.000        3rd Qu.:   0.00        
    ##  Max.   :359.00        Max.   :1902.700        Max.   :3099.50        
    ##                                                                       
    ##  DistanceToFirstStop_p50 DistanceToFirstStop_p60 DistanceToFirstStop_p80
    ##  Min.   :   0.00         Min.   :   0.00         Min.   :   0.00        
    ##  1st Qu.:   0.00         1st Qu.:   0.00         1st Qu.:   0.00        
    ##  Median :   0.00         Median :   0.00         Median :  60.40        
    ##  Mean   :  28.26         Mean   :  43.27         Mean   :  81.92        
    ##  3rd Qu.:  52.90         3rd Qu.:  64.10         3rd Qu.:  85.60        
    ##  Max.   :3099.50         Max.   :3581.60         Max.   :4064.30        
    ##                                                                         
    ##            City       
    ##  Atlanta     :153363  
    ##  Boston      :182050  
    ##  Chicago     :133674  
    ##  Philadelphia:388322  
    ##                       
    ##                       
    ## 

``` r
summary(test)
```

    ##      RowId         IntersectionId      Latitude       Longitude     
    ##  Min.   :      0   Min.   :   0.0   Min.   :33.65   Min.   :-87.90  
    ##  1st Qu.: 480084   1st Qu.: 294.0   1st Qu.:39.94   1st Qu.:-84.38  
    ##  Median : 960167   Median : 683.0   Median :40.01   Median :-75.17  
    ##  Mean   : 960167   Mean   : 836.2   Mean   :39.66   Mean   :-77.88  
    ##  3rd Qu.:1440250   3rd Qu.:1260.0   3rd Qu.:41.94   3rd Qu.:-75.05  
    ##  Max.   :1920334   Max.   :2875.0   Max.   :42.41   Max.   :-71.00  
    ##                                                                     
    ##              EntryStreetName                 ExitStreetName   
    ##  Roosevelt Boulevard :  39718   Roosevelt Boulevard :  39205  
    ##  North Broad Street  :  29232   North Broad Street  :  31058  
    ##  South Broad Street  :  23226   South Broad Street  :  23790  
    ##  Washington Street   :  19968   Massachusetts Avenue:  20527  
    ##  Massachusetts Avenue:  19747   Washington Street   :  20407  
    ##                      :  19157   Market Street       :  18977  
    ##  (Other)             :1769287   (Other)             :1766371  
    ##   EntryHeading     ExitHeading          Hour          Weekend      
    ##  E      :355231   W      :357598   Min.   : 0.00   Min.   :0.0000  
    ##  W      :353642   E      :350160   1st Qu.: 8.00   1st Qu.:0.0000  
    ##  S      :296875   S      :304496   Median :12.00   Median :0.0000  
    ##  N      :295654   N      :299086   Mean   :12.43   Mean   :0.2715  
    ##  SE     :157203   NW     :157508   3rd Qu.:17.00   3rd Qu.:1.0000  
    ##  SW     :156025   NE     :151378   Max.   :23.00   Max.   :1.0000  
    ##  (Other):305705   (Other):300109                                   
    ##      Month                                                 Path        
    ##  Min.   : 1.0   Roosevelt Boulevard_SW_Roosevelt Boulevard_SW:  12761  
    ##  1st Qu.: 7.0   Roosevelt Boulevard_NE_Roosevelt Boulevard_NE:  12361  
    ##  Median : 9.0   North Broad Street_N_North Broad Street_N    :  10990  
    ##  Mean   : 9.1   North Broad Street_S_North Broad Street_S    :  10982  
    ##  3rd Qu.:11.0   Walnut Street_W_Walnut Street_W              :  10554  
    ##  Max.   :12.0   Chestnut Street_E_Chestnut Street_E          :   9984  
    ##                 (Other)                                      :1852703  
    ##            City       
    ##  Atlanta     :344973  
    ##  Boston      :405842  
    ##  Chicago     :295504  
    ##  Philadelphia:874016  
    ##                       
    ##                       
    ##
