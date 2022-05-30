    lichenclean<-lichendata%>%
      rename("lichenpresence"="Lichen presence","speciesnum"="number lichen species","north"="light N (umols)","east"="light E", "south"="light S","west"="light W","percentcoverage"="Percentage of Tree covered by lichen", "yellow_north"="Yellow Lichen North", "yellow_south"="Yellow Lichen South","yellow_total"="Yellow lichen total","green_south"="Green Lichen South","green_north"="Green Lichen North","green_total"="Green Lichen Total","gray_south"="Gray Lichen South Side","gray_north"="Gray Lichen North Side...18","gray_total"="Gray Lichen Total...19","total_south"="Lichen on South Total...24","total_north"="Lichen on North Total...25","north_coverage"="North percent coverage","south_coverage"="South Percent coverage","bottom"="Lichen on South Total...20","top"="Gray Lichen Total...23","tree"="f")%>%
      select(!bottom:top)

    lichen_long<-lichenclean%>%
      pivot_longer(yellow_south:total_north,names_to=c("species","lichen_dir"),values_to="lichen_cov",names_sep = "_")%>%
      mutate("lichen_covprop"=lichen_cov/(circumference/2))

    light_long<-lichenclean%>%
      pivot_longer(north:west,names_to = "light_dir",values_to = "light_cov")

    #total light coverage w total lichen coverage --> no signifcant correlation, p>0.05 but 0.11
    light_perc<-light_long%>%
      group_by(tree)%>%
      mutate("total_light"=sum(light_cov))%>%
      summarize(total_light,percentcoverage)

    ## `summarise()` has grouped output by 'tree'. You can override using the `.groups` argument.

    cor.test(light_perc$total_light,light_perc$percentcoverage)

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  light_perc$total_light and light_perc$percentcoverage
    ## t = 1.6049, df = 118, p-value = 0.1112
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.03397556  0.31709031
    ## sample estimates:
    ##       cor 
    ## 0.1461558

    #total light vs lichen for each species 
    light_spec<-lichen_long%>%
      group_by(tree)%>%
      filter(lichen_dir=="total")%>%
      summarize("total_light"=sum(north,east,south,west),species,lichen_covprop)

    ## `summarise()` has grouped output by 'tree'. You can override using the `.groups` argument.

    #green not significant, p>0.05
    green<-light_spec%>%
      filter(species=="green")
    cor.test(green$lichen_covprop,green$total_light)

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  green$lichen_covprop and green$total_light
    ## t = 0.82546, df = 28, p-value = 0.4161
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.2182558  0.4873405
    ## sample estimates:
    ##      cor 
    ## 0.154133

    #yellow not significant p>0.05
    yellow<-light_spec%>%
      filter(species=="yellow")
    cor.test(yellow$lichen_covprop,yellow$total_light)

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  yellow$lichen_covprop and yellow$total_light
    ## t = 0.49555, df = 28, p-value = 0.6241
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.2763097  0.4387713
    ## sample estimates:
    ##       cor 
    ## 0.0932413

    #grey not significant p>0.05
    gray<-light_spec%>%
      filter(species=="gray")
    cor.test(gray$lichen_covprop,gray$total_light)

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  gray$lichen_covprop and gray$total_light
    ## t = 1.0646, df = 28, p-value = 0.2962
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1755083  0.5205152
    ## sample estimates:
    ##      cor 
    ## 0.197232

    #total light vs lichen diversity - significant at the p<0.05 level, there is a positive correlation between total light levels and total number of species of lichen 
    light_div<-light_long%>%
      group_by(tree)%>%
      summarize("total_light"=sum(light_cov),speciesnum=mean(speciesnum))
    cor.test(light_div$speciesnum,light_div$total_light)

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  light_div$speciesnum and light_div$total_light
    ## t = 2.5527, df = 28, p-value = 0.01643
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.08801049 0.68719919
    ## sample estimates:
    ##       cor 
    ## 0.4345027

    #light on north and south - significant difference p-value <0.001 
    t.test(lichenclean$north,lichenclean$south, paired=TRUE)

    ## 
    ##  Paired t-test
    ## 
    ## data:  lichenclean$north and lichenclean$south
    ## t = -4.1697, df = 29, p-value = 0.0002519
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -118.51912  -40.51421
    ## sample estimates:
    ## mean of the differences 
    ##               -79.51667

    #lichen on north and south - not significant, p-value>0.05 
    t.test(lichenclean$north_coverage,lichenclean$south_coverage,paired=TRUE)

    ## 
    ##  Paired t-test
    ## 
    ## data:  lichenclean$north_coverage and lichenclean$south_coverage
    ## t = 1.1876, df = 29, p-value = 0.2446
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.04710127  0.17754284
    ## sample estimates:
    ## mean of the differences 
    ##              0.06522078

    #lichen on north and south for green- not significant, p-value >0.05
    green_south<-lichen_long%>%
      filter(species=="green",lichen_dir=="south")
    green_north<-lichen_long%>%
      filter(species=="green",lichen_dir=="north")
    t.test(green_south$lichen_covprop,green_north$lichen_covprop,paired=TRUE)

    ## 
    ##  Paired t-test
    ## 
    ## data:  green_south$lichen_covprop and green_north$lichen_covprop
    ## t = -1.0024, df = 29, p-value = 0.3245
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.11697962  0.04002891
    ## sample estimates:
    ## mean of the differences 
    ##             -0.03847536

    #for yellow - not significant p>0.05
    yellow_south<-lichen_long%>%
      filter(species=="yellow",lichen_dir=="south")
    yellow_north<-lichen_long%>%
      filter(species=="yellow",lichen_dir=="north")
    t.test(yellow_south$lichen_covprop,yellow_north$lichen_covprop,paired=TRUE)

    ## 
    ##  Paired t-test
    ## 
    ## data:  yellow_south$lichen_covprop and yellow_north$lichen_covprop
    ## t = -0.32046, df = 29, p-value = 0.7509
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.013585701  0.009904971
    ## sample estimates:
    ## mean of the differences 
    ##            -0.001840365

    #for grey - signficant at the p<0.05, true difference is > 0 (more on south than north)
    gray_south<-lichen_long%>%
      filter(species=="gray",lichen_dir=="south")
    gray_north<-lichen_long%>%
      filter(species=="gray",lichen_dir=="north")
    t.test(gray_south$lichen_covprop,gray_north$lichen_covprop,paired=TRUE)

    ## 
    ##  Paired t-test
    ## 
    ## data:  gray_south$lichen_covprop and gray_north$lichen_covprop
    ## t = 2.0951, df = 29, p-value = 0.045
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.001362651 0.113039675
    ## sample estimates:
    ## mean of the differences 
    ##              0.05720116

    #for differences in diversity - not significant, p>0.05
    div<-lichen_long%>%
      mutate("lichen_pres"=factor(ifelse(lichen_cov>0, "1", "0")))

    div_north<-div%>% #getting species diversity for north side
      filter(lichen_dir=="north")%>%
      filter(species!="total")%>%
      select(tree,lichen_pres)%>%
      mutate("lichen_pres"=as.numeric(lichen_pres)-1)%>%
      group_by(tree)%>%
      summarize("diversity"=sum(lichen_pres))

    div_south<-div%>% #getting species diversity for south side
      filter(lichen_dir=="south")%>%
      filter(species!="total")%>%
      select(tree,lichen_pres)%>%
      mutate("lichen_pres"=as.numeric(lichen_pres)-1)%>%
      group_by(tree)%>%
      summarize("diversity"=sum(lichen_pres))

    t.test(div_north$diversity,div_south$diversity,paired=TRUE)

    ## 
    ##  Paired t-test
    ## 
    ## data:  div_north$diversity and div_south$diversity
    ## t = -0.25414, df = 29, p-value = 0.8012
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.3015866  0.2349199
    ## sample estimates:
    ## mean of the differences 
    ##             -0.03333333

    ggplot(light_div,aes(x=speciesnum,y=total_light))+
      geom_point()+
      geom_smooth(method="lm",se=FALSE)+
      labs(title="Lichen Diversity Increases with Light Levels",subtitle="cor=0.4345, p<0.05",y="Total Recorded Light",x = "Number of Species")

    ## `geom_smooth()` using formula 'y ~ x'

![](Lichen-Data-Analysis_files/figure-markdown_strict/figure%202%20(correlation%20between%20lichen%20diversity%20and%20total%20light%20coverage)-1.png)

    light_long_fig<-light_long%>%
      filter(light_dir=="north"|light_dir=="south")
    ggplot(light_long_fig)+
      geom_boxplot(aes(x=light_dir,y=light_cov))+
      labs(x="Direction",y="Recorded Light Levels (umols)",title="Difference in Light Levels on North and South Sides of Trees",subtitle="paired t-test, mean of differences = 79.517, p<0.001")

![](Lichen-Data-Analysis_files/figure-markdown_strict/figure%203%20(difference%20between%20light%20on%20north%20and%20south%20sides)-1.png)

    gray_south<-gray_south%>%
      select(tree,lichen_covprop)%>%
      mutate("south"=lichen_covprop)%>%
      select(tree,south)
    gray_north<-gray_north%>%
      select(tree,lichen_covprop)%>%
      mutate("north"=lichen_covprop)%>%
      select(tree,north)
    gray_coverage<-left_join(gray_south,gray_north,by="tree")%>%
      pivot_longer(south:north,names_to="direction",values_to = "coverage_prop")

    ggplot(gray_coverage)+
      geom_boxplot(aes(x=direction,y=coverage_prop))+
      labs(title="Difference in Gray Lichen Coverage on North and South Sides of Trees",subtitle="Paired t-test, mean of differences = 0.0572, p<0.05", x = "Direction", y= "Proportion of Circumference with Gray Lichen Presence")

![](Lichen-Data-Analysis_files/figure-markdown_strict/figure%204%20(difference%20between%20coverage%20on%20north%20and%20south%20sides%20for%20grey%20lichen)-1.png)
