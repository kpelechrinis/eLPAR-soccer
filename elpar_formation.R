elpar_formation <- function(defense,middlefield,attack,rep.levels,mod,min.rating,max.rating){
    
    #defense/middlefield/attack: number of defenders/middlefielders/forwards in the formation
    #rep.levels: data.frame with replacement levels for the various lines
    #mod: is the win prob model
    #min/max.rating: the min/max player FIFA rating to calculate the elpar
    
    elpar_formation = data.frame(rating=c(),position=c(),elpar = c())
    
    # We first calculate the expected points for a home team of replacement players (against a team of only replacement players) - this should be a little more than 1.5 points since it will essentially include only the home-field advantage
    
    # since both teams have only replacement players the average ratings of the lines have 0 difference
    
    test = data.frame(V1=0,V2=0,V3=0,V4=0)
    
    m = predict_skellam(mod_skellam_mle,test)
    rep.points = (3*sum(dskellam(c(1:10),m$mu1,m$mu2))) + dskellam(0,m$mu1,m$mu2)
    
    # calculate for each player rating and position the elpar
    
    for(i in min.rating:max.rating){
        
        #defense
        lin.rtg = rep(rep.levels$defense,defense-1)
        lin.rtg = append(lin.rtg,i)
        lin.rtg = mean(lin.rtg)
        test = data.frame(V1=lin.rtg-rep.levels$defense,V2=0,V3=0,V4=0)
        m = predict_skellam(mod_skellam_mle,test)
        def.points = (3*sum(dskellam(c(1:10),m$mu1,m$mu2))) + dskellam(0,m$mu1,m$mu2)
        elpar_formation <- rbind(elpar_formation,data.frame(rating=i,position="D",elpar=def.points-rep.points))
        
        #middlefield
        lin.rtg = rep(rep.levels$middlefield,middlefield-1)
        lin.rtg = append(lin.rtg,i)
        lin.rtg = mean(lin.rtg)
        test = data.frame(V1=0,V2=lin.rtg-rep.levels$middlefield,V3=0,V4=0)
        m = predict_skellam(mod_skellam_mle,test)
        mid.points = (3*sum(dskellam(c(1:10),m$mu1,m$mu2))) + dskellam(0,m$mu1,m$mu2)
        elpar_formation <- rbind(elpar_formation,data.frame(rating=i,position="M",elpar=mid.points-rep.points))
        
        #attack
        lin.rtg = rep(rep.levels$offense,attack-1)
        lin.rtg = append(lin.rtg,i)
        lin.rtg = mean(lin.rtg)
        test = data.frame(V1=0,V2=0,V3=lin.rtg-rep.levels$offense,V4=0)
        m = predict_skellam(mod_skellam_mle,test)
        off.points = (3*sum(dskellam(c(1:10),m$mu1,m$mu2))) + dskellam(0,m$mu1,m$mu2)
        elpar_formation <- rbind(elpar_formation,data.frame(rating=i,position="O",elpar=off.points-rep.points))
        
        #gk
        lin.rtg = i
        test = data.frame(V1=0,V2=0,V3=0,V4=lin.rtg-rep.levels$gk)
        m = predict_skellam(mod_skellam_mle,test)
        gk.points = (3*sum(dskellam(c(1:10),m$mu1,m$mu2))) + dskellam(0,m$mu1,m$mu2)
        elpar_formation <- rbind(elpar_formation,data.frame(rating=i,position="GK",elpar=gk.points-rep.points))
        
    }
    
    elpar_formation


}