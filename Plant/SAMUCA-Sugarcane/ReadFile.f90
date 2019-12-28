subroutine ReadFile(task)
    
    use VarDefs
    implicit none
    
    !Local Variables
    
    integer     i
    integer     task
    integer     ios
    integer     nhd
    integer     nf
    integer     ni
    integer     nline_met
    logical     flvalue
    
    integer     year_met
    integer     doy_met
    
    integer     ncalib_var   
    
    character (len=1000)    line
    character (len=100)     search
    character (len=30)      ignore
    character (len=1)    :: skip = '!'
    character (len=1)    :: stch = '*'    
    
    save
    
    goto (10,20,30,40,50,60) task
        
10  continue   
    
    !--- Initialization
    nsim = 0   
    
    !-------------------------!
    !--- Reading List File ---!
    !-------------------------!
    
    open(list,file=trim(pathwork)//'\Control\'//'ListControl.sam',status='old',action='read',iostat=ios)    
    call checkios(ios,1)   
    
    call readint('Simulation',1,nsim,size(nsim),.false.,.false.,list,trim('ListControl'),msg)
    call readcha('Simulation',2,ctrlfile,size(ctrlfile),.false.,.false.,list,trim('ListControl'),msg)    
    close(list)
    
    return
    
20  continue
    
    !----------------------------!
    !--- Reading Control File ---!
    !----------------------------!
    
    open(ctrl,file=trim(pathwork)//'\Control\'//trim(ctrlfile(sn))//'.ctl',status='old',action='read',iostat=ios)
    call checkios(ios,1)
    
    !General information
    call readcha('Project Name',1,prjname,1,.true.,.false.,ctrl,ctrlfile(sn),msg)
    call readcha('Authors',1,authors,1,.true.,.false.,ctrl,ctrlfile(sn),msg)
    call readcha('Contact',1,contact,1,.true.,.false.,ctrl,ctrlfile(sn),msg)
    call readcha('Site',1,site,1,.true.,.false.,ctrl,ctrlfile(sn),msg)
    call readcha('Simulation details',1,simdet,1,.true.,.false.,ctrl,ctrlfile(sn),msg)
    
    !Simulation Period
    call readint('Simulation Period',1,sp_id,size(sp_id),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Simulation Period',2,years,size(years),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Simulation Period',3,doys,size(doys),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Simulation Period',4,yeare,size(yeare),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Simulation Period',5,doye,size(doye),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readlog('Simulation Period',6,flsequencial,size(flsequencial),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Simulation Period',7,nseq,size(nseq),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readlog('Simulation Period',8,flreplicate,size(flreplicate),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Simulation Period',9,nrep,size(nrep),.false.,.false.,ctrl,ctrlfile(sn),msg)
    
    !Fields
    call readint('Fields',1,fd_id,size(fd_id),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readcha('Fields',2,weathfile,size(weathfile),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Fields',3,lat,size(lat),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Fields',4,lon,size(lon),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Fields',5,alt,size(alt),.false.,.false.,ctrl,ctrlfile(sn),msg)
    
    !Soil
    call readint('Soil',1,so_id,size(so_id),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readcha('Soil',2,soilfile,size(soilfile),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readcha('Soil',3,soilprof,size(soilprof),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Soil',4,cn,size(cn),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Soil',5,albedo,size(albedo),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Soil',6,swcon,size(swcon),.false.,.false.,ctrl,ctrlfile(sn),msg)
    
    !Crops
    call readint('Crops',1,cr_id,size(cr_id),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readcha('Crops',2,cropfile,size(cropfile),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readcha('Crops',3,cultivar_id,size(cultivar_id),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readcha('Crops',4,cultivar,size(cultivar),.false.,.false.,ctrl,ctrlfile(sn),msg)
    
    !Planting
    call readint('Planting',1,pl_id,size(pl_id),.false.,.false.,ctrl,ctrlfile(sn),msg)   
    call readint('Planting',2,yearp,size(yearp),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Planting',3,doyp,size(doyp),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Planting',4,rowsp,size(rowsp),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Planting',5,plantdepth,size(plantdepth),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readlog('Planting',6,ratoon,size(ratoon),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Planting',7,seq,size(seq),.false.,.false.,ctrl,ctrlfile(sn),msg)
    
    !Harvest
    call readint('Harvest',1,hv_id,size(hv_id),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Harvest',2,yearh,size(yearh),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Harvest',3,doyh,size(doyh),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Harvest',4,hindex,size(hindex),.false.,.false.,ctrl,ctrlfile(sn),msg)   
    
    !Irrigation
    call readint('Irrigation',1,ir_id,size(ir_id),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readlog('Irrigation',2,flirr,size(flirr),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Irrigation',3,irreff,size(irreff),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readcha('Irrigation',4,irrmethod,size(irrmethod),.false.,.false.,ctrl,ctrlfile(sn),msg)    
    
    !Soil Surface Residue
    call readint('Soil Surface Residue',1,ir_id,size(ir_id),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Soil Surface Residue',2,yearmulch,size(yearmulch),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Soil Surface Residue',3,doymulch,size(doymulch),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Soil Surface Residue',4,restype,size(restype),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Soil Surface Residue',5,mumass,size(mumass),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Soil Surface Residue',6,muwatfac,size(muwatfac),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Soil Surface Residue',7,muam,size(muam),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Soil Surface Residue',8,muext,size(muext),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Soil Surface Residue',9,mualb,size(mualb),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Soil Surface Residue',10,lam_mu_dry,size(lam_mu_dry),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Soil Surface Residue',11,lam_dmu_wet,size(lam_dmu_wet),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Soil Surface Residue',12,max_mulch_evap,size(max_mulch_evap),.false.,.false.,ctrl,ctrlfile(sn),msg)
    
    !Simulation Options
    call readint('Simulation Options',1,ir_id,size(ir_id),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Simulation Options',2,mesev,size(mesev),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Simulation Options',3,petmethod,size(petmethod),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Simulation Options',4,stempm,size(stempm),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Simulation Options',5,SwBotbHea,size(SwBotbHea),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Simulation Options',6,P_type,size(P_type),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Simulation Options',7,metpg,size(metpg),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Simulation Options',8,metpart,size(metpart),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readint('Simulation Options',9,tillermet,size(tillermet),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readlog('Simulation Options',10,usetsoil,size(usetsoil),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readlog('Simulation Options',11,sw_mulcheffect,size(sw_mulcheffect),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readlog('Simulation Options',12,potential_growth,size(potential_growth),.false.,.false.,ctrl,ctrlfile(sn),msg)    
    
    !Methods Parameters
    call readint('Methods Parameters',1,ir_id,size(ir_id),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Methods Parameters',2,slu1_rd,size(slu1_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Methods Parameters',3,hrnc_rd,size(hrnc_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Methods Parameters',4,dhrl_rd,size(dhrl_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Methods Parameters',5,drya1_rd,size(drya1_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Methods Parameters',6,drya2_rd,size(drya2_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Methods Parameters',7,dryb1_rd,size(dryb1_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Methods Parameters',8,dryb2_rd,size(dryb2_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Methods Parameters',9,sweta_rd,size(sweta_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Methods Parameters',10,swetb_rd,size(swetb_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Methods Parameters',11,swequa_rd,size(swequa_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Methods Parameters',12,swequb_rd,size(swequb_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)    
    call readrea('Methods Parameters',13,tbot_mean,size(tbot_mean),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Methods Parameters',14,tbot_ampli,size(tbot_ampli),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Methods Parameters',15,tbot_imref,size(tbot_imref),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readrea('Methods Parameters',16,tbot_ddamp,size(tbot_ddamp),.false.,.false.,ctrl,ctrlfile(sn),msg)
    
    !Outputs Options
    call readint('Outputs Options',1,ir_id,size(ir_id),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readlog('Outputs Options',2,detailedsoil,size(detailedsoil),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readlog('Outputs Options',3,flsoiltemp,size(flsoiltemp),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readlog('Outputs Options',4,writepotout,size(writepotout),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readlog('Outputs Options',5,writeactout,size(writeactout),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readlog('Outputs Options',6,writedetphoto,size(writedetphoto),.false.,.false.,ctrl,ctrlfile(sn),msg)
    call readlog('Outputs Options',7,writedcrop,size(writedcrop),.false.,.false.,ctrl,ctrlfile(sn),msg)
    
    close(ctrl)
    
    !WARNING: HAD TO DO THIS TO AVOID MAJOR CHANGE IN WHOLE CODE...EVALUATE HOW MULTI-YEARS-SIMS SHOULD BE IMPLEMENTED PROPERLY
    mulcheffect = sw_mulcheffect(1) 
    slu1        = slu1_rd(1)
	hrnc        = hrnc_rd(1)
	dhrlai      = dhrl_rd(1)
	a_val_3_a   = drya1_rd(1)
	a_val_3_b   = drya2_rd(1)
	b_val_3_a   = dryb1_rd(1)
	b_val_3_b   = dryb2_rd(1)
	a_val_1     = sweta_rd(1)
	b_val_1     = swetb_rd(1)
	a_val_2     = swequa_rd(1)

    lat_sim = lat(1)
    
        
    call wmsg(1)    
    !   Check control file
    call wmsg(2)

    return
        
!   Weather files opening
30 continue

    open(metf,file=trim(pathwork)//'\Weather\'//trim(weathfile(1))//'.met',status='old',action='read',iostat=ios)    
    call checkios(ios,1)    
    
    !keep file opened for daily readings
    return


40  continue
    
    !----------------------------!
    !--- Weather data reading ---!
    !----------------------------!
       
    !--- dummy nchar
    write(char_host(1),'(I4)') year
    
    if(doy .lt. 10) then
        write(char_host(2),'(I1)') doy
    elseif(doy .lt. 100) then
        write(char_host(2),'(I2)') doy
    else
        write(char_host(2),'(I3)') doy
    endif    
        
    !--- 7 wth data types
    npar = 7
    do i = 1,npar
        call readmeteo(trim(char_host(1))//'-'//trim(char_host(2)),2,i+2,real_host(i),size(real_host),.false.,.true.,metf,trim(weathfile(1)),msg)   
    enddo
    
    tmin     = real_host(1)
    tmax     = real_host(2)
    rain     = real_host(3)
    srad     = real_host(4)
    rh       = real_host(5)
    wind     = real_host(6)
    girr     = real_host(7)
      
    !Warning: kc input flag /n
    flkcinput = .false.
       
    !--- Check if simulations ended
    if(flrunend) close(metf)
    
    return
    

50  continue 
    
    !--------------------------!
    !--- Soil files reading ---!
    !--------------------------!
    
    open(soil_io,file=trim(pathwork)//'\Soil\'//trim(soilfile(1))//'.spd',status='old',action='read',iostat=ios)    
    call checkios(ios,1)
   
    call readrea(trim(soilprof(1)),1,wpp,size(wpp),.false.,.false.,soil_io,soilfile(1),msg)
    call readrea(trim(soilprof(1)),2,fcp,size(fcp),.false.,.false.,soil_io,soilfile(1),msg)
    call readrea(trim(soilprof(1)),3,sat,size(sat),.false.,.false.,soil_io,soilfile(1),msg)
    call readrea(trim(soilprof(1)),4,dep,size(dep),.false.,.false.,soil_io,soilfile(1),msg)
    call readrea(trim(soilprof(1)),5,drns,size(drns),.false.,.false.,soil_io,soilfile(1),msg)
    call readrea(trim(soilprof(1)),6,ksat,size(ksat),.false.,.false.,soil_io,soilfile(1),msg)
    call readrea(trim(soilprof(1)),7,swc_init,size(swc_init),.false.,.false.,soil_io,soilfile(1),msg) !Reading init conditions here could complicate multi-simulations with difent inital conditions
    call readrea(trim(soilprof(1)),8,psand,size(psand),.false.,.false.,soil_io,soilfile(1),msg)
    call readrea(trim(soilprof(1)),9,psilt,size(psilt),.false.,.false.,soil_io,soilfile(1),msg)
    call readrea(trim(soilprof(1)),10,pclay,size(pclay),.false.,.false.,soil_io,soilfile(1),msg)
    call readrea(trim(soilprof(1)),11,orgmat,size(orgmat),.false.,.false.,soil_io,soilfile(1),msg)
    call readrea(trim(soilprof(1)),12,snc,size(snc),.false.,.false.,soil_io,soilfile(1),msg)
    call readrea(trim(soilprof(1)),13,spc,size(spc),.false.,.false.,soil_io,soilfile(1),msg)
    call readrea(trim(soilprof(1)),14,skc,size(skc),.false.,.false.,soil_io,soilfile(1),msg)
   
    nlay = 0   
    do while(wpp(nlay+1) .gt. 0.001)
        nlay = nlay + 1       
    enddo
   
    close(soil_io)   

    return
    

60  continue
    
    !-------------------------------!
    !--- Crop parameters reading ---!
    !-------------------------------!
    
    open(crop,file=trim(pathwork)//'\Crop\'//trim(cropfile(1))//'.crp',status='old',action='read',iostat=ios)    
    call checkios(ios,1)
    
    !--- Read Integer parameters    
    nhd  = 10   ! Number of lines of file header   
    nf   = 108  ! Number of Real parameters (72 previous)
    ni   =  6   ! Number of Integer parameters
    
119 format(<nhd>(/)     , <ni>(I26,a30,/))    
120 format(<nf-1>(f26.5,a30,/)    ,f26.5,a30)
          
    read (crop, 119)   		        &
        inte_host(1)  , ignore, 	& ! (I)
        inte_host(2)  , ignore, 	& ! (I)
        inte_host(3)  , ignore, 	& ! (I)
        inte_host(4)  , ignore, 	& ! (I)
        inte_host(5)  , ignore, 	& ! (I)
        inte_host(6)
    
    read (crop, 120)                &
        real_host(1)  , ignore, 	& ! (R)
        real_host(2)  , ignore, 	& ! (R)
        real_host(3)  , ignore, 	& ! (R)
        real_host(4)  , ignore, 	& ! (R)
        real_host(5)  , ignore, 	& ! (R)
        real_host(6)  , ignore, 	& ! (R)
        real_host(7)  , ignore, 	& ! (R)
        real_host(8)  , ignore, 	& ! (R)
        real_host(9)  , ignore, 	& ! (R)
        real_host(10) , ignore, 	& ! (R)
        real_host(11) , ignore, 	& ! (R)
        real_host(12) , ignore, 	& ! (R)
        real_host(13) , ignore, 	& ! (R)
        real_host(14) , ignore, 	& ! (R)
        real_host(15) , ignore, 	& ! (R)
        real_host(16) , ignore, 	& ! (R)
        real_host(17) , ignore, 	& ! (R)
        real_host(18) , ignore, 	& ! (R)
        real_host(19) , ignore, 	& ! (R)
        real_host(20) , ignore, 	& ! (R)
        real_host(21) , ignore, 	& ! (R)
        real_host(22) , ignore, 	& ! (R)
        real_host(23) , ignore, 	& ! (R)
        real_host(24) , ignore, 	& ! (R)
        real_host(25) , ignore, 	& ! (R)
        real_host(26) , ignore, 	& ! (R)
        real_host(27) , ignore, 	& ! (R)
        real_host(28) , ignore, 	& ! (R)
        real_host(29) , ignore, 	& ! (R)
        real_host(30) , ignore, 	& ! (R)
        real_host(31) , ignore, 	& ! (R)
        real_host(32) , ignore, 	& ! (R)
        real_host(33) , ignore, 	& ! (R)
        real_host(34) , ignore, 	& ! (R)
        real_host(35) , ignore, 	& ! (R)
        real_host(36) , ignore, 	& ! (R)
        real_host(37) , ignore, 	& ! (R)
        real_host(38) , ignore, 	& ! (R)
        real_host(39) , ignore, 	& ! (R)
        real_host(40) , ignore, 	& ! (R)
        real_host(41) , ignore, 	& ! (R)
        real_host(42) , ignore, 	& ! (R)
        real_host(43) , ignore, 	& ! (R)
        real_host(44) , ignore, 	& ! (R)
        real_host(45) , ignore, 	& ! (R)
        real_host(46) , ignore, 	& ! (R)
        real_host(47) , ignore, 	& ! (R) 
        real_host(48) , ignore, 	& ! (R)
        real_host(49) , ignore, 	& ! (R)
        real_host(50) , ignore, 	& ! (R)
        real_host(51) , ignore, 	& ! (R)
        real_host(52) , ignore, 	& ! (R)
        real_host(53) , ignore, 	& ! (R)
        real_host(54) , ignore, 	& ! (R)
        real_host(55) , ignore, 	& ! (R)
        real_host(56) , ignore, 	& ! (R)
        real_host(57) , ignore, 	& ! (R)
        real_host(58) , ignore, 	& ! (R)
        real_host(59) , ignore, 	& ! (R)
        real_host(60) , ignore, 	& ! (R)
        real_host(61) , ignore, 	& ! (R)
        real_host(62) , ignore, 	& ! (R)
        real_host(63) , ignore, 	& ! (R)
        real_host(64) , ignore, 	& ! (R)
        real_host(65) , ignore, 	& ! (R)
        real_host(66) , ignore, 	& ! (R)
        real_host(67) , ignore, 	& ! (R)
        real_host(68) , ignore, 	& ! (R)
        real_host(69) , ignore, 	& ! (R)
        real_host(70) , ignore, 	& ! (R)
        real_host(71) , ignore, 	& ! (R)
        real_host(72) , ignore, 	& ! (R)
        real_host(73) , ignore, 	& ! (R)
        real_host(74) , ignore, 	& ! (R)
        real_host(75) , ignore, 	& ! (R)
        real_host(76) , ignore, 	& ! (R)
        real_host(77) , ignore, 	& ! (R)
        real_host(78) , ignore, 	& ! (R)
        real_host(79) , ignore, 	& ! (R)
        real_host(80) , ignore, 	& ! (R)
        real_host(81) , ignore, 	& ! (R)
        real_host(82) , ignore, 	& ! (R)
        real_host(83) , ignore, 	& ! (R)
        real_host(84) , ignore, 	& ! (R)
        real_host(85) , ignore, 	& ! (R)
        real_host(86) , ignore, 	& ! (R)
        real_host(87) , ignore, 	& ! (R)
        real_host(88) , ignore, 	& ! (R)
        real_host(89) , ignore, 	& ! (R)
        real_host(90) , ignore, 	& ! (R)
        real_host(91) , ignore, 	& ! (R)
        real_host(92) , ignore, 	& ! (R)
        real_host(93) , ignore, 	& ! (R)
        real_host(94) , ignore, 	& ! (R)
        real_host(95) , ignore, 	& ! (R)
        real_host(96) , ignore, 	& ! (R)
        real_host(97) , ignore, 	& ! (R)
        real_host(98) , ignore, 	& ! (R)
        real_host(99) , ignore, 	& ! (R)
        real_host(100) , ignore, 	& ! (R)
        real_host(101) , ignore, 	& ! (R)
        real_host(102) , ignore, 	& ! (R)
        real_host(103) , ignore, 	& ! (R)
        real_host(104) , ignore, 	& ! (R)
        real_host(105) , ignore, 	& ! (R)
        real_host(106) , ignore, 	& ! (R)
        real_host(107) , ignore, 	& ! (R)
        real_host(108)
    
    !--- Pass to crop global variables
    extcoef = real_host(107)

    close(crop)    
    
    return

    end subroutine ReadFile
    
    
    subroutine checkios(ios,task)
    ! Check i/o status of a file
    ! MSV - 11-jan-2017
    
    use VarDefs
    implicit none
    
    integer ios                 !i/o status
    integer task                !Dynamic control variable
    character (len=100) file    !File name
        
    goto (10) task
    
10  continue
        
    if(ios .ne. 0) then
        msg = 'Check List File in Control Folder'
        call wmsg(4)
    endif
    
    return
    
    
    end subroutine
    
    
    subroutine readcha(search,col,varcha,dime,singlelv,sameline,iou,filen,msg)
    ! Reads a character variable within an input file (unit = iou) based on a search string and columm
    ! MSV - 11-jan-2017
    
    implicit none
    
    integer iou                                 !i/o unit
    integer ios                                 !i/o status
    integer lnumber                             !line counter
    integer dimnumber                           !Variable dimension index
    integer col                                 !Column number
    integer dime                                !Variable dimension    
    
    character (len=*)       filen               !File name
    character (len=1000)    line                !Line of file (RECORD)
    character (len=1000)    msg                 !Ordinary msg for warnings
    character (len=10)      skipcol             !Ignored variable
    character (len=100)     fmtslv              !Format for a single value and line
    character (len=*)       search              !Search string
    character (len=*)       varcha(dime)        !Host variable
    
    character (len=1)    :: skip = '!'          !Skip character indicator
    character (len=1)    :: stch = '*'          !Search character indicator
    
    logical singlelv                            !Flag for single value at line
    logical sameline                            !Flag to start the reading on same search line

    
    lnumber     = 1
    dimnumber   = 1
    
    do
        read(iou,'(A1000)',iostat=ios) line
        if(ios .gt. 0) then
            write(msg,'(A,A,A,I,A)') 'Problem in reading, please check file',trim(filen),'. Line ',lnumber,'.'
            call wmsg(5)
        
        else if(ios .lt. 0) then
            write(msg,'(A,A,A,A,A)') 'No ',trim(search),' information found in ',trim(filen),' file.'
            call wmsg(5)
            exit
        endif      
    
        if(line(1:1) == skip .or. line(1:1)==char(9) .or. trim(line)=='') cycle
        
        if(line(1:(len_trim(search)+1)) == trim('*'//search)) then            
                        
            do while(dimnumber .le. dime)
                
            if(.not. sameline) read(iou,'(A1000)',iostat=ios) line            
            
            if(ios .gt. 0) then
            write(msg,'(A,A,A,A,A,I,A)') 'Problem in ',search,' reading, please check file',trim(filen),'. Line ',lnumber,'.'
            call wmsg(5)
            else if(ios .lt. 0)then
                exit
            endif   
            
            lnumber = lnumber + 1
            
            if(line(1:1) == skip .or. line(1:1)==char(9) .or. trim(line)=='') cycle            
            if(line(1:1) == stch) exit            
            
            if(singlelv) then                
                
                write(fmtslv,'(A,I,A)') '(A',len(varcha(dimnumber)),')'
                read(line,fmtslv,iostat=ios) varcha(dimnumber)
                exit
                
            else
                
            select case (col)       
            case (1)
                
                read(line,*) varcha(dimnumber)

            case (2)
                read(line,*) skipcol,varcha(dimnumber)
       
            case (3)
                read(line,*) skipcol,skipcol,varcha(dimnumber)
       
            case (4)
                read(line,*) skipcol,skipcol,skipcol,varcha(dimnumber)
       
            case (5)
                read(line,*) skipcol,skipcol,skipcol,skipcol,varcha(dimnumber)
       
            case (6)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,varcha(dimnumber)
       
            case (7)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varcha(dimnumber)

            case (8)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varcha(dimnumber)

            case (9)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varcha(dimnumber)

            case (10)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varcha(dimnumber)

            case (11)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varcha(dimnumber)

            case (12)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varcha(dimnumber)

            case (13)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varcha(dimnumber)

            case (14)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varcha(dimnumber)

            case (15)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varcha(dimnumber)

            case (16)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varcha(dimnumber)

            case (17)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varcha(dimnumber)

            case (18)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varcha(dimnumber)

            case (19)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varcha(dimnumber)

            case (20)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varcha(dimnumber)
       
            end select            
            
            dimnumber   =   dimnumber + 1
            
            endif
            
            enddo
            
            exit      
        endif
        
        
    enddo
    
    rewind(iou)
    
    end subroutine readcha
    
    
    subroutine readint(search,col,varint,dime,singlelv,sameline,iou,filen,msg)   
    ! Reads an integer variable within an input file (unit = iou) based on a search string and columm
    ! MSV - 11-jan-2017
    
    implicit none
    
    integer iou                                 !i/o unit
    integer ios                                 !i/o status
    integer lnumber                             !line counter
    integer dimnumber                           !Variable dimension index
    integer col                                 !Column number
    integer dime                                !Variable dimension
    integer varint(dime)                        !Host variable
    
    character (len=*)     filen                 !File name
    character (len=1000)    line                !Line of file (RECORD)
    character (len=1000)    msg                 !Ordinary msg for warnings
    character (len=10)      skipcol             !Ignored variable
    character (len=100)     fmtslv              !Format for a single value and line
    character (len=*)       search              !Search string
    
    character (len=1)    :: skip = '!'          !Skip character indicator
    character (len=1)    :: stch = '*'          !Search character indicator
    
    logical singlelv                            !Flag for single value at line
    logical sameline                            !Flag to start the reading on same search line

    
    lnumber     = 1
    dimnumber   = 1   
    
    
    do
        read(iou,'(A1000)',iostat=ios) line
        if(ios .gt. 0) then
            write(msg,'(A,A,A,I,A)') 'Problem in reading, please check file',trim(filen),'. Line ',lnumber,'.'
            call wmsg(5)
        
        else if(ios .lt. 0) then
            write(msg,'(A,A,A,A,A)') 'No ',trim(search),' information found in ',trim(filen),' file.'
            call wmsg(5)
            exit
        endif      
    
        if(line(1:1) == skip .or. line(1:1)==char(9) .or. trim(line)=='') cycle
        
        if(line(1:(len_trim(search)+1)) == trim('*'//search)) then            
                        
            do while(dimnumber .le. dime)
                
            if(.not. sameline) read(iou,'(A1000)',iostat=ios) line            
            
            if(ios .gt. 0) then
            write(msg,'(A,A,A,A,A,I,A)') 'Problem in ',search,' reading, please check file',trim(filen),'. Line ',lnumber,'.'
            call wmsg(5)
            else if(ios .lt. 0)then
                exit
            endif   
            
            lnumber = lnumber + 1
            
            if(line(1:1) == skip .or. line(1:1)==char(9) .or. trim(line)=='') cycle            
            if(line(1:1) == stch) exit
            
            if(singlelv) then                
                
                
                read(line,'(I5)',iostat=ios) varint(dimnumber)
                exit
                
            else
                
            select case (col)       
            case (1)                
                read(line,*) varint(dimnumber)          

            case (2)
                read(line,*) skipcol,varint(dimnumber)
       
            case (3)
                read(line,*) skipcol,skipcol,varint(dimnumber)
       
            case (4)
                read(line,*) skipcol,skipcol,skipcol,varint(dimnumber)
       
            case (5)
                read(line,*) skipcol,skipcol,skipcol,skipcol,varint(dimnumber)
       
            case (6)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,varint(dimnumber)
       
            case (7)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varint(dimnumber)

            case (8)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varint(dimnumber)

            case (9)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varint(dimnumber)

            case (10)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varint(dimnumber)

            case (11)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varint(dimnumber)

            case (12)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varint(dimnumber)

            case (13)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varint(dimnumber)

            case (14)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varint(dimnumber)

            case (15)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varint(dimnumber)

            case (16)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varint(dimnumber)

            case (17)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varint(dimnumber)

            case (18)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varint(dimnumber)

            case (19)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varint(dimnumber)

            case (20)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varint(dimnumber)
       
            end select            
            
            dimnumber   =   dimnumber + 1
            
            endif
            
            enddo
            
            exit      
        endif
        
        
    enddo
    
    rewind(iou)
    
    
    end subroutine readint
    
    
    subroutine readrea(search,col,varrea,dime,singlelv,sameline,iou,filen,msg)   
    ! Reads a real variable within an input file (unit = iou) based on a search string and columm
    ! MSV - 13-jan-2017
    
    implicit none
    
    integer iou                                 !i/o unit
    integer ios                                 !i/o status
    integer lnumber                             !line counter
    integer dimnumber                           !Variable dimension index
    integer col                                 !Column number
    integer dime                                !Variable dimension    
    
    character (len=*)       filen               !File name
    character (len=1000)    line                !Line of file (RECORD)
    character (len=1000)    msg                 !Ordinary msg for warnings
    character (len=10)      skipcol             !Ignored variable
    character (len=100)     fmtslv              !Format for a single value and line
    character (len=*)       search              !Search string
    
    real varrea(dime)                           !Host variable
    
    character (len=1)    :: skip = '!'          !Skip character indicator
    character (len=1)    :: stch = '*'          !Search character indicator
    
    logical singlelv                            !Flag for single value at line
    logical sameline                            !Flag to start the reading on same search line
    
    lnumber     = 1
    dimnumber   = 1
    
    do
        read(iou,'(A1000)',iostat=ios) line
        if(ios .gt. 0) then
            write(msg,'(A,A,A,I,A)') 'Problem in reading, please check file',trim(filen),'. Line ',lnumber,'.'
            call wmsg(5)
        
        else if(ios .lt. 0) then
            write(msg,'(A,A,A,A,A)') 'No ',trim(search),' information found in ',trim(filen),' file.'
            call wmsg(5)
            exit
        endif      
    
        if(line(1:1) == skip .or. line(1:1)==char(9) .or. trim(line)=='') cycle
        
        if(line(1:(len_trim(search)+1)) == trim('*'//search)) then            
                        
            do while(dimnumber .le. dime)
                
            if(.not. sameline) read(iou,'(A1000)',iostat=ios) line            
            
            if(ios .gt. 0) then
            write(msg,'(A,A,A,A,A,I,A)') 'Problem in ',search,' reading, please check file',trim(filen),'. Line ',lnumber,'.'
            call wmsg(5)
            else if(ios .lt. 0)then
                exit
            endif   
            
            lnumber = lnumber + 1
            
            if(line(1:1) == skip .or. line(1:1)==char(9) .or. trim(line)=='') cycle            
            if(line(1:1) == stch) exit
            
            if(singlelv) then                
                
                
                read(line,'(I5)',iostat=ios) varrea(dimnumber)
                exit
                
            else
                
            select case (col)       
            case (1)                
                read(line,*) varrea(dimnumber)          

            case (2)
                read(line,*) skipcol,varrea(dimnumber)
       
            case (3)
                read(line,*) skipcol,skipcol,varrea(dimnumber)
       
            case (4)
                read(line,*) skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (5)
                read(line,*) skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (6)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (7)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (8)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (9)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (10)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (11)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (12)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (13)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (14)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (15)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (16)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (17)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (18)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (19)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (20)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (21)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (22)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (23)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (24)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (25)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (26)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (27)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (28)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (29)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (30)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (31)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (32)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (33)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (34)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (35)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (36)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (37)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (38)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (39)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (40)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (41)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
            
            case (42)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (43)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (44)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (45)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (46)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
            
            case (47)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (48)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (49)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (50)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (51)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (52)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (53)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (54)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (55)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (56)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (57)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
            
            case (58)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
                
            case (59)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
            
            case (60)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            end select            
            
            dimnumber   =   dimnumber + 1
            
            endif
            
            enddo
            
            exit      
        endif
        
        
    enddo
    
    rewind(iou)
    
    
    end subroutine readrea
    
    
    subroutine readlog(search,col,varrea,dime,singlelv,sameline,iou,filen,msg)   
    ! Reads a real variable within an input file (unit = iou) based on a search string and columm
    ! MSV - 13-jan-2017
    
    implicit none
    
    integer iou                                 !i/o unit
    integer ios                                 !i/o status
    integer lnumber                             !line counter
    integer dimnumber                           !Variable dimension index
    integer col                                 !Column number
    integer dime                                !Variable dimension    
    
    character (len=*)       filen               !File name
    character (len=1000)    line                !Line of file (RECORD)
    character (len=1000)    msg                 !Ordinary msg for warnings
    character (len=10)      skipcol             !Ignored variable
    character (len=100)     fmtslv              !Format for a single value and line
    character (len=*)       search              !Search string
    
    logical varrea(dime)                        !Host variable
    
    character (len=1)    :: skip = '!'          !Skip character indicator
    character (len=1)    :: stch = '*'          !Search character indicator
    
    logical singlelv                            !Flag for single value at line
    logical sameline                            !Flag to start the reading on same search line
    
    lnumber     = 1
    dimnumber   = 1
    
    do
        read(iou,'(A1000)',iostat=ios) line
        if(ios .gt. 0) then
            write(msg,'(A,A,A,I,A)') 'Problem in reading, please check file',trim(filen),'. Line ',lnumber,'.'
            call wmsg(5)
        
        else if(ios .lt. 0) then
            write(msg,'(A,A,A,A,A)') 'No ',trim(search),' information found in ',trim(filen),' file.'
            call wmsg(5)
            exit
        endif      
    
        if(line(1:1) == skip .or. line(1:1)==char(9) .or. trim(line)=='') cycle
        
        if(line(1:(len_trim(search)+1)) == trim('*'//search)) then            
                        
            do while(dimnumber .le. dime)
                
            if(.not. sameline) read(iou,'(A1000)',iostat=ios) line            
            
            if(ios .gt. 0) then
            write(msg,'(A,A,A,A,A,I,A)') 'Problem in ',search,' reading, please check file',trim(filen),'. Line ',lnumber,'.'
            call wmsg(5)
            else if(ios .lt. 0)then
                exit
            endif   
            
            lnumber = lnumber + 1
            
            if(line(1:1) == skip .or. line(1:1)==char(9) .or. trim(line)=='') cycle            
            if(line(1:1) == stch) exit
            
            if(singlelv) then                
                
                
                read(line,'(I5)',iostat=ios) varrea(dimnumber)
                exit
                
            else
                
            select case (col)       
            case (1)                
                read(line,*) varrea(dimnumber)          

            case (2)
                read(line,*) skipcol,varrea(dimnumber)
       
            case (3)
                read(line,*) skipcol,skipcol,varrea(dimnumber)
       
            case (4)
                read(line,*) skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (5)
                read(line,*) skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (6)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (7)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (8)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (9)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (10)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (11)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (12)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (13)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (14)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (15)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (16)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (17)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (18)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (19)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (20)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            end select            
            
            dimnumber   =   dimnumber + 1
            
            endif
            
            enddo
            
            exit      
        endif
        
        
    enddo
    
    rewind(iou)
    
    
    end subroutine readlog
    
    
    subroutine readmeteo(search,nindex,col,varrea,dime,singlelv,sameline,iou,filen,msg)   
    ! Reads a real variable within an input file (unit = iou) based on a search string and columm
    ! MSV - 13-jan-2017
    
    implicit none
    
    integer iou                                 !i/o unit
    integer ios                                 !i/o status
    integer lnumber                             !line counter
    integer dimnumber                           !Variable dimension index
    integer col                                 !Column number
    integer dime                                !Variable dimension
    integer nindex
    
    
    character (len=*)       filen               !File name
    character (len=1000)    line                !Line of file (RECORD)
    character (len=1000)    msg                 !Ordinary msg for warnings
    character (len=10)      skipcol             !Ignored variable
    character (len=100)     fmtslv              !Format for a single value and line
    character (len=*)       search              !Search string
    character (len=100)     searchtarget        
    character (len=4)       lineindex(nindex)        
    
    real varrea(dime)                           !Host variable 
    
    character (len=1)    :: skip    = '!'       !Skip character indicator
    character (len=1)    :: stch    = '*'       !Search character indicator
    character (len=1)    :: datesep = '-'       !Date separator character i.e. (year-doy = 2017-123)
    
    logical singlelv                            !Flag for single value at line                             
    logical sameline                            !Flag to start the reading on same search line
    logical  :: singleline = .true.    
    
    lnumber     = 1
    dimnumber   = 1
    
    do
        read(iou,'(A1000)',iostat=ios) line
        if(ios .gt. 0) then
            write(msg,'(A,A,A,I,A)') 'Problem in reading, please check file',trim(filen),'. Line ',lnumber,'.'
            call wmsg(5)
        
        else if(ios .lt. 0) then
            write(msg,'(A,A,A,A,A)') 'Date ',trim(search),' not found in ',trim(filen),'.met file.'
            call wmsg(4)
            exit
        endif      
    
        if(line(1:1) == skip .or. line(1:1)==char(9) .or. trim(line)=='') cycle
        
        select case(nindex)            
        case(1)
            read(line,*) searchtarget
        case(2)
            read(line,*) lineindex(1),lineindex(2)
            searchtarget = trim(lineindex(1))//datesep//trim(lineindex(2))
        case(3)
            read(line,*) lineindex(1),lineindex(2),lineindex(3)
            searchtarget = trim(lineindex(1))//datesep//trim(lineindex(2))//datesep//trim(lineindex(3))
        end select        
        
        if(trim(searchtarget(1:(len_trim(search)+1))) == trim(search)) then            
                        
            do while(dimnumber .le. dime)
                
            if(.not. sameline) read(iou,'(A1000)',iostat=ios) line            
            
            if(ios .gt. 0) then
            write(msg,'(A,A,A,A,A,I,A)') 'Problem in ',search,' reading, please check file',trim(filen),'. Line ',lnumber,'.'
            call wmsg(5)
            else if(ios .lt. 0)then
                exit
            endif   
            
            lnumber = lnumber + 1
            
            if(line(1:1) == skip .or. line(1:1)==char(9) .or. trim(line)=='') cycle            
            if(line(1:1) == stch) exit
            
            if(singlelv) then                
                
                
                read(line,'(F10.2)',iostat=ios) varrea(dimnumber)
                exit
                
            else
                
            select case (col)       
            case (1)                
                read(line,*) varrea(dimnumber)          

            case (2)
                read(line,*) skipcol,varrea(dimnumber)
       
            case (3)
                read(line,*) skipcol,skipcol,varrea(dimnumber)
       
            case (4)
                read(line,*) skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (5)
                read(line,*) skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (6)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (7)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (8)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (9)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (10)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (11)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (12)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (13)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (14)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (15)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (16)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (17)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (18)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (19)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (20)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            end select            
            
            !Reading one line(day) at time step
            if(singleline) exit
            
            dimnumber   =   dimnumber + 1
            
            endif
            
            enddo
            
            exit      
        endif
        
        
    enddo
    
    if(singleline) then
        backspace(iou)
    else
        rewind(iou)
    endif
    
    
    end subroutine readmeteo