!======================================================================================
! MZ_AG_Utilities
! Functions used by AgMaize subroutines
!----------------------------------------------------------------------
! Revision History
! 09/13/2012 KAD Created
!----------------------------------------------------------------------
!======================================================================================
!-----------------------------------------------------------------------
! Subroutine readfileio
! Reads DSSAT's INP file (aka fileio)
!-----------------------------------------------------------------------
   subroutine readfileio(control, secread, dssatinpout)
      use ModuleDefs
      use MZ_AG_ModuleDefs 
      implicit none
      integer :: lunio, errnum, isens, lnum, linc, found, trtnum, yremrg, i
      character(len=6)  :: thesections(2), section, varno, econo 
      character(len=12) :: filea, files, filee, filec
      character(len=17), parameter :: errkey='MZ_GM_READFILEIO' 
      character(len=80) :: pathex, pathsr, pather, pathcr
      character :: vrname*16, fileio*30
      real :: lfnum, rlamx, taint, photp, lftop, lalfx, ampli, asymp, pltpop, sdepth, rowspc, azir 
      type (ControlType), intent(in) :: control
      character,intent(in) :: secread*6
      type(FileioType) dssatinpout
      
      !Initialize local variables
      write(section, '(6X)')
      write(varno, '(6X)')
      write(econo, '(6X)')
      write(filea, '(12X)')
      write(files, '(12X)')
      write(filee, '(12X)')
      write(filec, '(12X)')
      write(pathex, '(80X)')
      write(pathsr, '(80X)')
      write(pather, '(80X)')
      write(pathcr, '(80X)')
      write(vrname, '(16X)')
      do i=1,2
         write(thesections(i), '(8X)')
      end do
      lnum = 0
      isens = 0
      trtnum = 0
      yremrg = 0
      lfnum = 0.0
      rlamx = 0.0
      taint = 0.0
      photp = 0.0
      lftop = 0.0
      lalfx = 0.0
      ampli = 0.0
      asymp = 0.0
      pltpop = 0.0
      sdepth = 0.0
      rowspc = 0.0
      azir = 0.0
      
      !Determine if reading all sections
      if(secread == 'ALLSEC') then
         thesections(1:2) = (/ 'FILENP', 'INPUTS' /)
      else
         thesections(1) =  secread  
      end if
      
      !Read file
      fileio  = control % fileio
      call getlun('FILEIO', lunio)
      open(lunio, file=fileio, status='old', iostat=errnum)
      if(errnum /= 0) call error(errkey, errnum, fileio, 0)

      do i = 1, size(thesections)
         select case(thesections(i))
            case('FILENP')       !Files and paths
            !---Read files and paths
            read(lunio, '(55X,I5)', iostat=errnum) isens; lnum = lnum + 1
            if(errnum /= 0) call error(errkey, errnum, fileio, lnum)
            read(lunio, '(3(/),15X,A12,1X,A80)', iostat=errnum) filea, pathex ; lnum = lnum + 4  
            if(errnum /= 0) call error(errkey, errnum, fileio, lnum)
            read(lunio, '(/,15X,A12,1X,A80)', iostat=errnum) files, pathsr; lnum = lnum + 2
            if(errnum /= 0) call error(errkey, errnum, fileio, lnum) 
            read(lunio, '(15X,A12,1X,A80)', iostat=errnum) filee, pather; lnum = lnum + 1
            if(errnum /= 0) call error(errkey, errnum, fileio, lnum)
            read(lunio, '(15X,A12,1X,A80)', iostat=errnum) filec, pathcr; lnum = lnum + 1
            if(errnum /= 0) call error(errkey, errnum, fileio, lnum)   
            dssatinpout % isens = isens
            dssatinpout % filea = filea   
            dssatinpout % files = files
            dssatinpout % filee = filee
            dssatinpout % filec = filec
            dssatinpout % pathex = pathex
            dssatinpout % pathsr = pathsr
            dssatinpout % pather = pather
            dssatinpout % pathcr = pathcr
    
            case('INPUTS')
            !---Read treatment section
            section = '*TREAT'
            call find(lunio, section, linc, found) ; lnum = lnum + linc
            if(found == 0) then
               call error(section, 42, fileio, lnum)
            else
               read(lunio, '(I3)', iostat=errnum) trtnum ; lnum = lnum + 1
               if(errnum /= 0) call error(errkey, errnum, fileio, lnum)
            end if
            dssatinpout % trtnum = trtnum
      
            !---Read planting details section
            section = '*PLANT'
            call find(lunio, section, linc, found); lnum = lnum + linc
            if(found == 0) then
               call error(section, 42, fileio, lnum)
            else
               read(lunio, '(11X, I7, 7X, F5.2, 13X, F5.2, 1X, F5.2, 1X, F5.2)', iostat=errnum)  &
                           yremrg, pltpop, rowspc, azir, sdepth
               lnum = lnum + 1
               if(errnum /= 0) call error(errkey, errnum, fileio, lnum)
            end if
            dssatinpout % yremrg = yremrg
            dssatinpout % pltpop = pltpop
            dssatinpout % rowspc = rowspc
            dssatinpout % azir   = azir
            dssatinpout % sdepth = sdepth
        
            !---Read cultivar section
            section = '*CULTI'
            call find(lunio, section, linc, found); lnum = lnum + linc
            if(found == 0) then
               call error(section, 42, fileio, lnum)
            else
               read(lunio,'(A6, 1X, A16, 1X, A6, 5(1X,F5.2), 1X, F5.3, 2(1X,F5.0))', iostat=errnum)   &  
                           varno, vrname, econo, lfnum, rlamx, taint, photp, lftop, lalfx, ampli, asymp    
               lnum = lnum + 1
               if(errnum /= 0) call error(errkey, errnum, fileio, lnum) 
            end if 
            dssatinpout % varno  = varno
            dssatinpout % vrname = vrname
            dssatinpout % econo  = econo
            dssatinpout % lfnum  = lfnum
            dssatinpout % rlamx  = rlamx
            dssatinpout % taint  = taint
            dssatinpout % photp  = photp
            dssatinpout % lftop  = lftop
            dssatinpout % lalfx  = lalfx
            dssatinpout % ampli  = ampli
            dssatinpout % asymp  = asymp
         end select 
      end do
         
      close(lunio)  
       
   end subroutine readfileio
   
   
!-----------------------------------------------------------------------
! Subroutine readspecies
! Reads DSSAT's species file 
!-----------------------------------------------------------------------
   subroutine readspecies(files, pathsr, section, spefileout)
      use ModuleDefs 
      use MZ_AG_ModuleDefs
      implicit none  
      character,intent(in) :: section*6, files*12, pathsr*80
      character(len=6)  :: xsections(8), secread
      character(len=17), parameter :: errkey='MZ_GM_READSPECIES'      
      character(len=80) :: C80
      character(len=92) :: filecc
      integer :: luncrp, errnum, lnum, found, isect, i
      real :: tceil, torla, tbrla, mldvs, bldvs        !Phenology
      real :: swcg                                     !Seed germination
      real :: xtemp(5), ygdd(5)                        !APSIM thermal time parameters
      real :: tempCoef(3,3)                            !Leaf area
      real :: asmax, canh, xc                          !Photosynthesis
      real :: pormin, rwumx, rlwr, rwuep1              !Roots
      real :: res30c, r30c2                            !Respiration
      real :: pcarlf, pcarst, pcarrt, pcarea, pcarsd   !Carbohydrate fractions in leaves, stems, roots, reproductive organs and grain
      real :: pprolf, pprost, pprort, pproea, pprosd   !Protein fractions in leaves, stems, roots, reproductive organs and grain
      real :: pliplf, plipst, pliprt, plipea, plipsd   !Lipid fractions in leaves, stems, roots, reproductive organs and grain
      real :: pliglf, pligst, pligrt, pligea, pligsd   !Lignin fractions in leaves, stems, roots, reproductive organs and grain
      real :: poalf, poast, poart, poaea, poasd        !Organic acid fractions in leaves, stems, roots, reproductive organs and grain
      real :: pminlf, pminst, pminrt, pminea, pminsd   !Mineral fractions in leaves, stems, roots, reproductive organs and grain
      type(SpeciesType) spefileout
      
      !Initialize local variables
      write(C80, '(80X)')
      write(secread, '(8X)')
      write(filecc, '(92X)')
      do i=1,8
         write(xsections(i), '(8X)')
      end do
      tceil = 0.0
      torla = 0.0
      tbrla = 0.0
      mldvs = 0.0
      bldvs = 0.0
      res30c = 0.0
      r30c2 = 0.0
      pcarlf = 0.0
      pcarst = 0.0
      pcarrt = 0.0
      pcarea = 0.0
      pcarsd = 0.0
      pprolf = 0.0
      pprost = 0.0
      pprort = 0.0
      pproea = 0.0
      pprosd = 0.0
      pliplf = 0.0
      plipst = 0.0
      pliprt = 0.0
      plipea = 0.0
      plipsd = 0.0
      pliglf = 0.0
      pligst = 0.0
      pligrt = 0.0
      pligea = 0.0
      pligsd = 0.0
      poalf = 0.0
      poast = 0.0
      poart = 0.0
      poaea = 0.0
      poasd = 0.0
      pminlf = 0.0
      pminst = 0.0
      pminrt = 0.0
      pminea = 0.0
      pminsd = 0.0
      
      filecc = trim(pathsr) // files
      call getlun('FILEC', luncrp)
      open(luncrp, file=filecc, status='old', iostat=errnum)
      if(errnum /= 0) call error(errkey, errnum, filecc, 0)
      
      !Determine if reading all sections or just one section
      if(section == 'ALLSEC') then
         xsections(1:8) = (/ '*SEED ','*PHENO','*APSIM','*LEAF ','*PHOTO','*ROOT ','*RESPI','*PLANT' /)
      else
         xsections(1) =  section  
      end if
      
      !Read the specified sections
      do i = 1, size(xsections)
         select case(xsections(i))
         case('*SEED ')
         secread = xsections(i)
         call find(luncrp, secread, lnum, found)
         if(found == 0) then
            call error(secread, 42, filecc, lnum)
         else  
            call ignore(luncrp, lnum, isect, C80)    
            call ignore(luncrp, lnum, isect, C80)
            call ignore(luncrp, lnum, isect, C80)
            call ignore(luncrp, lnum, isect, C80)
            call ignore(luncrp, lnum, isect, C80)
            call ignore(luncrp, lnum, isect, C80)
            call ignore(luncrp, lnum, isect, C80)                        
            read(C80, '(8X,F6.2)', iostat=errnum) swcg
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)            
         end if      
         spefileout % swcg = swcg
         rewind(luncrp)
      
         case('*PHENO')
         secread = xsections(i)      
         call find(luncrp, secread, lnum, found)
         if(found == 0) then
            call error(secread, 42, filecc, lnum)
         else  
            call ignore(luncrp, lnum, isect, C80)                
            read(C80, '(8X,F6.2)', iostat=errnum) tceil
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)
            call ignore(luncrp, lnum, isect, C80)
            read(C80, '(8X,F6.2)', iostat=errnum) torla
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)
            call ignore(luncrp, lnum, isect, C80)
            read(C80, '(8X,F6.2)', iostat=errnum) tbrla
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)
            call ignore(luncrp, lnum, isect, C80)
            read(C80, '(8X,F6.2)', iostat=errnum) mldvs
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)   
            call ignore(luncrp, lnum, isect, C80)
            read(C80, '(8X,F6.2)', iostat=errnum) bldvs
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum) 
         end if      
         spefileout % tceil = tceil
         spefileout % torla = torla
         spefileout % tbrla = tbrla
         spefileout % mldvs = mldvs
         spefileout % bldvs = bldvs
         rewind(luncrp)
         
         case('*APSIM')
         secread = xsections(i)      
         call find(luncrp, secread, lnum, found)
         if(found == 0) then
            call error(secread, 42, filecc, lnum)
         else  
            call ignore(luncrp, lnum, isect, C80)                
            read(C80, '(5(1X,F6.1))', iostat=errnum) xtemp(1:5)
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)
            call ignore(luncrp, lnum, isect, C80)
            read(C80, '(5(1X,F6.1))', iostat=errnum) ygdd(1:5)
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)
         end if      
         spefileout % xtemp = xtemp
         spefileout % ygdd  = ygdd
         rewind(luncrp)
         
         case('*LEAF ')
         secread = xsections(i)      
         call find(luncrp, secread, lnum, found)
         if(found == 0) then
            call error(secread, 42, filecc, lnum)
         else  
            call ignore(luncrp, lnum, isect, C80)                
            read(C80, '(3(F8.4))', iostat=errnum) tempCoef(1,:)
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)
            call ignore(luncrp, lnum, isect, C80)
            read(C80, '(3(F8.4))', iostat=errnum) tempCoef(2,:)
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)
            call ignore(luncrp, lnum, isect, C80)
            read(C80, '(3(F8.4))', iostat=errnum) tempCoef(3,:)
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)                       
         end if      
         spefileout % tempCoef = tempCoef
         rewind(luncrp)
      
         case('*PHOTO')
         secread = xsections(i)      
         call find(luncrp, secread, lnum, found)
         if(found == 0) then
            call error(secread, 42, filecc, lnum)
         else  !Check and clean this part
            call ignore(luncrp, lnum, isect, C80)
            call ignore(luncrp, lnum, isect, C80)
            call ignore(luncrp, lnum, isect, C80)
            call ignore(luncrp, lnum, isect, C80)                
            read(C80, '(8X,F6.2)', iostat=errnum) asmax
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)
            call ignore(luncrp, lnum, isect, C80)
            read(C80, '(8X,F6.2)', iostat=errnum) xc
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)
            call ignore(luncrp, lnum, isect, C80)
            read(C80, '(8X,F6.2)', iostat=errnum) canh
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)        
         end if      
         spefileout % asmax = asmax
         spefileout % xc    = xc
         spefileout % canh  = canh
         rewind(luncrp)
         
         case('*ROOT ')
         secread = xsections(i)
         call find(luncrp, secread, lnum, found)
         if(found == 0) then
            call error(secread, 42, filecc, lnum)
         else
            !Minimum volume required for supplying oxygen to roots for optimum growth
            call ignore(luncrp, lnum, isect, C80)
            read(C80, '(9X,F6.3)', iostat=errnum) pormin     
            if (errnum /= 0) call error(errkey, errnum, filecc, lnum)
            !Maximum water uptake per unit root length (cm3[water]/cm[root])
            call ignore(luncrp, lnum, isect, C80)
            read(C80, '(9X,F6.3)', iostat=errnum) rwumx     
            if (errnum /= 0) call error(errkey, errnum, filecc, lnum)
            !Root length to weight ratio (units?)
            call ignore(luncrp, lnum, isect, C80)
            read(C80, '(9X,F6.3)', iostat=errnum) rlwr     
            if (errnum /= 0) call error(errkey, errnum, filecc, lnum)
            !Factor to modify water stress for cell expansion (mm/day)
            call ignore(luncrp, lnum, isect, C80)
            read(C80, '(9X,F6.3)', iostat=errnum) rwuep1     
            if (errnum /= 0) call error(errkey, errnum, filecc, lnum)
         endif
         spefileout % pormin = pormin
         spefileout % rwumx  = rwumx
         spefileout % rlwr   = rlwr
         spefileout % rwuep1 = rwuep1
         rewind(luncrp)  
         
         case('*RESPI')
         secread = xsections(i)
         call find(luncrp, secread, lnum, found)
         if(found == 0) then
            call error(secread, 42, filecc, lnum)
         else
            call ignore(luncrp, lnum, isect, C80)
            read(C80, '(X,F8.6,F7.4)', iostat=errnum) res30c, r30c2     !No space between the two variables?
            if (errnum /= 0) call error(errkey, errnum, filecc, lnum)
         endif
         spefileout % res30c = res30c
         spefileout % r30c2  = r30c2
         rewind(luncrp)  
         
         case('*PLANT')
         secread = xsections(i)
         call find(luncrp, secread, lnum, found)
         if(found == 0) then
            call error(secread, 42, filecc, lnum)
         else
            !Read carbohydrate fractions in leaves, stems, roots, reproductive organs and grain
            call ignore(luncrp, lnum, isect, C80)
            read(C80,'(5F6.3)', iostat=errnum) pcarlf, pcarst, pcarrt, pcarea, pcarsd
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)
            
            !Read protein fractions in leaves, stems, roots, reproductive organs and grain
            call ignore(luncrp, lnum, isect, C80)
            read(C80,'(5F6.3)', iostat=errnum) pprolf, pprost, pprort, pproea, pprosd
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)
            
            !Read lipid fractions in leaves, stems, roots, reproductive organs and grain
            call ignore(luncrp, lnum, isect, C80)
            read(C80,'(5F6.3)', iostat=errnum) pliplf, plipst, pliprt, plipea, plipsd
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)
            
            !Read lignin fractions in leaves, stems, roots, reproductive organs and grain
            call ignore(luncrp, lnum, isect, C80)
            read(C80,'(5F6.3)', iostat=errnum) pliglf, pligst, pligrt, pligea, pligsd
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)
            
            !Read organic acid fractions in leaves, stems, roots, reproductive organs and grain
            call ignore(luncrp, lnum, isect, C80)
            read(C80,'(5F6.3)', iostat=errnum) poalf, poast, poart, poaea, poasd
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)
            
            !Read mineral fractions in leaves, stems, roots, reproductive organs and grain
            call ignore(luncrp, lnum, isect, C80)
            read(C80,'(5F6.3)', iostat=errnum) pminlf, pminst, pminrt, pminea, pminsd
            if(errnum /= 0) call error(errkey, errnum, filecc, lnum)
         endif
         !Save carbohydrate fractions
         spefileout % pcarlf = pcarlf  
         spefileout % pcarst = pcarst   
         spefileout % pcarrt = pcarrt 
         spefileout % pcarea = pcarea 
         spefileout % pcarsd = pcarsd 
         
         !Save protein fractions
         spefileout % pprolf = pprolf  
         spefileout % pprost = pprost   
         spefileout % pprort = pprort 
         spefileout % pproea = pproea 
         spefileout % pprosd = pprosd 
		 
		 !Save lipid fractions
         spefileout % pliplf = pliplf  
         spefileout % plipst = plipst   
         spefileout % pliprt = pliprt 
         spefileout % plipea = plipea 
         spefileout % plipsd = plipsd 
		 
		 !Save lignin fractions
         spefileout % pliglf = pliglf  
         spefileout % pligst = pligst   
         spefileout % pligrt = pligrt 
         spefileout % pligea = pligea 
         spefileout % pligsd = pligsd 
		 
		 !Save organic acid fractions
         spefileout % poalf = poalf  
         spefileout % poast = poast   
         spefileout % poart = poart 
         spefileout % poaea = poaea 
         spefileout % poasd = poasd
		 
		 !Save mineral fractions
         spefileout % pminlf = pminlf  
         spefileout % pminst = pminst   
         spefileout % pminrt = pminrt 
         spefileout % pminea = pminea 
         spefileout % pminsd = pminsd 
         rewind(luncrp)

      end select 
      end do
      close(luncrp)
       
   end subroutine readspecies
   
   
!-----------------------------------------------------------------------
! Subroutine readecotype
! Reads DSSAT's ecotype file 
!-----------------------------------------------------------------------
   subroutine readecotype(pather, filee, econo, ecofileout)
      use MZ_AG_ModuleDefs
      implicit none
      character,intent(in) :: econo*6, filee*12, pather*80
      character(len=1), parameter :: blank=' '
      character(len=17), parameter :: errkey='MZ_GM_READECOTYPE'
      character :: ecotyp*6, econam*16, filegc*92
      character(len=78) :: message(10)
      character(len=255) :: C255
      integer :: lnum, pathl, luneco, errnum, isect, cday
      real :: tbase, topt, ropt, tsen
      type(EcotypeType) ecofileout
      
   !--Initialize all parameter values
      cday  = 0
      tbase = 0.0
      topt  = 0.0
      ropt  = 0.0
      tsen  = 0.0   
      
   !--Open Ecotype File filee
      lnum = 0
      pathl = index(pather, blank)
      if(pathl <= 1) then
         filegc = filee
      else
         filegc = pather(1:(pathl-1)) // filee
      end if

   !--Read Ecotype Parameter File
      call getlun('FILEE', luneco)
      open(luneco, file=filegc, status='old', iostat=errnum)
      if(errnum /= 0) call error(errkey, errnum, filee, 0)
      ecotyp = '      '
      lnum = 0
      do while(ecotyp /= econo)
         call ignore(luneco, lnum, isect, C255)
         if(isect == 1 .AND. C255(1:1) /= ' ' .AND. C255(1:1) /= '*') then
            !Only tbase, topt, and ropt are read for now
            read(C255, '(A6,1X,A16,2X,3(1X,F5.1))', iostat=errnum) ecotyp, econam, tbase, topt, ropt
              if(errnum /= 0) call error(errkey, errnum, filee, lnum)        
              if(ecotyp == econo) then
                 !Read optional cold sensitivity parameter. Default to TSEN = 6.0 if no value given.
                 if(C255(80:84) == '     ') then
                    tsen = 6.0
                 else
                    read(C255(80:84),'(F5.1)', iostat=errnum) tsen
                    if(errnum /= 0 .OR. tsen < 1.E-6) tsen = 6.0
                 end if
        
                 !Read optional number of cold days parameter. Default to CDAY = 15.0 if no value given.
                 if(C255(86:90) == '     ') then
                    cday = 15
                 else
                    read(C255(86:90),'(I5)', iostat=errnum) cday
                    if(errnum /= 0 .OR. cday < 0) cday = 15
                 end if
                 
                 !Save values
                 ecofileout % ecotyp = ecotyp
                 ecofileout % econam = econam
                 ecofileout % tbase  = tbase
                 ecofileout % topt   = topt
                 ecofileout % ropt   = ropt
                 ecofileout % tsen   = tsen
                 ecofileout % cday   = cday
                 
              !Write warning message if default ecotype is used
              if(econo == 'DFAULT') then
                 call error(errkey, 35, filegc, lnum)
                 write(message(1), '("Ecotype ", A6, " not found in file: ", A12)') econo, filee
                 write(message(2), '("Default ecotype parameters will be used.")') 
                 call warning(2, errkey, message)
              end if
              
              exit
              end if

         else if (isect == 0) then
            call error(errkey, 7, filee, lnum)
         end if
      end do
      close(luneco)
   end subroutine readecotype
   
!-----------------------------------------------------------------------
! Subroutine copy_file
! Copies a file file_name to file_name_new
! file_name and file_name_new must include the path information and may include wildcard characters
!-----------------------------------------------------------------------
   subroutine copy_file(file_name, file_name_new)
      use ifport 
      implicit none
      character(*), intent(in) :: file_name, file_name_new 
      character(len=1000) :: fnam
      integer :: l, len1, len2
      logical(4) :: logical_result
      
      len1 = len_trim(file_name); len2 = len_trim(file_name_new)
      fnam = 'copy/y ' // file_name(1:len1) // ' ' // file_name_new(1:len2) // ' > nul'
      l = len_trim(fnam)
      logical_result = systemqq(fnam(1:l))
   end subroutine copy_file

!-----------------------------------------------------------------------
! Function getcoltlu to compute collar tlu 
!-----------------------------------------------------------------------
  real function getcoltlu(tip, lgnode)
     real, intent(in) :: tip, lgnode
     
     if(tip < 1.0) then 
        getcoltlu = tip
     else if(tip == 1.0) then        
        getcoltlu = 3.0  
     else if(tip > 1.0 .AND. tip <= lgnode) then
        getcoltlu = max(1.7*tip + 1, 3.0)
     else if(tip > lgnode) then 
        getcoltlu = max((1.7*lgnode + 1) + (tip-lgnode), 3.0)
     end if
  end function getcoltlu    
    
!-----------------------------------------------------------------------
! Function Decimal
! This function takes a real number as input and returns its decimal portion
!-----------------------------------------------------------------------
   function Decimal(RealNumber) result(DecimalPart)
      real, intent(in) :: RealNumber
      real :: DecimalPart
      DecimalPart = RealNumber - aint(RealNumber)
   end function Decimal
  
!-----------------------------------------------------------------------
! Function to remove blank spaces in a string; adapted from the Internet 8/29/2012
!-----------------------------------------------------------------------
   character(30) function sweep_blanks(in_str)
        character(*), intent(in) :: in_str
        character(30) :: out_str
        character :: ch
        integer :: j

        out_str = " "
        do j = 1, len_trim(in_str)
            ch = in_str(j:j) !Get j-th char
            if (ch /= " ") then
                out_str = trim(out_str) // ch
            endif
            sweep_blanks = out_str
        end do
    end function sweep_blanks
  
!-----------------------------------------------------------------------      
! Function replace blank by zero
!-----------------------------------------------------------------------      
   character(30) function zero_blanks(in_str)
      character(*), intent(in) :: in_str
      character(30) :: out_str
      character :: ch
      integer :: j
  
      out_str = " "
      do j=1, len_trim(in_str)
         ch = in_str(j:j)           !Get j-th char
         if(ch == " ") then
            out_str = trim(out_str) // "0"
         else
            out_str = trim(out_str) // ch 
         endif
         zero_blanks = out_str 
      end do
   end function zero_blanks
   
!-----------------------------------------------------------------------      
! Function to paste a vector of strings and a vector of integers element by element
! Incomplete, rename variables
!-----------------------------------------------------------------------      
   function paste(instr, n, charwidth)
      integer, intent(in) :: n, charwidth
      character(charwidth), intent(in) :: instr
      !integer, intent(in) :: intvec(n)
      character(charwidth+2) :: strint(n), paste(n)
      character:: zero_blanks*30
      integer :: i
      
      do i = 1, n
         write(strint(i),'(I2)') i
         strint(i) = instr // strint(i)
         strint(i) = zero_blanks(strint(i))
      end do
      paste = strint
   end function paste
   
!-----------------------------------------------------------------------      
! This function replaces any century in years after 2010 by 19 to allow graphing of 
! outputs in GBuild. Temporary fix. GBuild needs to be checked. 
! For example, 2011 is 1911, etc. KAD 01/04/2013
!-----------------------------------------------------------------------      
   pure character*4 function correctyear(thisyear)
      integer, intent(in) :: thisyear
      character thisyearchar*4
      
      !Initialize
      thisyearchar = '    '
      
      write(thisyearchar, '(I4)') thisyear
      if(thisyear > 2010) then 
         write(correctyear,'(A2,A2)') "19", thisyearchar(3:4)
      else
         correctyear = thisyearchar   
      end if  
   end function correctyear         
!----------------------------------------------------------------------- 
      
     
      