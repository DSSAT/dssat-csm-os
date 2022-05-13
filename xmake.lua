set_defaultmode("releasedbg")
-- add supported extension name
add_rules("fortran", {extensions = {"f90", "for"}})
-- add_toolchains('cuda','gfortran','ifort')
-- set config variable 
-- version
set_configvar("MAJOR", 4)
set_configvar("MINOR", 7)
set_configvar("MODEL", 5)
set_configvar("BUILD", 42)
-- branch
set_configvar("BRANCH", 'xmake')
-- default install path
set_configvar("CMAKE_INSTALL_PREFIX", 'DSSAT47')

-- setup domain --
target('dsscsm047')
    -- compile type
    set_kind("binary")
    -- set .mod file output path
    set_values("fortran.moduledir", "$(buildir)/mod")
    -- .for.in config file output path
    set_configdir("./")
    add_configfiles("src/Utilities/CSMVersion.for.in", {pattern = "@([^\n]-)@",filename = "CSMVersion.for",prefixdir="lib/Utilities"})
    add_configfiles("src/Utilities/run_dssat.in", {pattern = "@([^\n]-)@",filename = "run_dssat",prefixdir="src/Utilities/"})
    add_configfiles("src/Data/DSSATPRO.L47.in", {pattern = "@([^\n]-)@",filename = "DSSATPRO.L47",prefixdir="src/Data/"})
    -- set_policy("check.auto_ignore_flags", false)

   

    if is_plat('linux') then 
        add_files('lib/Utilities/OSDefsLINUX.for')
    end 
    if is_plat('windows') then
        add_files('lib/Utilities/OSDefsWINDOW.for')
    end
   
    -- if is_arch('gcc') then
    -- print('1')
        -- add_fcflags
        -- gnu
        -- add_fcflags("-fd-lines-as-comments")
        -- add_fcflags("-fbounds-check")
        -- add_fcflags("-ffixed-line-length-none")
        -- add_fcflags("-ffree-line-length-none")
        -- add_fcflags("-finit-character=32")
        -- add_fcflags("-noexternal")
        -- add_fcflags("-cpp")
        -- add_fcflags("-ffpe-trap=invalid,zero,overflow")
        -- nvidia
        add_fcflags("-Mbackslash")
        add_fcflags("-traceback")
        add_fcflags("-Mbounds")
        -- -- add_fcflags("-Mextend")  
        -- add_fcflags("-fpic")
        -- add_fcflags("-i8")
        -- add_fcflags("-Minfo=ccff")
        -- add_fcflags("-Munroll")
        -- add_fcflags("-Minline")
        -- add_fcflags("-Mbackslash")
        -- ifort
        -- add_fcflags("-132")
        -- add_fcflags("-fpp")
        -- add_fcflags("-libs:static")
        -- add_fcflags("-threads")
        -- add_fcflags("-fpe0")
    -- end 
        before_build(function(target)
            os.mv('./*.mod','$(buildir)/mod/')
        end)
   
    
    

    -- add_fcflags("-Og")
    -- add_fcflags("-Wall")
    -- add_fcflags("-fbacktrace")

    -- add_linkdirs
    add_includedirs("include")
    -- add_libdirs
    add_files("lib/Utilities/CSMVersion.for")
    add_files("lib/Utilities/**|OSDefsLINUX.for|OSDefsWINDOWS.for|CSMVersion.for.in")
    add_files('lib/InputModule/*.for') 
    add_files('lib/Soil/GHG/*',
              'lib/Soil/SoilUtilities/*')
    add_files('lib/Management/*for')
    add_files('lib/Weather/*.f90')
    add_files('lib/Plant/CSYCA-Cassava/YCA_Node.f90')
    add_files('lib/Plant/**|YCA_Node.f90')
  -- add_srcdirs
    add_files('src/Utilities/*.for',
              'src/Utilities/*.f90') 
    add_files('src/Management/*for')
    add_files('src/Plant/**.for',
              'src/Plant/**.f90',
              'src/Plant/*.for')
    add_files('src/Soil/**.for|SoilUtilities/SoilNoPoBal_C.for',
              'src/Soil/**.f90',
              'src/Soil/*.for')
    add_files('src/SPAM/*for')
    add_files('src/Weather/*for')  
    add_files('src/InputModule/**')
    add_files('src/CSM_Main/*.for')
