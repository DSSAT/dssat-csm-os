/*=======================================================================
  InputWeather.cpp, Fabio Oliveira
  Function to read Daily weather with Comma-Separated Values (CSV)
-----------------------------------------------------------------------
  REVISION HISTORY
  02/01/2025 FO Written.
  
========================================================================*/
#include <cstdlib>
#include <algorithm>
#include <iostream>
#include <string>
#include <cstring>
#include <fstream>
#include <vector>
#include <sstream>
#include <regex>
#include "../Data/FlexibleIO.hpp"

extern "C" {
void READ_WTH_CSV(char *FILEWW, int *FirstWeatherDate, int *YRDOY, 
                      int *FirstWeatherDay, int *LastWeatherDay, 
                      int *LNUM, int *NRECORDS, int *MXRECORDS, 
                      int *ERRCODE);
}

void READ_WTH_CSV(char *FILEWW, int *FirstWeatherDate, int *YRDOY, 
                      int *FirstWeatherDay, int *LastWeatherDay, 
                      int *LNUM, int *NRECORDS, int *MXRECORDS, 
                      int *ERRCODE){
  FlexibleIO *flexibleio = FlexibleIO::getInstance();
  std::string fileww(FILEWW), line, value;
  std::ifstream file;
  int hdcounter, datecol, nrow, fwd, lwd, nrec, century, yeardoy, yydoy,yy,doy;
  bool hdsection;
  std::vector<std::string> hddata, dtdata;
  std::regex_iterator<std::string::iterator> hrit;
  std::regex_iterator<std::string::iterator> drit;
  std::regex_iterator<std::string::iterator> rend;
  
  //Initialize
  fileww.erase(fileww.find_first_of(" "), fileww.size());
  std::regex word_regex("[^,\\s]+");
  hdsection = false;
  
  file.open(fileww, std::ifstream::in);
  
  if(file.is_open()){
    *ERRCODE  = 0;
    datecol   = 0;
    hdcounter = 0;
    nrow      = 0;
    fwd       = 0;
    lwd       = 0;
    nrec      = 0;
    yeardoy   = 0;
    century   = int(*YRDOY/100000);

    //Process data
    while(file.good()){
      line = "";
      getline(file, line);
      
      line.erase(0, line.find_first_not_of(" "));
      
      //Read Headers
      if(line[0] == '@'){
        hdcounter++;
        
        line.erase(0, 1);
        hdsection = true;
        hrit = std::regex_iterator<std::string::iterator>(line.begin(), line.end(), word_regex);
        
        int i = 1;
        hddata.clear();
        while(hrit != rend) {
          hddata.push_back(hrit->str());
          if("DATE" == hrit->str()) datecol = i;
          //std::cout << hrit->str() << " ";
          ++hrit;
          ++i;
        }
        //std::cout << std::endl;
      }
      //Read Data
      else if(hdsection && line.size() > 1 && line[0] != '\32' && line[0] != '!' && 
              line[0] != '$' && line[1] != '$' && line[0] != '*' && line[0] != '@'){
        
        drit = std::regex_iterator<std::string::iterator>(line.begin(), line.end(), word_regex);
        
        if(hdcounter == 1){
          // Store Weather Station data.
          int i = 0;
          while(drit != rend && i < hddata.size()) {
            flexibleio->setString("WTH", hddata[i], drit->str());
            //std::cout << "REGEX DATA WSTAT:" << hddata[i] << " " << drit->str() << std::endl;
            ++drit;
            ++i;
          }
        }
        else if(hdcounter == 2){
          // Store CSV Weather Data.
          int i = 0;
          while(drit != rend && i < hddata.size()) {
            if(hddata[i] == "DATE" && *FirstWeatherDate == -99){
              yydoy =  std::stoi(drit->str());
              yy  = yydoy / 1000;
              doy = yydoy - yy * 1000;
              //cross-over year based on DATES.for
              if(yy <= 35){
                yeardoy = (2000 + yy) * 1000 + doy;
              }
              else{
                yeardoy = (1900 + yy) * 1000 + doy;
              }
              
              if(yeardoy >= *YRDOY){
                value = std::to_string(yeardoy);
                lwd = yeardoy;
                if(yeardoy <= fwd || fwd == 0)
                  fwd = yeardoy;
                nrec+=1;
              }
              else{
                break;
              }
            }
            else if(hddata[i] == "DATE" && *FirstWeatherDate != -99){
              yeardoy = std::stoi(drit->str());
              if(yeardoy >= *YRDOY){
                value = drit->str();
                lwd = yeardoy;
                if(yeardoy <= fwd || fwd == 0)
                  fwd = yeardoy;
                nrec+=1;
              }
              else{
                break;
              }
            }

            flexibleio->setForKeyString("WTH", value, hddata[i], drit->str());
            //std::cout << "ADDED FIO: " << hddata[i] << "  " <<  value << " " << 
            //flexibleio->getCharYrdoy("WTH", value, hddata[i]) << " " << std::endl;
            
            ++drit;
            ++i;
          }
          if(nrec >= *MXRECORDS) 
            break;
        }
      }
      
      //Count Rows
      nrow += 1;
    } // end: while(file.good())
    
    *LNUM = nrow;
    *NRECORDS = nrec;
    *FirstWeatherDay = fwd;
    *LastWeatherDay = lwd;
    //std::cout << "Cpp values: " << *FirstWeatherDay << ", " << *LastWeatherDay << std::endl;
  }
  else{
    // Error: File not found.  Please check file name or create file.
    *ERRCODE = 29;
    return;
    
  } // end: if(file.is_open())
  
  file.close();
  return;
}