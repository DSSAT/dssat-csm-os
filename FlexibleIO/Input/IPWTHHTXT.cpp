/*=======================================================================
  IPWTHHR, Fabio Oliveira
  Function to read hourly weather data
-----------------------------------------------------------------------
  REVISION HISTORY
  10/15/2024 FO Written.
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
  void READ_WTH_HOURLY(char *FILEWW, int *YRDOY, 
                      int *FirstWeatherDay, int *LastWeatherDay, 
                      int *LNUM, int *NRECORDS, int *MXRECORDS, 
                      int *ERRCODE);
}

void READ_WTH_HOURLY(char *FILEWW, int *YRDOY, 
                      int *FirstWeatherDay, int *LastWeatherDay, 
                      int *LNUM, int *NRECORDS, int *MXRECORDS, 
                      int *ERRCODE){
  FlexibleIO *flexibleio = FlexibleIO::getInstance();
  std::string fileww(FILEWW), line, value, hour;
  std::ifstream file;
  int hdcounter, datecol, nrow, fwd, lwd, nrec, century, yeardoy;
  bool hdsection;
  std::vector<std::string> hddata, dtdata;
  std::regex_iterator<std::string::iterator> hrit;
  std::regex_iterator<std::string::iterator> drit;
  std::regex_iterator<std::string::iterator> rend;
  
  //Initialize
  fileww.erase(fileww.find_first_of(" "), fileww.size());
  std::regex word_regex("\\S+");
  hdsection = false;
  *MXRECORDS = *MXRECORDS * 24;
  
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
          ++hrit;
          ++i;
        }
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
            ++drit;
            ++i;
          }
        }
        else if(hdcounter == 2){
          // Store Hourly Weather Data.
          int i = 0;
          while(drit != rend && i < hddata.size()) {
            if(hddata[i] == "DATE"){
              value = drit->str();
              std::size_t pos = value.find("-");
              yeardoy = std::stoi(value.substr(0,pos));
              if(yeardoy >= *YRDOY){
                hour = std::to_string(std::stoi(value.substr(pos+1,2)));
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

            flexibleio->setFor2KeyString("WTH", value, hour, hddata[i], drit->str());
            
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
  }
  else{
    // Error: File not found.  Please check file name or create file.
    *ERRCODE = 29;
    return;
    
  } // end: if(file.is_open())
  
  file.close();
  return;
}