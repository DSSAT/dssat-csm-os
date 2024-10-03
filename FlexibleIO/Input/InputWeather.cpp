/*=======================================================================
  InputWeather.cpp, Fabio Oliveira, Willingthon Pavan, Felipe de Vargas
  Function to read Weather header and data.
-----------------------------------------------------------------------
  REVISION HISTORY
  09/01/2016 FV Written.
  06/18/2017 FV Create new version for weather data.
	12/11/2017 FO Added new date format.
	12/11/2017 FO Added variable to return error code.
	03/29/2018 FO Added FlexibleIO get/set to manipulate memory data.
  08/10/2018 FO Created a new read format for weather data.
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
  void FILETYPE(char *FILEWW, char *RTYPE,int *ERRCODE);
  void READ_WSTAT(char *FILEWW, int *ERRCODE);
  void READ_WTH_Y2_4K(char *FILEWW, char *RTYPE, int *YRDOY, 
                      int *FirstWeatherDay, int *LastWeatherDay, 
                      int *LNUM, int *NRECORDS, int *MXRECORDS, 
                      int *ERRCODE);
}

void FILETYPE(char *FILEWW, char *RTYPE,int *ERRCODE){
  std::string fileww(FILEWW), ftype, line;
  std::ifstream file;
  std::istringstream streamstr;
  fileww.erase(fileww.find_first_of(" "), fileww.size());
  
  file.open(fileww, std::ifstream::in);
  
  if(file.is_open()){
    while(file.good()){
      line = "";
      getline(file, line);
      
      line.erase(0, line.find_first_not_of(" "));
          
      if(line[0] == '*'){
        ftype = "Y2K";
        sprintf(RTYPE, ftype.c_str());
        *ERRCODE = 0;
        return;
      }
      else if(line[0] == '$'){
        ftype = "Y4K";
        sprintf(RTYPE, ftype.c_str());
        *ERRCODE = 0;
        return;
      }
    }
    //No * or $ found
    *ERRCODE = 1;
  }
  
  return;
}

void READ_WSTAT(char *FILEWW, int *ERRCODE){
  FlexibleIO *flexibleio = FlexibleIO::getInstance();
  std::string fileww(FILEWW), line, var, value;
  std::ifstream file;
  bool hdsection, datasection;
  std::vector<std::string> hddata, dtdata;
  std::regex_iterator<std::string::iterator> hrit;
  std::regex_iterator<std::string::iterator> drit;
  std::regex_iterator<std::string::iterator> rend;
  
  // Initialization
  fileww.erase(fileww.find_first_of(" "), fileww.size());
  // Regular expression to match words (sequences of non-space characters)
  std::regex word_regex("\\S+");
  hdsection = false;
  datasection = false;
  
  file.open(fileww, std::ifstream::in);
  
  if(file.is_open()){
    while(file.good()){
      line = "";
      getline(file, line);
      line.erase(0, line.find_first_not_of(" "));      
      
      if(line[0] == '@'){
        line.erase(0, 1);
        hdsection = true;
        hrit = std::regex_iterator<std::string::iterator>(line.begin(), line.end(), word_regex);
        while(hrit != rend) {
          hddata.push_back(hrit->str());
          //std::cout << "REGEX HEADER WSTAT:" << hrit->str() << std::endl;
          ++hrit;
        }
        
      }
      else if(hdsection && line.size() > 1 && line[0] != '\32' && line[0] != '!' && 
              line[0] != '$' && line[1] != '$' && line[0] != '*' && line[0] != '@'){
        datasection = true;
        drit = std::regex_iterator<std::string::iterator>(line.begin(), line.end(), word_regex);
        while(drit != rend) {
          dtdata.push_back(drit->str());
          //std::cout << "REGEX DATA WSTAT:" << drit->str() << std::endl;
          ++drit;
        }
        break;
      }
    }
    
    if(hdsection && datasection){
      for(int i = 0; i < hddata.size(); i++){
        //std::cout << "VEC FIO WSTAT: " << hddata[i] << ": " << dtdata[i] << std::endl;
        flexibleio->setCharMemory("WTH", hddata[i], dtdata[i]);
      }
    }
    else{
      //Error: No header or data section available.
      *ERRCODE = 29;
      return;
    }
  }
  else{
    //Error: Not able to open the Weather file input.
    *ERRCODE = 29;
    return;
  }
  
}

  void READ_WTH_Y2_4K(char *FILEWW, char *RTYPE, int *YRDOY, 
                      int *FirstWeatherDay, int *LastWeatherDay, 
                      int *LNUM, int *NRECORDS, int *MXRECORDS, 
                      int *ERRCODE){
  FlexibleIO *flexibleio = FlexibleIO::getInstance();
  std::string fileww(FILEWW), rtype(RTYPE),line, value;
  std::ifstream file;
  int hdcount, datecol, nrow, fwd, lwd, nrec, century, yeardoy;
  bool hdsection, dtsection;
  std::vector<std::string> hddata, dtdata;
  std::regex_iterator<std::string::iterator> hrit;
  std::regex_iterator<std::string::iterator> drit;
  std::regex_iterator<std::string::iterator> rend;
  
  //Initialize
  fileww.erase(fileww.find_first_of(" "), fileww.size());
  std::regex word_regex("\\S+");
  hdsection = false;
  dtsection = false;
  
  std::cout << "STRINGS: " << fileww << "/" << rtype << std::endl;
  
  file.open(fileww, std::ifstream::in);
  
  if(file.is_open()){
    *ERRCODE  = 0;
    datecol   = 0;
    hdcount   = 0;
    nrow      = 0;
    fwd       = 0;
    lwd       = 0;
    nrec      = 0;
    yeardoy   = 0;
    century   = int(*YRDOY/100000);

    //Process YRSIM Y2K or Y4K
    while(file.good()){
      line = "";
      getline(file, line);
      
      line.erase(0, line.find_first_not_of(" "));
      
      //Read 2nd Header
      if(line[0] == '@'){
        hdcount++;
        
        if(hdcount == 2){
          line.erase(0, 1);
          hdsection = true;
          hrit = std::regex_iterator<std::string::iterator>(line.begin(), line.end(), word_regex);
          //std::cout << "REGEX DATA HEADER" << std::endl;
          int i = 1;
          while(hrit != rend) {
            hddata.push_back(hrit->str());
            if("DATE" == hrit->str()) datecol = i;
            //std::cout << hrit->str() << " ";
            ++hrit;
            ++i;
          }
          //std::cout << std::endl;
        }
      }
      //Read Data
      else if(hdsection && line.size() > 1 && line[0] != '\32' && line[0] != '!' && 
              line[0] != '$' && line[1] != '$' && line[0] != '*' && line[0] != '@'){        
        std::replace_if(line.begin(), line.end(), [](char x)
        {return !(x == 32 || x >= 45 && x <= 57 );}, ' ');
        
        dtsection = true;
        drit = std::regex_iterator<std::string::iterator>(line.begin(), line.end(), word_regex);
        int i = 0;
        while(drit != rend && i < hddata.size()) {
          if(hddata[i] == "DATE" && rtype == "Y2K"){
            yeardoy = (century * 100000) + std::stoi(drit->str());
            if(yeardoy >= *YRDOY){
              value = std::to_string(yeardoy);
              //std::cout << value << " ";
              lwd = yeardoy;
              if(yeardoy <= fwd || fwd == 0)
                fwd = yeardoy;
              nrec+=1;
            }
            else{
              break;
            }
          }
          else if(hddata[i] == "DATE" && rtype == "Y4K"){
            yeardoy = std::stoi(drit->str());
            if(yeardoy >= *YRDOY){
              value = drit->str();
              //std::cout << value << " "; 
              lwd = yeardoy;
              if(yeardoy <= fwd || fwd == 0)
                fwd = yeardoy;
              nrec+=1;
            }
            else{
              break;
            }
          }          
                       
          flexibleio->setCharYrdoyMemory("WTH", value, hddata[i], drit->str());
          
          //std::cout << "ADDED FIO: " << hddata[i] << "  " <<  value << " " << 
          //flexibleio->getCharYrdoy("WTH", value, hddata[i]) << " " << std::endl;
                    
          ++drit;
          ++i;
        }
        if(nrec >= *MXRECORDS) 
          break;
      }
      
      //Count Rows
      nrow += 1;
    } // end: while(file.good())
    
    *LNUM = nrow;
    *NRECORDS = nrec;
    *FirstWeatherDay = fwd;
    *LastWeatherDay = lwd;
    std::cout << "Cpp values: " << *FirstWeatherDay << ", " << *LastWeatherDay << std::endl;
  }
  else{
    // Error: File not found.  Please check file name or create file.
    *ERRCODE = 29;
    return;
    
  } // end: if(file.is_open())
  
  file.close();
  return;
}