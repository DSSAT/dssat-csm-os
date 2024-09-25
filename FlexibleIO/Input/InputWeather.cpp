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
  void READ_WTH_Y2_4K(char *FILEWW, char *RTYPE, int *YRDOY, int *FirstWeatherDay, 
                      int *LastWeatherDay, int *ENDFILE, int *LNUM, 
                      int *NRECORDS, int *ERRCODE);
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
        std::cout << "Line read: " << line << std::endl;
        std::cout << "C++ FILETYPE " << RTYPE << std::endl;
        return;
      }
      else if(line[0] == '$'){
        ftype = "Y4K";
        sprintf(RTYPE, ftype.c_str());
        *ERRCODE = 0;
        std::cout << "Line read: " << line << std::endl;
        std::cout << "C++ FILETYPE " << RTYPE << std::endl;
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
  std::istringstream headerstream;
  std::istringstream datastream;
  bool hdsection, datasection;
  fileww.erase(fileww.find_first_of(" "), fileww.size());
  
  // Initialization
  hdsection = false;
  datasection = false;
  
  file.open(fileww, std::ifstream::in);
  
  if(file.is_open()){
    while(file.good()){
      line = "";
      getline(file, line);
      line.erase(0, line.find_first_not_of(" "));
      
      //// Regular expression to match words (sequences of non-space characters)
      //std::regex word_regex("\\S+");
      //
      //// Use regex_iterator to iterate through all the matches in the input string
      //std::regex_iterator<std::string::iterator> rit(line.begin(), line.end(), word_regex);
      //std::regex_iterator<std::string::iterator> rend;
      //
      //while (rit!=rend) {
      //  std::cout << "REGEX: " << rit->str() << std::endl;
      //  ++rit;
      //}
      
      if(line[0] == '@'){
        line.erase(0, 1);
        headerstream.clear();                
        headerstream.str(line); 
        hdsection = true;
        std::cout << "READ WSTAT:" << headerstream.rdbuf()->str() << std::endl;
      }
      else if(hdsection && line.size() > 1 && line[0] != '\32' && line[0] != '!' && 
              line[0] != '$' && line[1] != '$' && line[0] != '*' && line[0] != '@'){
        datastream.clear();                
        datastream.str(line);
        std::cout << "READ WSTAT:" << datastream.rdbuf()->str() << std::endl;
        break;
      }
    }
    
    if(hdsection){
      while(headerstream >> var){
        datastream >> value;
        flexibleio->setCharMemory("WTH", var, value);
        std::cout << "SSTREAM: " << var << " " << value << std::endl;
        var = ""; value = "";
      }
    }
    else{
      *ERRCODE = 29;
    }
    
  }
  else{
    *ERRCODE = 29;
    return;
    
  } //endif
  
}

void READ_WTH_Y2_4K(char *FILEWW, char *RTYPE, int *YRDOY, int *FirstWeatherDay, 
                    int *LastWeatherDay, int *ENDFILE, int *LNUM, 
                    int *NRECORDS, int *ERRCODE){
  FlexibleIO *flexibleio = FlexibleIO::getInstance();
  std::string fileww(FILEWW), rtype(RTYPE),line, var, value;
  std::ifstream file;
  std::istringstream headerstream;
  std::istringstream datastream;
  int hdcount, datecol, icol, ncol, nrow, fwd, nrec, century, y2kdoy;
  bool hdsection, gdata;
  std::vector<std::pair<std::string, std::string>> vecwth;
  std::pair<std::string, std::string> mp;
  fileww.erase(fileww.find_first_of(" "), fileww.size());
  
  std::cout << "STRINGS: " << fileww << "/" << rtype << std::endl;
  
  file.open(fileww, std::ifstream::in);
  
  if(file.is_open()){
    hdsection = false;
    gdata     = false;
    datecol   = 0;
    hdcount   = 0;
    nrow      = 0;
    fwd       = 0;
    nrec      = 0;
    y2kdoy    = 0;
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
          headerstream.clear();                
          headerstream.str(line); 
          hdsection = true;
          std::cout << "READ WTH DATA HEADER:" << headerstream.rdbuf()->str() << std::endl;
        }
      }
      //Read Data
      else if(hdsection && line.size() > 1 && line[0] != '\32' && line[0] != '!' && 
              line[0] != '$' && line[1] != '$' && line[0] != '*' && line[0] != '@'){
                
        std::replace_if(line.begin(), line.end(), [](char x)
        {if(x == 32 || x >= 45 && x <= 57 ){return 0;}}, ' ' );
        
        datastream.clear();                
        datastream.str(line);
        std::cout << "READ WTH DATA:" << datastream.rdbuf()->str() << std::endl;
        
        icol = 0;
        headerstream.clear(std::stringstream::goodbit); headerstream.seekg(0);
        while(headerstream >> var){
          datastream >> value;
          if(var == "DATE" && rtype == "Y2K"){
            y2kdoy = (century * 100000) + std::stoi(value);
            
            if(y2kdoy >= *YRDOY){
              value = std::to_string(y2kdoy);
              std::cout << "Y2K: " << var << " " << value << " " << *YRDOY << std::endl;
              fwd = nrec;
            }
            datecol = icol;
          }
          else if(var == "DATE" && rtype == "Y4K"){
            if(std::stoi(value) >= *YRDOY){
              std::cout << "Y4K: " << var << " " << value << " " << *YRDOY << std::endl;
              fwd = nrec;
            }
            datecol = icol;
          }
          
          vecwth.emplace_back(var, value);
          
          var = ""; value = "";
          icol += 1;
        }
        ncol = icol;
        nrec += 1;        
      }
      
      nrow += 1;
    }
    
    if(hdsection){
      int i = fwd * ncol;
      std::cout << "I: " << i << " " << fwd << " " << ncol << std::endl;
      while(i < vecwth.size()){
        if(i == datecol){
          mp = vecwth[datecol];
          datecol += ncol;
          i++;
        }
        flexibleio->setCharYrdoyMemory("WTH", mp.second, vecwth[i].first, vecwth[i].second);             
        std::cout << "ADDED FIO: " << vecwth[i].first << "  " << mp.second << " " << 
        flexibleio->getCharYrdoy("WTH", mp.second, vecwth[i].first) << " " << std::endl;
        i += 1;
      }
    }
    else{
      *ERRCODE = 29;
    }
    
  }
  else{
    *ERRCODE = 29;
    return;
    
  } //endif
  
  //Read and set the variables in the data structure from YRDOY to EOF or Max records
  
  // Close file
}

// void INPUTWEATHER(char *FILEWW, int *YRDOY, int *FirstWeatherDay, int *LastWeatherDay, int *ENDFILE, int *LNUM, int *NRECORDS, char *ERRYRDOY, int *ERRCODE){
  
//     //Variables
//     FlexibleIO *flexibleio = FlexibleIO::getInstance();
//     std::string fileww(FILEWW), line;
//     std::ifstream file;
//     HeaderData data;
//     std::istringstream streamstr;
//     std::vector<HeaderData> storeHeaderData;
//     std::vector<std::vector<HeaderData> > dateStorage;
//     bool convertedDate;
//     bool icasaFormat;
//     int readFormat;
//     int nrecordRead; 
//     int linecounter;
//     int yeardoy;
//     int dbsyrsim;
//     int datepos;
//     int icasayearpos; 
//     int icasadoypos;
    
//     //Initialize
//     *FirstWeatherDay = 0;
//     *LastWeatherDay = 0;
//     *LNUM = 0;
//     *ENDFILE = 1;
//     *ERRCODE = 0;
//     *ERRYRDOY = '\0';
//     nrecordRead = 0;
//     yeardoy = 0;
//     dbsyrsim = *YRDOY-1;
//     readFormat = 0;
//     convertedDate = false;
//     icasaFormat = false;
//     datepos = 0;
//     icasayearpos = 0;
//     icasadoypos = 0;
//     flexibleio->eraseGroupMemory("WTH");
//     fileww.erase(fileww.find_first_of(" "), fileww.size());
//     file.open(fileww, std::ifstream::in);
    
    
//     //Open file
//     if(file.is_open()){
      
//       //Read lines 
//       while(file.good()){
//           line = "";
          
//           getline(file, line);
          
//           //Update line counter
//           *LNUM = *LNUM + 1;
          
//           //TRIM and TOUPPERCASE line
//           line.erase(0, line.find_first_not_of(" "));
//           line.erase(line.find_last_not_of(" ") + 1, line.size());
//           std::transform(line.begin(), line.end(), line.begin(), ::toupper);
          
//           //Select data lines
//           if(line.size() > 1 && line[0] != '\32' && line[0] != '!' && line[0] != '$' && line[1] != '$' && line[0] != '*' && line[0] != '@'){
            
              
//               if(storeHeaderData.size() > 0){
                
//                 //Read DATE data
//                 if(storeHeaderData[datepos].varname == "DATE"){

//                       //Check Records Limit
//                       if(nrecordRead >= *NRECORDS){*ENDFILE = 0; break;}
                      
//                       //Replace alphabetic characters start stream
//                       std::replace_if(line.begin(), line.end(), [](char x){if(x == 32 || x >= 45 && x <= 57 ){return 0;}}, ' ' );
//                       //Store data from stream
//                       streamstr.clear();
//                       streamstr.str(line);
                      
//                       //Read and store all daily weather data
//                       for(int i=0; i < storeHeaderData.size(); i++){
//                         streamstr >> storeHeaderData[i].valuestr;
//                       }
                      
//                       //ICASA format
//                       if(icasaFormat){
//                         storeHeaderData[datepos].valuestr += storeHeaderData[icasadoypos].valuestr;
//                       }
                      
//                       //Format date
//                       convertedDate = FORMATDATE(storeHeaderData[datepos].valuestr, yeardoy, readFormat, *ERRCODE);
                      
//                       //Converted and no error
//                       if(convertedDate && *ERRCODE == 0){
                          
//                           //Unformatted Date not used
//                           if(yeardoy < dbsyrsim && !dateStorage.empty()){
//                             nrecordRead = nrecordRead - dateStorage.size();
//                             dateStorage.clear();
//                           }
//                           //Store all date identified to the day before YRSIM
//                           else if(yeardoy >= dbsyrsim){
//                               storeHeaderData[datepos].status = true;
//                               storeHeaderData[datepos].valueint = yeardoy;
//                               storeHeaderData[datepos].line = *LNUM;
                              
//                               dateStorage.emplace_back(storeHeaderData);
//                               nrecordRead++;
//                           }
                          
              
//                       }
//                       //!Converted
//                       else if(!convertedDate && *ERRCODE == 0){

//                           storeHeaderData[datepos].status = false;
//                           storeHeaderData[datepos].valueint = 0;
//                           storeHeaderData[datepos].line = *LNUM;
                          
//                           dateStorage.emplace_back(storeHeaderData);
//                           nrecordRead++;
//                       }
//                       //Format error
//                       else{
//                         //*ERRCODE = returned by FORMATDATE
//                         //*LNUM = returned by the line count
//                         strcpy(ERRYRDOY, storeHeaderData[datepos].valuestr.c_str());
//                         return;
//                       }
                      
                      
//                 }
//                 //Read simple data
//                 else{
                  
//                     for(unsigned int i = 0; i < storeHeaderData.size(); i++) {
                        
//                         //Simple data
//                         if(storeHeaderData[i].size == 0){
//                             //Substr and store
//                             flexibleio->setCharMemory("WTH", storeHeaderData[i].varname, line.substr(0, line.find_first_of(" ")));
                            
//                             line.erase(0 , line.find_first_of(" "));
                            
//                             //std::cout << storeHeaderData[i].varname << " " << flexibleio->getChar("WTH", storeHeaderData[i].varname) << std::endl;
//                         }
//                         //String variables
//                         else{
                            
//                             if(line.size() >= storeHeaderData[i].size){
//                               flexibleio->setCharMemory("WTH", storeHeaderData[i].varname, line.substr(0, storeHeaderData[i].size));
//                               line.erase(0 , storeHeaderData[i].size);
                              
//                               std::cout << storeHeaderData[i].varname << " " << flexibleio->getChar("WTH", storeHeaderData[i].varname) << std::endl;
//                             }
//                             else{
//                               flexibleio->setCharMemory("WTH", storeHeaderData[i].varname, line);
//                               line = "";
                              
//                               //std::cout << storeHeaderData[i].varname << " " << flexibleio->getChar("WTH", storeHeaderData[i].varname) << std::endl;
//                             }
//                         }
                        
//                       line.erase(0, line.find_first_not_of(" "));                    
//                     }
                  
//                 }
                
//               }
//               else{
//                 //Header section not found in weather file.
//                 //*LNUM = returned by the line count
//                 *ERRCODE = 11;
//                 return;
//               }
              
              
//           }
//           //Select header lines
//           else{
//             // $ line
//             //if(line[0] == '$' || line[1] == '$'){}
//             // * line
//             //if(line[0] == '*'){}
//             // @ line
//             if(line[0] == '@'){
              
//               //erase first character
//               line.erase(0, 1);
//               storeHeaderData.clear();
//               //Clean stream
//               streamstr.clear();
                
//               //Read and test stream header
//               streamstr.str(line); 
                         
//               //Store header
//               while(streamstr >> data.varname){
                
//                 if(data.varname.find(".") == std::string::npos){
//                   //Not Find '.' 
//                   data.size = 0;
//                 }
//                 else{
//                   //Find '.' set size
//                   data.size = data.varname.size();
//                   data.varname.erase(data.varname.find_first_of("."), data.varname.size());
//                 }
//                 storeHeaderData.emplace_back(data);
                
//                 if(data.varname == "DATE"){
//                   datepos = storeHeaderData.size()-1;
//                 }
//                 else if(data.varname == "YEAR"){
//                   datepos = storeHeaderData.size()-1;
//                   icasaFormat = true;
//                   storeHeaderData[datepos].varname = "DATE";
//                 }
//                 else if(data.varname == "DOY"){
//                   icasadoypos = storeHeaderData.size()-1;
//                 }
//                 //std::cout << "Varname:" << data.varname << " - " << data.size << std::endl; 
//               }
              
//             }
            
//           }
//       //End read loop    
//       }
      
      
//       if(dateStorage.size() > 0){
//           //Check if no readFormat is present, and set to 1 (default MM/DD)
//           if(readFormat == 0){readFormat = 1;}
          
//           for(unsigned int i = 0; i < dateStorage.size(); i++) {
            
//               //Date unformatted exception
//               if(dateStorage[i][datepos].status == false){
//                   //Format date
//                   convertedDate = FORMATDATE(dateStorage[i][datepos].valuestr, dateStorage[i][datepos].valueint, readFormat, *ERRCODE);
//                   //Update status
//                   if(convertedDate){
//                     dateStorage[i][datepos].status = true;
//                   }
//                   //Error format
//                   else{
//                     strcpy(ERRYRDOY, dateStorage[i][datepos].valuestr.c_str());
//                     *LNUM = dateStorage[i][datepos].line;
//                     //*ERRCODE = returned by FORMATDATE
//                     return;
//                   }
//               }
              
//               //Set First Last day
//               if(*FirstWeatherDay == 0){
//                   if(dateStorage[i][datepos].valueint == dbsyrsim){
//                       *FirstWeatherDay = dateStorage[i][datepos].valueint;
//                   }
//                   else if(dateStorage[i][datepos].valueint == *YRDOY){
//                       *FirstWeatherDay = dateStorage[i][datepos].valueint;
//                   }
//                   //Missing day in weather data file.
//                   else if(dateStorage[i][datepos].valueint > *YRDOY){
//                       strcpy(ERRYRDOY, dateStorage[i][datepos].valuestr.c_str());
//                       *LNUM = dateStorage[i][datepos].line;
//                       *ERRCODE = 2;
//                       return;
//                   }
                  
//               }
              
              
//               if(*LastWeatherDay < dateStorage[i][datepos].valueint){
//                   *LastWeatherDay = dateStorage[i][datepos].valueint;
//               }
//               //Duplicate record in weather data file.
//               else if(*LastWeatherDay == dateStorage[i][datepos].valueint){
//                   strcpy(ERRYRDOY, dateStorage[i][datepos].valuestr.c_str());
//                   *LNUM = dateStorage[i][datepos].line;
//                   *ERRCODE = 7;
//                   return;
//               }
//               //Non-sequential record in weather data file.
//               else if(*LastWeatherDay > dateStorage[i][datepos].valueint){
//                   strcpy(ERRYRDOY, dateStorage[i][datepos].valuestr.c_str());
//                   *LNUM = dateStorage[i][datepos].line;
//                   *ERRCODE = 8;
//                   return;
//               }
              
              
//               //Date Formatted
//               if(dateStorage[i][datepos].status == true && *ERRCODE == 0){
//                   //Add FlexibleIO line number for Warnings and error.
//                   flexibleio->setIntegerYrdoyMemory("WTH", dateStorage[i][datepos].valuestr, "LNUM", dateStorage[i][datepos].line);
//                   //Add FlexibleIO Data
//                   //std::cout << dateStorage[i][datepos].valuestr << "  ";
//                   for(unsigned int j = 0; j < dateStorage[i].size() ; j++) {
//                       flexibleio->setCharYrdoyMemory("WTH", dateStorage[i][datepos].valuestr, dateStorage[i][j].varname, dateStorage[i][j].valuestr);
                      
//                       //std::cout << dateStorage[i][j].varname << "  " << 
//                       //flexibleio->getCharYrdoy("WTH", dateStorage[i][datepos].valuestr, dateStorage[i][j].varname) << " ";
//                   }
//                   //std::cout << "\n";
//               }
//           }
//       }
//       //Error Missing day in weather data file.
//       else{
//           *ERRCODE = 2;
//           return;
//       }
      
//     }
//     //File not found.  Please check file name or create file.
//     else{
//         *ERRCODE = 29;
//         return;
//     }
    
//     return;
// }