/*=======================================================================
  InputWeather.cpp, Felipe de Vargas, Willingthon Pavan, Fabio Oliveira
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
#include "../Data/FlexibleIO.hpp"

extern "C" {
    void INPUTWEATHER(char *FILEWW, int *YRDOY, int *FirstWeatherDay, int *LastWeatherDay, int *ENDFILE, int *LNUM, int *NRECORDS, char *ERRYRDOY, int *ERRCODE);
}

struct HeaderData {
  bool status;
  std::string varname;
  std::string valuestr;
  int size;
  int valueint;
  int line;
};

int YEARDOY(int YRDOY);
int YEAR2KDOY(int YRDOY);
bool ISLEAPYEAR(int year);
bool FORMATDATE(std::string &date, int &yrdoy, int &readFormat, int &errorcode);

void INPUTWEATHER(char *FILEWW, int *YRDOY, int *FirstWeatherDay, int *LastWeatherDay, int *ENDFILE, int *LNUM, int *NRECORDS, char *ERRYRDOY, int *ERRCODE){
  
    //Variables
    FlexibleIO *flexibleio = FlexibleIO::getInstance();
    std::string fileww(FILEWW), line;
    std::ifstream file;
    HeaderData data;
    std::istringstream streamstr;
    std::vector<HeaderData> storeHeaderData;
    std::vector<std::vector<HeaderData>> dateStorage;
    bool convertedDate;
    bool icasaFormat;
    int readFormat;
    int nrecordRead; 
    int linecounter;
    int yeardoy;
    int dbsyrsim;
    int datepos;
    int icasayearpos; 
    int icasadoypos;
    
    //Initialize
    *FirstWeatherDay = 0;
    *LastWeatherDay = 0;
    *LNUM = 0;
    *ENDFILE = 1;
    *ERRCODE = 0;
    *ERRYRDOY = '\0';
    nrecordRead = 0;
    yeardoy = 0;
    dbsyrsim = *YRDOY-1;
    readFormat = 0;
    convertedDate = false;
    icasaFormat = false;
    datepos = 0;
    icasayearpos = 0;
    icasadoypos = 0;
    flexibleio->eraseGroupMemory("WTH");
    fileww.erase(fileww.find_first_of(" "), fileww.size());
    file.open(fileww, std::ifstream::in);
    
    
    //Open file
    if(file.is_open()){
      
      //Read lines 
      while(file.good()){
          line = "";
          
          getline(file, line);
          
          //Update line counter
          *LNUM = *LNUM + 1;
          
          //TRIM and TOUPPERCASE line
          line.erase(0, line.find_first_not_of(" "));
          line.erase(line.find_last_not_of(" ") + 1, line.size());
          std::transform(line.begin(), line.end(), line.begin(), ::toupper);
          
          //Select data lines
          if(line.size() > 1 && line[0] != '\32' && line[0] != '!' && line[0] != '$' && line[1] != '$' && line[0] != '*' && line[0] != '@'){
            
              
              if(storeHeaderData.size() > 0){
                
                //Read DATE data
                if(storeHeaderData[datepos].varname == "DATE"){

                      //Check Records Limit
                      if(nrecordRead >= *NRECORDS){*ENDFILE = 0; break;}
                      
                      //Replace alphabetic characters start stream
                      std::replace_if(line.begin(), line.end(), [](char x){if(x == 32 || x >= 45 && x <= 57 ){return 0;}}, ' ' );
                      //Store data from stream
                      streamstr.clear();
                      streamstr.str(line);
                      
                      //Read and store all daily weather data
                      for(int i=0; i < storeHeaderData.size(); i++){
                        streamstr >> storeHeaderData[i].valuestr;
                      }
                      
                      //ICASA format
                      if(icasaFormat){
                        storeHeaderData[datepos].valuestr += storeHeaderData[icasadoypos].valuestr;
                      }
                      
                      //Format date
                      convertedDate = FORMATDATE(storeHeaderData[datepos].valuestr, yeardoy, readFormat, *ERRCODE);
                      
                      //Converted and no error
                      if(convertedDate && *ERRCODE == 0){
                          
                          //Unformatted Date not used
                          if(yeardoy < dbsyrsim && !dateStorage.empty()){
                            nrecordRead = nrecordRead - dateStorage.size();
                            dateStorage.clear();
                          }
                          //Store all date identified to the day before YRSIM
                          else if(yeardoy >= dbsyrsim){
                              storeHeaderData[datepos].status = true;
                              storeHeaderData[datepos].valueint = yeardoy;
                              storeHeaderData[datepos].line = *LNUM;
                              
                              dateStorage.emplace_back(storeHeaderData);
                              nrecordRead++;
                          }
                          
              
                      }
                      //!Converted
                      else if(!convertedDate && *ERRCODE == 0){

                          storeHeaderData[datepos].status = false;
                          storeHeaderData[datepos].valueint = 0;
                          storeHeaderData[datepos].line = *LNUM;
                          
                          dateStorage.emplace_back(storeHeaderData);
                          nrecordRead++;
                      }
                      //Format error
                      else{
                        //*ERRCODE = returned by FORMATDATE
                        //*LNUM = returned by the line count
                        strcpy(ERRYRDOY, storeHeaderData[datepos].valuestr.c_str());
                        return;
                      }
                      
                      
                }
                //Read simple data
                else{
                  
                    for(unsigned int i = 0; i < storeHeaderData.size(); i++) {
                        
                        //Simple data
                        if(storeHeaderData[i].size == 0){
                            //Substr and store
                            flexibleio->setCharMemory("WTH", storeHeaderData[i].varname, line.substr(0, line.find_first_of(" ")));
                            
                            line.erase(0 , line.find_first_of(" "));
                            
                            //std::cout << storeHeaderData[i].varname << " " << flexibleio->getChar("WTH", storeHeaderData[i].varname) << std::endl;
                        }
                        //String variables
                        else{
                            
                            if(line.size() >= storeHeaderData[i].size){
                              flexibleio->setCharMemory("WTH", storeHeaderData[i].varname, line.substr(0, storeHeaderData[i].size));
                              line.erase(0 , storeHeaderData[i].size);
                              
                              std::cout << storeHeaderData[i].varname << " " << flexibleio->getChar("WTH", storeHeaderData[i].varname) << std::endl;
                            }
                            else{
                              flexibleio->setCharMemory("WTH", storeHeaderData[i].varname, line);
                              line = "";
                              
                              //std::cout << storeHeaderData[i].varname << " " << flexibleio->getChar("WTH", storeHeaderData[i].varname) << std::endl;
                            }
                        }
                        
                      line.erase(0, line.find_first_not_of(" "));                    
                    }
                  
                }
                
              }
              else{
                //Header section not found in weather file.
                //*LNUM = returned by the line count
                *ERRCODE = 11;
                return;
              }
              
              
          }
          //Select header lines
          else{
            // $ line
            //if(line[0] == '$' || line[1] == '$'){}
            // * line
            //if(line[0] == '*'){}
            // @ line
            if(line[0] == '@'){
              
              //erase first character
              line.erase(0, 1);
              storeHeaderData.clear();
              //Clean stream
              streamstr.clear();
                
              //Read and test stream header
              streamstr.str(line); 
                         
              //Store header
              while(streamstr >> data.varname){
                
                if(data.varname.find(".") == std::string::npos){
                  //Not Find '.' 
                  data.size = 0;
                }
                else{
                  //Find '.' set size
                  data.size = data.varname.size();
                  data.varname.erase(data.varname.find_first_of("."), data.varname.size());
                }
                storeHeaderData.emplace_back(data);
                
                if(data.varname == "DATE"){
                  datepos = storeHeaderData.size()-1;
                }
                else if(data.varname == "YEAR"){
                  datepos = storeHeaderData.size()-1;
                  icasaFormat = true;
                  storeHeaderData[datepos].varname = "DATE";
                }
                else if(data.varname == "DOY"){
                  icasadoypos = storeHeaderData.size()-1;
                }
                //std::cout << "Varname:" << data.varname << " - " << data.size << std::endl; 
              }
              
            }
            
          }
      //End read loop    
      }
      
      
      if(dateStorage.size() > 0){
          //Check if no readFormat is present, and set to 1 (default MM/DD)
          if(readFormat == 0){readFormat = 1;}
          
          for(unsigned int i = 0; i < dateStorage.size(); i++) {
            
              //Date unformatted exception
              if(dateStorage[i][datepos].status == false){
                  //Format date
                  convertedDate = FORMATDATE(dateStorage[i][datepos].valuestr, dateStorage[i][datepos].valueint, readFormat, *ERRCODE);
                  //Update status
                  if(convertedDate){
                    dateStorage[i][datepos].status = true;
                  }
                  //Error format
                  else{
                    strcpy(ERRYRDOY, dateStorage[i][datepos].valuestr.c_str());
                    *LNUM = dateStorage[i][datepos].line;
                    //*ERRCODE = returned by FORMATDATE
                    return;
                  }
              }
              
              //Set First Last day
              if(*FirstWeatherDay == 0){
                  if(dateStorage[i][datepos].valueint == dbsyrsim){
                      *FirstWeatherDay = dateStorage[i][datepos].valueint;
                  }
                  else if(dateStorage[i][datepos].valueint == *YRDOY){
                      *FirstWeatherDay = dateStorage[i][datepos].valueint;
                  }
                  //Missing day in weather data file.
                  else if(dateStorage[i][datepos].valueint > *YRDOY){
                      strcpy(ERRYRDOY, dateStorage[i][datepos].valuestr.c_str());
                      *LNUM = dateStorage[i][datepos].line;
                      *ERRCODE = 2;
                      return;
                  }
                  
              }
              
              
              if(*LastWeatherDay < dateStorage[i][datepos].valueint){
                  *LastWeatherDay = dateStorage[i][datepos].valueint;
              }
              //Duplicate record in weather data file.
              else if(*LastWeatherDay == dateStorage[i][datepos].valueint){
                  strcpy(ERRYRDOY, dateStorage[i][datepos].valuestr.c_str());
                  *LNUM = dateStorage[i][datepos].line;
                  *ERRCODE = 7;
                  return;
              }
              //Non-sequential record in weather data file.
              else if(*LastWeatherDay > dateStorage[i][datepos].valueint){
                  strcpy(ERRYRDOY, dateStorage[i][datepos].valuestr.c_str());
                  *LNUM = dateStorage[i][datepos].line;
                  *ERRCODE = 8;
                  return;
              }
              
              
              //Date Formatted
              if(dateStorage[i][datepos].status == true && *ERRCODE == 0){
                  //Add FlexibleIO line number for Warnings and error.
                  flexibleio->setIntegerYrdoyMemory("WTH", dateStorage[i][datepos].valuestr, "LNUM", dateStorage[i][datepos].line);
                  //Add FlexibleIO Data
                  //std::cout << dateStorage[i][datepos].valuestr << "  ";
                  for(unsigned int j = 0; j < dateStorage[i].size() ; j++) {
                      flexibleio->setCharYrdoyMemory("WTH", dateStorage[i][datepos].valuestr, dateStorage[i][j].varname, dateStorage[i][j].valuestr);
                      
                      //std::cout << dateStorage[i][j].varname << "  " << 
                      //flexibleio->getCharYrdoy("WTH", dateStorage[i][datepos].valuestr, dateStorage[i][j].varname) << " ";
                  }
                  //std::cout << "\n";
              }
          }
      }
      //Error Missing day in weather data file.
      else{
          *ERRCODE = 2;
          return;
      }
      
    }
    //File not found.  Please check file name or create file.
    else{
        *ERRCODE = 29;
        return;
    }
    
    return;
}


bool FORMATDATE(std::string &date, int &yrdoy, int &readFormat, int &errorcode){
  
  //Variables
  int year; 
  int month;
  int day;
  int monthday;
  int daymonth;
  std::string FORMATDATE;
  
  //Initialize
  FORMATDATE = date;
  yrdoy = 0;
  year = 0;
  month = 0;
  day = 0;
  monthday = 0;
  daymonth = 0;
  //Format Date MM/DD or DD/MM
  if(FORMATDATE[1] == '/' || FORMATDATE[2] == '/') {
    std::replace(FORMATDATE.begin(), FORMATDATE.end(), '/', ' ');
    std::istringstream streamstr(FORMATDATE);
    streamstr >> daymonth >> monthday >> year;
    
    //Read MM/DD
    if(readFormat == 1 || monthday > 12){
        readFormat = 1;
        month = daymonth;
        day = monthday;
        
        if(month > 0 && month <= 12 && day > 0 && day <= 31 && year > 0){
            //!Leap year
            if(ISLEAPYEAR(year) == false){
                int daysPerMonth[] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365};
                yrdoy = daysPerMonth[month-1];
                yrdoy = yrdoy + day;
                year = year * 1000;
                yrdoy = yrdoy + year;
                
                date = std::to_string(YEARDOY(yrdoy));
                return true;            
            }
            else{
                int daysPerMonth[] = {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366};
                yrdoy = daysPerMonth[month-1];
                yrdoy = yrdoy + day;
                year = year * 1000;
                yrdoy = yrdoy + year;
        
                date = std::to_string(YEARDOY(yrdoy));
                return true;            
            }
        }
        else{
            //Invalid format in weather file. 59
            errorcode = 59;
            return false;
        }
        
    }
    //Read DD/MM
    else if (readFormat == 2 || daymonth > 12){
        readFormat = 2;
        day = daymonth;
        month = monthday;
        
        if(month > 0 && month <= 12 && day > 0 && day <= 31 && year > 0){
            //!Leap year
            if(ISLEAPYEAR(year) == false){
                int daysPerMonth[] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365};
                yrdoy = daysPerMonth[month-1];
                yrdoy = yrdoy + day;
                year = year * 1000;
                yrdoy = yrdoy + year;
                
                date = std::to_string(YEARDOY(yrdoy));
                return true;            
            }
            else{
                int daysPerMonth[] = {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366};
                yrdoy = daysPerMonth[month-1];
                yrdoy = yrdoy + day;
                year = year * 1000;
                yrdoy = yrdoy + year;
        
                date = std::to_string(YEARDOY(yrdoy));
                return true;            
            }

        }
        else{
            //Invalid format in weather file. 59
            errorcode = 59;
            return false;
        }
    }
    //No formatDate
    else{
        return false;      
    }
    
  }  
  //Format Date YYYY-MM-DD
  else if(FORMATDATE[4] == '-'){
      
      std::replace(FORMATDATE.begin(), FORMATDATE.end(), '-', ' ');
      std::istringstream streamstr(FORMATDATE);
      streamstr >> year >> month >> day;
      
      if(year > 0 && month > 0 && month <= 12 && day > 0 && day <= 366){
          
          //!Leap year
          if(ISLEAPYEAR(year) == false){
              int daysPerMonth[] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365};
              yrdoy = daysPerMonth[month-1];
              yrdoy = yrdoy + day;
              year = year * 1000;
              yrdoy = yrdoy + year;
              
              date = std::to_string(YEARDOY(yrdoy));
              return true;            
          }
          else{
              int daysPerMonth[] = {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366};
              yrdoy = daysPerMonth[month-1];
              yrdoy = yrdoy + day;
              year = year * 1000;
              yrdoy = yrdoy + year;
      
              date = std::to_string(YEARDOY(yrdoy));
              return true;            
          }

      }
      //Error date
      else{
          //Invalid format in weather file. 59
          errorcode = 59;
          return false;
      }
    
  }
  //Format Date YYYYDOY
  else if(FORMATDATE.size() == 7){
      
      yrdoy = YEARDOY(std::atoi(FORMATDATE.c_str()));
      if(yrdoy){
          date = std::to_string(yrdoy);
          return true;
      }
      //Error date
      else{
        //Invalid format in weather file. 59
        errorcode = 59;
        return false;
      }      
      
  }
  //Format Date YYDOY
  else if(FORMATDATE.size() == 5){
    
      yrdoy = YEAR2KDOY(std::atoi(FORMATDATE.c_str()));
      if(yrdoy){
          date = std::to_string(yrdoy);
          return true;
      }
      //Error date
      else{
        //Invalid format in weather file. 59
        errorcode = 59;
        return false;
      }
      
  }
  //Error
  else{
      //Invalid format in weather file. 59
      errorcode = 59;
      return false;
  }
  
  //Invalid format in weather file. 59
  errorcode = 59;
  return false;
}

int YEARDOY(int YRDOY){
  
    int DOY, YR;
    
    if (YRDOY <= 9999365){
        YR  = int(YRDOY / 1000);
        DOY = YRDOY - YR * 1000;
        if (YR > 0 && DOY <= 366){
          return YRDOY;
        }
    }
    return 0;
}

int YEAR2KDOY(int YRDOY){
  
    int DOY, YR;
    if (YRDOY <= 99365){
        YR  = int(YRDOY / 1000);
        DOY = YRDOY - YR * 1000;
        if (YRDOY > 0){
            if (YR <= 30){
                YRDOY = (2000 + YR) * 1000 + DOY;
            }
            else{
              YRDOY = (1900 + YR) * 1000 + DOY;
            }
        }
      return YRDOY;
    }
    return 0;
}

bool ISLEAPYEAR(int year){
  
    if ((year % 4 == 0 && year % 100 != 0) || ( year % 400 == 0)){
      return true;
    }
    else{
      return false;
    }
}