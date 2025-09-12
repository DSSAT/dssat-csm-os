/*=======================================================================
  FlexibleIO.hpp, Fabio Oliveira, Willingthon Pavan
  Functions to manipulate memory data in C++.
-----------------------------------------------------------------------
  REVISION HISTORY
  03/08/2018 FO Created the class header.
  03/08/2018 FO Mapped all CInterface functions.
  03/09/2018 FO Added two static data structures to store data.
  03/10/2018 FO Added a function to erase groups of data from memory.
========================================================================*/
#ifndef FLEXIBLEIO_H
#define FLEXIBLEIO_H

#include <string>
#include <unordered_map>

class FlexibleIO
{

private:
    static std::unordered_map<std::string,std::unordered_map<std::string, std::string>> datatwodimensional;
    static std::unordered_map<std::string,std::unordered_map<std::string,std::unordered_map<std::string, std::string>>> datathreedimensional;
    static std::unordered_map<std::string,std::unordered_map<std::string,std::unordered_map<std::string, std::unordered_map<std::string, std::string>>>> datafourdimensional;

protected:
    FlexibleIO();
    static FlexibleIO *instance;

public:
    static FlexibleIO* getInstance();
    static FlexibleIO* newInstance();

    float       getFloat(std::string GROUP, std::string VARNAME);
    int         getInteger(std::string GROUP, std::string VARNAME);
    std::string getString(std::string GROUP, std::string VARNAME);
    float       getIndexFloat(std::string GROUP, std::string VARNAME, int INDEX);
    int         getIndexInteger(std::string GROUP, std::string VARNAME, int INDEX);
    std::string getIndexString(std::string GROUP, std::string VARNAME, int INDEX);
    float*      getArrayFloat(std::string GROUP, std::string VARNAME, std::string SIZE);
    int*        getArrayInteger(std::string GROUP, std::string VARNAME, std::string SIZE);
    std::string getArrayString(std::string GROUP, std::string VARNAME, std::string SIZE);
    float       getForKeyFloat(std::string GROUP, std::string KEY, std::string VARNAME);
    int         getForKeyInteger(std::string GROUP, std::string KEY, std::string VARNAME);
    std::string getForKeyString(std::string GROUP, std::string KEY, std::string VARNAME);
    float       getFor2KeyFloat(std::string GROUP, std::string KEY, std::string KEY2, std::string VARNAME);
    int         getFor2KeyInteger(std::string GROUP, std::string KEY, std::string KEY2, std::string VARNAME);
    std::string getFor2KeyString(std::string GROUP, std::string KEY, std::string KEY2, std::string VARNAME);

    
    void setFloat(std::string GROUP, std::string VARNAME, float VALUE);
    void setInteger(std::string GROUP, std::string VARNAME, int VALUE);
    void setString(std::string GROUP, std::string VARNAME, std::string VALUE);
    void setIndexFloat(std::string GROUP, std::string VARNAME, float VALUE, int INDEX);
    void setIndexInteger(std::string GROUP, std::string VARNAME, int VALUE, int INDEX);
    void setIndexString(std::string GROUP, std::string VARNAME, std::string VALUE, int INDEX);
    void setForKeyFloat(std::string GROUP, std::string KEY, std::string VARNAME, float VALUE);
    void setForKeyInteger(std::string GROUP, std::string KEY, std::string VARNAME, int VALUE);
    void setForKeyString(std::string GROUP, std::string KEY, std::string VARNAME, std::string VALUE);
    void setFor2KeyFloat(std::string GROUP, std::string KEY, std::string KEY2, std::string VARNAME, float VALUE);
    void setFor2KeyInteger(std::string GROUP, std::string KEY, std::string KEY2, std::string VARNAME, int VALUE);
    void setFor2KeyString(std::string GROUP, std::string KEY, std::string KEY2, std::string VARNAME, std::string VALUE);

    void eraseGroupMemory(std::string GROUP);

};

#endif // FLEXIBLEIO_H
