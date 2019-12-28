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
    static std::unordered_map<std::string, std::unordered_map<std::string, std::string>> datatwodimensional;
    static std::unordered_map<std::string, std::unordered_map<std::string, std::unordered_map<std::string, std::string>>> datathreedimensional;

protected:
    FlexibleIO();
    static FlexibleIO *instance;

public:
    static FlexibleIO* getInstance();
    static FlexibleIO* newInstance();

    void readInputFile(std::string GROUP) {}

    float getReal(std::string GROUP, std::string VARNAME);
    int getInteger(std::string GROUP, std::string VARNAME);
    std::string getChar(std::string GROUP, std::string VARNAME);
    float getRealIndex(std::string GROUP, std::string VARNAME, int INDEX);
    int getIntegerIndex(std::string GROUP, std::string VARNAME, int INDEX);
    std::string getCharIndex(std::string GROUP, std::string VARNAME, int INDEX);
    float* getRealArray(std::string GROUP, std::string VARNAME, std::string SIZE);
    int* getIntegerArray(std::string GROUP, std::string VARNAME, std::string SIZE);
    std::string getCharArray(std::string GROUP, std::string VARNAME, std::string SIZE);
    float getRealYrdoy(std::string GROUP, std::string YRDOY, std::string VARNAME);
    int getIntegerYrdoy(std::string GROUP, std::string YRDOY, std::string VARNAME);
    std::string getCharYrdoy(std::string GROUP, std::string YRDOY, std::string VARNAME);

    void setRealMemory(std::string GROUP, std::string VARNAME, float VALUE);
    void setIntegerMemory(std::string GROUP, std::string VARNAME, int VALUE);
    void setCharMemory(std::string GROUP, std::string VARNAME, std::string VALUE);
    void setRealIndexMemory(std::string GROUP, std::string VARNAME, float VALUE, int INDEX);
    void setIntegerIndexMemory(std::string GROUP, std::string VARNAME, int VALUE, int INDEX);
    void setCharIndexMemory(std::string GROUP, std::string VARNAME, std::string VALUE, int INDEX);
    void setRealYrdoyMemory(std::string GROUP, std::string YRDOY, std::string VARNAME, float VALUE);
    void setIntegerYrdoyMemory(std::string GROUP, std::string YRDOY, std::string VARNAME, int VALUE);
    void setCharYrdoyMemory(std::string GROUP, std::string YRDOY, std::string VARNAME, std::string VALUE);

    void eraseGroupMemory(std::string GROUP);

};

#endif // FLEXIBLEIO_H
