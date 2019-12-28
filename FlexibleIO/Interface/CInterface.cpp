/*=======================================================================
  Cinterface.cpp, Felipe de Vargas, Willingthon Pavan, Fabio Oliveira
  Functions to manipulate memory in C++.
-----------------------------------------------------------------------
  REVISION HISTORY
  07/10/2016 FV Written.
  09/01/2016 FV Create get's for read weather data.
  09/25/2016 FV Create set's for write data in memory.
  06/18/2017 FV Create new version of getters for weather data.
  12/04/2017 FO Update all FlexibleIO Getters and Setters data structure
  12/04/2017 FO Added a parameter in the functions for the new FlexibleIO data structure
  03/05/2018 FO Restructured all get/set functions to link with the new FlexibleIO.
  03/06/2018 FO Removed Json input.
========================================================================*/
#include <cstdlib>
#include <sstream>
#include <string>
#include <cstring>
#include <vector>
#include <algorithm>
#include "../Data/FlexibleIO.hpp"

extern "C" {

    void readinputfile(char *GROUP);

    void getReal(char *GROUP, char *VARNAME, float *VALUE);
    void getInteger(char *GROUP, char *VARNAME, int *VALUE);
    void getChar(char *GROUP, char *VARNAME, char  *VALUE);
    void getRealIndex(char *GROUP, char *VARNAME, float *VALUE, int *INDEX);
    void getIntegerIndex(char *GROUP, char *VARNAME, int *VALUE, int *INDEX);
    void getCharIndex(char *GROUP, char *VARNAME, char *VALUE, int *INDEX);
    void getRealArray(char *GROUP, char *VARNAME, float *VALUE, char *SIZE);
    void getIntegerArray(char *GROUP, char *VARNAME, int *VALUE, char *SIZE);
    void getCharArray(char *GROUP, char *VARNAME, char *VALUE, char *SIZE);
    void getRealYrdoy(char *GROUP, int *YRDOY, char *VARNAME, float *VALUE);
    void getIntegerYrdoy(char *GROUP, int *YRDOY, char *VARNAME, int *VALUE);
    void getCharYrdoy(char *GROUP, int *YRDOY, char *VARNAME, char  *VALUE);

    void setRealMemory(char *GROUP, char *VARNAME, float *VALUE);
    void setIntegerMemory(char *GROUP, char *VARNAME, int *VALUE);
    void setCharMemory(char *GROUP, char *VARNAME, char  *VALUE);
    void setRealIndexMemory(char *GROUP, char *VARNAME, float *VALUE, int *INDEX);
    void setIntegerIndexMemory(char *GROUP, char *VARNAME, int *VALUE, int *INDEX);
    void setCharIndexMemory(char *GROUP, char *VARNAME, char  *VALUE, int *INDEX);
    void setRealYrdoyMemory(char *GROUP, int *YRDOY, char *VARNAME, float *VALUE);
    void setIntegerYrdoyMemory(char *GROUP, int *YRDOY, char *VARNAME, int *VALUE);
    void setCharYrdoyMemory(char *GROUP, int *YRDOY, char *VARNAME, char *VALUE);

}


void readinputfile(char *GROUP)
{
    std::string group(GROUP);

    FlexibleIO::getInstance()->readInputFile(group);

}

void getReal(char *GROUP, char *VARNAME, float *VALUE)
{

    std::string group(GROUP), varname(VARNAME);

    *VALUE = FlexibleIO::getInstance()->getReal(group, VARNAME);

}

void getInteger(char *GROUP, char *VARNAME, int *VALUE)
{

    std::string group(GROUP), varname(VARNAME);

    *VALUE = FlexibleIO::getInstance()->getInteger(group, varname);

}

void getChar(char *GROUP, char *VARNAME, char  *VALUE)
{

    *VALUE = '\0';
    std::string group(GROUP), varname(VARNAME);

    strcpy(VALUE, FlexibleIO::getInstance()->getChar(group, varname).c_str());

}

void getRealIndex(char *GROUP, char *VARNAME, float *VALUE, int *INDEX)
{

    std::string group(GROUP), varname(VARNAME);

    *VALUE = FlexibleIO::getInstance()->getRealIndex(group, varname, *INDEX);

}

void getIntegerIndex(char *GROUP, char *VARNAME, int *VALUE, int *INDEX)
{

    std::string group(GROUP), varname(VARNAME);

    *VALUE = FlexibleIO::getInstance()->getIntegerIndex(group, varname, *INDEX);

}

void getCharIndex(char *GROUP, char *VARNAME, char *VALUE, int *INDEX)
{

    *VALUE = '\0';
    std::string group(GROUP), varname(VARNAME);

    strcpy(VALUE, FlexibleIO::getInstance()->getCharIndex(group, varname, *INDEX).c_str());

}

void getRealArray(char *GROUP, char *VARNAME, float *VALUE, char *SIZE)
{

    std::string group(GROUP), varname(VARNAME), size(SIZE);

    float *array = FlexibleIO::getInstance()->getRealArray(group, varname, size);

    std::copy(array, array + std::stoi(size,NULL, 0), VALUE);

}

void getIntegerArray(char *GROUP, char *VARNAME, int *VALUE, char *SIZE)
{

    std::string group(GROUP), varname(VARNAME), size(SIZE);

    int *array = FlexibleIO::getInstance()->getIntegerArray(group, varname, size);

    std::copy(array, array + std::stoi(size,NULL, 0), VALUE);

}

void getCharArray(char *GROUP, char *VARNAME, char *VALUE, char *SIZE)
{

    std::string group(GROUP), varname(VARNAME), size(SIZE);

    strcpy(VALUE, FlexibleIO::getInstance()->getCharArray(group, varname, size).c_str());

}

void getRealYrdoy(char *GROUP, int *YRDOY, char *VARNAME, float *VALUE)
{

    std::string group(GROUP), yrdoy(std::to_string(*YRDOY)), varname(VARNAME);

    *VALUE = FlexibleIO::getInstance()->getRealYrdoy(group, yrdoy, varname);

}

void getIntegerYrdoy(char *GROUP, int *YRDOY, char *VARNAME, int *VALUE)
{

    std::string group(GROUP), yrdoy(std::to_string(*YRDOY)), varname(VARNAME);

    *VALUE = FlexibleIO::getInstance()->getIntegerYrdoy(group, yrdoy, varname);

}

void getCharYrdoy(char *GROUP, int *YRDOY, char *VARNAME, char  *VALUE)
{

    std::string group(GROUP), yrdoy(std::to_string(*YRDOY)), varname(VARNAME);

    strcpy(VALUE, FlexibleIO::getInstance()->getCharYrdoy(group, yrdoy, varname).c_str());

}


void setRealMemory(char *GROUP, char *VARNAME, float *VALUE)
{

    std::string group(GROUP), varname(VARNAME);

    FlexibleIO::getInstance()->setRealMemory(group, varname, *VALUE);

}

void setIntegerMemory(char *GROUP, char *VARNAME, int *VALUE)
{

    std::string group(GROUP), varname(VARNAME);

    FlexibleIO::getInstance()->setIntegerMemory(group, varname, *VALUE);

}

void setCharMemory(char *GROUP, char *VARNAME, char  *VALUE)
{

    std::string group(GROUP), varname(VARNAME), value(VALUE);

    FlexibleIO::getInstance()->setCharMemory(group, varname, value);

}

void setRealIndexMemory(char *GROUP, char *VARNAME, float *VALUE, int *INDEX)
{

    std::string group(GROUP), varname(VARNAME);

    FlexibleIO::getInstance()->setRealIndexMemory(group, varname, *VALUE, *INDEX);

}

void setIntegerIndexMemory(char *GROUP, char *VARNAME, int *VALUE, int *INDEX)
{

    std::string group(GROUP), varname(VARNAME);

    FlexibleIO::getInstance()->setIntegerIndexMemory(group, varname, *VALUE, *INDEX);

}

void setCharIndexMemory(char *GROUP, char *VARNAME, char  *VALUE, int *INDEX)
{

    std::string group(GROUP), varname(VARNAME), value(VALUE);

    FlexibleIO::getInstance()->setCharIndexMemory(group, varname, value, *INDEX);

}

void setRealYrdoyMemory(char *GROUP, int *YRDOY, char *VARNAME, float *VALUE)
{

    std::string group(GROUP), yrdoy(std::to_string(*YRDOY)), varname(VARNAME);

    FlexibleIO::getInstance()->setRealYrdoyMemory(group, yrdoy, varname, *VALUE);

}

void setIntegerYrdoyMemory(char *GROUP, int *YRDOY, char *VARNAME, int *VALUE)
{

    std::string group(GROUP), yrdoy(std::to_string(*YRDOY)), varname(VARNAME);

    FlexibleIO::getInstance()->setIntegerYrdoyMemory(group, yrdoy, varname, *VALUE);

}

void setCharYrdoyMemory(char *GROUP, int *YRDOY, char *VARNAME, char *VALUE)
{

    std::string group(GROUP), yrdoy(std::to_string(*YRDOY)), varname(VARNAME), value(VALUE);

    FlexibleIO::getInstance()->setCharYrdoyMemory(group, yrdoy, varname, value);

}
