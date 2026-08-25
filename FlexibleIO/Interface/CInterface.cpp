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

    void getFloat(char *GROUP, char *VARNAME, float *VALUE);
    void getInteger(char *GROUP, char *VARNAME, int *VALUE);
    void getString(char *GROUP, char *VARNAME, char  *VALUE, int *VSIZE);
    void getIndexFloat(char *GROUP, char *VARNAME, float *VALUE, int *INDEX);
    void getIndexInteger(char *GROUP, char *VARNAME, int *VALUE, int *INDEX);
    void getIndexString(char *GROUP, char *VARNAME, char *VALUE, int *VSIZE, int *INDEX);
    void getArrayFloat(char *GROUP, char *VARNAME, float *VALUE, char *SIZE);
    void getArrayInteger(char *GROUP, char *VARNAME, int *VALUE, char *SIZE);
    void getArrayString(char *GROUP, char *VARNAME, char *VALUE, int *VSIZE, char *SIZE);
    void getForKeyFloat(char *GROUP, int *KEY, char *VARNAME, float *VALUE);
    void getForKeyInteger(char *GROUP, int *KEY, char *VARNAME, int *VALUE);
    void getForKeyString(char *GROUP, int *KEY, char *VARNAME, char  *VALUE, int *VSIZE);
    void getFor2KeyFloat(char *GROUP, int *KEY, int *KEY2, char *VARNAME, float *VALUE);
    void getFor2KeyInteger(char *GROUP, int *KEY, int *KEY2, char *VARNAME, int *VALUE);
    void getFor2KeyString(char *GROUP, int *KEY, int *KEY2, char *VARNAME, char  *VALUE, int *VSIZE);

    void setFloat(char *GROUP, char *VARNAME, float *VALUE);
    void setInteger(char *GROUP, char *VARNAME, int *VALUE);
    void setString(char *GROUP, char *VARNAME, char  *VALUE);
    void setIndexFloat(char *GROUP, char *VARNAME, float *VALUE, int *INDEX);
    void setIndexInteger(char *GROUP, char *VARNAME, int *VALUE, int *INDEX);
    void setIndexString(char *GROUP, char *VARNAME, char  *VALUE, int *INDEX);
    void setForKeyFloat(char *GROUP, int *KEY, char *VARNAME, float *VALUE);
    void setForKeyInteger(char *GROUP, int *KEY, char *VARNAME, int *VALUE);
    void setForKeyString(char *GROUP, int *KEY, char *VARNAME, char *VALUE);
    void setFor2KeyFloat(char *GROUP, int *KEY, int *KEY2, char *VARNAME, float *VALUE);
    void setFor2KeyInteger(char *GROUP, int *KEY, int *KEY2, char *VARNAME, int *VALUE);
    void setFor2KeyString(char *GROUP, int *KEY, int *KEY2, char *VARNAME, char *VALUE);


}

void getFloat(char *GROUP, char *VARNAME, float *VALUE)
{

    std::string group(GROUP), varname(VARNAME);

    *VALUE = FlexibleIO::getInstance()->getFloat(group, VARNAME);

}

void getInteger(char *GROUP, char *VARNAME, int *VALUE)
{

    std::string group(GROUP), varname(VARNAME);

    *VALUE = FlexibleIO::getInstance()->getInteger(group, varname);

}

void getString(char *GROUP, char *VARNAME, char  *VALUE, int *VSIZE)
{
    *VALUE = '\0';
    std::string group(GROUP), varname(VARNAME);
    
    std::string result = FlexibleIO::getInstance()->getString(group, varname);
    size_t size = static_cast<size_t>(*VSIZE);    
    size_t sizecpy = std::min(result.size(), size);
    std::memcpy(VALUE, result.c_str(), sizecpy);
}

void getIndexFloat(char *GROUP, char *VARNAME, float *VALUE, int *INDEX)
{

    std::string group(GROUP), varname(VARNAME);

    *VALUE = FlexibleIO::getInstance()->getIndexFloat(group, varname, *INDEX);

}

void getIndexInteger(char *GROUP, char *VARNAME, int *VALUE, int *INDEX)
{

    std::string group(GROUP), varname(VARNAME);

    *VALUE = FlexibleIO::getInstance()->getIndexInteger(group, varname, *INDEX);

}

void getIndexString(char *GROUP, char *VARNAME, char *VALUE, int *VSIZE, int *INDEX)
{

    *VALUE = '\0';
    std::string group(GROUP), varname(VARNAME);
    
    std::string result = FlexibleIO::getInstance()->getIndexString(group, varname, *INDEX).c_str();
    size_t size = static_cast<size_t>(*VSIZE);    
    size_t sizecpy = std::min(result.size(), size);
    std::memcpy(VALUE, result.c_str(), sizecpy);
}

void getArrayFloat(char *GROUP, char *VARNAME, float *VALUE, char *SIZE)
{

    std::string group(GROUP), varname(VARNAME), size(SIZE);

    float *array = FlexibleIO::getInstance()->getArrayFloat(group, varname, size);

    std::copy(array, array + std::stoi(size,NULL, 0), VALUE);

}

void getArrayInteger(char *GROUP, char *VARNAME, int *VALUE, char *SIZE)
{

    std::string group(GROUP), varname(VARNAME), size(SIZE);

    int *array = FlexibleIO::getInstance()->getArrayInteger(group, varname, size);

    std::copy(array, array + std::stoi(size,NULL, 0), VALUE);

}

void getArrayString(char *GROUP, char *VARNAME, char *VALUE, int *VSIZE, char *SIZE)
{

    std::string group(GROUP), varname(VARNAME), size(SIZE);
    
    std::string result = FlexibleIO::getInstance()->getArrayString(group, varname, size).c_str();
    size_t vsz = static_cast<size_t>(*VSIZE);    
    size_t sizecpy = std::min(result.size(), vsz);
    std::memcpy(VALUE, result.c_str(), sizecpy);
}

void getForKeyFloat(char *GROUP, int *KEY, char *VARNAME, float *VALUE)
{

    std::string group(GROUP), K(std::to_string(*KEY)), varname(VARNAME);

    *VALUE = FlexibleIO::getInstance()->getForKeyFloat(group, K, varname);

}

void getForKeyInteger(char *GROUP, int *KEY, char *VARNAME, int *VALUE)
{

    std::string group(GROUP), K(std::to_string(*KEY)), varname(VARNAME);

    *VALUE = FlexibleIO::getInstance()->getForKeyInteger(group, K, varname);

}

void getForKeyString(char *GROUP, int *KEY, char *VARNAME, char  *VALUE, int *VSIZE)
{

    std::string group(GROUP), K(std::to_string(*KEY)), varname(VARNAME);
    
    std::string result = FlexibleIO::getInstance()->getForKeyString(group, K, varname).c_str();
    size_t size = static_cast<size_t>(*VSIZE);    
    size_t sizecpy = std::min(result.size(), size);
    std::memcpy(VALUE, result.c_str(), sizecpy);
}

void getFor2KeyFloat(char *GROUP, int *KEY, int *KEY2, char *VARNAME, float *VALUE)
{
    std::string group(GROUP);
    std::string K(std::to_string(*KEY)), K2(std::to_string(*KEY2));
    std::string varname(VARNAME);

    *VALUE = FlexibleIO::getInstance()->getFor2KeyFloat(group, K, K2, varname);

}

void getFor2KeyInteger(char *GROUP, int *KEY, int *KEY2, char *VARNAME, int *VALUE)
{
    std::string group(GROUP);
    std::string K(std::to_string(*KEY)), K2(std::to_string(*KEY2));
    std::string varname(VARNAME);

    *VALUE = FlexibleIO::getInstance()->getFor2KeyInteger(group, K, K2, varname);

}

void getFor2KeyString(char *GROUP, int *KEY, int *KEY2, char *VARNAME, char  *VALUE, int *VSIZE)
{
    std::string group(GROUP);
    std::string K(std::to_string(*KEY)), K2(std::to_string(*KEY2));
    std::string varname(VARNAME);
    
    std::string result = FlexibleIO::getInstance()->getFor2KeyString(group, K, K2, varname).c_str();
    size_t size = static_cast<size_t>(*VSIZE);    
    size_t sizecpy = std::min(result.size(), size);
    std::memcpy(VALUE, result.c_str(), sizecpy);
}


void setFloat(char *GROUP, char *VARNAME, float *VALUE)
{

    std::string group(GROUP), varname(VARNAME);

    FlexibleIO::getInstance()->setFloat(group, varname, *VALUE);

}

void setInteger(char *GROUP, char *VARNAME, int *VALUE)
{

    std::string group(GROUP), varname(VARNAME);

    FlexibleIO::getInstance()->setInteger(group, varname, *VALUE);

}

void setString(char *GROUP, char *VARNAME, char  *VALUE)
{

    std::string group(GROUP), varname(VARNAME), value(VALUE);

    FlexibleIO::getInstance()->setString(group, varname, value);

}

void setIndexFloat(char *GROUP, char *VARNAME, float *VALUE, int *INDEX)
{

    std::string group(GROUP), varname(VARNAME);

    FlexibleIO::getInstance()->setIndexFloat(group, varname, *VALUE, *INDEX);

}

void setIndexInteger(char *GROUP, char *VARNAME, int *VALUE, int *INDEX)
{

    std::string group(GROUP), varname(VARNAME);

    FlexibleIO::getInstance()->setIndexInteger(group, varname, *VALUE, *INDEX);

}

void setIndexString(char *GROUP, char *VARNAME, char  *VALUE, int *INDEX)
{

    std::string group(GROUP), varname(VARNAME), value(VALUE);

    FlexibleIO::getInstance()->setIndexString(group, varname, value, *INDEX);

}

void setForKeyFloat(char *GROUP, int *KEY, char *VARNAME, float *VALUE)
{

    std::string group(GROUP), key(std::to_string(*KEY)), varname(VARNAME);

    FlexibleIO::getInstance()->setForKeyFloat(group, key, varname, *VALUE);

}

void setForKeyInteger(char *GROUP, int *KEY, char *VARNAME, int *VALUE)
{

    std::string group(GROUP), key(std::to_string(*KEY)), varname(VARNAME);

    FlexibleIO::getInstance()->setForKeyInteger(group, key, varname, *VALUE);

}

void setForKeyString(char *GROUP, int *KEY, char *VARNAME, char *VALUE)
{

    std::string group(GROUP), key(std::to_string(*KEY)), varname(VARNAME), value(VALUE);

    FlexibleIO::getInstance()->setForKeyString(group, key, varname, value);

}

void setFor2KeyFloat(char *GROUP, int *KEY, int *KEY2, char *VARNAME, float *VALUE)
{
    std::string group(GROUP);
    std::string key(std::to_string(*KEY)), key2(std::to_string(*KEY2));
    std::string varname(VARNAME);

    FlexibleIO::getInstance()->setFor2KeyFloat(group, key, key2, varname, *VALUE);

}

void setFor2KeyInteger(char *GROUP, int *KEY, int *KEY2, char *VARNAME, int *VALUE)
{
    std::string group(GROUP);
    std::string key(std::to_string(*KEY)), key2(std::to_string(*KEY2));
    std::string varname(VARNAME);

    FlexibleIO::getInstance()->setFor2KeyInteger(group, key, key2, varname, *VALUE);

}

void setFor2KeyString(char *GROUP, int *KEY, int *KEY2, char *VARNAME, char *VALUE)
{
    std::string group(GROUP);
    std::string key(std::to_string(*KEY)), key2(std::to_string(*KEY2));
    std::string varname(VARNAME), value(VALUE);

    FlexibleIO::getInstance()->setFor2KeyString(group, key, key2, varname, value);

}
