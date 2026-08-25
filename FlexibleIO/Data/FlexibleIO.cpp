/*=======================================================================
  FlexibleIO.cpp, Fabio Oliveira, Willingthon Pavan
  Functions to manipulate memory data in C++.
-----------------------------------------------------------------------
  REVISION HISTORY
  03/08/2018 FO Added singleton pattern.
  03/08/2018 FO Implemented all class functions.
  03/14/2018 FO Added two static data structures to store data.
  03/15/2018 FO Added a function to erase groups of data from memory.
========================================================================*/
#include "FlexibleIO.hpp"
#include <new>
#include <cstdlib>
#include <iostream>
#include <string>
#include <unordered_map>
#include <algorithm>

//// Future Development
//struct InnerData {std::unordered_map<std::string, std::string> variables;};
//struct ThirdLevel {std::unordered_map<std::string, InnerData> thirdLevelMap;};
//struct SecondLevel {std::unordered_map<std::string, ThirdLevel> secondLevelMap;};
//struct FirstLevel {std::unordered_map<std::string, SecondLevel> firstLevelMap;};
//// Define the main map
//static std::unordered_map<std::string, FirstLevel> dataStore;
//// Access
//dataStore["Group1"].firstLevelMap["ID1"].secondLevelMap["Var1"].thirdLevelMap["SubVar1"].variables["Value1"];

std::unordered_map<std::string,std::unordered_map<std::string, std::string>> FlexibleIO::datatwodimensional;
std::unordered_map<std::string,std::unordered_map<std::string,std::unordered_map<std::string, std::string>>> FlexibleIO::datathreedimensional;
std::unordered_map<std::string,std::unordered_map<std::string,std::unordered_map<std::string, std::unordered_map<std::string, std::string>>>> FlexibleIO::datafourdimensional;


FlexibleIO* FlexibleIO::instance = nullptr;

FlexibleIO* FlexibleIO::getInstance()
{
    if (instance == nullptr)
        instance = new FlexibleIO();
    return instance;
}

FlexibleIO* FlexibleIO::newInstance()
{
    instance=nullptr;
    return getInstance();
}

FlexibleIO::FlexibleIO(){}

// Public Functions Definitions
// Getters
float FlexibleIO::getFloat(std::string GROUP, std::string VARNAME)
{

    if ( this->datatwodimensional[GROUP][VARNAME] != "\0")
    {
        return strtof(this->datatwodimensional[GROUP][VARNAME].c_str(), NULL);
    }
    else
    {
        return -99.0;
    }

}

int FlexibleIO::getInteger(std::string GROUP, std::string VARNAME)
{

    if ( this->datatwodimensional[GROUP][VARNAME] != "\0")
    {
        return std::stoi(this->datatwodimensional[GROUP][VARNAME], NULL, 0);
    }
    else
    {
        return -99;
    }

}

std::string FlexibleIO::getString(std::string GROUP, std::string VARNAME)
{

    if ( this->datatwodimensional[GROUP][VARNAME] != "\0")
    {
        return this->datatwodimensional[GROUP][VARNAME];
    }
    else
    {
        return " ";
    }

}

float FlexibleIO::getIndexFloat(std::string GROUP, std::string VARNAME, int INDEX)
{

    if ( this->datathreedimensional[GROUP][VARNAME][std::to_string(INDEX)] != "\0")
    {
        return strtof(this->datathreedimensional[GROUP][VARNAME][std::to_string(INDEX)].c_str(), NULL);
    }
    else
    {
        return -99.0;
    }

}

int FlexibleIO::getIndexInteger(std::string GROUP, std::string VARNAME, int INDEX)
{

    if ( this->datathreedimensional[GROUP][VARNAME][std::to_string(INDEX)] != "\0")
    {
        return std::stoi(this->datathreedimensional[GROUP][VARNAME][std::to_string(INDEX)], NULL, 0);
    }
    else
    {
        return -99;
    }

}

std::string FlexibleIO::getIndexString(std::string GROUP, std::string VARNAME, int INDEX)
{

    if ( this->datathreedimensional[GROUP][VARNAME][std::to_string(INDEX)] != "\0")
    {
        return this->datathreedimensional[GROUP][VARNAME][std::to_string(INDEX)];
    }
    else
    {
        return " ";
    }

}

float* FlexibleIO::getArrayFloat(std::string GROUP, std::string VARNAME, std::string SIZE)
{

    int size = std::stoi(SIZE, NULL, 0);

    float *array = (float*) malloc(size * sizeof(float));
    if (array == NULL)
    {
        return array;
    }

    if(size >= 1)
    {
        for(int i = 1; i <= size; i++)
        {
            array[i-1] = -99;
            if ( this->datathreedimensional[GROUP][VARNAME][std::to_string(i)] != "\0")
            {
                array[i-1] = strtof(this->datathreedimensional[GROUP][VARNAME][std::to_string(i)].c_str(), NULL);
            }
        }
    }

    return &array[0];

}

int* FlexibleIO::getArrayInteger(std::string GROUP, std::string VARNAME, std::string SIZE)
{

    int size = std::stoi(SIZE, NULL, 0);

    int *array = (int*) malloc(size * sizeof(int));
    if (array == NULL)
    {
        return array;
    }

    if(size >= 1)
    {
        for(int i = 1; i <= size; i++)
        {
            array[i-1] = -99;
            if ( this->datathreedimensional[GROUP][VARNAME][std::to_string(i)] != "\0")
            {
                array[i-1] = std::stoi(this->datathreedimensional[GROUP][VARNAME][std::to_string(i)], NULL, 0);
            }
        }
    }

    return &array[0];

}

std::string FlexibleIO::getArrayString(std::string GROUP, std::string VARNAME, std::string SIZE)
{

    std::string strarray = "";
    int size = std::stoi(SIZE, NULL, 0);

    if(size >= 1)
    {
        for(int i = 1; i <= size; i++)
        {
            if ( this->datathreedimensional[GROUP][VARNAME][std::to_string(i)] != "\0")
            {
                strarray += this->datathreedimensional[GROUP][VARNAME][std::to_string(i)] + " ";
            }
            else
            {
                strarray += "-99 ";
            }
        }
    }

    return strarray;
}

float FlexibleIO::getForKeyFloat(std::string GROUP, std::string KEY, std::string VARNAME)
{

    if ( this->datathreedimensional[GROUP][KEY][VARNAME] != "\0")
    {
        return strtof(this->datathreedimensional[GROUP][KEY][VARNAME].c_str(), NULL);
    }
    else
    {
        return -99.0;
    }

}

int FlexibleIO::getForKeyInteger(std::string GROUP, std::string KEY, std::string VARNAME)
{

    if ( this->datathreedimensional[GROUP][KEY][VARNAME] != "\0")
    {
        return std::stoi(this->datathreedimensional[GROUP][KEY][VARNAME], NULL, 0);
    }
    else
    {
        return -99;
    }

}

std::string FlexibleIO::getForKeyString(std::string GROUP, std::string KEY, std::string VARNAME)
{

    if ( this->datathreedimensional[GROUP][KEY][VARNAME] != "\0")
    {
        return this->datathreedimensional[GROUP][KEY][VARNAME];
    }
    else
    {
        return "-99";
    }

}

float FlexibleIO::getFor2KeyFloat(std::string GROUP, std::string KEY, std::string KEY2, std::string VARNAME)
{

    if ( this->datafourdimensional[GROUP][KEY][KEY2][VARNAME] != "\0")
    {
        return strtof(this->datafourdimensional[GROUP][KEY][KEY2][VARNAME].c_str(), NULL);
    }
    else
    {
        return -99.0;
    }

}

int FlexibleIO::getFor2KeyInteger(std::string GROUP, std::string KEY, std::string KEY2, std::string VARNAME)
{

    if ( this->datafourdimensional[GROUP][KEY][KEY2][VARNAME] != "\0")
    {
        return std::stoi(this->datafourdimensional[GROUP][KEY][KEY2][VARNAME], NULL, 0);
    }
    else
    {
        return -99;
    }

}

std::string FlexibleIO::getFor2KeyString(std::string GROUP, std::string KEY, std::string KEY2, std::string VARNAME)
{

    if ( this->datafourdimensional[GROUP][KEY][KEY2][VARNAME] != "\0")
    {
        return this->datafourdimensional[GROUP][KEY][KEY2][VARNAME];
    }
    else
    {
        return "-99";
    }

}


// Setters
void FlexibleIO::setFloat(std::string GROUP, std::string VARNAME, float VALUE)
{

    this->datatwodimensional[GROUP][VARNAME] = std::to_string(VALUE);

}

void FlexibleIO::setInteger(std::string GROUP, std::string VARNAME, int VALUE)
{

    this->datatwodimensional[GROUP][VARNAME] = std::to_string(VALUE);

}

void FlexibleIO::setString(std::string GROUP, std::string VARNAME, std::string VALUE)
{

    this->datatwodimensional[GROUP][VARNAME] = VALUE;

}

void FlexibleIO::setIndexFloat(std::string GROUP, std::string VARNAME, float VALUE, int INDEX)
{

    this->datathreedimensional[GROUP][VARNAME][std::to_string(INDEX)] = std::to_string(VALUE);

}

void FlexibleIO::setIndexInteger(std::string GROUP, std::string VARNAME, int VALUE, int INDEX)
{

    this->datathreedimensional[GROUP][VARNAME][std::to_string(INDEX)] = std::to_string(VALUE);

}

void FlexibleIO::setIndexString(std::string GROUP, std::string VARNAME, std::string VALUE, int INDEX)
{

    this->datathreedimensional[GROUP][VARNAME][std::to_string(INDEX)] = VALUE;

}

void FlexibleIO::setForKeyFloat(std::string GROUP, std::string KEY, std::string VARNAME, float VALUE)
{

    this->datathreedimensional[GROUP][KEY][VARNAME] = std::to_string(VALUE);

}

void FlexibleIO::setForKeyInteger(std::string GROUP, std::string KEY, std::string VARNAME, int VALUE)
{

    this->datathreedimensional[GROUP][KEY][VARNAME] = std::to_string(VALUE);

}

void FlexibleIO::setForKeyString(std::string GROUP, std::string KEY, std::string VARNAME, std::string VALUE)
{

    this->datathreedimensional[GROUP][KEY][VARNAME] = VALUE;

}

void FlexibleIO::setFor2KeyFloat(std::string GROUP, std::string KEY, std::string KEY2, std::string VARNAME, float VALUE)
{

    this->datafourdimensional[GROUP][KEY][KEY2][VARNAME] = std::to_string(VALUE);

}

void FlexibleIO::setFor2KeyInteger(std::string GROUP, std::string KEY, std::string KEY2, std::string VARNAME, int VALUE)
{

    this->datafourdimensional[GROUP][KEY][KEY2][VARNAME] = std::to_string(VALUE);

}

void FlexibleIO::setFor2KeyString(std::string GROUP, std::string KEY, std::string KEY2, std::string VARNAME, std::string VALUE)
{

    this->datafourdimensional[GROUP][KEY][KEY2][VARNAME] = VALUE;

}

// Others
void FlexibleIO::eraseGroupMemory(std::string GROUP)
{

    this->datatwodimensional.erase(GROUP);
    this->datathreedimensional.erase(GROUP);
    this->datafourdimensional.erase(GROUP);

}
