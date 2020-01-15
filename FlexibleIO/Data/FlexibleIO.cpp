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
#include <stdlib.h>
#include <iostream>
#include <string.h>
#include <unordered_map>
#include <algorithm>

std::unordered_map<std::string, std::unordered_map<std::string, std::string>> FlexibleIO::datatwodimensional;
std::unordered_map<std::string, std::unordered_map<std::string, std::unordered_map<std::string, std::string>>> FlexibleIO::datathreedimensional;


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

FlexibleIO::FlexibleIO()
{

}


float FlexibleIO::getReal(std::string GROUP, std::string VARNAME)
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

std::string FlexibleIO::getChar(std::string GROUP, std::string VARNAME)
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

float FlexibleIO::getRealIndex(std::string GROUP, std::string VARNAME, int INDEX)
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

int FlexibleIO::getIntegerIndex(std::string GROUP, std::string VARNAME, int INDEX)
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

std::string FlexibleIO::getCharIndex(std::string GROUP, std::string VARNAME, int INDEX)
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

float* FlexibleIO::getRealArray(std::string GROUP, std::string VARNAME, std::string SIZE)
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

int* FlexibleIO::getIntegerArray(std::string GROUP, std::string VARNAME, std::string SIZE)
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

std::string FlexibleIO::getCharArray(std::string GROUP, std::string VARNAME, std::string SIZE)
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

float FlexibleIO::getRealYrdoy(std::string GROUP, std::string YRDOY, std::string VARNAME)
{

    if ( this->datathreedimensional[GROUP][YRDOY][VARNAME] != "\0")
    {
        return strtof(this->datathreedimensional[GROUP][YRDOY][VARNAME].c_str(), NULL);
    }
    else
    {
        return -99.0;
    }

}

int FlexibleIO::getIntegerYrdoy(std::string GROUP, std::string YRDOY, std::string VARNAME)
{

    if ( this->datathreedimensional[GROUP][YRDOY][VARNAME] != "\0")
    {
        return std::stoi(this->datathreedimensional[GROUP][YRDOY][VARNAME], NULL, 0);
    }
    else
    {
        return -99;
    }

}

std::string FlexibleIO::getCharYrdoy(std::string GROUP, std::string YRDOY, std::string VARNAME)
{

    if ( this->datathreedimensional[GROUP][YRDOY][VARNAME] != "\0")
    {
        return this->datathreedimensional[GROUP][YRDOY][VARNAME];
    }
    else
    {
        return "-99";
    }

}


void FlexibleIO::setRealMemory(std::string GROUP, std::string VARNAME, float VALUE)
{

    this->datatwodimensional[GROUP][VARNAME] = std::to_string(VALUE);

}

void FlexibleIO::setIntegerMemory(std::string GROUP, std::string VARNAME, int VALUE)
{

    this->datatwodimensional[GROUP][VARNAME] = std::to_string(VALUE);

}

void FlexibleIO::setCharMemory(std::string GROUP, std::string VARNAME, std::string VALUE)
{

    this->datatwodimensional[GROUP][VARNAME] = VALUE;

}

void FlexibleIO::setRealIndexMemory(std::string GROUP, std::string VARNAME, float VALUE, int INDEX)
{

    this->datathreedimensional[GROUP][VARNAME][std::to_string(INDEX)] = std::to_string(VALUE);

}

void FlexibleIO::setIntegerIndexMemory(std::string GROUP, std::string VARNAME, int VALUE, int INDEX)
{

    this->datathreedimensional[GROUP][VARNAME][std::to_string(INDEX)] = std::to_string(VALUE);

}

void FlexibleIO::setCharIndexMemory(std::string GROUP, std::string VARNAME, std::string VALUE, int INDEX)
{

    this->datathreedimensional[GROUP][VARNAME][std::to_string(INDEX)] = VALUE;

}

void FlexibleIO::setRealYrdoyMemory(std::string GROUP, std::string YRDOY, std::string VARNAME, float VALUE)
{

    this->datathreedimensional[GROUP][YRDOY][VARNAME] = std::to_string(VALUE);

}

void FlexibleIO::setIntegerYrdoyMemory(std::string GROUP, std::string YRDOY, std::string VARNAME, int VALUE)
{

    this->datathreedimensional[GROUP][YRDOY][VARNAME] = std::to_string(VALUE);

}

void FlexibleIO::setCharYrdoyMemory(std::string GROUP, std::string YRDOY, std::string VARNAME, std::string VALUE)
{

    this->datathreedimensional[GROUP][YRDOY][VARNAME] = VALUE;

}


void FlexibleIO::eraseGroupMemory(std::string GROUP)
{

    this->datatwodimensional.erase(GROUP);
    this->datathreedimensional.erase(GROUP);

}
