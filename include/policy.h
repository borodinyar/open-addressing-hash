#pragma once

#include <cstdlib>

struct LinearProbing
{
    std::size_t size;
    std::size_t currentIndex;

    LinearProbing(size_t size, size_t index)
        : size(size)
        , currentIndex(index)
    {
    }

    std::size_t next()
    {
        currentIndex = (currentIndex + 1) % size;
        return currentIndex;
    }
};

struct QuadraticProbing
{
    std::size_t size;
    std::size_t stepNum;
    std::size_t currentIndex;

    QuadraticProbing(size_t size, size_t startIndex)
        : size(size)
        , stepNum(0)
        , currentIndex(startIndex)
    {
    }

    std::size_t next()
    {
        ++stepNum;
        currentIndex = (currentIndex + stepNum * stepNum) % size;
        return currentIndex;
    }
};
