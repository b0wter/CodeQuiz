#include <QCoreApplication>
#include <opencv2/opencv.hpp>
#include <algorithm>
#include <QElapsedTimer>
#include <QtDebug>

#ifndef PRIMES_H
#define PRIMES_H

#define vecInt std::vector<int>

class primes
{
private:

public:
    primes();
    vecInt calcPrimes();
    vecInt readPrimes(std::string);
    int writePrimes(vecInt, std::string);
    vecInt checkPrimes(vecInt);
};

#endif // PRIMES_H
