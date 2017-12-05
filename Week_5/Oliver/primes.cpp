#include "primes.h"

primes::primes(){}

vecInt primes::readPrimes(std::string hastTable)
{
    vecInt myReadedPrimes;
    //Read Primes
    cv::FileStorage fsREAD = cv::FileStorage(hastTable, cv::FileStorage::READ);
    if(!fsREAD.isOpened())
    {
        std::cerr << "ERROR: Could not open file MyPrimes.yml!" << std::endl;
        cv::waitKey(0);
        exit(1);
    }else{
        fsREAD["primes"] >> myReadedPrimes;
    }
    return myReadedPrimes;
}

vecInt primes::checkPrimes(vecInt myReadedPrims)
{
    vecInt myCheckedPrimes;
    //CheckPrime
    for(int number=1; number < 2000000; number = number + 2)
    {
        if(std::find(myReadedPrims.begin(), myReadedPrims.end(), number) != myReadedPrims.end())
        {
            //std::cout << "Primes: " << number << std::endl;
            myCheckedPrimes.push_back(int(number));
        }
    }
    return myCheckedPrimes;
}

int primes::writePrimes(vecInt myPrimes, std::string hastTable)
{
    //Write Primes
    cv::FileStorage fs(hastTable, CV_STORAGE_WRITE);
    if(fs.isOpened())
    {
        fs << "primes" << myPrimes;
        fs.release();
    }
    return 1;
}

vecInt primes::calcPrimes()
{
    vecInt mycalcedPrimes;
    int countPrim = 1;
    int PrimSum = 2;

    for(int j = 3; j <= 2000000; j = j + 2)
    {
        int i = 2;
        for(; i <= j - 1; i++)
        {
            if(j % i == 0)
            {
                break;
            }
        }
        if(i == j && i != 2)
        {
            mycalcedPrimes.push_back(int(j));
            countPrim++;
            PrimSum += j;
        }
    }
    return mycalcedPrimes;
}

