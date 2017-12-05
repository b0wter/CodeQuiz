#include "primes.h"

std::string hastTable = "MyPrimes.yml";

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);
    //Variablen deklarieren
    QElapsedTimer timer;
    vecInt myCalcedPrimes;
    vecInt myReadedPrims;
    vecInt myCheckedPrimes;
    primes myPrimer;
    int error = -1;

    //CreatePrimes
    timer.start();
    myCalcedPrimes = myPrimer.calcPrimes();
    qDebug() << "The slow prime operation took" << timer.elapsed() << "milliseconds";

    //Write
    timer.start();
    error = myPrimer.writePrimes(myCalcedPrimes, hastTable);
    qDebug() << "The slow read operation took" << timer.elapsed() << "milliseconds";
    //Needed 511946 ms (8,53 minutes) on NUC (read and write)

    //readPrimes
    timer.start();
    myReadedPrims = myPrimer.readPrimes(hastTable);
    qDebug() << "The read operation took" << timer.elapsed() << "milliseconds";

    //checkPrimes
    timer.start();
    myCheckedPrimes = myPrimer.checkPrimes(myReadedPrims);
    qDebug() << "The read operation took" << timer.elapsed() << "milliseconds";

    return a.exec();
}
