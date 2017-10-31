#include <QApplication>

#include <QDebug>

#include <iostream>
#include <limits>
#include <iomanip>
#include <queue>

#include "monopoly.h"

bool contains(std::vector<int> arr, int val)
{
    for(unsigned int i = 0; i < arr.size(); i++)
        if(arr[i] == val)
            return true;
    return false;
}

std::vector<int> getHighestN(quint64 vec[], int vecLen, int n) {
    std::priority_queue<std::pair<quint64, int>> w;
    std::vector<int> highestW;
    for(int i = 0; i < vecLen; i++)
        w.push(std::pair<quint64, int>(vec[i], i));
    for(int i = 0; i < n; i++) {
        highestW.push_back(w.top().second);
        w.pop();
    }
    return highestW;
}

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    Monopoly monopoly(&app);

    monopoly.play();

    Monopoly::Stats stats = monopoly.getStats();

    std::vector<int> highest3 = getHighestN(stats.visits, 40, 3);
    std::cout << "Monopoly Stats" << std::endl;
    for(int i = 0; i < 40; i++) {
        std::cout << (contains(highest3, i) ? "* " : "  ")
                  << std::setw(22) << std::left
                  << monopoly.fieldToString((Monopoly::Fields)i).toStdString()
                  << std::setw(10) << stats.visits[i] << " : "
                  << stats.visits[i] / double(stats.rolls) << std::endl;
    }
    std::cout << "\nPaschs: " << stats.doubles << std::endl;
    std::cout << "\nAugenzahlen: " << std::endl;
    for(int i = 0; i < 11; i++) {
        std::cout << std::setw(2) << i + 2 << " : "
                  << stats.pipRolls[i] << std::endl;
    }

    monopoly.play();

    stats = monopoly.getStats();

    highest3 = getHighestN(stats.visits, 40, 3);
    std::cout << "Monopoly Stats" << std::endl;
    for(int i = 0; i < 40; i++) {
        std::cout << (contains(highest3, i) ? "* " : "  ")
                  << std::setw(22) << std::left
                  << monopoly.fieldToString((Monopoly::Fields)i).toStdString()
                  << std::setw(10) << stats.visits[i] << " : "
                  << stats.visits[i] / double(stats.rolls) << std::endl;
    }
    std::cout << "\nPaschs: " << stats.doubles << std::endl;
    std::cout << "\nAugenzahlen: " << std::endl;
    for(int i = 0; i < 11; i++) {
        std::cout << std::setw(2) << i + 2 << " : "
                  << stats.pipRolls[i] << std::endl;
    }

    return app.exec();
}
