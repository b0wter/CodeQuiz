#ifndef GAMEOFLIFEBASE_H
#define GAMEOFLIFEBASE_H

#include <vector>
#include <utility>

class GameOfLifeBase
{

public:
    GameOfLifeBase() {}

    virtual void init(std::vector<std::pair<int, int>> initialValues) = 0;
    virtual void evolve() = 0;
};

#endif // GAMEOFLIFEBASE_H
