#ifndef GAMEOFLIFEBASE_H
#define GAMEOFLIFEBASE_H

#include <vector>
#include <utility>

class GameOfLifeBase
{

public:
    GameOfLifeBase() {}

    virtual void init(std::vector<std::pair<int, int>> initialValues) = 0;
    virtual int evolve() = 0;
    virtual void clear() = 0;
    virtual void randomFill(int percentage) = 0;

    virtual int getHeight() = 0;
    virtual int getWidth() = 0;
};

#endif // GAMEOFLIFEBASE_H
