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

    virtual int getHeight() const = 0;
    virtual int getWidth() const = 0;
    virtual const unsigned char *getData() = 0;
    virtual inline int getEvolution() const = 0;

    virtual unsigned char get(int idxH, int idxW) = 0;
    virtual void set(int idxH, int idxW) = 0;
    virtual void unset(int idxH, int idxW) = 0;
    virtual void swap(int idxH, int idxW) = 0;
    virtual void setTo(int idxH, int idxW, unsigned char val) = 0;
};

#endif // GAMEOFLIFEBASE_H
