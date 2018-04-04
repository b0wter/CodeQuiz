#ifndef SIMPLEGAMEOFLIFE_H
#define SIMPLEGAMEOFLIFE_H

#include "gameoflifebase.h"

class SimpleGameOfLife : public GameOfLifeBase
{
private:
    int mBuffer;
    int mHeight;
    int mWidth;
    int mSize;
    int mEvolution;
    bool *mCell;

    SimpleGameOfLife();

public:
    explicit SimpleGameOfLife(int height, int width);
    virtual ~SimpleGameOfLife();

    virtual void init(std::vector<std::pair<int, int>> initialValues);
    virtual void evolve();

    void print();

    inline bool get(int idxH, int idxW) {
        return mCell[mBuffer*mSize + idxH*mWidth + idxW];
    }
    inline void set(int idxH, int idxW) {
        mCell[mBuffer*mSize + idxH*mWidth + idxW] = true;
    }
    inline void unset(int idxH, int idxW) {
        mCell[mBuffer*mSize + idxH*mWidth + idxW] = false;
    }
    inline void setTo(int idxH, int idxW, bool val) {
        mCell[mBuffer*mSize + idxH*mWidth + idxW] = val;
    }

};

#endif // SIMPLEGAMEOFLIFE_H
