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
    unsigned char *mCell;

    SimpleGameOfLife();

public:
    explicit SimpleGameOfLife(int height, int width);
    virtual ~SimpleGameOfLife();

    virtual void init(std::vector<std::pair<int, int>> initialValues);
    virtual int evolve();
    virtual void clear();
    virtual void randomFill(int percentage);

    virtual inline int getHeight() const;
    virtual inline int getWidth() const;
    virtual const unsigned char* getData();
    virtual inline int getEvolution() const;

    void print();

    virtual inline unsigned char get(int idxH, int idxW) {
        return mCell[mBuffer*mSize + idxH*mWidth + idxW];
    }
    virtual inline void set(int idxH, int idxW) {
        if(idxH != 0 && idxH != (mHeight - 1) && idxW != 0 && idxW != (mWidth - 1))
            mCell[mBuffer*mSize + idxH*mWidth + idxW] = 1;
    }
    virtual inline void unset(int idxH, int idxW) {
        if(idxH != 0 && idxH != (mHeight - 1) && idxW != 0 && idxW != (mWidth - 1))
            mCell[mBuffer*mSize + idxH*mWidth + idxW] = 0;
    }
    virtual inline void swap(int idxH, int idxW) {
        if(idxH != 0 && idxH != (mHeight - 1) && idxW != 0 && idxW != (mWidth - 1))
            mCell[mBuffer*mSize + idxH*mWidth + idxW] ^= 1;
    }
    virtual inline void setTo(int idxH, int idxW, unsigned char val) {
        if(idxH != 0 && idxH != (mHeight - 1) && idxW != 0 && idxW != (mWidth - 1))
            mCell[mBuffer*mSize + idxH*mWidth + idxW] = val;
    }
};

#endif // SIMPLEGAMEOFLIFE_H
