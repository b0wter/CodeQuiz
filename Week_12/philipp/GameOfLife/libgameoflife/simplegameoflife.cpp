#include "simplegameoflife.h"

#include <iostream>

SimpleGameOfLife::SimpleGameOfLife(int height, int width)
    : GameOfLifeBase()
    , mBuffer(0)
    , mHeight(height)
    , mWidth(width)
    , mSize(height*width)
    , mEvolution(0)
{
    mCell = new bool[2*height*width]();
}

SimpleGameOfLife::~SimpleGameOfLife()
{
    delete[] mCell;
}

void SimpleGameOfLife::init(std::vector<std::pair<int, int> > initialValues)
{
    for(std::pair<int, int> &p : initialValues) {
        set(p.first, p.second);
    }
}

void SimpleGameOfLife::evolve()
{
    int newBuffer = mBuffer ^ 1;
    for(int h = 1; h < (mHeight - 1); ++h) {
        for(int w = 1; w < (mWidth - 1); ++w) {
            // check neighborhood
            int neighbors = 0;
            for(int y = (h - 1); y <= (h + 1); ++y) {
                for(int x = (w - 1); x <= (w + 1); ++x) {
                    if(mCell[mBuffer*mSize + y*mWidth + x] && !(y == h && x == w))
                        ++neighbors;
                }
            }
            if(mCell[mBuffer*mSize + h*mWidth + w]) {
                if(neighbors == 2 || neighbors == 3) {
                    mCell[newBuffer*mSize + h*mWidth + w] = true;
                } else {
                    mCell[newBuffer*mSize + h*mWidth + w] = false;
                }
            } else {
                mCell[newBuffer*mSize + h*mWidth + w] = neighbors == 3 ? true : false;
            }
        }
    }
    mBuffer = newBuffer;
    ++mEvolution;
}

void SimpleGameOfLife::print()
{
    std::cout << "Evolution: " << mEvolution << "\n\n  ";
    for(int w = 0; w < (mWidth - 2); ++w)
        std::cout << w % 10;
    std::cout << "\n " << char(218);
    for(int w = 1; w < (mWidth - 1); ++w)
        std::cout << char(196);
    std::cout << char(191) << "\n";
    for(int h = 1; h < (mHeight - 1); ++h) {
        std::cout << (h - 1) % 10 << char(179);
        for(int w = 1; w < (mWidth - 1); ++w) {
            std::cout << (mCell[mBuffer*mSize + h*mWidth + w] ? 'x' : '-');
        }
        std::cout << char(179) << "\n";
    }
    std::cout << " " << char(192);
    for(int w = 1; w < (mWidth - 1); ++w)
        std::cout << char(196);
    std::cout << char(217) << "\n" << std::endl;
}
