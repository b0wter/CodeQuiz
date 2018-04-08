#include "simplegameoflife.h"

#include <ctime>
#include <cstdlib>
#include <iostream>

SimpleGameOfLife::SimpleGameOfLife(int height, int width)
    : GameOfLifeBase()
    , mBuffer(0)
    , mHeight(height)
    , mWidth(width)
    , mSize(height*width)
    , mEvolution(0)
{
    mCell = new unsigned char[2*height*width]();

    std::srand((unsigned int)std::time(nullptr));
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

int SimpleGameOfLife::evolve()
{
    int newBuffer = mBuffer ^ 1;
    for(int h = 1; h < (mHeight - 1); ++h) {
        for(int w = 1; w < (mWidth - 1); ++w) {
            // check neighborhood
            int neighbors = 0;
            for(int y = (h - 1); y <= (h + 1); ++y) {
                for(int x = (w - 1); x <= (w + 1); ++x) {
                    if((mCell[mBuffer*mSize + y*mWidth + x] == 1) && !(y == h && x == w))
                        ++neighbors;
                }
            }
            if(mCell[mBuffer*mSize + h*mWidth + w] == 1) {
                if(neighbors == 2 || neighbors == 3) {
                    mCell[newBuffer*mSize + h*mWidth + w] = 1;
                } else {
                    mCell[newBuffer*mSize + h*mWidth + w] = 0;
                }
            } else {
                mCell[newBuffer*mSize + h*mWidth + w] = (neighbors == 3) ? 1 : 0;
            }
        }
    }
    mBuffer = newBuffer;
    ++mEvolution;

    return mEvolution;
}

void SimpleGameOfLife::clear()
{
    for(int h = 1; h < (mHeight - 1); ++h) {
        for(int w = 1; w < (mWidth - 1); ++w) {
             mCell[h*mWidth + w] = 0;
             mCell[mSize + h*mWidth + w] = 0;
        }
    }
}

void SimpleGameOfLife::randomFill(int percentage)
{
    for(int h = 1; h < (mHeight - 1); ++h) {
        for(int w = 1; w < (mWidth - 1); ++w) {
             if(rand() % 100 < percentage)
                 mCell[mBuffer*mSize + h*mWidth + w] = 1;
             else
                 mCell[mBuffer*mSize + h*mWidth + w] = 0;
        }
    }
}

int SimpleGameOfLife::getHeight() const
{
    return mHeight;
}

int SimpleGameOfLife::getWidth() const
{
    return mWidth;
}

const unsigned char *SimpleGameOfLife::getData()
{
    return &mCell[mBuffer*mSize];
}

int SimpleGameOfLife::getEvolution() const
{
    return mEvolution;
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
            std::cout << ((mCell[mBuffer*mSize + h*mWidth + w] == 1) ? 'x' : '-');
        }
        std::cout << char(179) << "\n";
    }
    std::cout << " " << char(192);
    for(int w = 1; w < (mWidth - 1); ++w)
        std::cout << char(196);
    std::cout << char(217) << "\n" << std::endl;
}
