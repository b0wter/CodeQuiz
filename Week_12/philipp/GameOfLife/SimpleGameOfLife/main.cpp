#include <iostream>
#include <vector>
#include <utility>

class Spielfeld
{
private:
    int buffer;
    int height;
    int width;
    int size;
    int evolution;
    bool *cell;

    Spielfeld();

public:
    Spielfeld(int height, int width)
        : height(height)
        , width(width)
        , size(height*width)
        , buffer(0)
        , evolution(0)
    {
        cell = new bool[2*height*width]();
    }
    ~Spielfeld() {
        delete[] cell;
    }
    void init(std::vector<std::pair<int, int>> initialValues) {
        for(std::pair<int, int> &p : initialValues) {
            set(p.first, p.second);
        }
    }
    inline bool get(int idxH, int idxW) {
        return cell[buffer*size + idxH*width + idxW];
    }
    inline void set(int idxH, int idxW) {
        cell[buffer*size + idxH*width + idxW] = true;
    }
    inline void unset(int idxH, int idxW) {
        cell[buffer*size + idxH*width + idxW] = false;
    }
    inline void setTo(int idxH, int idxW, bool val) {
        cell[buffer*size + idxH*width + idxW] = val;
    }
    void print() {
        std::cout << "Evolution: " << evolution << "\n  ";
        for(int w = 0; w < (width - 2); ++w)
            std::cout << w % 10;
        std::cout << "\n " << char(218);
        for(int w = 1; w < (width - 1); ++w)
            std::cout << char(196);
        std::cout << char(191) << "\n";
        for(int h = 1; h < (height - 1); ++h) {
            std::cout << (h - 1) % 10 << char(179);
            for(int w = 1; w < (width - 1); ++w) {
                std::cout << (cell[buffer*size + h*width + w] ? 'x' : '-');
            }
            std::cout << char(179) << "\n";
        }
        std::cout << " " << char(192);
        for(int w = 1; w < (width - 1); ++w)
            std::cout << char(196);
        std::cout << char(217) << "\n" << std::endl;
    }
    void evolve() {
        int newBuffer = buffer ^ 1;
        for(int h = 1; h < (height - 1); ++h) {
            for(int w = 1; w < (width - 1); ++w) {
                // check neighborhood
                int neighbors = 0;
                for(int y = (h - 1); y <= (h + 1); ++y) {
                    for(int x = (w - 1); x <= (w + 1); ++x) {
                        if(cell[buffer*size + y*width + x] && !(y == h && x == w))
                            ++neighbors;
                    }
                }
                if(cell[buffer*size + h*width + w]) {
                    if(neighbors == 2 || neighbors == 3) {
                        cell[newBuffer*size + h*width + w] = true;
                    } else {
                        cell[newBuffer*size + h*width + w] = false;
                    }
                } else {
                    cell[newBuffer*size + h*width + w] = neighbors == 3 ? true : false;
                }
            }
        }
        buffer = newBuffer;
        ++evolution;
    }
};

int main()
{
    Spielfeld feld(40, 40);
    feld.init({{10, 10}, {10, 11}, {10, 12}, {11, 12}, {12, 11} });

    for(int evolutions = 0; evolutions <= 150; ++evolutions) {
        feld.print();
        feld.evolve();
    }

    return 0;
}
