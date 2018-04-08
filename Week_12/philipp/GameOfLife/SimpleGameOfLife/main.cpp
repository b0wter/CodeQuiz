#include "simplegameoflife.h"

int main()
{
    SimpleGameOfLife feld(40, 40);

    feld.randomFill(50);
    //feld.init({{19, 19}, {18, 17}, {19, 16}, {18, 16}, {17, 19}, {18, 18}, {17, 17}, {10, 10}, {10, 11}, {10, 12}, {11, 12}, {12, 11} });

    for(int evolutions = 0; evolutions <= 20; ++evolutions) {
        feld.print();
        feld.evolve();
    }

    return 0;
}
