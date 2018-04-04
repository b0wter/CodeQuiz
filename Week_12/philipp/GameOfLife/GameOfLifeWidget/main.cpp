#include "gameoflifewidget.h"
#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    GameOfLifeWidget w;
    w.show();

    return a.exec();
}
