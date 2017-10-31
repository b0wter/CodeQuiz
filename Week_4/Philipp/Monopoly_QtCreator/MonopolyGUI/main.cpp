#include <QApplication>

#include "monopolywidget.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    MonopolyWidget m;
    m.show();

    return a.exec();
}
