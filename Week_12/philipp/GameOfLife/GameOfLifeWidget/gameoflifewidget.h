#ifndef GAMEOFLIFEWIDGET_H
#define GAMEOFLIFEWIDGET_H

#include <QWidget>

namespace Ui {
class GameOfLifeWidget;
}

class GameOfLifeWidget : public QWidget
{
    Q_OBJECT

public:
    explicit GameOfLifeWidget(QWidget *parent = 0);
    ~GameOfLifeWidget();

private:
    Ui::GameOfLifeWidget *ui;
};

#endif // GAMEOFLIFEWIDGET_H
