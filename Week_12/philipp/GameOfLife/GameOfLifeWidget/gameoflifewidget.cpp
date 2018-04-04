#include "gameoflifewidget.h"
#include "ui_gameoflifewidget.h"

GameOfLifeWidget::GameOfLifeWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::GameOfLifeWidget)
{
    ui->setupUi(this);
}

GameOfLifeWidget::~GameOfLifeWidget()
{
    delete ui;
}
