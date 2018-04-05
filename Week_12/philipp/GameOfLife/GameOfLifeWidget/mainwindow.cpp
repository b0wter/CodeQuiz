#include "mainwindow.h"
#include "ui_mainwindow.h"

#include <QDebug>
#include <QMessageBox>
#include <QCloseEvent>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::on_createGameButton_clicked()
{
    qDebug() << "create Game";

    int height      = ui->heightSpin->value();
    int width       = ui->widthSpin->value();
    int gameType    = ui->gameTypeCombo->currentIndex();

    ui->gameWidget->createNewGame(height, width, gameType);
}

void MainWindow::closeEvent(QCloseEvent *event)
{
    QMessageBox::StandardButton btn
            = QMessageBox::question(this,
                                    tr("Game of Life Widget"),
                                    tr("Do you want to close the program?"));
    if(btn == QMessageBox::No) {
        event->ignore();
    }
}
