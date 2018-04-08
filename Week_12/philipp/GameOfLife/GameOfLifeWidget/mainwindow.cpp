#include "mainwindow.h"
#include "ui_mainwindow.h"

#include <QDebug>
#include <QLabel>
#include <QTimer>
#include <QMessageBox>
#include <QCloseEvent>

#include "sysinfo.h"

static std::vector<std::pair<QString, int>> ramDataPair =
{
    { "B", 1 },
    { "kB", 1024 },
    { "MB", 1024*1024 },
};

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
    , mCpuTimer(nullptr)
    , mCpuWidget(new QLabel("", this))
    , mRamWidget(new QLabel("", this))
    , mCurrentActionIndex(1)
{
    ui->setupUi(this);
    initActions();

    if(sysinfo_init()) {
        statusBar()->addPermanentWidget(mCpuWidget);
        statusBar()->addPermanentWidget(mRamWidget);
        mCpuTimer = new QTimer(this);
        mCpuTimer->setInterval(500);
        connect(mCpuTimer, &QTimer::timeout,
                this, [=](){ mCpuWidget->setText(QString(" CPU: %0% ")
                                                 .arg(qRound(getCurrentProcessCpuValue())));});
        connect(mCpuTimer, &QTimer::timeout,
                this, [=](){ mRamWidget->setText(QString(" RAM: %0 %1 ")
                                                 .arg(QLocale("de").toString(getCurrentProcessRamValue() / ramDataPair[mCurrentActionIndex].second))
                                                 .arg(ramDataPair[mCurrentActionIndex].first));});
        mCpuTimer->start();
        statusBar()->show();
    } else {
        statusBar()->hide();
    }
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::on_createGameButton_clicked()
{
    ui->gameWidget->createNewGame(ui->heightSpin->value(),
                                  ui->widthSpin->value(),
                                  ui->gameTypeCombo->currentIndex());
}

void MainWindow::onRamActionGroup_triggered(QAction *action)
{
    mCurrentActionIndex = action->data().toInt();
}

void MainWindow::closeEvent(QCloseEvent *event)
{
    if(QMessageBox::No == QMessageBox::question(this,
                                                tr("Game of Life Widget"),
                                                tr("Do you want to close the program?"))) {
        event->ignore();
    }
}

void MainWindow::initActions()
{
    ramActionGroup = new QActionGroup(this);
    ui->ramByteAction->setData(0);
    ramActionGroup->addAction(ui->ramByteAction);
    ui->ramKiloByteAction->setData(1);
    ramActionGroup->addAction(ui->ramKiloByteAction);
    ui->ramMegaByteAction->setData(2);
    ramActionGroup->addAction(ui->ramMegaByteAction);

    connect(ramActionGroup, SIGNAL(triggered(QAction*)),
            this, SLOT(onRamActionGroup_triggered(QAction*)));
}
