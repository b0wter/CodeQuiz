#include "gamewidget.h"
#include "ui_gamewidget.h"

#include <QDebug>
#include <QTimer>
#include <QMessageBox>

// Game Of Life
#include "simplegameoflife.h"


GameWidget::GameWidget(QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::GameWidget)
    , mGame(nullptr)
    , mCurrentEvolution(0)
{
    ui->setupUi(this);

    mAutoEvolutionTimer = new QTimer();
    mAutoEvolutionTimer->setInterval(1000);
    connect(mAutoEvolutionTimer, SIGNAL(timeout()), this, SLOT(on_nextEvolutionButton_clicked()));
}

GameWidget::~GameWidget()
{
    delete ui;
    if(mGame)
        delete mGame;
}

void GameWidget::createNewGame(int height, int width, int gameType)
{
    qDebug() << "createNewGame : (" << height << " x " << width
             << ") T: " << gameType;

    if(mGame) {
        if(QMessageBox::No == QMessageBox::question(this,
                                                    tr("Game of Life"),
                                                    tr("Thre is already a game. Should it be replaced?"))) {
            return;
        } else {
            delete mGame;
        }
    }

    if(gameType == 0) {
        mGame = new SimpleGameOfLife(height, width);

        this->setEnabled(true);
    } else {
        qDebug() << "Invalid Index";
        return;
    }
}

void GameWidget::setCurrentEvolution(int evolution)
{
    mCurrentEvolution = evolution;
    ui->currentEvolutionLabel->setText(QString("Evolution: %1").arg(mCurrentEvolution));
}

void GameWidget::on_nextEvolutionButton_clicked()
{
    qDebug() << "Next evolution";
    int newEvolution = mGame->evolve();

    setCurrentEvolution(newEvolution);
}

void GameWidget::on_autoEvolveButton_clicked()
{
    if(mAutoEvolutionTimer->isActive()) {
        qDebug() << "Stop clicked";
        ui->autoEvolveButton->setText("Run");
        ui->nextEvolutionButton->setEnabled(true);
        ui->clearButton->setEnabled(true);
        ui->fillSpin->setEnabled(true);
        ui->randomFillButton->setEnabled(true);
        mAutoEvolutionTimer->stop();
    } else {
        qDebug() << "Run clicked";
        ui->autoEvolveButton->setText("Stop");
        ui->nextEvolutionButton->setEnabled(false);
        ui->clearButton->setEnabled(false);
        ui->fillSpin->setEnabled(false);
        ui->randomFillButton->setEnabled(false);
        mAutoEvolutionTimer->start();
    }
}

void GameWidget::on_clearButton_clicked()
{
    qDebug() << "Clear clicked";
    mGame->clear();
}

void GameWidget::on_randomFillButton_clicked()
{
    qDebug() << "RandomFill clicked -> Fill: " << ui->fillSpin->value();
    mGame->randomFill(ui->fillSpin->value());
}
