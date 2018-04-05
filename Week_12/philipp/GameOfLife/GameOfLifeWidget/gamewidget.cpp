#include "gamewidget.h"
#include "ui_gamewidget.h"

#include <QDebug>
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
    ui->currentEvolutionLabel->setText(QString());
}

void GameWidget::on_nextEvolutionButton_clicked()
{
    int newEvolution = mGame->evolve();

    setCurrentEvolution(newEvolution);
}

void GameWidget::on_autoEvolveButton_clicked()
{
    qDebug() << "Run clicked";
}

void GameWidget::on_clearButton_clicked()
{
    qDebug() << "Clear clicked";
}

void GameWidget::on_randomFillButton_clicked()
{
    qDebug() << "RandomFill clicked -> Fill: " << ui->fillSpin->value();
}
