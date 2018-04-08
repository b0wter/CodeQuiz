#include "gamewidget.h"
#include "ui_gamewidget.h"

#include <QDebug>
#include <QTimer>
#include <QBitmap>
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
    mAutoEvolutionTimer->setInterval(1000 * ui->evolutionIntervalSpin->value());
    connect(mAutoEvolutionTimer, SIGNAL(timeout()),
            this, SLOT(on_nextEvolutionButton_clicked()));
    connect(ui->gameMap, SIGNAL(coordinateClicked(QPointF)),
            this, SLOT(on_coordinateClicked(QPointF)));
}

GameWidget::~GameWidget()
{
    delete ui;
    if(mGame)
        delete mGame;
}

void GameWidget::createNewGame(int height, int width, int gameType)
{
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
        setCurrentEvolution(mGame->getEvolution());
        setImageData();

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

void GameWidget::setImageData()
{
    QImage img = QImage(mGame->getData(),
                        mGame->getWidth(),
                        mGame->getHeight(),
                        mGame->getWidth(),
                        QImage::Format_Indexed8);
    img.setColorTable({qRgb(255, 255, 255), qRgb(0, 0, 0)});
    ui->gameMap->setPixmap(QPixmap::fromImage(img).scaled(qMax(ui->gameMap->width(), mGame->getWidth()),
                                                          qMax(ui->gameMap->height(), mGame->getHeight()),
                                                          Qt::KeepAspectRatio,
                                                          Qt::FastTransformation));
    ui->gameMap->show();
}

void GameWidget::on_nextEvolutionButton_clicked()
{
    setCurrentEvolution(mGame->evolve());
    setImageData();
}

void GameWidget::on_autoEvolveButton_clicked()
{
    bool enabled = mAutoEvolutionTimer->isActive();
    if(enabled) {
        ui->autoEvolveButton->setText("Run");
        mAutoEvolutionTimer->stop();
    } else {
        ui->autoEvolveButton->setText("Stop");
        mAutoEvolutionTimer->start();
    }
    ui->nextEvolutionButton->setEnabled(enabled);
    ui->clearButton->setEnabled(enabled);
    ui->fillSpin->setEnabled(enabled);
    ui->randomFillButton->setEnabled(enabled);
}

void GameWidget::on_clearButton_clicked()
{
    mGame->clear();
    setImageData();
}

void GameWidget::on_randomFillButton_clicked()
{
    mGame->randomFill(ui->fillSpin->value());
    setImageData();
}

void GameWidget::on_evolutionIntervalSpin_valueChanged(double d)
{
    mAutoEvolutionTimer->setInterval(1000 * d);
}

void GameWidget::on_coordinateClicked(QPointF point)
{
    float x = point.x() / ui->gameMap->pixmap()->width() * mGame->getWidth();
    float y = point.y() / ui->gameMap->pixmap()->height() * mGame->getHeight();

    mGame->swap(int(y), int(x));
    setImageData();
}
