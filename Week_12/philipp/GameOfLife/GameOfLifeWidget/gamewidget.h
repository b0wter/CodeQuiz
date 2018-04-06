#ifndef GAMEWIDGET_H
#define GAMEWIDGET_H

#include <QWidget>

class QTimer;

class GameOfLifeBase;

namespace Ui {
class GameWidget;
}

class GameWidget : public QWidget
{
    Q_OBJECT

public:
    explicit GameWidget(QWidget *parent = 0);
    ~GameWidget();

    void createNewGame(int height, int width, int gameType);

private:
    void setCurrentEvolution(int evolution);

public slots:
    void on_nextEvolutionButton_clicked();
    void on_autoEvolveButton_clicked();
    void on_clearButton_clicked();
    void on_randomFillButton_clicked();

private:
    Ui::GameWidget *ui;

    GameOfLifeBase *mGame;
    int mCurrentEvolution;
    QTimer *mAutoEvolutionTimer;
};

#endif // GAMEWIDGET_H
