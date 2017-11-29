#ifndef MONOPOLYWIDGET_H
#define MONOPOLYWIDGET_H

#include <QWidget>
#include <QThread>
#include <QTableView>
#include <QSortFilterProxyModel>
class QPushButton;
class QComboBox;
class QSpinBox;
class QProgressBar;
class QCheckBox;


#include "../Monopoly/monopoly.h"
#include "monopolystatmodel.h"
class MonopolyFieldWidget;

class MonopolyWidget : public QWidget
{
    Q_OBJECT

public:
    MonopolyWidget(QWidget *parent = nullptr);
    ~MonopolyWidget();

public slots:
    void onPlay();
    void onReset();
    void onFinished();

    void run();

    void onNewStats(Monopoly::Stats stats);

signals:
    void iterationFinished(int);
    void newStats(Monopoly::Stats);
    void finished();

private:
    bool mPlaying;
    Monopoly *mMonopoly;
    Monopoly::Stats mStats;
    MonopolyStatModel mStatmodel;
    QSortFilterProxyModel mProxyModel;
    QTableView *mStatTable;
    QComboBox *mDiceCombo;
    QCheckBox *mDoublePenaltyCheck;
    MonopolyFieldWidget *mFieldWidget;

    QPushButton *mPlayBtn;
    QPushButton *mResetBtn;
    QSpinBox *mIterationSpin;
    QSpinBox *mRollSpin;
    QProgressBar *mProgressBar;

    std::vector<int> mDiceRule[2];
};

Q_DECLARE_METATYPE(Monopoly::Stats)

#endif // MONOPOLYWIDGET_H

