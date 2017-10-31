#include "monopolywidget.h"

#include <QPushButton>
#include <QSpinBox>
#include <QProgressBar>
#include <QLayout>
#include <QLabel>
#include <QComboBox>
#include <QCheckBox>
#include <QDebug>
#include <QHeaderView>
#include <QtConcurrent>

#include "monopolyfieldwidget.h"

const QString tableViewStyle =
        "QTableView {"
        "   background: #EEEEEE;"
        "   alternate-background-color: #CCCCCC;"
        "}";

MonopolyWidget::MonopolyWidget(QWidget *parent)
    : QWidget(parent)
    , mPlaying(false)
    , mMonopoly(new Monopoly())
{
    // Data
    mDiceRule[0] = std::vector<int>({1, 2, 3, 4});
    mDiceRule[1] = std::vector<int>({1, 2, 3, 4, 5, 6});

    // layout
    QHBoxLayout *hLayout = new QHBoxLayout();

    mProgressBar = new QProgressBar();
    mDoublePenaltyCheck = new QCheckBox("Doubles Penalty");
    mDoublePenaltyCheck->setChecked(true);
    mDiceCombo = new QComboBox();
    mDiceCombo->addItems(QStringList({"4 Sided Dice", "6 Sided Dice" }));
    mDiceCombo->setCurrentIndex(1);
    mIterationSpin = new QSpinBox();
    mIterationSpin->setRange(1, 1000);
    mIterationSpin->setValue(20);
    mRollSpin = new QSpinBox();
    mRollSpin->setRange(100, 50000000);
    mRollSpin->setSingleStep(100);
    mRollSpin->setValue(100000);
    mPlayBtn = new QPushButton("Play");
    connect(mPlayBtn, SIGNAL(clicked()), this, SLOT(onPlay()));
    mResetBtn = new QPushButton("Reset");
    connect(mResetBtn, SIGNAL(clicked()), this, SLOT(onReset()));

    hLayout->addWidget(mProgressBar);
    hLayout->addWidget(mDoublePenaltyCheck);
    hLayout->addWidget(new QLabel("Dice Type:"));
    hLayout->addWidget(mDiceCombo);
    hLayout->addWidget(new QLabel("Iterations:"));
    hLayout->addWidget(mIterationSpin);
    hLayout->addWidget(new QLabel("Rolls per Iteration:"));
    hLayout->addWidget(mRollSpin);
    hLayout->addWidget(mPlayBtn);
    hLayout->addWidget(mResetBtn);

    QHBoxLayout *hLayout2 = new QHBoxLayout();
    mProxyModel.setSourceModel(&mStatmodel);
    mStatTable = new QTableView();
    mStatTable->setModel(&mProxyModel);
    mStatTable->setSortingEnabled(true);
    mStatTable->sortByColumn(1, Qt::DescendingOrder);
    mStatTable->setAlternatingRowColors(true);
    mStatTable->setStyleSheet(tableViewStyle);
    mStatTable->verticalHeader()->hide();
    mStatTable->verticalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);
    mStatTable->horizontalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);
    mStatTable->setMinimumWidth(270);
    mStatTable->setSizePolicy(QSizePolicy(QSizePolicy::Minimum, QSizePolicy::Expanding));
    mFieldWidget = new MonopolyFieldWidget();
    mFieldWidget->setSizePolicy(QSizePolicy(QSizePolicy::Expanding,QSizePolicy::Expanding));
    hLayout2->addWidget(mStatTable);
    hLayout2->addWidget(mFieldWidget);
    //hLayout2->addStretch(1);

    QVBoxLayout *vLayout = new QVBoxLayout();
    vLayout->addLayout(hLayout);
    vLayout->addLayout(hLayout2);
    //vLayout->addStretch(1);
    setLayout(vLayout);
    resize(1024, 768);

    //
    qRegisterMetaType<Monopoly::Stats>();
    connect(this, SIGNAL(newStats(Monopoly::Stats)),
            this, SLOT(onNewStats(Monopoly::Stats)),
            Qt::QueuedConnection);
    connect(this, SIGNAL(newStats(Monopoly::Stats)),
            mFieldWidget, SLOT(setNewStats(Monopoly::Stats)),
            Qt::QueuedConnection);
    connect(this, SIGNAL(finished()),
            this, SLOT(onFinished()),
            Qt::QueuedConnection);
}

MonopolyWidget::~MonopolyWidget()
{
    delete mMonopoly;
}

void MonopolyWidget::onPlay()
{
    mMonopoly->setRules(mDoublePenaltyCheck->isChecked(),
                        mDiceRule[mDiceCombo->currentIndex()]);
    if(mPlaying) {
        // stop monopoly
        mPlaying = false;
        mPlayBtn->setText("Play");
    } else {
        // start
        mPlaying = true;
        mProgressBar->setMinimum(0);
        mProgressBar->setMaximum(mIterationSpin->value());
        connect(this, SIGNAL(iterationFinished(int)),
                mProgressBar, SLOT(setValue(int)),
                Qt::QueuedConnection);

        QtConcurrent::run(this, &MonopolyWidget::run);

        mPlayBtn->setText("Stop");
    }
}

void MonopolyWidget::onReset()
{
    mPlaying = false;
    mMonopoly->reset();
    mFieldWidget->reset();
    mStats = mMonopoly->getStats();
    mStatmodel.updateData(mStats);
    mProxyModel.invalidate();
    mPlayBtn->setText("Play");
}

void MonopolyWidget::onFinished()
{
    mPlaying = false;
    mProgressBar->setValue(0);
    mPlayBtn->setText("Play");
}

void MonopolyWidget::run()
{
    int rolls = mRollSpin->value();
    int iterations = mIterationSpin->value();

    for(int i = 0; i < iterations; i++) {
        if(!mPlaying)
            break;
        mMonopoly->play(rolls);
        emit iterationFinished(i+1);
        emit newStats(mMonopoly->getStats());
    }
    emit finished();
}

void MonopolyWidget::onNewStats(Monopoly::Stats stats)
{
    mStatmodel.updateData(stats);
    mProxyModel.invalidate();
    //mFieldWidget->setNewStats(stats);
    mStats = stats;
    mStatTable->verticalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);
    mStatTable->horizontalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);
}
