#ifndef MONOPOLYFIELDWIDGET_H
#define MONOPOLYFIELDWIDGET_H

#include <QWidget>

#include <vector>

#include "../Monopoly/monopoly.h"

class MonopolyFieldWidget : public QWidget
{
    Q_OBJECT

public:
    MonopolyFieldWidget(QWidget *parent = nullptr);

    void reset();

    QColor& getColor(quint64 &val);

public slots:
    void setNewStats(Monopoly::Stats stats);

protected:
    virtual void paintEvent(QPaintEvent *event);

private:
    Monopoly::Stats mStats;

    double mMinProb;
    double mMaxProb;
    std::vector<double> mProbs;

    QColor mColorValues[256];
    std::vector<QColor> mColors;
};

#endif // MONOPOLYFIELDWIDGET_H

