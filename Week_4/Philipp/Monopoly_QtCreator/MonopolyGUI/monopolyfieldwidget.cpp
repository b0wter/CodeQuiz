#include "monopolyfieldwidget.h"

#include <QPainter>
#include <QStyleOption>

#include <cmath>

MonopolyFieldWidget::MonopolyFieldWidget(QWidget *parent)
    : QWidget(parent)
    , mStats({0})
    , mMinProb(0.0)
    , mMaxProb(0.0)
    , mProbs(std::vector<double>(40, 0.0))
    , mColors(std::vector<QColor>(40, QColor(0, 0, 0, 0)))
{
    setStyleSheet("background: #E0E0E0;");

    resize(704, 704);

    // precalculate Colors
    mColorValues[255] = QColor(0, 0, 0, 0);
    for(int i = 0; i < 255; i++) {
        mColorValues[i] = QColor(i, 0, 255 - i, 255);
    }
}

void MonopolyFieldWidget::setNewStats(Monopoly::Stats stats)
{
    mStats = stats;
    for(int i = 0; i < 40; i++) {
        mProbs[i] = mStats.visits[i];
    }
    std::sort(mProbs.begin(), mProbs.end());

    mMaxProb = mProbs[39];
    mMinProb = mProbs[1];

    for(int i = 0; i < 40; i++) {
        mColors[i] = getColor(mStats.visits[i]);
    }
    update();
}

void MonopolyFieldWidget::reset()
{
    Monopoly::Stats stats = {0};
    setNewStats(stats);
}

QColor &MonopolyFieldWidget::getColor(quint64 &val)
{
    if(val < mMinProb || val == 0)
        return mColorValues[255];
    double value = (val - mMinProb) / (mMaxProb - mMinProb);
    int idx = (int)(value*254);

    if(value > 254)
        return mColorValues[254];
    else if(value < 0)
        return mColorValues[0];
    else
        return mColorValues[idx];
}

void MonopolyFieldWidget::paintEvent(QPaintEvent *event)
{
    Q_UNUSED(event)
    QStyleOption opt;
    opt.init(this);
    QPainter p(this);
    style()->drawPrimitive(QStyle::PE_Widget, &opt, &p, this);

    // check size and calculate offsets
    QSize s = size();
    if(s.width() > 704) {
        p.translate((s.width() - 704) / 2, 0);
    }
    if(s.width() > 704) {
        p.translate(0, (s.height() - 704) / 2);
    }

    p.setPen(QPen(Qt::black));
    // first 10  fields
    int i = 0;
    int y = 0;
    for(int x = 0; x < 640; x+=64, i++) {
        p.setBrush(QBrush(mColors[i]));
        p.drawRect(x, y, 64, 64);
        p.drawText(x, y, 64, 64, Qt::AlignCenter, QString("%1\n%2").arg(QString::number(i)).arg(Monopoly::fieldNameToShortString((Monopoly::Fields)i)));
    }

    // second 10 fields
    int x = 640;
    for(int y = 0; y < 640; y+=64, i++) {
        p.setBrush(QBrush(mColors[i]));
        p.drawRect(x, y, 64, 64);
        p.drawText(x, y, 64, 64, Qt::AlignCenter, QString("%1\n%2").arg(QString::number(i)).arg(Monopoly::fieldNameToShortString((Monopoly::Fields)i)));
    }

    // thrid 10  fields
    y = 640;
    for(int x = 640; x > 0; x -= 64, i++) {
        p.setBrush(QBrush(mColors[i]));
        p.drawRect(x, y, 64, 64);
        p.drawText(x, y, 64, 64, Qt::AlignCenter, QString("%1\n%2").arg(QString::number(i)).arg(Monopoly::fieldNameToShortString((Monopoly::Fields)i)));
    }

    // fourth 10 fields
    x = 0;
    for(int y = 640; y > 0; y -= 64, i++) {
        p.setBrush(QBrush(mColors[i]));
        p.drawRect(x, y, 64, 64);
        p.drawText(x, y, 64, 64, Qt::AlignCenter, QString("%1\n%2").arg(QString::number(i)).arg(Monopoly::fieldNameToShortString((Monopoly::Fields)i)));
    }
}
