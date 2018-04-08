#ifndef GAMEMAP_H
#define GAMEMAP_H

#include <QLabel>

class GameMap : public QLabel
{
    Q_OBJECT
public:
    GameMap(QWidget *parent = nullptr);

protected:
    virtual void mousePressEvent(QMouseEvent *event);

signals:
    void coordinateClicked(QPointF);
};

#endif // GAMEMAP_H
