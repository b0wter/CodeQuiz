#include "gamemap.h"

#include <QDebug>
#include <QMouseEvent>

GameMap::GameMap(QWidget *parent)
    : QLabel(parent)
{

}

void GameMap::mousePressEvent(QMouseEvent *event)
{
    emit coordinateClicked(event->localPos());
}
