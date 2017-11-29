#ifndef MONOPOLYSTATMODEL_H
#define MONOPOLYSTATMODEL_H

#include <QAbstractTableModel>

#include "../Monopoly/monopoly.h"

class MonopolyStatModel : public QAbstractTableModel
{
    Q_OBJECT

public:
    MonopolyStatModel(QObject *parent = nullptr);

    int rowCount(const QModelIndex &parent) const;
    int columnCount(const QModelIndex &parent) const;
    QVariant data(const QModelIndex &index, int role) const;
    QVariant headerData(int section, Qt::Orientation orientation, int role) const;
    void updateData(Monopoly::Stats stats);

private:
    Monopoly::Stats mStats;
    Monopoly monopoly;

};

#endif // MONOPOLYSTATMODEL_H

