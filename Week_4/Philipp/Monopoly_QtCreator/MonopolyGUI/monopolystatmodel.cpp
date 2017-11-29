#include "monopolystatmodel.h"

MonopolyStatModel::MonopolyStatModel(QObject *parent)
    : QAbstractTableModel(parent)
    , mStats({0})
{

}

int MonopolyStatModel::rowCount(const QModelIndex &parent) const
{
    Q_UNUSED(parent);
    return 40;
}

int MonopolyStatModel::columnCount(const QModelIndex &parent) const
{
    Q_UNUSED(parent);
    return 3;
}

QVariant MonopolyStatModel::data(const QModelIndex &index, int role) const
{
    if(!index.isValid())
        return QVariant();

    if(index.row() >= 40 || index.row() < 0)
        return QVariant();

    if(role == Qt::DisplayRole) {
        if(index.column() == 0)
            //return index.row();
            return Monopoly::fieldNameToString((Monopoly::Fields)index.row());
            // return monopoly.fieldToString((Monopoly::Fields)index.row());
        else if(index.column() == 1)
            return mStats.visits[index.row()];
        else if(index.column() == 2)
            return QString::number(double(mStats.visits[index.row()]) / double(mStats.rolls), 'f', 5);
    }
    if(role == Qt::TextAlignmentRole) {
        if(index.column() == 0)
            return Qt::AlignLeft + Qt::AlignVCenter;
        else
            return Qt::AlignRight + Qt::AlignVCenter;
    }
    return QVariant();
}

QVariant MonopolyStatModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if(role != Qt::DisplayRole)
        return QVariant();

    if(orientation == Qt::Horizontal) {
        switch(section) {
            case 0:
                return QString("Field");
            case 1:
                return QString("Visits");
            case 2:
                return QString("Visit Probability");
            default:
                return QVariant();
        }
    }
    return QVariant();
}

void MonopolyStatModel::updateData(Monopoly::Stats stats)
{
    mStats = stats;
    emit dataChanged(this->index(0,0), this->index(39, 3));
}
