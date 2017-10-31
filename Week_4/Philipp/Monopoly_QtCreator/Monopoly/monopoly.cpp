#include "monopoly.h"

#include <algorithm>
#include <chrono>
#include <random>

Monopoly::Monopoly(QObject *parent)
    : QObject(parent)
    , mJailAtThreeDoubles(true)
    , mCurrentField(Los)
    , mChance(new ChanceDeck())
    , mCommunityChest(new CommunityChestDeck())
    , mStats({0})
{
    mMetaEnumField = QMetaEnum::fromType<Fields>();

    roll = &Monopoly::rollNormal;
    mDices[0] = new Dice<std::mt19937>();
    mDices[1] = new Dice<std::mt19937>();

    setRules();
    reset();
}

Monopoly::~Monopoly()
{
    delete mChance;
    delete mCommunityChest;
    delete mDices[0];
    delete mDices[1];
}

QString Monopoly::fieldNameToString(Monopoly::Fields field)
{
    static QMetaEnum sMetaEnum = QMetaEnum::fromType<Monopoly::Fields>();
    return sMetaEnum.valueToKey(field);
}

QString Monopoly::fieldNameToShortString(Monopoly::Fields field)
{
    static QMetaEnum sMetaEnum = QMetaEnum::fromType<Monopoly::Fields>();
    return sMetaEnum.valueToKeys(field).split('|')[1];
}

bool Monopoly::setRules(const bool jailAtThreeDoubles,
                        const std::vector<int> dices)
{
    mJailAtThreeDoubles = jailAtThreeDoubles;
    if(mJailAtThreeDoubles) {
        roll = &Monopoly::rollDoublePenalty;
    } else {
        roll = &Monopoly::rollNormal;
    }
    delete mDices[0];
    delete mDices[1];
    mDices[0] = new Dice<std::mt19937>(dices);
    mDices[1] = new Dice<std::mt19937>(dices);

    return true;
}

QString Monopoly::fieldToString(const Fields field)
{
    return mMetaEnumField.valueToKey(field);
}

void Monopoly::reset()
{
    mCurrentField = Los;
    mCommunityChest->reset();
    mChance->reset();
    mStats.doubles = 0;
    mStats.rolls = 0;
    std::fill(mStats.pipRolls, mStats.pipRolls+11, 0);
    std::fill(mStats.visits, mStats.visits+cMaxFields, 0);
}

void Monopoly::play(quint64 maxRolls)
{
    for(quint64 i = 0; i < maxRolls; i++) {
        int pips = (this->*roll)();
        if(pips < 0) {
            mCurrentField = Gefaengnis;
            ++mStats.doubles;
        } else {
            mCurrentField = (Fields)((mCurrentField + pips) % cMaxFields);
        }
        if(mCurrentField == Gemeinschaftsfeld1
                || mCurrentField == Gemeinschaftsfeld2
                || mCurrentField == Gemeinschaftsfeld3) {
            mCurrentField = mCommunityChest->drawCard()->moveTo(mCurrentField);
        } else if(mCurrentField == Ereignisfeld1
                  || mCurrentField == Ereignisfeld2
                  || mCurrentField == Ereignisfeld3) {
            mCurrentField = mChance->drawCard()->moveTo(mCurrentField);
        } else if(mCurrentField == InsGefaengnis) {
            mCurrentField = Gefaengnis;
        }
        ++mStats.visits[mCurrentField];
        ++mStats.pipRolls[pips - 2];
    }
    mStats.rolls += maxRolls;
}

Monopoly::Stats Monopoly::getStats()
{
    return mStats;
}

int Monopoly::rollNormal()
{
    return (mDices[0]->roll() + mDices[1]->roll());
}

int Monopoly::rollDoublePenalty()
{
    static int doubles = 0;
    int pips1 = mDices[0]->roll();
    int pips2 = mDices[1]->roll();
    if(pips1 == pips2) {
        ++doubles;
        if(doubles == 3) {
            doubles = 0;
            return -1;
        }
    } else {
        doubles = 0;
    }
    return (pips1 + pips2);
}

Card::Card(Monopoly::Fields field)
    : mStay(field == Monopoly::UNDEFINED ? true : false)
    , mField(field)
{

}

Monopoly::Fields Card::moveTo(Monopoly::Fields field) const
{
    if(mStay) {
        return field;
    } else {
        if(mField == Monopoly::NaechsterBahnhof) {
            if(field == Monopoly::Ereignisfeld1)
                return Monopoly::Westbahnhof;
            else if(field == Monopoly::Ereignisfeld2)
                return Monopoly::Nordbahnhof;
            else
                return Monopoly::Suedbahnhof;
        } else if(mField == Monopoly::NaechstesWerk) {
            if(field == Monopoly::Ereignisfeld1
                    || field == Monopoly::Ereignisfeld3)
                return Monopoly::Elektrizitaetswerk;
            else
                return Monopoly::Wasserwerk;
        } else if(mField == Monopoly::DreiZurueck) {
            if(field == Monopoly::Ereignisfeld1)
                return Monopoly::Einkommensteuer;
            else if(field == Monopoly::Ereignisfeld2)
                return Monopoly::BerlinerStrasse;
            else
                return Monopoly::Gemeinschaftsfeld3;
        } else {
            return mField;
        }
    }
}

Deck::Deck()
    : mCurrentCard(0)
{

}

void Deck::reset()
{
    mCurrentCard = 0;
    shuffle();
}

const Card* Deck::drawCard()
{
    if(mCurrentCard == mCards.size()) {
        mCurrentCard = 0;
        shuffle();
    }
    return mCards[mCurrentCard++];
}

void Deck::shuffle()
{
    unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
    std::shuffle(mCards.begin(),
                 mCards.end(),
                 std::default_random_engine(seed));
}

ChanceDeck::ChanceDeck()
{
    mCards.push_back(new Card(Monopoly::Los));
    mCards.push_back(new Card(Monopoly::Gefaengnis));
    mCards.push_back(new Card(Monopoly::Seestrasse));
    mCards.push_back(new Card(Monopoly::Opernplatz));
    mCards.push_back(new Card(Monopoly::Schlossallee));
    mCards.push_back(new Card(Monopoly::Suedbahnhof));
    mCards.push_back(new Card(Monopoly::NaechsterBahnhof));
    mCards.push_back(new Card(Monopoly::NaechsterBahnhof));
    mCards.push_back(new Card(Monopoly::NaechstesWerk));
    mCards.push_back(new Card(Monopoly::DreiZurueck));
    for(int i = 0; i < 6; i++)
        mCards.push_back(new Card());
}

CommunityChestDeck::CommunityChestDeck()
{
    mCards.push_back(new Card(Monopoly::Los));
    mCards.push_back(new Card(Monopoly::Gefaengnis));
    for(int i = 0; i < 14; i++)
        mCards.push_back(new Card());
}
