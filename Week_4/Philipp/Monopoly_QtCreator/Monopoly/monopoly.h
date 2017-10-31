#ifndef MONOPOLY_H
#define MONOPOLY_H

#include <QObject>
#include <QMetaEnum>

#include <vector>
#include <random>
#include <chrono>

class CommunityChestDeck;
class ChanceDeck;
template<class T>
class Dice;

class Monopoly : QObject
{
    Q_OBJECT

public:
    const int cMaxFields = 40;

    enum Fields {
        Los = 0,
        GO  = 0,
        Badstrasse = 1,
        A1 = 1,
        Gemeinschaftsfeld1 = 2,
        CC1 = 2,
        Turmstrasse = 3,
        A2 = 3,
        Einkommensteuer = 4,
        T1 = 4,
        Suedbahnhof = 5,
        R1 = 5,
        Chaussestrasse = 6,
        B1 = 6,
        Ereignisfeld1 = 7,
        CH1 = 7,
        Elisenstrasse = 8,
        B2 = 8,
        Poststrasse = 9,
        B3 = 9,
        Gefaengnis = 10,
        JAIL = 10,
        Seestrasse = 11,
        C1 = 11,
        Elektrizitaetswerk = 12,
        U1 = 12,
        Hafenstrasse = 13,
        C2 = 13,
        NeueStrasse = 14,
        C3 = 14,
        Westbahnhof = 15,
        R2 = 15,
        MuenchenerStrasse = 16,
        D1 = 16,
        Gemeinschaftsfeld2 = 17,
        CC2 = 17,
        WienerStrasse = 18,
        D2 = 18,
        BerlinerStrasse = 19,
        D3 = 19,
        FreiParken = 20,
        FP = 20,
        Theaterstrasse = 21,
        E1 = 21,
        Ereignisfeld2 = 22,
        CH2 = 22,
        Museumstrasse = 23,
        E2 = 23,
        Opernplatz = 24,
        E3 = 24,
        Nordbahnhof = 25,
        R3 = 25,
        Lessingstrasse = 26,
        F1 = 26,
        Schillerstrasse = 27,
        F2 = 27,
        Wasserwerk = 28,
        U2 = 28,
        Goethestrasse = 29,
        F3 = 29,
        InsGefaengnis = 30,
        G2J = 30,
        Rathausplatz = 31,
        G1 = 31,
        Hauptstrasse = 32,
        G2 = 32,
        Gemeinschaftsfeld3 = 33,
        CC3 = 33,
        Bahnhofstrasse = 34,
        G3 = 34,
        Hauptbahnhof = 35,
        R4 = 35,
        Ereignisfeld3 = 36,
        CH3 = 36,
        Parkstrasse = 37,
        H1 = 37,
        Zusatzsteuer = 38,
        T2 = 38,
        Schlossallee = 39,
        H2 = 39,

        NaechsterBahnhof = 50,
        R = 50,
        NaechstesWerk = 51,
        U = 51,
        DreiZurueck = 52,
        Back3 = 52,
        //
        UNDEFINED = 100,
    };
    Q_ENUM(Fields)

    struct Stats {
        quint64 visits[40];
        quint64 doubles;
        quint64 pipRolls[11];
        quint64 rolls;
    };

public:
    Monopoly(QObject *parent = nullptr);
    ~Monopoly();

    static QString fieldNameToString(Fields field);
    static QString fieldNameToShortString(Fields field);

    bool setRules(const bool jailAtThreeDoubles = true,
                  const std::vector<int> dices = {1, 2, 3, 4, 5, 6} );
    QString fieldToString(const Fields field);

    void reset();
    void play(quint64 maxRolls = 10000000);

    Stats getStats();

private:
    int (Monopoly::*roll)(void);
    int rollNormal();
    int rollDoublePenalty();

private:
    QMetaEnum mMetaEnumField;

    // game elements
    ChanceDeck *mChance;
    CommunityChestDeck *mCommunityChest;
    Dice<std::mt19937>* mDices[2];
    Fields mCurrentField;
    Stats mStats;

    // rules
    bool mJailAtThreeDoubles;
};

class Card
{
public:
    Card(Monopoly::Fields field = Monopoly::UNDEFINED);
    Monopoly::Fields moveTo(Monopoly::Fields feld) const;

private:
    bool mStay;
    Monopoly::Fields mField;
};

class Deck
{
public:
    Deck();

    void reset();
    const Card* drawCard();

protected:
    void shuffle();

protected:
    int mCurrentCard;
    std::vector<Card*> mCards;
};

class ChanceDeck : public Deck
{
public:
    ChanceDeck();
};

class CommunityChestDeck : public Deck
{
public:
    CommunityChestDeck();
};

template<class T>
class Dice
{
public:
    Dice(std::vector<int> pips = {1, 2, 3, 4, 5, 6});

    int roll();

private:
    static int sDiceCount;
    T mGenerator;
    std::vector<int> mPips;
};

template<class T>
int Dice<T>::sDiceCount = 0;

template<class T>
Dice<T>::Dice(std::vector<int> pips = {1, 2, 3, 4, 5, 6})
    : mPips(pips)
{
    sDiceCount++;
    mGenerator = T(std::chrono::system_clock::now().time_since_epoch().count() + sDiceCount * 100);
}

template<class T>
int Dice<T>::roll()
{
    return mPips[mGenerator() % mPips.size()];
}

#endif // MONOPOLY_H

