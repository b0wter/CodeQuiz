#ifndef FUNCTION_H
#define FUNCTION_H

#include <sstream>
#include <string>
#include <memory>

inline bool almostEqual(double a, double b)
{
    return fabs(a - b) <= DBL_EPSILON;
}


enum TermTypes {
    TT_Invalid,
    TT_Identifier,
    TT_Polynom,
};

class Term
{
public:
    Term() : type(TT_Invalid) { }

    virtual std::shared_ptr<Term> derive() = 0;
    virtual TermTypes getType() { return type; }

    virtual double getValue() { return DBL_MAX; }
    virtual std::string toString() { return ""; }

protected:
    TermTypes type;
};

class Identifier : public Term
{
public:
    Identifier(double value) : value(value)
    {
        type = TT_Identifier;
    }

    double getValue() { return value; }

    std::string toString() {
        std::ostringstream ss;
        ss << value;
        return ss.str();
    }

    virtual std::shared_ptr<Term> derive() {
        return std::shared_ptr<Term>(nullptr);
    }

private:
    double value;
};


class Polynom : public Term
{
public:
    Polynom(std::string var="x",
            std::shared_ptr<Term> coefficient = std::shared_ptr<Term>(new Identifier(1)), 
            std::shared_ptr<Term> exponent = std::shared_ptr<Term>(new Identifier(1)))
        : variable(var), coefficient(coefficient), exponent(exponent) 
    {
        type = TT_Polynom;
    }

    std::string getVariable() { return variable; }

    std::string toString() {
        std::stringstream ss;
        if(coefficient->getType() == TT_Identifier && almostEqual(coefficient->getValue(), 0.0)) {
            ss << "0";
        } else {
            ss << coefficient->toString();
            if(exponent->getType() == TT_Identifier && almostEqual(exponent->getValue(), 1.0)) {
                ss << variable;
            } else if(exponent->getType() == TT_Identifier && !almostEqual(exponent->getValue(), 0.0)) {
                ss << variable << "^(" << exponent->toString() << ")";
            }
        }
        return ss.str();
    }

    virtual std::shared_ptr<Term> derive() {
    // TODO: implement
        return std::shared_ptr<Term>(nullptr);
    }

private:
    std::string variable;
    std::shared_ptr<Term> coefficient;
    std::shared_ptr<Term> exponent;
};


//
//class Expression : public Term
//{
//public:
//    Expression();
//
//    virtual std::shared_ptr<Term> derive() = 0;
//};
//
//class Function : public Term
//{
//public:
//    Function() : coefficient(nullptr), exponent(nullptr), argument(nullptr) {}
//
//    virtual std::shared_ptr<Term> derive();
//
//private:
//    Term* coefficient;
//    Term* exponent;
//    Term* argument;
//};

#endif // FUNCTION_H