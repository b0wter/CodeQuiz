#ifndef MATH_EXPRESSION_H
#define MATH_EXPRESSION_H

#include <map>
#include <vector>
#include <ostream>
#include <cmath>
#include <stdexcept>
#include <limits>
#include <iomanip>
#include <string>

/*
TODO: Evaluate für die Funktionen nachrüsten. 
        d.h. Constanten auf Funktionen und Operatoren auswerten wenn möglich.
        SIN(C(x)) => C(y) <- auswerten !!!
TODO: Simplify
TODO: shared_ptr verwenden! derivative NodeExprPtr einführen als shared_ptr type
TODO: Zeiger Zählen ->siehe shared_ptr
TODO: Die Rückgabe von einer neuen NullExpr kann unterlassen werden, wenn bereits
        eine NullExpr durch Ableitung entstanden ist. Diese müsste dann auch nicht
        gelöscht werden.
TODO: Momentan können Terme wie "2x" | "2x^2" | ... nicht geparset werden
        dafür muss der parser angepasst werden.
        Lsg-Ansatz: 
            - Grammatik anpassen sodass '*' in bestimmten Situationen ausgelassen werden darf
            - evtl notwendig: Parameter ändern, sodass nur 1 Buchstabe geparst wird 
*/

enum class NodeTypes
{
    ExprNode,
    BinaryExpr,
    FuncExpr,
    NullExpr,
    ConstantExpr,
    ParameterExpr,
    VariableExpr,
    NegateExpr,
    AddExpr,
    SubtractExpr,
    MultiplyExpr,
    DivideExpr,
    PowerExpr,
    SinFunc,
    CosFunc,
    TanFunc,
    ExpFunc,
    LnFunc,
    SqrtFunc
};

typedef std::numeric_limits<double> dbl;

// Abstract base class of all expressions
class ExprNode 
{
    const NodeTypes _type;
public:
    ExprNode(const NodeTypes type)
        : _type(type) {}

    virtual ~ExprNode() {}

    virtual NodeTypes type() const final {
        return _type;
    }

    virtual bool isNullExpr() const {
        return false;
    }

    virtual bool isConstExpr() const {
        return false;
    }

    virtual bool isVarExpr() const {
        return false;
    }

    virtual ExprNode* copy() const = 0;
    virtual ExprNode* derivative() const = 0;
    virtual void print(std::ostream &os, unsigned int depth = 0) const = 0;

    virtual double evaluate() {
        return DBL_MAX;
    }

    static inline std::string indent(unsigned int d) {
        return std::string(d * 2, ' ');
    }
};

class BinaryExpr : public ExprNode
{
protected:
    ExprNode *_left;
    ExprNode *_right;
public:
    BinaryExpr(const NodeTypes type, ExprNode *left, ExprNode *right)
        : ExprNode(type), _left(left), _right(right) {}

    virtual ~BinaryExpr() {
        delete _left;
        delete _right;
    }

    virtual bool isConstExpr() const {
        return (_left->isConstExpr() || _right->isConstExpr());
    }

    virtual bool isVarExpr() const {
        return (_left->isVarExpr() || _right->isVarExpr());
    }

    inline ExprNode* leftNode() {
        return _left;
    }

    inline ExprNode* rightNode() {
        return _right;
    }
};

class FuncExpr : public ExprNode
{
protected:
    ExprNode *_argNode;
public:
    FuncExpr(const NodeTypes type, ExprNode *arg)
        : ExprNode(type), _argNode(arg) {}

    virtual ~FuncExpr() {
        delete _argNode;
    }

    virtual bool isConstExpr() const {
        return _argNode->isConstExpr();
    }

    virtual bool isVarExpr() const {
        return _argNode->isVarExpr();
    }

    ExprNode* innerNode() {
        return _argNode;
    }
};

class NullExpr : public ExprNode
{
public:
    explicit NullExpr()
        : ExprNode(NodeTypes::NullExpr) {}

    virtual ~NullExpr() {}

    virtual bool isNullExpr() const {
        return true;
    }

    virtual ExprNode* copy() const {
        return new NullExpr();
    }

    virtual ExprNode* derivative() const {
        return new NullExpr();
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        return;
    }
};

class ConstantExpr : public ExprNode
{
    double _value;
public:
    explicit ConstantExpr(double value)
        : ExprNode(NodeTypes::ConstantExpr), _value(value) {}
    
    virtual ~ConstantExpr() {}

    virtual bool isConstExpr() const {
        return true;
    }

    virtual ExprNode* copy() const {
        return new ConstantExpr(_value);
    }

    virtual ExprNode* derivative() const {
        return new NullExpr();
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "C(" << _value << ")";
    }

    virtual double evaluate() {
        return _value;
    }
};

class ParameterExpr : public ExprNode
{
    std::string _value;
public:
    explicit ParameterExpr(std::string value)
        : ExprNode(NodeTypes::ParameterExpr), _value(value) {}
    
    virtual ~ParameterExpr() {}

    virtual bool isConstExpr() const {
        return true;
    }

    virtual ExprNode* copy() const {
        return new ParameterExpr(_value);
    }

    virtual ExprNode* derivative() const {
        return new NullExpr();
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "P(" << _value << ")";
    }

    std::string value() const {
        return _value;
    }
};

class VariableExpr : public ExprNode
{
    std::string _value;
public:
    explicit VariableExpr(std::string value)
        : ExprNode(NodeTypes::VariableExpr), _value(value) {}
    
    virtual ~VariableExpr() {}
    
    virtual bool isVarExpr() const {
        return true;
    }

    virtual ExprNode* copy() const {
        return new VariableExpr(_value);
    }

    virtual ExprNode* derivative() const {
        return new ConstantExpr(1);
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "V(" << _value << ")";
    } 

    std::string value() const {
        return _value;
    }
};

class NegateExpr : public FuncExpr
{
public:
    explicit NegateExpr(ExprNode *node)
        : FuncExpr(NodeTypes::NegateExpr, node) {}

    virtual ~NegateExpr() {}

    virtual ExprNode* copy() const {
        return new NegateExpr(_argNode->copy());
    }

    virtual ExprNode* derivative() const;

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "(-)";
        _argNode->print(os, depth + 1);
    }
};

class AddExpr : public BinaryExpr
{
public:
    explicit AddExpr(ExprNode *left, ExprNode *right)
        : BinaryExpr(NodeTypes::AddExpr, left, right) {}

    virtual ~AddExpr() {}

    virtual ExprNode* copy() const {
        return new AddExpr(_left->copy(), _right->copy());
    }

    virtual ExprNode* derivative() const;

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "ADD(";
        _left->print(os, depth + 1);
        os << ", ";
        _right->print(os, depth + 1);
        os << ")";
    }
};

class SubtractExpr : public BinaryExpr
{
public:
    explicit SubtractExpr(ExprNode *left, ExprNode *right)
        : BinaryExpr(NodeTypes::SubtractExpr, left, right) {}

    virtual ~SubtractExpr() {}

    virtual ExprNode* copy() const {
        return new SubtractExpr(_left->copy(), _right->copy());
    }

    virtual ExprNode* derivative() const;

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "SUB(";
        _left->print(os, depth + 1);
        os << ", ";
        _right->print(os, depth + 1);
        os << ")";
    }
};

class PowerExpr : public BinaryExpr
{
public:
    explicit PowerExpr(ExprNode *left, ExprNode *right)
        : BinaryExpr(NodeTypes::PowerExpr, left, right) {}

    virtual ~PowerExpr() {}

    virtual ExprNode* copy() const {
        return new PowerExpr(_left->copy(), _right->copy());
    }

    virtual ExprNode* derivative() const;

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "POW(";
        _left->print(os, depth + 1);
        os << ", ";
        _right->print(os, depth + 1);
        os << ")";
    }
};

class MultiplyExpr : public BinaryExpr
{
public:
    explicit MultiplyExpr(ExprNode *left, ExprNode *right)
        : BinaryExpr(NodeTypes::MultiplyExpr, left, right) {}

    virtual ~MultiplyExpr() {}

    virtual ExprNode* copy() const {
        return new MultiplyExpr(_left->copy(), _right->copy());
    }

    virtual ExprNode* derivative() const;

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "MUL(";
        _left->print(os, depth + 1);
        os << ", ";
        _right->print(os, depth + 1);
        os << ")";
    }
};

class DivideExpr : public BinaryExpr
{
public:
    explicit DivideExpr(ExprNode *left, ExprNode *right)
        : BinaryExpr(NodeTypes::DivideExpr, left, right) {}

    virtual ~DivideExpr() {}

    virtual ExprNode* copy() const {
        return new DivideExpr(_left->copy(), _right->copy());
    }

    virtual ExprNode* derivative() const;

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "DIV(";
        _left->print(os, depth + 1);
        os << ", ";
        _right->print(os, depth + 1);
        os << ")";
    }
};

class SinFunc : public FuncExpr
{
public:
    explicit SinFunc(ExprNode *arg)
        : FuncExpr(NodeTypes::SinFunc, arg) {}

    virtual ~SinFunc() {}

    virtual ExprNode* copy() const {
        return new SinFunc(_argNode->copy());
    }

    virtual ExprNode* derivative() const;

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "SIN(";
        _argNode->print(os, depth + 1);
        os << ")";
    }
};

class CosFunc : public FuncExpr
{
public:
    explicit CosFunc(ExprNode *arg)
        : FuncExpr(NodeTypes::CosFunc, arg) {}

    virtual ~CosFunc() {}

    virtual ExprNode* copy() const {
        return new CosFunc(_argNode->copy());
    }

    virtual ExprNode* derivative() const;

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "COS(";
        _argNode->print(os, depth + 1);
        os << ")";
    }
};

class TanFunc : public FuncExpr
{
public:
    explicit TanFunc(ExprNode *arg)
        : FuncExpr(NodeTypes::TanFunc, arg) {}

    virtual ~TanFunc() {}

    virtual ExprNode* copy() const {
        return new TanFunc(_argNode->copy());
    }

    virtual ExprNode* derivative() const;

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "TAN(";
        _argNode->print(os, depth + 1);
        os << ")";
    }
};

class ExpFunc : public FuncExpr
{
public:
    explicit ExpFunc(ExprNode *arg)
        : FuncExpr(NodeTypes::ExpFunc, arg) {}

    virtual ~ExpFunc() {}

    virtual ExprNode* copy() const {
        return new ExpFunc(_argNode->copy());
    }

    virtual ExprNode* derivative() const;

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "EXP(";
        _argNode->print(os, depth + 1);
        os << ")";
    }
};

class LnFunc : public FuncExpr
{
public:
    explicit LnFunc(ExprNode *arg)
        : FuncExpr(NodeTypes::LnFunc, arg) {}

    virtual ~LnFunc() {}

    virtual ExprNode* copy() const {
        return new LnFunc(_argNode->copy());
    }

    virtual ExprNode* derivative() const;

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "LN(";
        _argNode->print(os, depth + 1);
        os << ")";
    }
};

class SqrtFunc : public FuncExpr
{
public:
    explicit SqrtFunc(ExprNode *arg)
        : FuncExpr(NodeTypes::SqrtFunc, arg) {}

    virtual ~SqrtFunc() {}

    virtual ExprNode* copy() const {
        return new SqrtFunc(_argNode->copy());
    }

    virtual ExprNode* derivative() const;

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "SQRT(";
        _argNode->print(os, depth + 1);
        os << ")";
    }
};

/***** Context *****/
class ExprContext
{
public:
    typedef std::map<std::string, double> parameterMapType;

    parameterMapType parameters;

    std::string variable;

    std::vector<ExprNode*> expressions;

    ExprContext()
        : variable("x") {}

    ~ExprContext() {
        clearExpressions();
    }

    void clearExpressions() {
	    for(unsigned int i = 0; i < expressions.size(); ++i) {
	        delete expressions[i];
	    }
	    expressions.clear();
    }
        
    bool existsParameter(const std::string &pname) const {
	    return parameters.find(pname) != parameters.end();
    }

    bool isVariable(const std::string &vname) const {
        return (variable.compare(vname) == 0);
    }

    double getParameter(const std::string &pname) const {
	    parameterMapType::const_iterator vi = parameters.find(pname);
	    if (vi == parameters.end())
	        throw(std::runtime_error("Unknown variable."));
	    else
	        return vi->second;
    }

    std::string getVariable() const {
        return variable;
    }
};

#endif // MATH_EXPRESSION_H