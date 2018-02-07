#ifndef MATH_EXPRESSION_H
#define MATH_EXPRESSION_H

#include <map>
#include <vector>
#include <ostream>
#include <cmath>
#include <stdexcept>
#include <limits>
#include <iomanip>

/*
TODO: Evaluate für die Funktionen nachrüsten. 
        d.h. Constanten auf Funktionen und Operatoren auswerten wenn möglich.
        SIN(C(x)) => C(y) <- auswerten !!!
TODO: Simplify
TODO: Unit-Tests erweitern / Test-Cases trennen => Expr-Funktionen splitten
TODO: shared_ptr verwenden! derivative NodeExprPtr einführen als shared_ptr type
TODO: test für copies
TODO: Zeiger Zählen ->siehe shared_ptr
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
    SqrtFunc
};

typedef NodeTypes NT;

typedef std::numeric_limits<double> dbl;

// Abstract base class of all expressions
class ExprNode 
{
public:
    virtual ~ExprNode() {}

    virtual void print(std::ostream &os, unsigned int depth = 0) const = 0;
    virtual ExprNode* derivative() const = 0;
    virtual NodeTypes type() const = 0;
    virtual ExprNode* copy() const = 0;

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
    BinaryExpr(ExprNode *left, ExprNode *right)
        : ExprNode(), _left(left), _right(right) {}

    virtual ~BinaryExpr() {
        delete _left;
        delete _right;
    }

    virtual NodeTypes type() const {
        return NT::BinaryExpr;
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
    FuncExpr(ExprNode *arg)
        : ExprNode(),_argNode(arg) {}

    virtual ~FuncExpr() {
        delete _argNode;
    }

    virtual NodeTypes type() const {
        return NT::FuncExpr;
    }

    ExprNode* innerNode() {
        return _argNode;
    }
};

class NullExpr : public ExprNode
{
public:
    explicit NullExpr()
        : ExprNode() {}

    virtual ExprNode* derivative() const {
        return nullptr;
    }

    virtual NodeTypes type() const {
        return NT::NullExpr;
    }

    virtual ExprNode* copy() const {
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
        : ExprNode(), _value(value) {}
    
    virtual ExprNode* derivative() const {
        return new NullExpr();
    }

    virtual NodeTypes type() const {
        return NT::ConstantExpr;
    }

    virtual ExprNode* copy() const {
        return new ConstantExpr(_value);
    }

    virtual double evaluate() {
        return _value;
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << /*std::setprecision(dbl::max_digits10) <<*/ "C(" << _value << ")";
    }
};

class ParameterExpr : public ExprNode
{
    std::string _value;
public:
    explicit ParameterExpr(std::string value)
        : ExprNode(), _value(value) {}
    
    virtual ExprNode* derivative() const {
        return new NullExpr();
    }

    std::string value() const {
        return _value;
    }

    virtual NodeTypes type() const {
        return NT::ParameterExpr;
    }

    virtual ExprNode* copy() const {
        return new ParameterExpr(_value);
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "P(" << _value << ")";
    }
};

class VariableExpr : public ExprNode
{
    std::string _value;
public:
    explicit VariableExpr(std::string value)
        : ExprNode(), _value(value) {}
    
    virtual ExprNode* derivative() const {
        return new ConstantExpr(1);
    }

    std::string value() const {
        return _value;
    }

    virtual NodeTypes type() const {
        return NT::VariableExpr;
    }

    virtual ExprNode* copy() const {
        return new VariableExpr(_value);
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "V(" << _value << ")";
    } 
};

class NegateExpr : public ExprNode
{
    ExprNode *_node;
public:
    explicit NegateExpr(ExprNode *node)
        : ExprNode(), _node(node) {}

    virtual ~NegateExpr() {
        delete _node;
    }

    virtual ExprNode* derivative() const {
        ExprNode *d = _node->derivative();
        if(d->type() == NT::NegateExpr) {
            return ((NegateExpr*)d)->innerNode();
        }
        return new NegateExpr(d);
    }

    ExprNode* innerNode() {
        return _node;
    }

    virtual NodeTypes type() const {
        return NT::NegateExpr;
    }

    virtual ExprNode* copy() const {
        return new NegateExpr(_node->copy());
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "(-)";
        _node->print(os, depth + 1);
    }
};

class AddExpr : public BinaryExpr
{
public:
    explicit AddExpr(ExprNode *left, ExprNode *right)
        : BinaryExpr(left, right) {}

    virtual ~AddExpr() {}

    virtual NodeTypes type() const {
        return NT::AddExpr;
    }

    virtual ExprNode* copy() const {
        return new AddExpr(_left->copy(), _right->copy());
    }

    virtual ExprNode* derivative() const {
        ExprNode *leftd = _left->derivative();
        NodeTypes ltype = leftd->type();
        ExprNode *rightd = _right->derivative();
        NodeTypes rtype = rightd->type();
        if(ltype == NodeTypes::NullExpr && rtype == NodeTypes::NullExpr) {
            return new NullExpr();
        } else if(ltype == NodeTypes::NullExpr && rtype != NodeTypes::NullExpr) {
            return rightd;
        } else if(ltype != NodeTypes::NullExpr && rtype == NodeTypes::NullExpr) {
            return leftd;
        } else {
            return new AddExpr(leftd, rightd);
        }

        return nullptr;
    }

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
        : BinaryExpr(left, right) {}

    virtual ~SubtractExpr() {}

    virtual NodeTypes type() const {
        return NT::SubtractExpr;
    }

    virtual ExprNode* copy() const {
        return new SubtractExpr(_left->copy(), _right->copy());
    }

    virtual ExprNode* derivative() const {
        ExprNode *leftd = _left->derivative();
        NodeTypes ltype = leftd->type();
        ExprNode *rightd = _right->derivative();
        NodeTypes rtype = rightd->type();
        if(ltype == NodeTypes::NullExpr && rtype == NodeTypes::NullExpr) {
            return new NullExpr();
        } else if(ltype == NodeTypes::NullExpr && rtype != NodeTypes::NullExpr) {
            return rightd;
        } else if(ltype != NodeTypes::NullExpr && rtype == NodeTypes::NullExpr) {
            return leftd;
        } else {
            return new SubtractExpr(leftd, rightd);
        }

        return nullptr;
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "SUB(";
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
        : BinaryExpr(left, right) {}

    virtual ~MultiplyExpr() {}

    virtual NodeTypes type() const {
        return NT::MultiplyExpr;
    }

    virtual ExprNode* copy() const {
        return new MultiplyExpr(_left->copy(), _right->copy());
    }

    virtual ExprNode* derivative() const {
        ExprNode *leftd = _left->derivative();
        NodeTypes ltype = leftd->type();
        ExprNode *rightd = _right->derivative();
        NodeTypes rtype = rightd->type();
        if(ltype == NodeTypes::NullExpr && rtype == NodeTypes::NullExpr) {
            return new NullExpr();
        } else if(ltype == NodeTypes::NullExpr && rtype != NodeTypes::NullExpr) {
            return new MultiplyExpr(_left->copy(), rightd);
        } else if(ltype != NodeTypes::NullExpr && rtype == NodeTypes::NullExpr) {
            return new MultiplyExpr(leftd, _right->copy());
        } else {
            return new AddExpr(new MultiplyExpr(leftd, _right->copy()), new MultiplyExpr(_left->copy(), rightd));
        }
        return nullptr;
    }

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
        : BinaryExpr(left, right) {}

    virtual ~DivideExpr() {}

    virtual NodeTypes type() const {
        return NT::DivideExpr;
    }

    virtual ExprNode* copy() const {
        return new DivideExpr(_left->copy(), _right->copy());
    }

    virtual ExprNode* derivative() const {
        return nullptr;
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "DIV(";
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
        : BinaryExpr(left, right) {}

    virtual ~PowerExpr() {}

    virtual NodeTypes type() const {
        return NT::PowerExpr;
    }

    virtual ExprNode* copy() const {
        return new PowerExpr(_left->copy(), _right->copy());
    }

    virtual ExprNode* derivative() const {
        return nullptr;
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "POW(";
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
        : FuncExpr(arg) {}

    virtual ~SinFunc() {}

    virtual ExprNode* derivative() const {
        return nullptr;
    }

    virtual NodeTypes type() const {
        return NT::SinFunc;
    }

    virtual ExprNode* copy() const {
        return new SinFunc(_argNode->copy());
    }

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
        : FuncExpr(arg) {}

    virtual ~CosFunc() {}

    virtual ExprNode* derivative() const {
        return nullptr;
    }

    virtual NodeTypes type() const {
        return NT::CosFunc;
    }

    virtual ExprNode* copy() const {
        return new CosFunc(_argNode->copy());
    }

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
        : FuncExpr(arg) {}

    virtual ~TanFunc() {}

    virtual ExprNode* derivative() const {
        return nullptr;
    }

    virtual NodeTypes type() const {
        return NT::TanFunc;
    }

    virtual ExprNode* copy() const {
        return new TanFunc(_argNode->copy());
    }

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
        : FuncExpr(arg) {}

    virtual ~ExpFunc() {}

    virtual ExprNode* derivative() const {
        return nullptr;
    }

    virtual NodeTypes type() const {
        return NT::ExpFunc;
    }

    virtual ExprNode* copy() const {
        return new ExpFunc(_argNode->copy());
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << "EXP(";
        _argNode->print(os, depth + 1);
        os << ")";
    }
};

class SqrtFunc : public FuncExpr
{
public:
    explicit SqrtFunc(ExprNode *arg)
        : FuncExpr(arg) {}

    virtual ~SqrtFunc() {}

    virtual ExprNode* derivative() const {
        return nullptr;
    }

    virtual NodeTypes type() const {
        return NT::SqrtFunc;
    }

    virtual ExprNode* copy() const {
        return new SqrtFunc(_argNode->copy());
    }

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

    ~ExprContext()
    {
        clearExpressions();
    }

    void clearExpressions()
    {
	    for(unsigned int i = 0; i < expressions.size(); ++i) {
	        delete expressions[i];
	    }
	    expressions.clear();
    }
        
    bool existsParameter(const std::string &pname) const
    {
	    return parameters.find(pname) != parameters.end();
    }

    bool isVariable(const std::string &vname) const 
    {
        return (variable.compare(vname) == 0);
    }

    double getParameter(const std::string &pname) const
    {
	    parameterMapType::const_iterator vi = parameters.find(pname);
	    if (vi == parameters.end())
	        throw(std::runtime_error("Unknown variable."));
	    else
	        return vi->second;
    }

    std::string getVariable() const
    {
        return variable;
    }
};

#endif // MATH_EXPRESSION_H