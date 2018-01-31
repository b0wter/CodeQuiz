#ifndef MATH_EXPRESSION_H
#define MATH_EXPRESSION_H

#include <map>
#include <vector>
#include <ostream>
#include <cmath>
#include <stdexcept>

// Abstract base class of all expressions
class ExprNode 
{
public:
    virtual ~ExprNode() {}

    virtual void print(std::ostream &os, unsigned int depth = 0) const = 0;

    static inline std::string indent(unsigned int d) {
        return std::string(d * 2, ' ');
    }
};

class ConstantExpr : public ExprNode
{
    double _value;

public:
    explicit ConstantExpr(double value)
        : ExprNode(), _value(value) {}

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << indent(depth) << _value << std::endl;
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

    virtual void print(std::ostream &os, unsigned int depth) const {
        os << indent(depth) << "(-)" << std::endl;
        _node->print(os, depth + 1);
    }
};

class AddExpr : public ExprNode
{
    ExprNode *_left;
    ExprNode *_right;
public:
    explicit AddExpr(ExprNode *left, ExprNode *right)
        : ExprNode(), _left(left), _right(right) {}

    virtual ~AddExpr() {
        delete _left;
        delete _right;
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        _left->print(os, depth + 1);
        os << indent(depth) << "+" << std::endl;
        _right->print(os, depth + 1);
    }
};

class SubtractExpr : public ExprNode
{
    ExprNode *_left;
    ExprNode *_right;
public:
    explicit SubtractExpr(ExprNode *left, ExprNode *right)
        : ExprNode(), _left(left), _right(right) {}

    virtual ~SubtractExpr() {
        delete _left;
        delete _right;
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        _left->print(os, depth + 1);
        os << indent(depth) << "-" << std::endl;
        _right->print(os, depth + 1);
    }
};

class MultiplyExpr : public ExprNode
{
    ExprNode *_left;
    ExprNode *_right;
public:
    explicit MultiplyExpr(ExprNode *left, ExprNode *right)
        : ExprNode(), _left(left), _right(right) {}

    virtual ~MultiplyExpr() {
        delete _left;
        delete _right;
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        _left->print(os, depth + 1);
        os << indent(depth) << "*" << std::endl;
        _right->print(os, depth + 1);
    }
};

class DivideExpr : public ExprNode
{
    ExprNode *_left;
    ExprNode *_right;
public:
    explicit DivideExpr(ExprNode *left, ExprNode *right)
        : ExprNode(), _left(left), _right(right) {}

    virtual ~DivideExpr() {
        delete _left;
        delete _right;
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        _left->print(os, depth + 1);
        os << indent(depth) << "/" << std::endl;
        _right->print(os, depth + 1);
    }
};

class PowerExpr : public ExprNode
{
    ExprNode *_left;
    ExprNode *_right;
public:
    explicit PowerExpr(ExprNode *left, ExprNode *right)
        : ExprNode(), _left(left), _right(right) {}

    virtual ~PowerExpr() {
        delete _left;
        delete _right;
    }

    virtual void print(std::ostream &os, unsigned int depth) const {
        _left->print(os, depth + 1);
        os << indent(depth) << "^" << std::endl;
        _right->print(os, depth + 1);
    }
};

class ExprContext
{
public:
    typedef std::map<std::string, double> variableMapType;

    variableMapType variables;

    std::vector<ExprNode*> expressions;

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
        
    bool existsVariable(const std::string &varname) const
    {
	    return variables.find(varname) != variables.end();
    }

    double	getVariable(const std::string &varname) const
    {
	    variableMapType::const_iterator vi = variables.find(varname);
	    if (vi == variables.end())
	        throw(std::runtime_error("Unknown variable."));
	    else
	        return vi->second;
    }
};

#endif // MATH_EXPRESSION_H