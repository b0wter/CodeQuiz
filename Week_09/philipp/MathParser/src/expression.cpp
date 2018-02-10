#include "expression.h"

ExprNode* NegateExpr::derivative() const {
    ExprNode *d = _argNode->derivative();
    if(d->type() == NodeTypes::NullExpr) {
        delete d;
        return new NullExpr();
    }
    return new NegateExpr(d);
}

ExprNode* NegateExpr::evaluate() const {
    ExprNode* v = _argNode->evaluate();
    if(v->isConstExpr()) {
        double value = ((ConstantExpr*)v)->value();
        delete v;
        return new ConstantExpr(-value);
    }
    return new NegateExpr(_argNode->copy());
}

ExprNode* AddExpr::derivative() const {
    ExprNode *leftd = _left->derivative();
    NodeTypes ltype = leftd->type();
    ExprNode *rightd = _right->derivative();
    NodeTypes rtype = rightd->type();
    if(ltype == NodeTypes::NullExpr && rtype == NodeTypes::NullExpr) {
        // (const + const)' = 0
        delete leftd;
        delete rightd;
        return new NullExpr();
    } else if(ltype == NodeTypes::NullExpr && rtype != NodeTypes::NullExpr) {
        // (const + g)' = g'
        delete leftd;
        return rightd;
    } else if(ltype != NodeTypes::NullExpr && rtype == NodeTypes::NullExpr) {
        // (f + const)' = f' 
        delete rightd;
        return leftd;
    } else {
        // (f + g)' = f' + g'
        return new AddExpr(leftd, rightd);
    }

    return nullptr;
}


ExprNode* AddExpr::evaluate() const
{
    ExprNode* leval = _left->evaluate();
    ExprNode* reval = _right->evaluate();
    if(leval->isConstExpr() && reval->isConstExpr()) {
        double lvalue = ((ConstantExpr*)leval)->value();
        double rvalue = ((ConstantExpr*)reval)->value();
        delete leval;
        delete reval;
        return new ConstantExpr(lvalue + rvalue);
    } else {
        return new AddExpr(leval, reval);
    }
}

ExprNode* SubtractExpr::derivative() const {
    ExprNode *leftd = _left->derivative();
    NodeTypes ltype = leftd->type();
    ExprNode *rightd = _right->derivative();
    NodeTypes rtype = rightd->type();
    if(ltype == NodeTypes::NullExpr && rtype == NodeTypes::NullExpr) {
        // (const - const)' = 0
        delete leftd;
        delete rightd;
        return new NullExpr();
    } else if(ltype == NodeTypes::NullExpr && rtype != NodeTypes::NullExpr) {
        // (const - f)' = -f'
        delete leftd;
        return new NegateExpr(rightd);
    } else if(ltype != NodeTypes::NullExpr && rtype == NodeTypes::NullExpr) {
        // (f - const)' = f'
        delete rightd;
        return leftd;
    } else {
        // (f - g)' = f' - g'
        return new SubtractExpr(leftd, rightd);
    }

    return nullptr;
}

ExprNode* SubtractExpr::evaluate() const
{
    ExprNode* leval = _left->evaluate();
    ExprNode* reval = _right->evaluate();
    if(leval->isConstExpr() && reval->isConstExpr()) {
        double lvalue = ((ConstantExpr*)leval)->value();
        double rvalue = ((ConstantExpr*)reval)->value();
        delete leval;
        delete reval;
        return new ConstantExpr(lvalue - rvalue);
    } else {
        return new SubtractExpr(leval, reval);
    }
}

ExprNode* PowerExpr::derivative() const {
    ExprNode *leftd = _left->derivative();
    ExprNode *rightd = _right->derivative();
    if(rightd->type() == NodeTypes::NullExpr && leftd->type() == NodeTypes::NullExpr) {
        // const ^ const 
        return new NullExpr();
    } else if(rightd->type() == NodeTypes::NullExpr) {
        // f(x)^n = n*f(x)^(n-1)
        return new MultiplyExpr(_right->copy(),
                                new PowerExpr(_left->copy(),
                                              new SubtractExpr(_right->copy(),
                                                               new ConstantExpr(1))));
    } else {
        // f'(x) = (g(x)^h(x))' = (exp(h(x)*ln(g(x))))'
        // = g(x)^h(x) * (h'(x)*ln(g(x)) + h(x)*g'(x)/g(x))
        return new MultiplyExpr(new PowerExpr(_left->copy(),
                                              _right->copy()),
                                new AddExpr(new MultiplyExpr(leftd,
                                                             new LnFunc(rightd->copy())),
                                            new MultiplyExpr(_left->copy(),
                                                             new DivideExpr(rightd,
                                                                            _right->copy()))));
    }
}

ExprNode* PowerExpr::evaluate() const
{
    ExprNode* leval = _left->evaluate();
    ExprNode* reval = _right->evaluate();
    if(leval->isConstExpr() && reval->isConstExpr()) {
        double lvalue = ((ConstantExpr*)leval)->value();
        double rvalue = ((ConstantExpr*)reval)->value();
        delete leval;
        delete reval;
        return new ConstantExpr(std::pow(lvalue, rvalue));
    } else {
        return new PowerExpr(leval, reval);
    }
}

ExprNode* MultiplyExpr::derivative() const {
    ExprNode *leftd = _left->derivative();
    NodeTypes ltype = leftd->type();
    ExprNode *rightd = _right->derivative();
    NodeTypes rtype = rightd->type();
    if(ltype == NodeTypes::NullExpr && rtype == NodeTypes::NullExpr) {
        // (const * const)' = 0
        delete leftd;
        delete rightd;
        return new NullExpr();
    } else if(ltype == NodeTypes::NullExpr && rtype != NodeTypes::NullExpr) {
        // (const * f)' = const * f'
        delete leftd;
        return new MultiplyExpr(_left->copy(), rightd);
    } else if(ltype != NodeTypes::NullExpr && rtype == NodeTypes::NullExpr) {
        // (f * const)' = f' * const
        delete rightd;
        return new MultiplyExpr(leftd, _right->copy());
    } else {
        // (f*g)' = f'g + fg'
        return new AddExpr(new MultiplyExpr(leftd,
                                            _right->copy()),
                            new MultiplyExpr(_left->copy(),
                                            rightd));
    }
    return nullptr;
}

ExprNode* MultiplyExpr::evaluate() const
{
    ExprNode* leval = _left->evaluate();
    ExprNode* reval = _right->evaluate();
    if(leval->isConstExpr() && reval->isConstExpr()) {
        double lvalue = ((ConstantExpr*)leval)->value();
        double rvalue = ((ConstantExpr*)reval)->value();
        delete leval;
        delete reval;
        return new ConstantExpr(lvalue * rvalue);
    } else {
        if(leval->isConstExpr() && reval->type() == NodeTypes::MultiplyExpr) {
            MultiplyExpr *m = (MultiplyExpr*)reval;
            if(m->leftNode()->isConstExpr()) {
                return new MultiplyExpr(new ConstantExpr(((ConstantExpr*)leval)->value() * ((ConstantExpr*)m->leftNode())->value()),
                                        m->rightNode());
            }
            if(m->rightNode()->isConstExpr()) {
                return new MultiplyExpr(new ConstantExpr(((ConstantExpr*)leval)->value() * ((ConstantExpr*)m->rightNode())->value()),
                                        m->leftNode());
            }
        }
        if(reval->isConstExpr() && leval->type() == NodeTypes::MultiplyExpr) {
            MultiplyExpr *m = (MultiplyExpr*)leval;
            if(m->leftNode()->isConstExpr()) {
                return new MultiplyExpr(new ConstantExpr(((ConstantExpr*)leval)->value() * ((ConstantExpr*)m->leftNode())->value()),
                                        m->rightNode());
            }
            if(m->rightNode()->isConstExpr()) {
                return new MultiplyExpr(new ConstantExpr(((ConstantExpr*)leval)->value() * ((ConstantExpr*)m->rightNode())->value()),
                                        m->leftNode());
            }
        }
        return new MultiplyExpr(leval, reval);
    }
}

ExprNode* DivideExpr::derivative() const {
    ExprNode *leftd = _left->derivative();
    NodeTypes ltype = leftd->type();
    ExprNode *rightd = _right->derivative();
    NodeTypes rtype = rightd->type();
    if(ltype == NodeTypes::NullExpr && rtype == NodeTypes::NullExpr) {
        // (const / const)' = 0
        delete leftd;
        delete rightd;
        return new NullExpr();
    } else if(ltype == NodeTypes::NullExpr && rtype != NodeTypes::NullExpr) {
        // (const / f)' = (const * (1/f)') = const * (-f' / f^2)
        delete leftd;
        return new MultiplyExpr(_left->copy(),
                                new DivideExpr(new NegateExpr(rightd),
                                                new PowerExpr(_right->copy(),
                                                                new ConstantExpr(2))));
    } else if(ltype != NodeTypes::NullExpr && rtype == NodeTypes::NullExpr) {
        // (f / const)' = (1 / const) * f'
        delete rightd;
        return new MultiplyExpr(new DivideExpr(new ConstantExpr(1),
                                                _right->copy()),
                                leftd);
    } else {
        // (g/h)' = ((g'*h - g*h') / h^2)
        return new DivideExpr(new SubtractExpr(new MultiplyExpr(leftd,
                                                                _right->copy()),
                                                new MultiplyExpr(_left->copy(),
                                                                rightd)),
                                new PowerExpr(_right->copy(),
                                            new ConstantExpr(2)));
    }
    return nullptr;
}

ExprNode* DivideExpr::evaluate() const
{
    ExprNode* leval = _left->evaluate();
    ExprNode* reval = _right->evaluate();
    if(leval->isConstExpr() && reval->isConstExpr()) {
        double lvalue = ((ConstantExpr*)leval)->value();
        double rvalue = ((ConstantExpr*)reval)->value();
        delete leval;
        delete reval;
        return new ConstantExpr(lvalue / rvalue);
    } else {
        return new DivideExpr(leval, reval);
    }
}

ExprNode* SinFunc::derivative() const {
    ExprNode *d = _argNode->derivative();
    if(d->type() == NodeTypes::NullExpr) {
        // (sin(const))' = 0
        delete d;
        return new NullExpr();
    } else if(_argNode->type() == NodeTypes::VariableExpr) {
        // (sin(x))' = cos(x)
        return new CosFunc(_argNode->copy());
    } else {
        // (sin(f(x)))' = f'(x) * cos(f(x))
        return new MultiplyExpr(d, new CosFunc(_argNode->copy()));
    }

    return nullptr;
}

ExprNode* SinFunc::evaluate() const
{
    ExprNode* v = _argNode->evaluate();
    if(v->isConstExpr()) {
        double value = ((ConstantExpr*)v)->value();
        delete v;
        return new ConstantExpr(std::sin(value));    
    }

    return new SinFunc(v);
}

ExprNode* CosFunc::derivative() const {
    ExprNode *d = _argNode->derivative();
    if(d->type() == NodeTypes::NullExpr) {
        // (cos(const))' = 0
        delete d;
        return new NullExpr();
    } else if(_argNode->type() == NodeTypes::VariableExpr) {
        // (cos(x))' = -sin(x)
        return new NegateExpr(new SinFunc(_argNode->copy()));
    } else {
        // (cos(f(x)))' = - f'(x) * sin(f(x))
        return new NegateExpr(new MultiplyExpr(d, new SinFunc(_argNode->copy())));
    }

    return nullptr;
}

ExprNode* CosFunc::evaluate() const
{
    ExprNode* v = _argNode->evaluate();
    if(v->isConstExpr()) {
        double value = ((ConstantExpr*)v)->value();
        delete v;
        return new ConstantExpr(std::cos(value));    
    }

    return new CosFunc(v);
}

ExprNode* TanFunc::derivative() const {
    ExprNode *d = _argNode->derivative();
    if(d->type() == NodeTypes::NullExpr) {
        // (tan(const))' = 0
        delete d;
        return new NullExpr();
    }/* else if(_argNode->type() == NodeTypes::VariableExpr) {
        return new DivideExpr(new ConstantExpr(1),
                              new PowerExpr(new CosFunc(_argNode->copy()),
                                            new ConstantExpr(2)));
    }*/ else {
        // (tan(f(x)))' = f'(x) / cos(x)^2 
        return new DivideExpr(d,
                              new PowerExpr(new CosFunc(_argNode->copy()),
                                            new ConstantExpr(2)));
    }

    return nullptr;
}

ExprNode* TanFunc::evaluate() const
{
    ExprNode* v = _argNode->evaluate();
    if(v->isConstExpr()) {
        double value = ((ConstantExpr*)v)->value();
        delete v;
        return new ConstantExpr(std::tan(value));    
    }

    return new TanFunc(v);
}

ExprNode* ExpFunc::derivative() const {
    ExprNode *d = _argNode->derivative();
    if(d->type() == NodeTypes::NullExpr) {
        // (exp(const))' = 0
        delete d;
        return new NullExpr();
    } else if(_argNode->type() == NodeTypes::VariableExpr) {
        // (exp(x))' = exp(x)
        return new ExpFunc(_argNode->copy());
    } else {
        // (exp(f(x)))' = f'(x) * exp(f(x))
        return new MultiplyExpr(d, new ExpFunc(_argNode->copy()));
    }

    return nullptr;
}

ExprNode* ExpFunc::evaluate() const
{
    ExprNode* v = _argNode->evaluate();
    if(v->isConstExpr()) {
        double value = ((ConstantExpr*)v)->value();
        delete v;
        return new ConstantExpr(std::exp(value));    
    }

    return new ExpFunc(v);
}

ExprNode* LnFunc::derivative() const {
    ExprNode *d = _argNode->derivative();
    if(d->type() == NodeTypes::NullExpr) {
        delete d;
        return new NullExpr();
    } else {
        return new DivideExpr(d, _argNode->copy());
    }
}

ExprNode* LnFunc::evaluate() const
{
    ExprNode* v = _argNode->evaluate();
    if(v->isConstExpr()) {
        double value = ((ConstantExpr*)v)->value();
        delete v;
        return new ConstantExpr(std::log(value));    
    }

    return new LnFunc(v);
}

ExprNode* SqrtFunc::derivative() const {
    ExprNode *d = _argNode->derivative();
    if(d->type() == NodeTypes::NullExpr) {
        // (sqrt(const))' = 0
        delete d;
        return new NullExpr();
    }/* else if(_argNode->type() == NodeTypes::VariableExpr) {
        return new DivideExpr(new ConstantExpr(1),
                              new MultiplyExpr(new ConstantExpr(2),
                                               new SqrtFunc(_argNode)));
    }*/ else {
        // (sqrt(f(x)))' = f'(x) / (2 * sqrt(f(x)))
        return new DivideExpr(d,
                              new MultiplyExpr(new ConstantExpr(2),
                                               new SqrtFunc(_argNode)));
    }
}

ExprNode* SqrtFunc::evaluate() const
{
    ExprNode* v = _argNode->evaluate();
    if(v->isConstExpr()) {
        double value = ((ConstantExpr*)v)->value();
        delete v;
        return new ConstantExpr(std::sqrt(value));    
    }

    return new SqrtFunc(v);
}