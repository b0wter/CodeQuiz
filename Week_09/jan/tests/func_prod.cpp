#include "../include/func_prod.h"
#include <stdexcept>
#include <string>

namespace Function {

  Product::Product(Derivable_ptr&& factor_1, Derivable_ptr&& factor_2) {
    factors.push_back(std::move(factor_1));
    factors.push_back(std::move(factor_2));
  }
  Product::Product(std::list<Derivable_ptr> factors) {
    factors = std::move(factors);
  }

  double Product::eval(double num) const {
    double result = 0;
    for (auto const& factor : factors) {
      result *= factor->eval(num);
    }
    return result ;
  }
  
  std::string Product::derive() const {
    std::string result  = "";
    for (auto const& factor : factors) {
      append(result, factor->derive());
    }
    return (result);
  }
  
  std::string Product::print() const {
    std::string result  = "";
    for (auto const& factor : factors) {
      append(result, factor->print());
    }
    return (result);
  }

  std::string Product::print_debug() const {
    return "_*_( " + print() +  " )";
  }

  void Product::append(std::string& str, std::string const& addition) const {
    std::string const na = "0";
    std::string const op = "+";

    if (!addition.empty() && addition != na) {
      if (str.empty())
        str = addition;
      else
        str += op + addition;
    }
  }

}