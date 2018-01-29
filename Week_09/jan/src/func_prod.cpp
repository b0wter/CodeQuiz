#include "../include/func_prod.h"
#include <stdexcept>
#include <string>

#include <iostream>

namespace Function {

  Product::Product(Derivable_ptr&& factor_1, Derivable_ptr&& factor_2) {
    factors.push_back(std::move(factor_1));
    factors.push_back(std::move(factor_2));
  }
  Product::Product(std::list<Derivable_ptr> factors) {
    factors = std::move(factors);
  }

  double Product::eval(double num) const {
    double result = 1;
    for (auto const& factor : factors) {
      result *= factor->eval(num);
    }
    return result;
  }
  
  std::string Product::derive() const {
    std::string result  = "";
    
    for (uint i = 0; i < factors.size(); ++i) {
      std::string part = "";
      uint j = 0;
      for (auto const& factor : factors) {
        if (i == j) {
          append_prod(part, factor->derive());
        } else {
          append_prod(part, factor->print());
        }
        ++j;
      }
      append_sum(result, part);
    }
    return (result);
  }
  
  std::string Product::print() const {
    std::string result  = "";
    for (auto const& factor : factors) {
      append_prod(result, factor->print());
    }
    return (result);
  }

  std::string Product::print_debug() const {
    return "_*_( " + print() +  " )";
  }

  void Product::append_prod(std::string& str, std::string const& addition) const {
    std::string const na = "1";
    std::string const op = "*";

    if (!addition.empty() && addition != na && str != "0") {
      if (str.empty())
        str = addition;
      else if (addition == "x")
        str += addition;
      else
        str += op + addition;
    }
  }
  void Product::append_sum(std::string& str, std::string const& addition) const {
    std::string const na = "0";
    std::string const op = "+";

    if (!addition.empty() && addition != na && str != na) {
      if (str.empty())
        str = addition;
      else
        str += op + addition;
    }
  }

}