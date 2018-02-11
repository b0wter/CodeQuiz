#include "../include/func_sum.h"
#include <stdexcept>
#include <string>

namespace Function {

  Sum::Sum(Derivable_ptr&& summand_1, Derivable_ptr&& summand_2) {
    summands.push_back(std::move(summand_1));
    summands.push_back(std::move(summand_2));
  }
  Sum::Sum(std::list<Derivable_ptr> summands) {
    summands = std::move(summands);
  }

  double Sum::eval(double num) const {
    double result = 0;
    for (auto const& summand : summands) {
      result += (*summand)(num);
    }
    return result ;
  }
  
  std::string Sum::derive() const {
    std::string result  = "";
    for (auto const& summand : summands) {
      append(result, summand->derive());
    }
    if (result.empty()) result = "0";
    return (result);
  }
  
  std::string Sum::print() const {
    std::string result  = "";
    for (auto const& summand : summands) {
      append(result, summand->print());
    }
    if (result.empty()) result = "0";
    return (result);
  }

  std::string Sum::print_debug() const {
    return "_+_( " + print() +  " )";
  }

  void Sum::append(std::string& str, std::string const& addition) const {
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