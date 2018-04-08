#include "../include/func_constant.h"

namespace Function {

  Constant::Constant(std::string const& str) {
    if (str.empty()) {
      value = 0;
    } else {
      value = stod(str);
    }
  }

  double Constant::eval(double) const {
    return value;
  }
  
  std::string Constant::derive() const {
    return "0";
  }
  
  std::string Constant::print() const {
    return value ? num2str(value) : "";
  }

  std::string Constant::print_debug() const {
    return "_constant_( " + print() +  " )";
  }

}