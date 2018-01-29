#include "../include/func_constant.h"

namespace Function {

  Constant::Constant(std::string const& str) {
    value = stod(str);
  }

  double Constant::eval(double) const {
    return value;
  }
  
  std::string Constant::derive() const {
    return "0";
  }
  
  std::string Constant::print() const {
    return num2str(value);
  }

}