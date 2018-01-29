#include "../include/func_constant.h"

namespace Function {

  Constant::Constant(std::string) {

  }
  Constant::~Constant() {

  }

  double Constant::eval(double) {
    return 0;
  }
  
  std::string Constant::derive() {
    return "-";
  }
  
  std::string Constant::print() {
    return "+";
  }

}