#include <stdexcept>
#include "../include/func_x.h"

namespace Function {

  X::X(std::string const& str) {
    if (str != "x")
      throw std::runtime_error("Incorrect usage of Function:X. Allowed syntax isonly 'x'.");
  }

  double X::eval(double num) const {
    return num;
  }
  
  std::string X::derive() const {
    return "1";
  }
  
  std::string X::print() const {
    return "x";
  }

}