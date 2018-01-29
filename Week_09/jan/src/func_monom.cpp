#include "../include/func_monom.h"
#include <math.h>
#include <stdexcept>
#include <string>

namespace Function {

  Monom::Monom(std::string str, Derivable_ptr&& inner_function) {
    size_t const token_pos = str.rfind('^');
    if (inner_function == nullptr)
      throw std::runtime_error("Incorrect usage of Function:Monom. Inner function is required.");
    if (token_pos == std::string::npos)
      throw std::runtime_error("Incorrect usage of Function:Monom. Function required a '^'.");

    power = stod(str.erase(0, token_pos+1));
    inner = std::move(inner_function);
  }

  double Monom::eval(double num) const {
    return pow(inner->eval(num), power);
  }
  
  std::string Monom::derive() const {
    std::string result  = inner->derive();
    if (result == "0") {
      return "0";
    } else if (result == "1") {
      result = num2str(power) + "*" + inner->print();
    } else {
      result = inner->print();
    }
    if (power != 2) {
      result +=  + "^" + num2str(power-1);
    } 
    return (result);
  }
  
  std::string Monom::print() const {
    return inner->print() + "^" + num2str(power);
  }

  std::string Monom::print_debug() const {
    return "_monom_( " + print() +  " )";
  }

}