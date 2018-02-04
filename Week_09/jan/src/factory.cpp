#include "../include/factory.h"

#include "../include/func_prod.h"
#include "../include/func_constant.h"
#include "../include/func_x.h"
#include "../include/func_sum.h"
#include "../include/func_monom.h"

#include <iostream>

namespace Function {
  namespace Factory {
    size_t find_next_token(std::string const& str, size_t pos = 0, std::string const& tokens = "+-(") {
      for (;pos < str.length(); ++pos) {
        if (tokens.find(str[pos]) != std::string::npos) {
            return pos;
        }
      }
      return std::string::npos;
    } /* function  find_next_token */

    Derivable_ptr parse(std::string const& str) {
      size_t const c = str.length();
      Derivable_ptr result = nullptr;

      size_t const pos = find_next_token(str);
      if (pos == std::string::npos) {
        size_t const token_monom = str.find('^');
        if (token_monom == std::string::npos) {

          if (str == "x") {
            result = new Function::X("x");
          } else
          if (str.back() == 'x') {
            auto remaining = str.substr(0,str.length() -1);
            if (remaining.back() == '*') remaining.erase(remaining.length()-1);
            auto factor = parse(remaining);
            auto x = new Function::X("x");
            result = new Function::Product(std::move(factor), std::move(x));
          } else {
            result = new Function::Constant(str);
          }

        } else {
          auto const remaining = str.substr(0, token_monom);
          auto const power = "x" + str.substr(token_monom);
          result = new Function::Monom(power, parse(remaining));
        }
      } else
      switch (str[pos]) {
        case '+': {
          auto const summand1 = str.substr(0, pos);
          auto const summand2 = str.substr(pos + 1);
          result = new Function::Sum(parse(summand1), parse(summand2));
        }
        break;
        case '-': {
          auto const summand1 = str.substr(0, pos -1);
          auto const summand2 = str.substr(pos + 1);
          result = new Function::Sum(parse(summand1), parse(summand2));
        }
        break;
        case '(': {
          size_t const token_close = str.find(')', pos);
          if (token_close == std::string::npos) {
            throw std::runtime_error("Incorrect usage of Factory. Missing token ')'.");
          }
          auto const remaining = str.substr(1, token_close -1);
          result = parse(remaining);
        }
            
        break;
      }
      return std::move(result);
    }
  } /* namespace Factory */
} /* namespace Function */
