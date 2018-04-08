#pragma once

#include <memory>
#include <string>
#include "derivable.h"

namespace Function {

  class Monom : public Derivable {
  public:

    Monom(std::string const&) = delete;
    Monom(std::string, Derivable_ptr&&);
    virtual ~Monom() = default;

    std::string print() const override;
    std::string derive() const override;
    std::string print_debug() const override;

  protected:
    double eval(double) const override;
  
  private:
    double power;
    Derivable_ptr inner;
  };
  //typedef std::unique_ptr<(.+)> .+_ptr;
typedef Monom* Monom_ptr;
}