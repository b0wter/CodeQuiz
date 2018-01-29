#pragma once

#include <memory>
#include <string>
#include "derivable.h"

namespace Function {

  class Monom : public Derivable {
  public:

    Monom(std::string const&) = delete;
    Monom(std::string, std::unique_ptr<Derivable>&&);
    virtual ~Monom() = default;

    double eval(double) const override;
    std::string derive() const override;
    std::string print() const override;

  private:

    double power;
    std::unique_ptr<Derivable> inner;
  };
}