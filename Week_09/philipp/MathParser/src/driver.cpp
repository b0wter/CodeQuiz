#include <fstream>
#include <sstream>

#include "driver.h"
#include "scanner.h"

namespace derivative {

MathDriver::MathDriver(class ExprContext &context)
    : traceScanning(false)
    , traceParsing(false)
    , context(context)
{
    
}

bool MathDriver::parseStream(std::istream &in,
                             const std::string &sname)
{
    streamName = sname;
    MathScanner scanner(&in);
    scanner.setDebug(traceScanning);
    this->lexer = &scanner;
    MathParser parser(*this);
    parser.set_debug_level(traceParsing);
    return (parser.parse() == 0);
}

bool MathDriver::parseString(const std::string &input,
                             const std::string &sname)
{
    std::stringstream iss(input);
    return parseStream(iss, sname);
}

bool MathDriver::parseFile(const std::string &filename)
{
    std::ifstream in(filename.c_str());
    if(!in.good())
        return false;
    return parseStream(in, filename);
}

void MathDriver::error(const class location &l,
                       const std::string &m)
{
    std::cerr << l << ": " << m << std::endl;
}

void MathDriver::error(const std::string &m)
{
    std::cerr << m << std::endl;
}


}   // namespace derivative