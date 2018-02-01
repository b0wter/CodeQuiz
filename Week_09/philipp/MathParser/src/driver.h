#ifndef DERIVATIVE_MATH_DRIVER_H
#define DERIVATIVE_MATH_DRIVER_H

#include <string>
#include <vector>

class ExprContext;

namespace derivative {

/** The MathDriver connects all components. It creates an instance of the MathParser
 * and the MathScanner classes. The input stream is fed into the Scanner object
 * and the parser gets its token sequence. The driver object is available in the
 * grammar as a parameter. The driver class contains a reference to the structure
 * into which the parsed data is saved.
 *
*/
class MathDriver
{
public:
    MathDriver(class ExprContext &context);

    bool traceScanning;
    bool traceParsing;
    std::string streamName;

    bool parseStream(std::istream &in,
                     const std::string &sname = "stream input");

    bool parseString(const std::string &input,
                     const std::string &sname = "string stream");

    bool parseFile(const std::string &filename);

    void error(const class location &l, const std::string &m);
    void error(const std::string &m);

    class MathScanner *lexer;
    class ExprContext &context;

};

}   // namespace derivative

#endif // DERIVATIVE_MATH_DRIVER_H