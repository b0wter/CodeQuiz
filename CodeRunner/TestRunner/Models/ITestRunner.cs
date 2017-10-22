using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace TestRunner.Models
{
    public interface ITestRunner
    {
        /// <summary>
        /// Startet einen Testlauf mit den Parametern. Timeout ist in Millisekunden.
        /// </summary>
        /// <param name="input"></param>
        /// <param name="expectedOutput"></param>
        /// <param name="timeout"></param>
        /// <returns></returns>
        Task<TestResult> Run(string command, string argument, List<string> input, List<string> expectedOutput, int timeout);
    }
}
