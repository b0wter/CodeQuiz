using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace TestRunner.Models
{
    public interface ITestRunner
    {
        Task<TestResult> Run(List<string> input, List<string> expectedOutput);
    }
}
