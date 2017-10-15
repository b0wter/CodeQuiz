using System;
using System.Collections.Generic;
using System.Text;

namespace TestRunner.Models
{
    public interface ITestRunner
    {
        TestResult Run(TestCase testCase);
    }
}
