using System;
using System.Collections.Generic;
using System.Text;

namespace TestRunner.Models
{
    /// <summary>
    /// Beschreibt einen Testfall, inklusive Eingabe und erwarteter Ausgabe.
    /// </summary>
    public class TestCase
    {
        public List<string> Input { get; set; }
        public List<string> ExpectedOutput { get; set; }

        public TestResult Run(ITestRunner runner)
        {
            throw new NotImplementedException();
        }
    }
}
