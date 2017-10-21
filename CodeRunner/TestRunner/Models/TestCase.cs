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
        /// <summary>
        /// Zeilenweise Eingaben in das Programm.
        /// </summary>
        public List<string> Input { get; set; }

        /// <summary>
        /// Ausgabe welche das zu testende Programm ausgeben sollte.
        /// </summary>
        public List<string> ExpectedOutput { get; set; }

        /// <summary>
        /// Maximale Dauer des Tests in Sekunden.
        /// </summary>
        public int Timeout { get; set; } = 5;

        public TestResult Result { get; private set; }

        public TestResult Run(ITestRunner runner)
        {
            throw new NotImplementedException();
        }
    }
}
