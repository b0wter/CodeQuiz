using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace TestRunner.Models
{
    /// <summary>
    /// Wird von einem Testcase zurückgegeben und enthält Informationen über den Mis(erfolg).
    /// </summary>
    public class TestResult
    {
        public IEnumerable<string> Input { get; }
        public IEnumerable<string> Output { get; }
        public IEnumerable<string> ExpectedOutput { get; }
        public bool RunSuccessful { get; }
        public bool OutputMatches { get; private set; }
        public string FailReason { get; private set; }
        public int Duration { get; private set; }
        public Exception Exception { get; private set; }

        public TestResult(IEnumerable<string> input, IEnumerable<string> output, IEnumerable<string> expectedOutput, bool runSuccessful, int duration, Exception ex)
        {
            Input = input;
            Output = output;
            ExpectedOutput = expectedOutput;
            RunSuccessful = runSuccessful;
            Duration = duration;
            Exception = ex;
            ValidateOutput();
        }

        private void ValidateOutput()
        {
            if(Output == null || Output.Count() == 0)
            {
                FailReason = "Ausgabe des Programms ist leer.";
                OutputMatches = false;
                return;
            }

            var cleanedOutput = Output.Where(x => string.IsNullOrWhiteSpace(x) == false);
            var cleanedExpectedOutput = ExpectedOutput.Where(x => string.IsNullOrWhiteSpace(x) == false);
            int lineDifference = cleanedExpectedOutput.Count() - cleanedOutput.Count();
            if(lineDifference != 0)
            {
                string linePlural = (Math.Abs(lineDifference) == 1 ? "" : "n");
                string difference = (lineDifference > 0 ? "wenig" : "viel");
                FailReason = $"Die Ausgabe hat {Math.Abs(lineDifference)} Zeile{linePlural} zu {difference}.";
                OutputMatches = false;
                return;
            }

            for(int i = 0; i < cleanedExpectedOutput.Count(); ++i)
            {
                if(string.Equals(cleanedOutput.ElementAt(i), cleanedExpectedOutput.ElementAt(i)) == false)
                {
                    FailReason = $"Zeile {i + 1} ist nicht identisch.";
                    OutputMatches = false;
                    return;
                }
            }

            OutputMatches = true;
        }
    }
}
