using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodeRunner.Models
{
    public class TestCase
    {
        public List<string> Input { get; set; }
        public List<string> ExpectedOutput { get; set; }

        public async Task<TestCaseResult> Run(Process process)
        {
            throw new NotImplementedException();
        }
    }
}
