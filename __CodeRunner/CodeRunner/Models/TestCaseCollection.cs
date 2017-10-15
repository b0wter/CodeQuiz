using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodeRunner.Models
{
    public class TestCaseCollection : ObservableCollection<TestCase>
    {
        private List<TestCase> TestCases { get; set; }

        public IDisposable Subscribe(IObserver<TestCase> observer)
        {
            throw new NotImplementedException();
        }
    }
}
