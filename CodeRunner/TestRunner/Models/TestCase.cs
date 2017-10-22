using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;

namespace TestRunner.Models
{
    /// <summary>
    /// Beschreibt einen Testfall, inklusive Eingabe und erwarteter Ausgabe.
    /// </summary>
    public class TestCase : INotifyPropertyChanged
    {
        private List<string> _input;
        /// <summary>
        /// Zeilenweise Eingaben in das Programm.
        /// </summary>
        public List<string> Input { get { return _input; } set { _input = value; NotifyPropertyChanged(); } }

        private List<string> _expectedOutput;
        /// <summary>
        /// Ausgabe welche das zu testende Programm ausgeben sollte.
        /// </summary>
        public List<string> ExpectedOutput { get { return _expectedOutput; } set { _expectedOutput = value; NotifyPropertyChanged(); } }

        private int _timeout = 5000;
        /// <summary>
        /// Maximale Dauer des Tests in Sekunden.
        /// </summary>
        public int Timeout { get { return _timeout; } set { _timeout = value; NotifyPropertyChanged(); } }

        private bool _wasRun = false;
        /// <summary>
        /// Liefert zurueck ob der Test durchgelaufen ist (auch true im Fehlerfall).
        /// </summary>
        public bool WasRun { get { return _wasRun; } set { _wasRun = value; NotifyPropertyChanged(); } }

        private TestResult _testResult;
        /// <summary>
        /// Ergebnis des Testdurchlaufs. Enthaelt Detailsinformationen und Laufzeiten.
        /// </summary>
        public TestResult Result { get { return _testResult; } set { _testResult = value; NotifyPropertyChanged(); } }

        private DateTime? _lastRun;
        /// <summary>
        /// Zeitpunkt zu dem der Test das letzte Mal gestartet wurde.
        /// </summary>
        public DateTime? LastRun { get { return _lastRun; } set { _lastRun = value; NotifyPropertyChanged(); } }
            

        public event PropertyChangedEventHandler PropertyChanged;

        public async Task<TestResult> Run(ITestRunner runner, string command, string argument)
        {
            TestResult result = null;
            LastRun = DateTime.Now;
            WasRun = true;
            Result = await runner.Run(command, argument, Input, ExpectedOutput, Timeout);
            return result;
        }

        private void NotifyPropertyChanged([CallerMemberName] string propertyName = null)
        {
            if (propertyName == null)
                return;
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
    }
}
