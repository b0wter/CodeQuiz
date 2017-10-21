using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;

namespace TestRunner.Models
{
    public class ProcessTestRunner : ITestRunner, INotifyPropertyChanged
    {
        // Members
        //
        private string _command;
        private string _argument;
        private ObservableCollection<string> _output;
        private int _timeout;

        // Properties
        //
        public ObservableCollection<string> Output { get { return _output; } set { _output = value; NotifyPropertyChanged(); NotifyPropertyChanged(nameof(OutputAsLine)); } }
        public string OutputAsLine => string.Join(Environment.NewLine, Output);
        public string Command => _command;
        public string Argument => _argument;
        public int Timeout => _timeout;

        // Events
        //
        public event PropertyChangedEventHandler PropertyChanged;

        public ProcessTestRunner(string command, string argument)
            : this(command, argument, 10000)
        {
            //
        }

        public ProcessTestRunner(string command, string argument, int timeout)
        {
            _command = command;
            _argument = argument;
            _timeout = timeout;
        }

        public async Task<TestResult> Run(List<string> input, List<string> expectedOutput)
        {
            var process = CreateProcess();
            var startTick = Environment.TickCount;
            process.Start();
            process.BeginOutputReadLine();
            process.BeginErrorReadLine();
            var result = await WaitForProcessExit(process, _timeout);
            var endTick = Environment.TickCount;
            return CreateTestResultFromCurrentState(input, expectedOutput, result, TimeSpan.FromTicks(endTick - startTick));
        }

        private TestResult CreateTestResultFromCurrentState(List<string> input, List<string> expectedOutput, bool result, TimeSpan duration)
        {
            var testResult = new TestResult(input, Output, expectedOutput, result, duration);
            return testResult;
        }

        private async Task<bool> WaitForProcessExit(Process process, int timeout)
        {
            try
            {
                bool didRunComplete = false;
                await Task.Run(() =>
                {
                    process.WaitForExit(timeout);
                    didRunComplete = true;
                });
                return didRunComplete;
            }
            catch (Exception ex)
            {
                Output.Add(ex.ToString());
                return false;
            }
        }

        private Process CreateProcess()
        {
            var startInfo = new ProcessStartInfo(_command, _argument)
            {
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                CreateNoWindow = true,
                UseShellExecute = false
            };

            var process = new Process
            {
                StartInfo = startInfo,
                EnableRaisingEvents = true
            };

            process.OutputDataReceived += Process_OutputDataReceived;
            process.ErrorDataReceived += Process_ErrorDataReceived;

            return process;
        }

        private void Process_ErrorDataReceived(object sender, DataReceivedEventArgs e)
        {
            Output.Add($">>ERROR:{Environment.NewLine}{e.Data}");
        }

        private void Process_OutputDataReceived(object sender, DataReceivedEventArgs e)
        {
            Output.Add(e.Data);
        }

        private void NotifyPropertyChanged([CallerMemberName] string propertyName = null)
        {
            if (propertyName == null)
                return;
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
    }
}
