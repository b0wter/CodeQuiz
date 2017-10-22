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
        // Constants
        //
        private const int InputDelay = 100;

        // Properties
        //
        public ObservableCollection<string> Output { get; } = new ObservableCollection<string>();//{ get { return _output; } set { _output = value; NotifyPropertyChanged(); NotifyPropertyChanged(nameof(OutputAsLine)); } }
        public string OutputAsLine => string.Join(Environment.NewLine, Output);

        // Events
        //
        public event PropertyChangedEventHandler PropertyChanged;

        public async Task<TestResult> Run(string command, string argument, List<string> input, List<string> expectedOutput, int timeout)
        {
            var process = CreateProcess(command, argument);
            process.Start();
            process.BeginOutputReadLine();
            await FeedInputToProcess(process, input);
            var startTick = Environment.TickCount;
            var result = await WaitForProcessExit(process, timeout);
            var endTick = Environment.TickCount;
            return CreateTestResultFromCurrentState(input, expectedOutput, result, (endTick - startTick));
        }

        private async Task FeedInputToProcess(Process process, List<string> input)
        {
            foreach (var line in input)
            {
                await Task.Delay(InputDelay);
                process.StandardInput.WriteLine(line);
            }
        }

        private TestResult CreateTestResultFromCurrentState(IEnumerable<string> input, List<string> expectedOutput, bool result, int duration)
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

        private Process CreateProcess(string command, string argument)
        {
            var startInfo = new ProcessStartInfo(command, argument)
            {
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                CreateNoWindow = true,
                UseShellExecute = false
            };

            var process = new Process
            {
                StartInfo = startInfo,
                EnableRaisingEvents = true
            };

            process.OutputDataReceived += Process_OutputDataReceived;

            return process;
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
