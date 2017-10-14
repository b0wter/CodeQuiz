using CodeRunner.Helpers;
using CodeRunner.Models;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;

namespace CodeRunner.ViewModels
{
    public class MainViewModel : BaseViewModel
    {
        private string _output;
        public string Output { get { return _output; } set { _output = value; NotifyPropertyChanged(); } }

        private string _command;
        public string Command { get { return _command; } set { _command = value; NotifyPropertyChanged(); } }

        private string _argument;
        public string Argument { get { return _argument; } set { _argument = value; NotifyPropertyChanged(); } }

        private string _input;
        public string Input { get { return _input; } set { _input = value; NotifyPropertyChanged(); } }

        private TimeSpan _duration;
        public TimeSpan Duration { get { return _duration; } set { _duration = value; NotifyPropertyChanged(); } }

        public ICommand StartCommand { get; }

        public ObservableCollection<TestCase> TestCases { get; } = new ObservableCollection<TestCase>();

        private bool _testSuccessful = false;
        public bool TestSuccessful { get { return _testSuccessful; } set { _testSuccessful = value; NotifyPropertyChanged(); } }

        public MainViewModel()
        {
            Logger.Info("Creating new MainViewModel.");
            StartCommand = new RelayCommand<object>((nullObject) => StartProcessWithDefaultParameters());

            TestCases.Add((JsonConvert.DeserializeObject<List<TestCase>>(System.IO.File.ReadAllText(@"d:\testcase_1.json"))).First());
        }

        private void StartProcessWithDefaultParameters()
        {
            StartProcess(Command, Argument, TestCases.First()).ConfigureAwait(false);
        }

        private async Task StartProcess(string executableName, string arguments, TestCase testCase)
        {
            IsBusy = true;
            try
            {
                Output = string.Empty;
                var startInfo = new ProcessStartInfo(executableName, arguments)
                {
                    RedirectStandardOutput = true,
                    RedirectStandardError = false,
                    RedirectStandardInput = true,
                    CreateNoWindow = true,
                    UseShellExecute = false
                };

                var process = new Process()
                {
                    StartInfo = startInfo,
                    EnableRaisingEvents = true
                };
                process.OutputDataReceived += Process_OutputDataReceived;
                process.Exited += Process_Exited;
                process.Start();
                process.BeginOutputReadLine();
                FeedInputToProcess(process, testCase).ConfigureAwait(false);
                await WaitForProcessExit(process, 10000);
                TestSuccessful = CompareOutputWithTestCase(testCase, Output);
            }
            catch(Exception ex)
            {
                Logger.Error(ex);
                Output += $"{Environment.NewLine}{Environment.NewLine}{ex}{Environment.NewLine}";
            }
            IsBusy = false;
        }

        private bool CompareOutputWithTestCase(TestCase testCase, string output)
        {
            if (string.IsNullOrWhiteSpace(output))
                return false;
            if(testCase.ExpectedOutput == null || testCase.ExpectedOutput.Count == 0)
            {
                ErrorMessage = "Die erwartete Ausgabe des Testfalls ist leer.";
                return false;
            }

            var outputLines = output.Split(new string[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries).Reverse().Skip(1).Reverse().ToList();
            if (outputLines.Count != testCase.ExpectedOutput.Count)
                return false;

            for (int i = 0; i < outputLines.Count; ++i)
            {
                /*
                if (outputLines[i].Length != testCase.ExpectedOutput[i].Length)
                    return false;

                for(int j = 0; j < outputLines[i].Length; ++j)
                {
                    if (outputLines[i][j] != testCase.ExpectedOutput[i][j])
                        return false;
                }
                */
                if (string.Equals(outputLines[i].TrimEnd(' '), testCase.ExpectedOutput[i].TrimEnd(' ')) == false)
                    return false;
            }

            return true;
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
            catch(Exception ex)
            {
                Output += $"{Environment.NewLine}{Environment.NewLine}{ex}{Environment.NewLine}";
                return false;
            }
        }

        private async Task FeedInputToProcess(Process process, TestCase testCase)
        {
            try
            {
                Logger.Info("Beginne mit dem Schreiben der Eingaben in Stdin");
                foreach (var line in testCase.Input)
                {
                    await Task.Delay(200);
                    process.StandardInput.WriteLine(line);
                }
                Logger.Info("Schreiben in den Stdin abgeschlossen.");
            }
            catch(Exception ex)
            {
                Logger.Error(ex);
            }
        }

        // TODO: Ausgabe eventuell umbauen: https://stackoverflow.com/questions/26340220/process-output-is-much-slower-than-with-cmd
        private void Process_OutputDataReceived(object sender, DataReceivedEventArgs e)
        {
            Output += (e.Data + Environment.NewLine);
        }

        private void Process_Exited(object sender, EventArgs e)
        {
            Output += $"{Environment.NewLine}{Environment.NewLine}>> Der Prozess wurde beendet.";
        }
    }
}
