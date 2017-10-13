using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Input;
using System.Windows.Threading;

namespace CodeQuizRunner
{
    public class MainViewModel : INotifyPropertyChanged
    {
        private string _output;
        public string Output { get { return _output; } set { _output = value; NotifyPropertyChanged(); } }

        private string _command;
        public string Command { get { return _command; } set { _command = value; NotifyPropertyChanged(); } }

        private string _argument;
        public string Argument { get { return _argument; } set { _argument = value; NotifyPropertyChanged(); } }

        private string _input;
        public string Input { get { return _input; } set { _input = value; NotifyPropertyChanged(); } }

        public ICommand StartCommand { get; }

        public MainViewModel()
        {
            StartCommand = new RelayCommand<object>((nullObject) => StartProcessWithDefaultParameters());
        }

        private void StartProcessWithDefaultParameters()
        {
            StartProcess(Command, Argument);
        }

        private void StartProcess(string executableName, string arguments)
        {
            var startInfo = new ProcessStartInfo(executableName, arguments)
            {
                RedirectStandardOutput = true,
                RedirectStandardError = false,
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
            process.WaitForExit(10000);
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

        #region INotifyPropertyChanged
        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged([CallerMemberName] string property = null)
        {
            if (property == null)
                return;
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(property));
        }
        #endregion
    }
}
