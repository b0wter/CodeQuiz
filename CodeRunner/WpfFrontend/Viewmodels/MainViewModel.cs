using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;
using TestRunner.Models;

namespace WpfFrontend.Viewmodels
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
    }
}
