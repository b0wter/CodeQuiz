using Newtonsoft.Json;
using Ninject;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;
using TestRunner.Models;
using WpfFrontend.Helpers;

namespace WpfFrontend.Viewmodels
{
    public class MainViewModel : BaseViewModel
    {
        private string _output;
        /// <summary>
        /// Ausgabe des Testrunners.
        /// </summary>
        public string Output { get { return _output; } set { _output = value; NotifyPropertyChanged(); } }

        private string _command;
        /// <summary>
        /// Befehl der vom Testrunner aufgerufen wird.
        /// </summary>
        public string Command { get { return _command; } set { _command = value; NotifyPropertyChanged(); } }

        private string _argument;
        /// <summary>
        /// (Kommandozeilen)argument für den Testrunner.
        /// </summary>
        public string Argument { get { return _argument; } set { _argument = value; NotifyPropertyChanged(); } }

        public ICommand StartTestcasesCommand { get; }
        public ICommand LoadTestcasesCommand { get; }

        public ObservableCollection<TestCase> TestCases { get; } = new ObservableCollection<TestCase>();

        private ISelectFileDialog _fileSelector;
        private IMessageDialog _messageDialog;

        [Inject]
        public MainViewModel(ISelectFileDialog fileSelector, IMessageDialog messageDialog)
        {
            _fileSelector = fileSelector;
            _messageDialog = messageDialog;

            StartTestcasesCommand = new RelayCommand<object>((nullArgument) => StartTestCases(), (nullArgument) => CanStartTestCases());
            LoadTestcasesCommand = new RelayCommand<object>((nullArgument) => LoadTestCases());
        }

        private void LoadTestCases()
        {
            var filename = _fileSelector.GetFilename("jsonTest");

            if(string.IsNullOrWhiteSpace(filename))
            {
                // Es wurde keine Datei ausgewählt.
                return;
            }

            if (System.IO.File.Exists(filename) == false)
            {
                _messageDialog.ShowMessage("Die ausgewählte Datei existiert nicht.", "CodeRunner", MessageDialogIcons.Error);
                return;
            }

            TestCases.Clear();

            List<TestCase> testCases = null;
            try
            {
                var fileContent = System.IO.File.ReadAllText(filename);
                testCases = JsonConvert.DeserializeObject<List<TestCase>>(fileContent);
            }
            catch(JsonException ex)
            {
                _messageDialog.ShowMessage($"Beim Deserialisieren des Json-Testfall ist ein Fehler aufgetreten:{Environment.NewLine}{Environment.NewLine}{ex.ToString()}", "CodeRunner", MessageDialogIcons.Error);
                return;
            }

            foreach (var test in testCases)
                TestCases.Add(test);
        }

        private void StartTestCases()
        {

        }

        private bool CanStartTestCases()
        {
            return string.IsNullOrWhiteSpace(Command) && TestCases.Count > 0;
        }
    }
}
