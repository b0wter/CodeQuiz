using NLog;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using CodeRunner.Helpers;

namespace CodeRunner.ViewModels
{
    public class BaseViewModel : INotifyPropertyChanged
    {
        /// <summary>
        /// Logger instance for this class.
        /// </summary>
        protected static Logger Logger = LogManager.GetCurrentClassLogger();

        private bool _isInitialized;
        /// <summary>
        /// Returns wether the viewmodel is in an initializing state.
        /// </summary>
        public bool IsInitialized { get { return _isInitialized; } protected set { _isInitialized = value; NotifyPropertyChanged(); } }

        private bool isBusy;
        /// <summary>
        /// Returns wether the viewmodel is performing a long-running task.
        /// </summary>
        public bool IsBusy { get { return isBusy; } protected set { isBusy = value; NotifyPropertyChanged(); } }

        private string errorMessage;
        /// <summary>
        /// Error message, to be displayed in the UI.
        /// </summary>
        public string ErrorMessage { get { return errorMessage; } set { errorMessage = value; NotifyPropertyChanged(); NotifyPropertyChanged("HasError"); } }

        /// <summary>
        /// Retuns wether the viewmodel has encountered an error. 
        /// </summary>
        /// <remarks>
        /// Returns true if an <see cref="ErrorMessage"/> is set.
        /// </remarks>
        public bool HasError => !string.IsNullOrWhiteSpace(ErrorMessage);

        /// <summary>
        /// Deletes the current <see cref="ErrorMessage"/>.
        /// </summary>
        public RelayCommand<object> ClearErrorCommand { get; set; }

        protected BaseViewModel()
        {
            IsInitialized = false;
            IsBusy = false;
            ClearErrorCommand = new RelayCommand<object>((nullObject) => ClearErrorMessage());
        }

        /// <summary>
        /// Deletes the current <see cref="ErrorMessage"/>.
        /// </summary>
        protected virtual void ClearErrorMessage()
        {
            ErrorMessage = string.Empty;
        }

        #region INotifyPropertyChanged Implementation
        public event PropertyChangedEventHandler PropertyChanged;
        protected virtual void NotifyPropertyChanged([CallerMemberName] string propertyName = null)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
        #endregion
    }
}
