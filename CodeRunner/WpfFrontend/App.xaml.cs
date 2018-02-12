using Ninject;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Linq;
using System.Threading.Tasks;
using System.Windows;
using TestRunner.Models;
using WpfFrontend.Helpers;

namespace WpfFrontend
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application
    {
        private readonly IKernel _kernel;

        public App() : base()
        {
            _kernel = new StandardKernel();
            InitKernel();
        }

        protected override void OnStartup(StartupEventArgs e)
        {
            base.OnStartup(e);
            StartWpfFrontEnd();
        }

        private void InitKernel()
        {
            _kernel.Bind<ISelectFileDialog>().To<Win32SelectFileDialog>();
            _kernel.Bind<IMessageDialog>().To<Win32MessageDialog>();
            _kernel.Bind<ITestRunner>().To<ProcessTestRunner>();
        }

        private void StartWpfFrontEnd()
        {
            var window = _kernel.Get<WpfFrontend.Windows.MainWindow>();
            window.Show();
        }
    }
}
