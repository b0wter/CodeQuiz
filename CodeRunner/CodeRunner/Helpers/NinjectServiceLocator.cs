using Ninject;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using CodeRunner.ViewModels;

namespace CodeRunner.Helpers
{
    /// <summary>
    /// The <see cref="NinjectMainViewModelLocator"/> is used to be able to initialize the <see cref="MainViewModel"/> for the <see cref="Windows.MainWindow"/> in XAML.
    /// All additional bindings are set in <see cref="Helpers.NinjectBindings"/>.
    /// </summary>
    public class NinjectMainViewModelLocator
    {
        private readonly IKernel _kernel;

        public NinjectMainViewModelLocator()
        {
            _kernel = new StandardKernel(new NinjectBindings());
        }

        /// <summary>
        /// Returns an instance of <see cref="MainViewModel"/> created by the Ninject kernel.
        /// </summary>
        public MainViewModel MainViewModel
        {
            get { return _kernel.Get<MainViewModel>(); }
        }
    }
}
