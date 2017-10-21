using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using WpfFrontend.Viewmodels;

namespace WpfFrontend.Helpers
{
    public static class WpfDesignHelpers
    {
        /// <summary>
        /// Gibt ein unvollständig befülltest Viewmodel zurück, damit der WPF Designer Intellisense im XAML anbieten kann.
        /// </summary>
        public static MainViewModel FakeMainViewModel = new MainViewModel(null, null);
    }
}
