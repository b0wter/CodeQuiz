using Microsoft.Win32;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace WpfFrontend.Helpers
{
    public class Win32SelectFileDialog : ISelectFileDialog
    {
        public string GetFilename(string extension = null)
        {
            if (extension == null || string.IsNullOrWhiteSpace(extension))
                extension = "Alle Dateien (*.*)|*.*";
            else
                extension = $"{extension.ToUpper()}-Datei (*.{extension})|*.{extension}";

            var dialog = new OpenFileDialog
            {
                Filter = extension,
                Multiselect = false
            };

            var result = dialog.ShowDialog();

            if (result == true)
                return dialog.FileName;
            else
                return null;
        }
    }
}
