using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace WpfFrontend.Helpers
{
    public interface ISelectFileDialog
    {
        /// <summary>
        /// Öffnet einen Dialog um eine Datei auszuwählen.
        /// </summary>
        /// <param name="extension">Dateiendung, angegeben ohne Punkt, z.B.: txt</param>
        /// <returns></returns>
        string GetFilename(string extension);
    }
}
