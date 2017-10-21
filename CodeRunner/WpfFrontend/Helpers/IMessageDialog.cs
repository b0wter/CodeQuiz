using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace WpfFrontend.Helpers
{
    public interface IMessageDialog
    {
        void ShowMessage(string message, string header, MessageDialogIcons icon);
    }

    public enum MessageDialogIcons
    {
        Info,
        Alert,
        Error
    }
}
