using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;

namespace WpfFrontend.Helpers
{
    public class Win32MessageDialog : IMessageDialog
    {
        public void ShowMessage(string message, string header, MessageDialogIcons icon)
        {
            MessageBox.Show(message, header, MessageBoxButton.OK, MessageDialogIconsToMessageBoxImage(icon));
        }

        private MessageBoxImage MessageDialogIconsToMessageBoxImage(MessageDialogIcons icon)
        {
            switch (icon)
            {
                case MessageDialogIcons.Alert:
                    return MessageBoxImage.Exclamation;
                case MessageDialogIcons.Error:
                    return MessageBoxImage.Error;
                case MessageDialogIcons.Info:
                    return MessageBoxImage.Information;
                default:
                    throw new NotImplementedException($"Es wurde versucht das unbekannte MessageDialogIcon {icon} zu verwenden.");
            }
        }
    }
}
