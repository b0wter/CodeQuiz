using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Data;

namespace WpfFrontend.Converters
{
    /// <summary>
    /// Abstract base converter that contains all logic to implement any bool-based converters.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public abstract class BoolToValueConverter<T> : IValueConverter
    {
        public abstract T TrueValue { get; set; }
        public abstract T FalseValue { get; set; }
        public abstract T NullValue { get; set; }

        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            if (value == null)
                return NullValue;

            if (value.GetType() != typeof(bool))
                throw new ArgumentException($"The parameter is of the type {value.GetType()}, but the converter only works on bools.");

            bool converted = (bool)value;
            return converted == true ? TrueValue : FalseValue;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }

    /// <summary>
    /// Converts a bool to a Visibility value.
    /// </summary>
    public class BoolToVisibilityConverter : BoolToValueConverter<System.Windows.Visibility>
    {
        public override Visibility TrueValue { get; set; } = Visibility.Visible;
        public override Visibility FalseValue { get; set; } = Visibility.Collapsed;
        public override Visibility NullValue { get; set; } = Visibility.Collapsed;
    }
}
