using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace WebApiServer.Helpers
{
    public enum StringContentType
    {
        DateTime,
        Coordinate,
        Double,
        Int,
        String
    }

    public class TypeGuesser
    {
        public StringContentType Guess(string s)
        {
            DateTime dtResult;
            if (DateTime.TryParse(s, out dtResult))
                return StringContentType.DateTime;
            long lResult;
            if (Int64.TryParse(s, out lResult))
                return StringContentType.Int;
            double dResult;
            if (Double.TryParse(s, out dResult))
                return StringContentType.Double;
        }
    }
}
