using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace CodeRunner.Helpers
{
    static class ExtensionMethods
    {
        public static string RemoveEmptyLines(this string s)
        {
            return Regex.Replace(s, @"^\s*$\n|\r", "", RegexOptions.Multiline).TrimEnd();
        }
    }
}
