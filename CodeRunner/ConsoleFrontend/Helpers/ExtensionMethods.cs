using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;
using TestRunner.Models;

namespace ConsoleFrontend.Helpers
{
    public static class ExtensionMethods
    {
        public static string ToConsoleOutput(this TestResult result)
        {
            var builder = new StringBuilder();
            builder.Append($"Duration:  {result.Duration}ms");
            builder.Append(Environment.NewLine);
            builder.Append($"Started?:  {result.RunSuccessful}");
            builder.Append(Environment.NewLine);
            builder.Append($"Success:   {result.OutputMatches}");
            builder.Append(Environment.NewLine);

            if (result.Exception != null)
            {
                builder.Append($"Exception:{Environment.NewLine}{result.Exception}");
                builder.Append(Environment.NewLine);
            }

            if (result.OutputMatches == false)
            {
                builder.AppendLine($"Reason:    {result.FailReason}");
                builder.AppendLine($"Expected:");
                builder.AppendLine(string.Join(Environment.NewLine, result.ExpectedOutput));
                builder.AppendLine("Received:");
                builder.Append(string.Join(Environment.NewLine, result.Output));
            }

            return builder.ToString();
        }
    }
}
