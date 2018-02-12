using CommandLine;
using System;
using System.Collections.Generic;
using System.Text;

namespace ConsoleFrontend.Helpers
{
    internal class ConfigOptions
    {
        [Option('c', "config", Required = true, HelpText = "Configuration file containing command, argument and test reference.")]
        public string ConfigFile { get; set; }
    }

    internal class StartupOptions
    {
        [Option('a', "argument", Required = false, HelpText = "Arguments to pass to the command.")]
        public string Argument { get; set; }
        [Option('c', "command", Required = true, HelpText = "Command to run. Should be double quoted.")]
        public string Command { get; set; }
        [Option('t', "test", Required = true, HelpText = "Name of the jsonTest file.")]
        public string Test { get; set; }

        public bool IsValid => string.IsNullOrWhiteSpace(Command) == false && string.IsNullOrWhiteSpace(Test) == false;
    }
}
