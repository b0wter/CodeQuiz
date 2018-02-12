using CommandLine;
using CommandLine.Text;
using ConsoleFrontend.Helpers;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using TestRunner.Models;

namespace ConsoleFrontend
{
    class Program
    {
        private static CancellationTokenSource _cancellationTokenSource = new CancellationTokenSource();
        private static CancellationToken _cancellationToken = _cancellationTokenSource.Token;
        private const int DefaultExecutionTimeout = 5000;

        static async Task Main(string[] args)
        {
            var parser = new Parser(config => config.HelpWriter = null);
            var results = parser.ParseArguments<StartupOptions>(args)
                          .WithParsed(async options => await StartFromStartupOptions(options))
                          .WithNotParsed(errors =>
                                Parser.Default.ParseArguments<ConfigOptions>(args)
                                .WithParsed(async options => await StartFromConfigOptions(options))
                                .WithNotParsed(errpors => OnStartupParameterError())
                          );

            while (_cancellationToken.IsCancellationRequested == false)
                await Task.Delay(250);
        }

        private static async Task StartFromStartupOptions(StartupOptions options)
        {
            var tests = LoadTestCasesFromFile(options.Test);
            await Run(options.Command, options.Argument, tests);
        }

        private static async Task StartFromConfigOptions(ConfigOptions options)
        {
            var content = File.ReadAllText(options.ConfigFile);
            var config = JsonConvert.DeserializeObject<StartupOptions>(content);
            var tests = LoadTestCasesFromFile(config.Test);
            await Run(config.Command, config.Argument, tests);
        }

        private static async Task Run(string command, string argument, IEnumerable<TestCase> tests)
        {
            var runner = new ProcessTestRunner();

            int counter = 0;
            foreach(var test in tests)
            {
                Console.WriteLine($"Starting test {counter++} of {tests.Count()}");
                var result = await runner.Run(command, argument, test.Input, test.ExpectedOutput, DefaultExecutionTimeout);
                Console.WriteLine(result.ToConsoleOutput());
            }

            _cancellationTokenSource.Cancel();
        }

        private static IEnumerable<TestCase> LoadTestCasesFromFile(string filename)
        {
            // Kein Error Handling, die Exceptions werden automatisch geworfen, wenn die Datei nicht existiert.
            var content = File.ReadAllText(filename);
            return JsonConvert.DeserializeObject<List<TestCase>>(content);
        }

        private static void OnStartupParameterError()
        {
            Console.WriteLine("You have not supplied correct startup parameters. You can either use a configuration file written in the following format:");
            Console.WriteLine();
            Console.WriteLine("{");
            Console.WriteLine("    \"command\": \"$Command_To_Run\"");
            Console.WriteLine("    \"argument\": \"$Argument_To_Add_To_Command\" (optional)");
            Console.WriteLine("    \"test\": \"$Filename_of_jsonTest-File\"");
            Console.WriteLine("}");
            Console.WriteLine();
            Console.WriteLine("And supply the path to the file with '-c'");
            Console.WriteLine();
            Console.WriteLine($"Alternatively, use {Environment.NewLine}\t-c to supply a command (double quoted){Environment.NewLine}\t-a to specify arguments added to the command (double quoted){Environment.NewLine}\t-t path to the jsonTest-file");

            _cancellationTokenSource.Cancel();
        }
    }
}
