using Ninject.Modules;
using NLog;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodeRunner.Helpers
{
    public class NinjectBindings : NinjectModule
    {
        /// <summary>
        /// Logger Instanz for this class.
        /// </summary>
        protected static Logger Logger = LogManager.GetCurrentClassLogger();

        /// <summary>
        /// Is called by the kernel, if this module is used for initialization.
        /// </summary>
        public override void Load()
        {
            Logger.Info("Loading Ninject Bindings.");
        }
    }
}
