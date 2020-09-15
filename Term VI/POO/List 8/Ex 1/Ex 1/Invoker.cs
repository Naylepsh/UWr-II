using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace Ex_1
{
    public class Invoker
    {
        private Queue<ICommand> _commands = new Queue<ICommand>();
        private bool _isThreaded;

        public Invoker(bool isThreaded = false)
        {
            _isThreaded = isThreaded; 
            if (_isThreaded)
            {
                var executer1 = new Thread(RemoveFromQueueAndExecute);
                var executer2 = new Thread(RemoveFromQueueAndExecute);

                executer1.Start();
                executer2.Start();
            }
        }

        private void RemoveFromQueueAndExecute()
        {

            while (true)
            {
                if (_commands.Count == 0)
                {
                    Thread.Sleep(50);
                }
                else
                {
                    lock (_commands)
                    {
                        if (_commands.Count > 0)
                        {
                            var command = _commands.Dequeue();
                            command.Execute();
                        }
                    }
                }
            }
        }

        public void Execute(ICommand command)
        {
            if (!_isThreaded)
            {
                command.Execute();
            }
            else
            {
                _commands.Enqueue(command);
            }
        }
    }
}
