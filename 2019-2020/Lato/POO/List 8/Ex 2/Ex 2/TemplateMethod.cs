using System;

namespace Ex_2
{
    // Concrete classes examples (XML and Database) are stored in test file,
    // due to them being mockups.

    public abstract class DataAccessHandler
    {
        protected abstract void Open();
        protected abstract void ReadData();
        protected abstract void ProcessData();
        protected abstract void Close();

        public void Execute()
        {
            Open();
            ReadData();
            ProcessData();
            Close();
        }
    }
}
