using System;

namespace Ex_2
{
    // Concrete mockup classes (XML and Database) are in the test file

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
