using System;

namespace Ex_3
{
    // Concrete mockup classes (XML and Database) are in the test file

    public interface IDataAccessStrategy<T>
    {
        void Open();
        void ReadData();
        void ProcessData();
        void Close();
        T GetResult();
    }

    public class DataAccessHandler<T>
    {
        private IDataAccessStrategy<T> accessStrategy;
        
        public DataAccessHandler(IDataAccessStrategy<T> accessor)
        {
            accessStrategy = accessor;
        }

        public void Execute()
        {
            accessStrategy.Open();
            accessStrategy.ReadData();
            accessStrategy.ProcessData();
            accessStrategy.Close();
        }

        public T Result
        {
            get
            {
                return accessStrategy.GetResult();
            }
        }
    }
}
