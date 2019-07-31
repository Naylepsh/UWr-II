namespace Zad_1
{
    public static class HelperFunctions
    {
        public static bool IsPrime(int n)
        {
            if (n < 2) return false;
            if (n == 2) return true;
            for (var i = 2; i * i <= n; i++)
                if (n % i == 0) return false;
            return true;
        }
    }
}