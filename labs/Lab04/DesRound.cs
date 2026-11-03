using System.Security.Cryptography;
using System.Text;

namespace Lab04;

public static class DesRound
{
    private const int KeyBits = 48;
    private const int HalfBits = 32;
    private const int BlockBits = 6;
    private const int BlockCount = 8;

    // Expansion table E (FIPS 46-3): output bit j is taken from input bit E[j] (1-based).
    private static readonly int[] E =
    {
        32,  1,  2,  3,  4,  5,
         4,  5,  6,  7,  8,  9,
         8,  9, 10, 11, 12, 13,
        12, 13, 14, 15, 16, 17,
        16, 17, 18, 19, 20, 21,
        20, 21, 22, 23, 24, 25,
        24, 25, 26, 27, 28, 29,
        28, 29, 30, 31, 32,  1
    };

    public static void Run()
    {
        PrintExpansionTable();

        while (true)
        {
            Console.WriteLine();
            string choice = ReadChoice();
            if (choice == "X")
            {
                Console.WriteLine("Goodbye!");
                return;
            }

            int round;
            int[] k;
            int[] r;

            if (choice == "R")
            {
                round = RandomNumberGenerator.GetInt32(1, 17);
                k = RandomBits(KeyBits);
                r = RandomBits(HalfBits);
                Console.WriteLine($"Generated round i = {round}");
            }
            else
            {
                round = ReadRound();
                k = ReadBits($"Enter K{round} (48 bits or 12 hex digits): ", KeyBits);
                r = ReadBits($"Enter R{round - 1} (32 bits or 8 hex digits): ", HalfBits);
            }

            Compute(round, k, r);
        }
    }

    private static void Compute(int round, int[] k, int[] r)
    {
        Console.WriteLine("\n=== Inputs ===");
        Console.WriteLine($"K{round}     = {Group(k, BlockBits)}  (hex {ToHex(k)})");
        Console.WriteLine($"R{round - 1}     = {Group(r, 4)}  (hex {ToHex(r)})");

        Console.WriteLine("\n=== Step 1: expansion E(R" + (round - 1) + ") ===");
        var expanded = new int[KeyBits];
        for (int j = 0; j < KeyBits; j++)
        {
            expanded[j] = r[E[j] - 1];
        }
        Console.WriteLine("Each output bit j is copied from input bit E[j]:");
        for (int b = 0; b < BlockCount; b++)
        {
            string sources = string.Join(",", Enumerable.Range(0, BlockBits).Select(c => E[b * BlockBits + c]));
            string bits = string.Concat(Enumerable.Range(0, BlockBits).Select(c => expanded[b * BlockBits + c]));
            Console.WriteLine($"  bits {b * BlockBits + 1,2}-{(b + 1) * BlockBits,2}  <- R positions {sources,-20} = {bits}");
        }
        Console.WriteLine($"E(R{round - 1}) = {Group(expanded, BlockBits)}");

        Console.WriteLine($"\n=== Step 2: E(R{round - 1}) XOR K{round} ===");
        var xor = new int[KeyBits];
        for (int j = 0; j < KeyBits; j++)
        {
            xor[j] = expanded[j] ^ k[j];
        }
        Console.WriteLine($"E(R{round - 1}) = {Group(expanded, BlockBits)}");
        Console.WriteLine($"K{round}     = {Group(k, BlockBits)}");
        Console.WriteLine($"XOR      = {Group(xor, BlockBits)}");

        Console.WriteLine("\n=== Step 3: split into eight 6-bit blocks ===");
        for (int b = 0; b < BlockCount; b++)
        {
            string bits = string.Concat(Enumerable.Range(0, BlockBits).Select(c => xor[b * BlockBits + c]));
            int value = Convert.ToInt32(bits, 2);
            Console.WriteLine($"B{b + 1} = {bits}  (decimal {value,2})");
        }

        string all = string.Join(" ", Enumerable.Range(0, BlockCount)
            .Select(b => string.Concat(Enumerable.Range(0, BlockBits).Select(c => xor[b * BlockBits + c]))));
        Console.WriteLine($"\nB1B2B3B4B5B6B7B8 = {all}");
    }

    private static void PrintExpansionTable()
    {
        Console.WriteLine("\nExpansion table E:");
        for (int row = 0; row < BlockCount; row++)
        {
            Console.WriteLine("  " + string.Join(" ", Enumerable.Range(0, BlockBits).Select(c => E[row * BlockBits + c].ToString().PadLeft(2))));
        }
    }

    private static string ReadChoice()
    {
        while (true)
        {
            Console.Write("Choose input mode: [T] type values, [R] random, [X] exit: ");
            string choice = (Console.ReadLine() ?? string.Empty).Trim().ToUpperInvariant();
            if (choice is "T" or "R" or "X")
            {
                return choice;
            }
            Console.WriteLine("Invalid option. Allowed values are T, R and X.");
        }
    }

    private static int ReadRound()
    {
        while (true)
        {
            Console.Write("Enter round number i (1-16): ");
            if (int.TryParse(Console.ReadLine(), out int round) && round is >= 1 and <= 16)
            {
                return round;
            }
            Console.WriteLine("Invalid round. Enter an integer from 1 to 16.");
        }
    }

    /// <summary>Reads a value given either as binary (length = bits) or hex (length = bits/4).</summary>
    private static int[] ReadBits(string prompt, int bits)
    {
        while (true)
        {
            Console.Write(prompt);
            string text = (Console.ReadLine() ?? string.Empty)
                .Replace(" ", string.Empty)
                .Replace("_", string.Empty);

            if (text.Length == bits && text.All(c => c is '0' or '1'))
            {
                return text.Select(c => c - '0').ToArray();
            }

            if (text.Length == bits / 4 && text.All(Uri.IsHexDigit))
            {
                return text.SelectMany(c =>
                    Convert.ToString(Convert.ToInt32(c.ToString(), 16), 2).PadLeft(4, '0'))
                    .Select(c => c - '0').ToArray();
            }

            Console.WriteLine($"Invalid input. Provide exactly {bits} binary digits or {bits / 4} hex digits.");
        }
    }

    private static int[] RandomBits(int count)
    {
        var bits = new int[count];
        for (int i = 0; i < count; i++)
        {
            bits[i] = RandomNumberGenerator.GetInt32(2);
        }
        return bits;
    }

    private static string Group(int[] bits, int size)
    {
        var sb = new StringBuilder();
        for (int i = 0; i < bits.Length; i++)
        {
            if (i > 0 && i % size == 0)
            {
                sb.Append(' ');
            }
            sb.Append(bits[i]);
        }
        return sb.ToString();
    }

    private static string ToHex(int[] bits)
    {
        var sb = new StringBuilder();
        for (int i = 0; i < bits.Length; i += 4)
        {
            sb.Append(Convert.ToString(bits[i] * 8 + bits[i + 1] * 4 + bits[i + 2] * 2 + bits[i + 3], 16).ToUpperInvariant());
        }
        return sb.ToString();
    }
}
