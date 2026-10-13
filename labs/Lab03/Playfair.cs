using System.Text;

namespace Lab03;

public static class Playfair
{
    private const string Alphabet = "AĂÂBCDEFGHIÎJKLMNOPQRSȘTȚUVWXYZ";
    private const char EmptyCell = '#';
    private const int Rows = 4;
    private const int Columns = 8;

    private static readonly char[,] Matrix = new char[Rows, Columns];
    private static readonly Dictionary<char, (int Row, int Column)> Positions = new();

    public static void Run()
    {
        Console.OutputEncoding = Encoding.UTF8;
        Console.InputEncoding = Encoding.UTF8;
        Console.WriteLine($"Alfabet: {Alphabet}");
        while (true)
        {
            Console.WriteLine();
            string operation = ReadOperation();

            if (operation is "X" or "EXIT")
            {
                Console.WriteLine("La revedere!");
                return;
            }

            string key = ReadKey();
            BuildMatrix(key);

            Console.WriteLine("\nMatricea Playfair:");
            PrintMatrix();

            if (operation == "E")
            {
                EncryptMessage();
            }
            else
            {
                DecryptMessage();
            }
        }
    }

    private static string ReadOperation()
    {
        while (true)
        {
            Console.Write("Alegeți operația: [E] Criptare, [D] Decriptare sau [X] Ieșire: ");

            string operation = (Console.ReadLine() ?? string.Empty)
                .Trim()
                .ToUpperInvariant();

            if (operation is "E" or "D" or "X" or "EXIT")
            {
                return operation;
            }

            Console.WriteLine("Opțiune invalidă. Valorile permise sunt E, D și X.");
        }
    }

    private static string ReadKey()
    {
        while (true)
        {
            Console.Write("Introduceți cheia (minimum 7 litere): ");

            try
            {
                string key = Normalize(Console.ReadLine() ?? string.Empty);

                if (key.Length < 7)
                {
                    Console.WriteLine(
                        "Cheie invalidă. Introduceți cel puțin 7 litere."
                    );
                    continue;
                }

                return key;
            }
            catch (ArgumentException exception)
            {
                Console.WriteLine($"Eroare: {exception.Message}");
            }
        }
    }

    private static void EncryptMessage()
    {
        while (true)
        {
            Console.Write("\nIntroduceți mesajul: ");

            try
            {
                string message = Console.ReadLine() ?? string.Empty;
                string preparedMessage = PreparePlaintext(message);
                string cryptogram = Transform(preparedMessage, encrypt: true);

                Console.WriteLine($"Mesaj pregătit: {preparedMessage}");
                Console.WriteLine($"Criptograma: {cryptogram}");
                return;
            }
            catch (ArgumentException exception)
            {
                Console.WriteLine($"Eroare: {exception.Message}");
                Console.WriteLine("Încercați din nou; cheia și matricea au fost păstrate.");
            }
        }
    }

    private static void DecryptMessage()
    {
        while (true)
        {
            Console.Write("\nIntroduceți criptograma: ");

            try
            {
                string cryptogram = Normalize(
                    Console.ReadLine() ?? string.Empty,
                    allowEmptyCell: true
                );

                if (cryptogram.Length % 2 != 0)
                {
                    throw new ArgumentException(
                        "Criptograma trebuie să conțină un număr par de caractere."
                    );
                }

                string message = Transform(cryptogram, encrypt: false);

                Console.WriteLine($"Mesaj decriptat: {message}");
                Console.WriteLine(
                    "Eliminați manual literele de umplere și adăugați spațiile."
                );
                return;
            }
            catch (ArgumentException exception)
            {
                Console.WriteLine($"Eroare: {exception.Message}");
                Console.WriteLine("Încercați din nou; cheia și matricea au fost păstrate.");
            }
        }
    }

    private static string Normalize(string text, bool allowEmptyCell = false)
    {
        var result = new StringBuilder();

        foreach (char character in text)
        {
            if (char.IsWhiteSpace(character))
            {
                continue;
            }

            char upperCharacter = char.ToUpperInvariant(character);
            bool isValid = Alphabet.Contains(upperCharacter) ||
                           (allowEmptyCell && upperCharacter == EmptyCell);

            if (!isValid)
            {
                throw new ArgumentException(
                    $"Caracterul '{character}' nu este permis. " +
                    $"Folosiți literele alfabetului român: {Alphabet}."
                );
            }

            result.Append(upperCharacter);
        }

        if (result.Length == 0)
        {
            throw new ArgumentException("Textul nu poate fi gol.");
        }

        return result.ToString();
    }

    private static void BuildMatrix(string key)
    {
        string symbols = RemoveDuplicates(key + Alphabet + EmptyCell);
        Positions.Clear();

        for (int index = 0; index < Rows * Columns; index++)
        {
            int row = index / Columns;
            int column = index % Columns;
            char symbol = symbols[index];

            Matrix[row, column] = symbol;
            Positions[symbol] = (row, column);
        }
    }

    private static string RemoveDuplicates(string text)
    {
        var seen = new HashSet<char>();
        var result = new StringBuilder();

        foreach (char character in text)
        {
            if (seen.Add(character))
            {
                result.Append(character);
            }
        }

        return result.ToString();
    }

    private static string PreparePlaintext(string message)
    {
        string normalized = Normalize(message);
        var result = new StringBuilder();

        for (int index = 0; index < normalized.Length;)
        {
            char first = normalized[index];
            char second;

            if (index + 1 >= normalized.Length)
            {
                second = ChooseFiller(first);
                index++;
            }
            else if (first == normalized[index + 1])
            {
                second = ChooseFiller(first);
                index++;
            }
            else
            {
                second = normalized[index + 1];
                index += 2;
            }

            result.Append(first);
            result.Append(second);
        }

        return result.ToString();
    }

    private static char ChooseFiller(char character)
    {
        return character == 'X' ? 'Q' : 'X';
    }

    private static string Transform(string text, bool encrypt)
    {
        var result = new StringBuilder();
        int direction = encrypt ? 1 : -1;

        for (int index = 0; index < text.Length; index += 2)
        {
            char first = text[index];
            char second = text[index + 1];

            (int firstRow, int firstColumn) = Positions[first];
            (int secondRow, int secondColumn) = Positions[second];

            if (firstRow == secondRow)
            {
                result.Append(Matrix[firstRow, Mod(firstColumn + direction, Columns)]);
                result.Append(Matrix[secondRow, Mod(secondColumn + direction, Columns)]);
            }
            else if (firstColumn == secondColumn)
            {
                result.Append(Matrix[Mod(firstRow + direction, Rows), firstColumn]);
                result.Append(Matrix[Mod(secondRow + direction, Rows), secondColumn]);
            }
            else
            {
                result.Append(Matrix[firstRow, secondColumn]);
                result.Append(Matrix[secondRow, firstColumn]);
            }
        }

        return result.ToString();
    }

    private static int Mod(int value, int modulus)
    {
        return (value % modulus + modulus) % modulus;
    }

    private static void PrintMatrix()
    {
        for (int row = 0; row < Rows; row++)
        {
            for (int column = 0; column < Columns; column++)
            {
                Console.Write($"{Matrix[row, column]} ");
            }

            Console.WriteLine();
        }
    }
}
