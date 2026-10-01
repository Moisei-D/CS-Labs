using System.Text;

namespace Lab01;

public static class Caesar
{
    private const string Alphabet = "AĂÂBCDEFGHIÎJKLMNOPQRSȘTȚUVWXYZ";
    private const int MinimumShift = 1;
    private const int MaximumShift = 30;
    private const int MinimumKeywordLength = 7;

    private static string Transform(string text, int shift, string activeAlphabet, bool encrypt)
    {
        var result = new StringBuilder(text.Length);
        int direction = encrypt ? 1 : -1;

        foreach (char letter in text)
        {
            int index = activeAlphabet.IndexOf(letter);
            int newIndex = (index + direction * shift + activeAlphabet.Length) % activeAlphabet.Length;
            result.Append(activeAlphabet[newIndex]);
        }

        return result.ToString();
    }

    private static string BuildPermutedAlphabet(string keyword)
    {
        return new string((keyword + Alphabet).Distinct().ToArray());
    }

    private static string NormalizeRomanianLetters(string value)
    {
        return value
            .Normalize(NormalizationForm.FormC)
            .ToUpperInvariant()
            .Replace('Ş', 'Ș')
            .Replace('Ţ', 'Ț');
    }

    private static string ReadMenuChoice(string prompt, params string[] allowedChoices)
    {
        while (true)
        {
            Console.Write(prompt);
            string choice = (Console.ReadLine() ?? string.Empty).Trim().ToUpperInvariant();

            if (allowedChoices.Contains(choice))
            {
                return choice;
            }

            Console.WriteLine($"Invalid choice. Allowed values: {string.Join(", ", allowedChoices)}.");
        }
    }

    private static int ReadShift()
    {
        while (true)
        {
            Console.Write($"Enter the numeric key ({MinimumShift}-{MaximumShift}): ");
            string input = Console.ReadLine() ?? string.Empty;

            if (int.TryParse(input, out int shift) && shift is >= MinimumShift and <= MaximumShift)
            {
                return shift;
            }

            Console.WriteLine($"Invalid key. Enter an integer from {MinimumShift} to {MaximumShift} inclusive.");
        }
    }

    private static string ReadKeyword()
    {
        while (true)
        {
            Console.Write($"Enter the permutation keyword (at least {MinimumKeywordLength} Romanian letters): ");
            string keyword = NormalizeRomanianLetters(Console.ReadLine() ?? string.Empty);
            char invalidCharacter = keyword.FirstOrDefault(letter => !Alphabet.Contains(letter));
            
            if (invalidCharacter != '\0')
            {
                Console.WriteLine(
                    $"Invalid keyword character: '{invalidCharacter}'. Use only Romanian alphabet letters: {Alphabet}.");
                continue;
            }

            if (keyword.Length < MinimumKeywordLength)
            {
                Console.WriteLine($"Invalid keyword. It must contain at least {MinimumKeywordLength} letters.");
                continue;
            }

            return keyword;
        }
    }

    private static string ReadText()
    {
        while (true)
        {
            Console.Write("Enter the text (spaces are removed): ");
            string text = NormalizeRomanianLetters(Console.ReadLine() ?? string.Empty);
            char invalidCharacter = text.FirstOrDefault(character => character != ' ' && !Alphabet.Contains(character));

            if (invalidCharacter != '\0')
            {
                Console.WriteLine(
                    $"Invalid text character: '{invalidCharacter}'. Use only spaces and Romanian alphabet letters: {Alphabet}.");
                continue;
            }

            string textWithoutSpaces = text.Replace(" ", string.Empty);
            if (textWithoutSpaces.Length == 0)
            {
                Console.WriteLine("Invalid text. Enter at least one Romanian alphabet letter.");
                continue;
            }

            return textWithoutSpaces;
        }
    }

    public static void Run()
    {
        while (true)
        {
            Console.WriteLine();
            string operation = ReadMenuChoice(
                "Choose an operation: [E]ncrypt, [D]ecrypt, or [X] Exit: ",
                "E", "D", "X", "EXIT");

            if (operation is "X" or "EXIT")
            {
                Console.WriteLine("Goodbye!");
                return;
            }

            string permutationChoice = ReadMenuChoice(
                "Use a keyword-permuted alphabet? [Y]es/[N]o: ",
                "Y", "N");

            string activeAlphabet = permutationChoice == "Y"
                ? BuildPermutedAlphabet(ReadKeyword())
                : Alphabet;

            Console.WriteLine(permutationChoice == "Y"
                ? $"Permuted alphabet: {activeAlphabet}"
                : $"Romanian alphabet: {activeAlphabet}");

            int shift = ReadShift();
            string text = ReadText();
            bool encrypt = operation == "E";
            string result = Transform(text, shift, activeAlphabet, encrypt);

            Console.WriteLine(encrypt ? $"Encrypted text: {result}" : $"Decrypted text: {result}");
        }
    }
}
