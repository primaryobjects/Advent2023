using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Net.Mime;
using System.Dynamic;
using System.Linq.Expressions;

namespace Managers
{
    // 521515
    // 69527306

    public static class CommonManager
    {
        #region Objects
        public struct Coordinate
        {
            public Coordinate(char symbol, int x, int y)
            {
                this.Symbol = symbol;
                this.X = x;
                this.Y = y;
            }

            public char Symbol { get; }
            public int X { get; }
            public int Y { get; }
        }

        public class Mapping
        {
            public HashSet<Coordinate> Hash = new HashSet<Coordinate>();
            public char[,] Map;

            public Mapping(int width, int height)
            {
                Map = new char[height, width];
            }
        }
        #endregion

        public static List<string> ReadInput(string filePath = "input.txt")
        {
            List<string> content = new();

            using (var fs = File.Open(filePath, FileMode.Open, FileAccess.Read))
            {
                using (var sr = new StreamReader(fs))
                {
                    string? line;
                    while ((line = sr.ReadLine()) != null)
                    {
                        content.Add(line);
                    }
                }
            }

            return content;
        }

        private static bool _isSymbol(char ch)
        {
            return ch != '.' && !Char.IsDigit(ch);
        }

        public static Mapping Analyze(List<string> content)
        {
            Mapping mapping = new Mapping(content[0].Length, content.Count);

            int x, y = 0;

            // Enumerate each line in the content.
            foreach (var line in content)
            {
                x = 0;

                // Enumerate each character in the line.
                foreach (var ch in line)
                {
                    // If we have a symbol, store it in the hash.
                    if (_isSymbol(ch))
                    {
                        // Store the symbol coordinate.
                        mapping.Hash.Add(new Coordinate(ch, x, y));
                    }

                    // Add the character to the overall map.
                    mapping.Map[y,x] = ch;

                    x++;
                }

                y++;
            }

            return mapping;
        }

        private static int _getNumber(string line, int index)
        {
            // Starting at line[index], find the left-most separator character (or 0-index) and then return the entire value up until the next separator (or end of line).
            if (index >= line.Length)
            {
                return 0;
            }

            int start = index;
            char ch = line[start];
            while (start >= 0 && Char.IsDigit(ch))
            {
                start--;
                if (start > -1)
                {
                    ch = line[start];
                }
            }

            // Start on the number.
            start++;

            // Find the index of the next separator.
            int end = start;
            while (end < line.Length && Char.IsDigit(line[end]))
            {
                end++;
            }

            return Int32.Parse(line.Substring(start, end - start));
        }

        private static string _getLine(char[,] chars, int y)
        {
            // Get the line.
            string line = "";
            for (int x = 0; x < chars.GetLength(0); x++)
            {
                line += chars[y, x];
            }

            return line;
        }

        private static List<int> _findAdjacentNumbers(Mapping mapping, Coordinate item)
        {
            List<int> numbers = new(8) { 0, 0, 0, 0, 0, 0, 0, 0 };

            // Up Left.
            if (item.Y > 0 && item.X > 0 && Char.IsDigit(mapping.Map[item.Y - 1, item.X - 1]))
            {
                // Get the full line.
                string line = _getLine(mapping.Map, item.Y - 1);

                // Extract the adjacent number.
                int number = _getNumber(line, item.X - 1);
                if (number > 0)
                {
                    numbers[0] = number;
                }
            }
            // Up.
            if (item.Y > 0 && Char.IsDigit(mapping.Map[item.Y - 1, item.X]))
            {
                // Get the full line.
                string line = _getLine(mapping.Map, item.Y - 1);

                // Extract the adjacent number.
                int number = _getNumber(line, item.X);
                if (number > 0)
                {
                    numbers[1] = number;
                }
            }
            // Up Right.
            if (item.Y > 0 && item.X < mapping.Map.GetLength(1) - 1 && Char.IsDigit(mapping.Map[item.Y - 1, item.X + 1]))
            {
                // Get the full line.
                string line = _getLine(mapping.Map, item.Y - 1);

                // Extract the adjacent number.
                int number = _getNumber(line, item.X + 1);
                if (number > 0)
                {
                    numbers[2] = number;
                }
            }
            // Right.
            if (item.X < mapping.Map.GetLength(1) - 1 && Char.IsDigit(mapping.Map[item.Y, item.X + 1]))
            {
                // Get the full line.
                string line = _getLine(mapping.Map, item.Y);

                // Extract the adjacent number.
                int number = _getNumber(line, item.X + 1);
                if (number > 0)
                {
                    numbers[3] = number;
                }
            }
            // Down Right.
            if (item.Y < mapping.Map.GetLength(0) - 1 && item.X < mapping.Map.GetLength(1) - 1 && Char.IsDigit(mapping.Map[item.Y + 1, item.X + 1]))
            {
                // Get the full line.
                string line = _getLine(mapping.Map, item.Y + 1);

                // Extract the adjacent number.
                int number = _getNumber(line, item.X + 1);
                if (number > 0)
                {
                    numbers[4] = number;
                }
            }
            // Down.
            if (item.Y < mapping.Map.GetLength(0) - 1 && Char.IsDigit(mapping.Map[item.Y + 1, item.X]))
            {
                // Get the full line.
                string line = _getLine(mapping.Map, item.Y + 1);

                // Extract the adjacent number.
                int number = _getNumber(line, item.X);
                if (number > 0)
                {
                    numbers[5] = number;
                }
            }
            // Down Left.
            if (item.Y < mapping.Map.GetLength(0) - 1 && item.X > 0 && Char.IsDigit(mapping.Map[item.Y + 1, item.X - 1]))
            {
                // Get the full line.
                string line = _getLine(mapping.Map, item.Y + 1);

                // Extract the adjacent number.
                int number = _getNumber(line, item.X - 1);
                if (number > 0)
                {
                    numbers[6] = number;
                }
            }
            // Left.
            if (item.X > 0 && Char.IsDigit(mapping.Map[item.Y, item.X - 1]))
            {
                // Get the full line.
                string line = _getLine(mapping.Map, item.Y);

                // Extract the adjacent number.
                int number = _getNumber(line, item.X - 1);
                if (number > 0)
                {
                    numbers[7] = number;
                }
            }

            // If we have an Up, discard Up-left and Up-Right. If we have a Down, discard Down-Left and Down-Right.
            if (numbers[1] > 0)
            {
                numbers[0] = 0;
                numbers[2] = 0;
            }

            if (numbers[5] > 0)
            {
                numbers[6] = 0;
                numbers[4] = 0;
            }

            return numbers;
        }

        /// <summary>
        /// Find all adjacent numbers to any symbol in the map and return a list for the sum of the values.
        /// </summary>
        /// <param name="mapping">Mapping</param>
        /// <returns>List of int</returns>
        public static List<int> FindAdjacent(Mapping mapping)
        {
            List<int> result = new();

            foreach (var item in mapping.Hash)
            {
                // Find all adjacent numbers to the current symbol.
                List<int> numbers = _findAdjacentNumbers(mapping, item);

                // Sum all numbers.
                result.AddRange(numbers.Where(number => number > 0));
            }

            return result;
        }

        /// <summary>
        /// Find all adjacent numbers to the symbol '*' where there are two or more numbers and return a list for the product of the values.
        /// </summary>
        /// <param name="mapping">Mapping</param>
        /// <returns>List of int</returns>
        public static List<int> FindAdjacentGears(Mapping mapping)
        {
            List<int> result = new();

            foreach (var item in mapping.Hash.Where(item => item.Symbol == '*'))
            {
                // Find all adjacent numbers to the current '*' symbol.
                List<int> numbers = _findAdjacentNumbers(mapping, item);

                // Check if there are at least two values.
                int count = numbers.Where(number => number > 0).Count();
                if (count >= 2)
                {
                    // Multiply the numbers.
                    result.Add(numbers.Where(number => number > 0).Aggregate((a, b) => a * b));
                }
            }

            return result;
        }
    }
}