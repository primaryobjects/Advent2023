using Managers;

// Read schematic input.
var lines = CommonManager.ReadInput(Path.Combine(Directory.GetCurrentDirectory(), "../../../input.txt"));

// Build a map, including coordinates of the symbols.
var mapping = CommonManager.Analyze(lines);

// Find all numbers that are adjacent to the symbols.
var values = CommonManager.FindAdjacent(mapping);
values.ForEach(Console.WriteLine);

Console.WriteLine(values.Sum());

Console.WriteLine(CommonManager.FindAdjacentGears(mapping).Sum());