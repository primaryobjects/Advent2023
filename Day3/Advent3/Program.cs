using Managers;

// Read schematic input.
var lines = CommonManager.ReadInput(Path.Combine(Directory.GetCurrentDirectory(), "../../../input.txt"));

// Build a map, including coordinates of the symbols.
var mapping = CommonManager.Analyze(lines);

// Find all numbers that are adjacent to the symbols.
Console.WriteLine(CommonManager.FindParts(mapping).Sum());
Console.WriteLine(CommonManager.FindGears(mapping).Sum());