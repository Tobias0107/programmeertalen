import csv

MAX_WEIGHT = 110
MAX_VOLUME = 150


class Recources:
    def __init__(self, points, weight, volume):
        self.recources = (points, weight, volume)

    def get_points(self):
        (points, weight, volume) = self.recources
        return points

    def get_weight(self):
        (points, weight, volume) = self.recources
        return weight

    def get_volume(self):
        (points, weight, volume) = self.recources
        return volume

    def get_points_weight_volume(self):
        return self.recources

    def add_volume(self, added_volume):
        (points, weight, volume) = self.recources
        volume += added_volume
        self.recources = (points, weight, volume)

    def add_weight(self, added_weight):
        (points, weight, volume) = self.recources
        weight += added_weight
        self.recources = (points, weight, volume)

    def add_points(self, added_points):
        (points, weight, volume) = self.recources
        points += added_points
        self.recources = (points, weight, volume)


class Item:
    def __init__(self, name, points, weight, volume):
        self.recources = Recources(points, weight, volume)
        self.name = str(name)

    def __repr__(self):
        return self.name

    def get_points(self):
        return self.recources.get_points()

    def get_weight(self):
        return self.recources.get_weight()

    def get_volume(self):
        return self.recources.get_volume()

    def get_name(self):
        return self.name

    def get_points_weight_volume(self):
        return self.recources.get_points_weight_volume()


class Items:
    def __init__(self):
        self.total_recources = Recources(0, 0, 0)
        self.itemlist = []

    def __repr__(self):
        string = ""
        for item in self.itemlist:
            string += (str(item.get_name()) + "\n")
        return string

    def add_item(self, item):
        (points, weight, volume) = self.total_recources
        points += item.get_points()
        weight += item.get_weight()
        volume += item.get_volume()
        self.total_recources = Recources(points, weight, volume)
        self.itemlist.append(item)

    def get_points(self):
        return self.total_recources.get_points()

    def get_weight(self):
        return self.total_recources.get_weight()

    def get_volume(self):
        return self.total_recources.get_volume()

    def get_points_weight_volume(self):
        return self.total_recources.get_points_weight_volume()


class Knapsack:
    def __init__(self, max_weight, max_volume):
        self.max_weight = max_weight
        self.max_volume = max_volume

    def add_items(self, Items):
    # Items should be a list with items of the class items
        self.items = Items

    def get_points(self):
    # The self.items is only initialised if the items are added by the add items
        try:
            return self.items.get_points()
        except:
            return 0

    def save(self, solution_file):
        with open(solution_file + ".svg", mode="a") as solutions_file:
            try:
                points_str = f"points: {self.get_points()}"
                solutions_file.write(points_str)
                solutions_file.writelines(self.items)
            except:
                solutions_file.write("No solution in knapsack yet")


def load_knapsack(knapsack_file):
    All_items = Items()
    with open(knapsack_file + ".csv", mode="r") as item_file:
        csv_reader = csv.DictReader(csv_reader, delimiter=", ")
        for row in csv_reader:
            if (row["name"] == "knapsack"):
                init_Knapsack = Knapsack(row["weight", row["volume"]])
            else:
                Item = Item(row["name"], row["points"], row["weight"], row["volume"])
                All_items.add_item(Item)
    return (init_Knapsack, All_items)


def main():
    solver_random = Solver_Random(1000)
    solver_optimal_recursive = Solver_Optimal_Recursive()
    solver_optimal_iterative_deepcopy = Solver_Optimal_Iterative_Deepcopy()
    solver_optimal_iterative = Solver_Optimal_Iterative()
    solver_random_improved = Solver_Random_Improved(5000)

    knapsack_file = "knapsack_small"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file + "_solution_random.csv")
    solve(solver_optimal_recursive, knapsack_file + ".csv", knapsack_file + "_solution_optimal_recursive.csv")
    solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    solve(solver_optimal_iterative, knapsack_file + ".csv", knapsack_file + "_solution_optimal_iterative.csv")
    solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")

    knapsack_file = "knapsack_medium"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file + "_solution_random.csv")
    solve(solver_optimal_recursive, knapsack_file + ".csv", knapsack_file + "_solution_optimal_recursive.csv")
    solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    solve(solver_optimal_iterative, knapsack_file + ".csv", knapsack_file + "_solution_optimal_iterative.csv")
    solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")

    knapsack_file = "knapsack_large"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file + "_solution_random.csv")
    solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")


def solve(solver, knapsack_file, solution_file):
    """ Uses 'solver' to solve the knapsack problem in file
    'knapsack_file' and writes the best solution to 'solution_file'.
    """
    knapsack, items = load_knapsack(knapsack_file)
    solver.solve(knapsack, items)
    knapsack = solver.get_best_knapsack()
    print(f"saving solution with {knapsack.get_points()} points to '{solution_file}'")
    knapsack.save(solution_file)


if __name__ == "__main__": # keep this at the bottom of the file
    main()
