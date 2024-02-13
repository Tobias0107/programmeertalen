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
        string = f"points:{self.get_points()}"
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


def load_knapsack(knapsack_file):
    with open(knapsack_file + ".csv", mode="r") as item_file:
        csv_reader = csv.DictReader(csv_reader)
        for row in csv_reader:



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
