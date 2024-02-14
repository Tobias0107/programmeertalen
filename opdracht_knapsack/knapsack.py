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

    def __gt__(self, other):
        if (not isinstance(other, Items)):
            raise TypeError("Compare should be between items classes")
        if (self.get_points() > other.get_points()):
            return True
        else:
            return False

    def add_item(self, item):
        if (not isinstance(item, Item)):
            raise TypeError("Expected Item class")
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

    def get_itemlist(self):
        return self.itemlist


class Knapsack:
    def __init__(self, max_weight, max_volume):
        self.max_weight = max_weight
        self.max_volume = max_volume

    def add_items(self, Item_list):
        # Items should be a list with items of the class items
        if (not isinstance(Item_list, Items)):
            raise TypeError("Expected Items class")
        self.items = Item_list

    def get_points(self):
        # The self.items is only initialised if the items are added by the add
        # items
        try:
            return self.items.get_points()
        except Exception as e:
            return 0

    def save(self, solution_file):
        with open(solution_file + ".svg", mode="a") as solutions_file:
            try:
                points_str = f"points: {self.get_points()}"
                solutions_file.write(points_str)
                solutions_file.writelines(self.items)
            except Exception as e:
                solutions_file.write("No solution in knapsack yet")


def load_knapsack(knapsack_file):
    All_items = Items()
    init_Knapsack = Knapsack(MAX_WEIGHT, MAX_VOLUME)
    with open(knapsack_file + ".csv", mode="r") as item_file:
        csv_reader = csv.DictReader(csv_reader, delimiter=", ")
        for row in csv_reader:
            if (row["name"] == "knapsack"):
                init_Knapsack = Knapsack(row["weight", row["volume"]])
            else:
                Item = Item(row["name"], row["points"], row["weight"],\
                             row["volume"])
                All_items.add_item(Item)
    return (init_Knapsack, All_items)


class Solver_Random:
    def __init__(self, number_of_tries):
        self.number_of_tries = number_of_tries
        self.knapsack = Knapsack(MAX_WEIGHT, MAX_VOLUME)

    def solve(self, knapsack, All_items):
        if (not isinstance(All_items, Items)):
            raise TypeError("Items class expected")
        if (not isinstance(knapsack, Knapsack)):
            raise TypeError("Knapsack class expected")
        Item_combination_try = Items()
        Item_combination_best = Items()
        for _ in range(self.number_of_tries):
            for item in All_items.get_itemlist():
                if (not isinstance(item, Item)):
                    raise TypeError("Item in itemlist of item class expected")
                weight_item = item.get_weight()
                new_weight = Item_combination_try.get_weight() + weight_item
                volume_item = item.get_volume()
                new_volume = Item_combination_try.get_volume() + volume_item
                if (new_weight > MAX_WEIGHT or new_volume > MAX_VOLUME):
                    if (Item_combination_try > Item_combination_best):
                        Item_combination_best = Item_combination_try
                        Item_combination_try = Items()
                    break
                Item_combination_try.add_item(item)
        knapsack.add_items(Item_combination_best)
        self.knapsack = knapsack

    def get_best_knapsack(self):
        return self.knapsack


class Solver_Optimal_Recursive:
    def solve(self, knapsack, All_items) -> None:
        pass


class Solver_Optimal_Iterative_Deepcopy:
    def solve(self, knapsack, All_items) -> None:
        pass


class Solver_Optimal_Iterative:
    def solve(self, knapsack, All_items) -> None:
        pass


class Solver_Random_Improved:
    def solve(self, knapsack, All_items) -> None:
        pass


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


if __name__ == "__main__":  # keep this at the bottom of the file
    main()
