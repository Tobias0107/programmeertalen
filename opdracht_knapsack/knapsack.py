import csv
import copy
import random
import itertools

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

    def __copy__(self):
        recources = self.total_recources
        itemlist = self.itemlist.copy()
        items = Items()
        items.add_item_list(itemlist=itemlist)
        items.replace_recources(recources=recources)
        return items

    def __len__(self):
        return len(self.itemlist)

    def __getitem__(self, index):
        return self.itemlist[index]

    def add_item(self, item):
        if (not isinstance(item, Item)):
            raise TypeError("Expected Item class")
        (points, weight, volume) = self.total_recources.get_points_weight_volume()
        points += item.get_points()
        weight += item.get_weight()
        volume += item.get_volume()
        self.total_recources = Recources(points, weight, volume)
        self.itemlist.append(item)

    def add_item_list(self, itemlist):
        self.itemlist = itemlist

    def replace_recources(self, recources):
        self.total_recources = recources

    def pop_item(self):
        item = self.itemlist.pop()
        if (not isinstance(item, Item)):
            raise TypeError("Expected Item class")
        (points, weight, volume) = self.total_recources.get_points_weight_volume()
        points -= item.get_points()
        weight -= item.get_weight()
        volume -= item.get_volume()
        self.total_recources = Recources(points, weight, volume)
        return item

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

    def get_string(self):
        string = ""
        for item in self.itemlist:
            string += (str(item.get_name()) + "\n")
        return string

    def shuffle(self):
        random.shuffle(self.itemlist)


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

    def get_max_weight_volume(self):
        return (self.max_weight, self.max_volume)

    def save(self, solution_file):
        with open(solution_file, mode="w") as solutions_file:
            try:
                points_str = f"points: {self.get_points()}\n"
                solutions_file.write(points_str)
                items_str = self.items.get_string()
                solutions_file.write("\n")
                solutions_file.write(items_str)
            except Exception as e:
                solutions_file.write("No solution in knapsack yet")


def load_knapsack(knapsack_file):
    All_items = Items()
    init_Knapsack = Knapsack(MAX_WEIGHT, MAX_VOLUME)
    with open(knapsack_file, mode="r") as item_file:
        csv_reader = csv.DictReader(item_file)
        for row in csv_reader:
            row = dict(map(lambda tuple: (tuple[0].strip(), tuple[1].strip()),
                           row.items()))
            if (row["name"] == "knapsack"):
                init_Knapsack = Knapsack(int(row["weight"]), int(row["volume"]))
            else:
                Item_object = Item(row["name"], int(row["points"]),
                                   int(row["weight"]), int(row["volume"]))
                All_items.add_item(Item_object)
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
        max_weight, max_volume = knapsack.get_max_weight_volume()
        for _ in range(self.number_of_tries):
            All_items.shuffle()
            for item in All_items.get_itemlist():
                if (not isinstance(item, Item)):
                    raise TypeError("Item in itemlist of item class expected")
                weight_item = item.get_weight()
                new_weight = Item_combination_try.get_weight() + weight_item
                volume_item = item.get_volume()
                new_volume = Item_combination_try.get_volume() + volume_item
                if (new_weight > max_weight or new_volume > max_volume):
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
    def __init__(self) -> None:
        self.knapsack = Knapsack(MAX_WEIGHT, MAX_VOLUME)

    def solve(self, knapsack, All_items) -> None:
        if (not isinstance(All_items, Items)):
            raise TypeError("Items class expected")
        if (not isinstance(knapsack, Knapsack)):
            raise TypeError("Knapsack class expected")
        max_weight, max_volume = knapsack.get_max_weight_volume()
        items_best = Items()
        items_best = self.recursive_solve(All_items=All_items,
                                          items_try=items_best,
                                          max_weight=max_weight,
                                          max_volume=max_volume)
        knapsack.add_items(items_best)
        self.knapsack = knapsack

    def recursive_solve(self, All_items, items_try, max_weight, max_volume):
        if (not isinstance(All_items, Items) and not isinstance(items_try, Items)):
            raise TypeError("Items class expected")
        try:
            item = All_items.pop_item()
            if (not isinstance(item, Item)):
                raise TypeError("Item class expected")
        except Exception as e:
            return items_try
        if (item.get_weight() + items_try.get_weight() > max_weight or item.get_volume() + items_try.get_volume() > max_volume):
            return self.recursive_solve(All_items=All_items,
                                        items_try=items_try,
                                        max_weight=max_weight,
                                        max_volume=max_volume)
        items_try_no_add = copy.copy(items_try)
        All_items_copy = copy.copy(All_items)
        items_try_no_add = self.recursive_solve(All_items=All_items_copy,
                                                items_try=items_try_no_add,
                                                max_weight=max_weight,
                                                max_volume=max_volume
                                                )
        items_try.add_item(item=item)
        items_try = self.recursive_solve(All_items=All_items,
                                         items_try=items_try,
                                         max_weight=max_weight,
                                         max_volume=max_volume)
        if (items_try.get_points() > items_try_no_add.get_points()):
            return items_try
        else:
            return items_try_no_add

    def get_best_knapsack(self):
        return self.knapsack


class Solver_Optimal_Iterative_Deepcopy_bfs:
    def __init__(self) -> None:
        self.knapsack = Knapsack(MAX_WEIGHT, MAX_VOLUME)

    def solve(self, knapsack, All_items) -> None:
        if (not isinstance(All_items, Items)):
            raise TypeError("Items class expected")
        if (not isinstance(knapsack, Knapsack)):
            raise TypeError("Knapsack class expected")
        best_combination = Items()
        max_weight, max_volume = knapsack.get_max_weight_volume()
        stack = [Items()]
        while len(All_items) > 0:
            item = All_items.pop_item()
            for list_items in stack:
                print("infinite in this loop")
                if (item.get_weight() + list_items.get_weight() > max_weight or item.get_volume() + list_items.get_volume() > max_volume):
                    if (list_items > best_combination):
                        best_combination = list_items
                    else:
                        stack.remove(list_items)
                        continue
                clone = copy.copy(list_items)
                clone.add_item(item)
                stack.append(clone)
        while len(stack) > 0:
            list_items = stack.pop()
            if (list_items > best_combination):
                best_combination = list_items
        knapsack.add_items(best_combination)

    def get_best_knapsack(self):
        return self.knapsack


class Solver_Optimal_Iterative_Deepcopy_failed:
    def __init__(self) -> None:
        self.knapsack = Knapsack(MAX_WEIGHT, MAX_VOLUME)

    def solve(self, knapsack, All_items) -> None:
        if (not isinstance(All_items, Items)):
            raise TypeError("Items class expected")
        if (not isinstance(knapsack, Knapsack)):
            raise TypeError("Knapsack class expected")
        best_combination = Items()
        max_weight, max_volume = knapsack.get_max_weight_volume()
        index = 0
        stack = [(Items(), index)]
        index_stack = []
        while len(stack) > 0:
            list_items, index = stack.pop()
            if index == len(All_items):
                if (list_items > best_combination):
                    best_combination = copy.copy(list_items)
                list_items.pop_item()
                try:
                    index = index_stack.pop() + 1
                    if (index == len(All_items)):
                        break
                except Exception as e:
                    break
            item = All_items[index]
            if not ((item.get_weight() + list_items.get_weight() < max_weight) and (item.get_volume() + list_items.get_volume() < max_volume)):
                stack.append((list_items, index + 1))
                continue
            list_items.add_item(item)
            index_stack.append(index)
            stack.append((list_items, index + 1))
        self.knapsack.add_items(best_combination)

    def get_best_knapsack(self):
        return self.knapsack


class Solver_Optimal_Iterative_Deepcopy:
    def __init__(self) -> None:
        self.knapsack = Knapsack(MAX_WEIGHT, MAX_VOLUME)

    def solve(self, knapsack, All_items) -> None:
        if (not isinstance(All_items, Items)):
            raise TypeError("Items class expected")
        if (not isinstance(knapsack, Knapsack)):
            raise TypeError("Knapsack class expected")
        max_weight, max_volume = knapsack.get_max_weight_volume()
        best_combination = Items()
        stack = [(All_items, Items())]
        while len(stack) > 0:
            To_add_items, current_combination = stack.pop()
            if (len(To_add_items) == 0):
                if (current_combination > best_combination):
                    best_combination = copy.copy(current_combination)
                continue
            next_item = To_add_items.pop_item()
            if (next_item.get_weight() + current_combination.get_weight() > max_weight or next_item.get_volume() + current_combination.get_volume() > max_volume):
                stack.append((To_add_items, current_combination))
                continue
            stack.append((copy.copy(To_add_items), copy.copy(current_combination)))
            current_combination.add_item(next_item)
            stack.append((To_add_items, current_combination))
        self.knapsack.add_items(best_combination)

    def get_best_knapsack(self):
        return self.knapsack


class Solver_Optimal_Iterative:
    def __init__(self) -> None:
        self.knapsack = Knapsack(MAX_WEIGHT, MAX_VOLUME)

    def solve(self, knapsack, All_items) -> None:
        if (not isinstance(All_items, Items)):
            raise TypeError("Items class expected")
        if (not isinstance(knapsack, Knapsack)):
            raise TypeError("Knapsack class expected")
        max_weight, max_volume = knapsack.get_max_weight_volume()
        best_combination = Items()
        for r in range(1, len(All_items) + 1):
            for tuple in itertools.combinations(All_items.get_itemlist(), r):
                list_combination = list(tuple)
                items_combination = Items()
                for item in list_combination:
                    items_combination.add_item(item)
                if (items_combination > best_combination and items_combination.get_weight() <= max_weight and items_combination.get_volume() <= max_volume):
                    best_combination = items_combination
        self.knapsack.add_items(best_combination)

    def get_best_knapsack(self):
        return self.knapsack


class Solver_Random_Improved:
    def __init__(self, amount_of_tries) -> None:
        pass

    def solve(self, knapsack, All_items) -> None:
        pass

    def get_best_knapsack(self):
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
    # solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")

    knapsack_file = "knapsack_medium"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file + "_solution_random.csv")
    solve(solver_optimal_recursive, knapsack_file + ".csv", knapsack_file + "_solution_optimal_recursive.csv")
    solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    solve(solver_optimal_iterative, knapsack_file + ".csv", knapsack_file + "_solution_optimal_iterative.csv")
    # solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")

    knapsack_file = "knapsack_large"
    print("=== solving:", knapsack_file)
    # solve(solver_random, knapsack_file + ".csv", knapsack_file + "_solution_random.csv")
    # solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")


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
