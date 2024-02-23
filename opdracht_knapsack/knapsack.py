import csv
import copy
import random
import itertools

MAX_WEIGHT = 110
MAX_VOLUME = 150


class Recources:
    """This class stores the recources, points, weight, and volume.

    Initiated with:
    argument1, points(any)
    argument2, weight(any)
    argument3, volume(any)

    Return value:
    none

    """
    def __init__(self, points, weight, volume):
        self.recources = (points, weight, volume)

    def get_points(self):
        """Returns the points stored in Recources"""
        (points, weight, volume) = self.recources
        return points

    def get_weight(self):
        """Returns the weight stored in Recources"""
        (points, weight, volume) = self.recources
        return weight

    def get_volume(self):
        """Returns the volume stored in Recources"""
        (points, weight, volume) = self.recources
        return volume

    def get_points_weight_volume(self):
        """Returns a tuple (points, weight, volume) with the stored recources
        """
        return self.recources

    def add_volume(self, added_volume):
        """This function adds the given volume to the recources for storage

        Parameter: to_add_volume(int)

        Returnvalue: none
        """
        (points, weight, volume) = self.recources
        volume += added_volume
        self.recources = (points, weight, volume)

    def add_weight(self, added_weight):
        """This function adds the given weight to the recources for storage

        Parameter: to_add_weight(int)

        Returnvalue: none
        """
        (points, weight, volume) = self.recources
        weight += added_weight
        self.recources = (points, weight, volume)

    def add_points(self, added_points):
        """This function adds the given points to the recources for storage

        Parameter: to_add_points(int)

        Returnvalue: none
        """
        (points, weight, volume) = self.recources
        points += added_points
        self.recources = (points, weight, volume)


class Item:
    """This class is for storing items. The name, points, weight, and volume
    are stored within the class.

    Initiated with:
    argument1, name_item(string)
    argument2, points_item(any)
    argument3, weight_item(any)
    argument4, volume_item(any)

    Return value initiation: none

    """
    def __init__(self, name, points, weight, volume):
        """Initiate item with given name and recources"""
        self.recources = Recources(points, weight, volume)
        self.name = str(name)

    def __repr__(self):
        """Returns the name of the item (string)"""
        return self.name

    def get_points(self):
        """Returns points item (any)"""
        return self.recources.get_points()

    def get_weight(self):
        """Returns Weight item (any)"""
        return self.recources.get_weight()

    def get_volume(self):
        """Returns volume item (any)"""
        return self.recources.get_volume()

    def get_name(self):
        """Returns name item (string)"""
        return self.name

    def get_points_weight_volume(self):
        """Returns a tuple (points, weight, volume) of the recources of the
        item"""
        return self.recources.get_points_weight_volume()


class Items:
    """This Itemsclass is a list for items of the Item class. This class keeps
    track of the total recources of all items in the list.

    This class supports the greater than sign (>)
    This class supports the length function ( len(Itemsclass) )
    This class supports the getitem dunder method. So indexing[] works
    This class has it's own copy dunder method (so copy.copy works)

    Initiated with:
    no arguments

    Return value initiation:
    none

    """
    def __init__(self):
        """Initiating item with zero recources as default, and an empty
        itemlist"""
        self.total_recources = Recources(0, 0, 0)
        self.itemlist = []

    def __repr__(self):
        """Returns a string representation of the items, That being the names
        of the items, for every item one line"""
        string = ""
        for item in self.itemlist:
            string += (str(item.get_name()) + "\n")
        return string

    def __gt__(self, other):
        """Compares the two itemsclasses and returnes a True boolian if the
        greater itemsclass has more points than the lesser itemsclass, False
        otherwise"""
        if (not isinstance(other, Items)):
            raise TypeError("Compare should be between items classes")
        if (self.get_points() > other.get_points()):
            return True
        else:
            return False

    def __copy__(self):
        """Returnes a itemsclass of the items class initalised with a copy of
        the itemslist and recources"""
        recources = self.total_recources
        itemlist = self.itemlist.copy()
        items = Items()
        items.add_item_list(itemlist=itemlist)
        items.replace_recources(recources=recources)
        return items

    def __len__(self):
        """returns the amount of items in the list"""
        return len(self.itemlist)

    def __getitem__(self, index):
        """returnes the item at the index of the itemlist"""
        return self.itemlist[index]

    def add_item(self, item):
        """This function adds the given item of the Item class to the
        Itemsclass. The item is added to the list, and the recources are
        updated."""
        if (not isinstance(item, Item)):
            raise TypeError("Expected Item class")
        (points, weight,
         volume) = self.total_recources.get_points_weight_volume()
        points += item.get_points()
        weight += item.get_weight()
        volume += item.get_volume()
        self.total_recources = Recources(points, weight, volume)
        self.itemlist.append(item)

    def add_item_list(self, itemlist):
        """This function replaces the items of the itemsclass with the given
        list with items.
        Be carefull, the recources are not updated. To do that one has to call
        replace_recources"""
        if (not isinstance(itemlist, list)):
            raise TypeError("Expected a list")
        self.itemlist = itemlist

    def replace_recources(self, recources):
        """This function replaces the recources of the item by the given
        recources"""
        self.total_recources = recources

    def pop_item(self):
        """This function pops the first item of the itemsclass, and gives it
        back. The recources are updated according to the recources of that item
        """
        item = self.itemlist.pop()
        if (not isinstance(item, Item)):
            raise TypeError("Expected Item class")
        (points, weight,
         volume) = self.total_recources.get_points_weight_volume()
        points -= item.get_points()
        weight -= item.get_weight()
        volume -= item.get_volume()
        self.total_recources = Recources(points, weight, volume)
        return item

    def get_points(self):
        """This function returns the total of the points of all items in the
        items class"""
        return self.total_recources.get_points()

    def get_weight(self):
        """This function returns the total of the weight of all the items in
        the items class"""
        return self.total_recources.get_weight()

    def get_volume(self):
        """This function returnes the total of the volume of all the items in
        the items class"""
        return self.total_recources.get_volume()

    def get_points_weight_volume(self):
        """This function returnes a tuple (points, weight, volume) with the
        Total of all the points, weight and volume of all the items in the
        items class"""
        return self.total_recources.get_points_weight_volume()

    def get_itemlist(self):
        """This function returnes a list with in it all the items in the
        itemsclass"""
        return self.itemlist

    def get_string(self):
        """This function returnes a string with all the names of all the items
        in the itemsclass, for every item a new line"""
        string = ""
        for item in self.itemlist:
            string += (str(item.get_name()) + "\n")
        return string

    def shuffle(self):
        """The items in the itemsclass are shuffled"""
        random.shuffle(self.itemlist)

    def remove_item(self, item):
        """This function removes the first item found that matches with the
        given item from the itemclass, and updates the recources."""
        self.itemlist.remove(item)
        (points, weight,
         volume) = self.total_recources.get_points_weight_volume()
        points -= item.get_points()
        weight -= item.get_weight()
        volume -= item.get_volume()
        self.total_recources = Recources(points, weight, volume)


class Knapsack:
    """Summary

    Initiated with:
    argument1, name(type)

    Return value initiation:


    """
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
    """Summary

    Parameters:
    argument1, name(type)

    Return value:


    """
    All_items = Items()
    init_Knapsack = Knapsack(MAX_WEIGHT, MAX_VOLUME)
    with open(knapsack_file, mode="r") as item_file:
        csv_reader = csv.DictReader(item_file)
        for row in csv_reader:
            row = dict(map(lambda tuple: (tuple[0].strip(), tuple[1].strip()),
                           row.items()))
            if (row["name"] == "knapsack"):
                init_Knapsack = Knapsack(int(row["weight"]),
                                         int(row["volume"]))
            else:
                Item_object = Item(row["name"], int(row["points"]),
                                   int(row["weight"]), int(row["volume"]))
                All_items.add_item(Item_object)
    return (init_Knapsack, All_items)


class Solver_Random:
    """Summary

    Initiated with:
    argument1, name(type)

    Return value initiation:


    """
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
    """Summary

    Initiated with:
    argument1, name(type)

    Return value initiation:


    """
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
        # the not not is for < 80 lines and visual indent combined
        if not not (not isinstance(All_items, Items) and
                    not isinstance(items_try, Items)):
            raise TypeError("Items class expected")
        try:
            item = All_items.pop_item()
            if (not isinstance(item, Item)):
                raise TypeError("Item class expected")
        except Exception as e:
            return items_try
        # the not not is for < 80 lines and visual indent combined
        if not not (item.get_weight() + items_try.get_weight() > max_weight or
                    item.get_volume() + items_try.get_volume() > max_volume):
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


class Solver_Optimal_Iterative_Deepcopy:
    """Summary

    Initiated with:
    argument1, name(type)

    Return value initiation:


    """
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
            # the not not is for < 80 lines and visual indent combined
            if not not (next_item.get_weight() +
                        current_combination.get_weight() > max_weight or
                        next_item.get_volume() +
                        current_combination.get_volume() > max_volume):
                stack.append((To_add_items, current_combination))
                continue
            stack.append((copy.copy(To_add_items),
                          copy.copy(current_combination)))
            current_combination.add_item(next_item)
            stack.append((To_add_items, current_combination))
        self.knapsack.add_items(best_combination)

    def get_best_knapsack(self):
        return self.knapsack


class Solver_Optimal_Iterative:
    """Summary

    Initiated with:
    argument1, name(type)

    Return value initiation:


    """
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
                # the not not is for < 80 lines and visual indent combined
                if not not (items_combination > best_combination and
                            items_combination.get_weight() <= max_weight and
                            items_combination.get_volume() <= max_volume):
                    best_combination = items_combination
        self.knapsack.add_items(best_combination)

    def get_best_knapsack(self):
        return self.knapsack


class Solver_Random_Improved:
    """Summary

    Initiated with:
    argument1, name(type)

    Return value initiation:


    """
    def __init__(self, number_of_tries) -> None:
        self.number_of_tries = number_of_tries
        self.knapsack = Knapsack(MAX_WEIGHT, MAX_VOLUME)

    def solve(self, knapsack, All_items) -> None:
        if (not isinstance(All_items, Items)):
            raise TypeError("Items class expected")
        if (not isinstance(knapsack, Knapsack)):
            raise TypeError("Knapsack class expected")
        Item_combination_best = Items()
        max_weight, max_volume = knapsack.get_max_weight_volume()
        All_items.shuffle()
        for item in All_items.get_itemlist():
            if (not isinstance(item, Item)):
                raise TypeError("Item in itemlist of item class expected")
            weight_item = item.get_weight()
            new_weight = Item_combination_best.get_weight() + weight_item
            volume_item = item.get_volume()
            new_volume = Item_combination_best.get_volume() + volume_item
            if (new_weight > max_weight or new_volume > max_volume):
                break
            All_items.remove_item(item)
            Item_combination_best.add_item(item)

        for _ in range(self.number_of_tries):
            Item_combination_best.shuffle()
            All_items.shuffle()
            Item_combination_try = copy.copy(Item_combination_best)
            copy_all_items = copy.copy(All_items)
            copy_all_items.add_item(Item_combination_try.pop_item())
            for item in All_items.get_itemlist():
                if (not isinstance(item, Item)):
                    raise TypeError("Item in itemlist of item class expected")
                weight_item = item.get_weight()
                new_weight = Item_combination_try.get_weight() + weight_item
                volume_item = item.get_volume()
                new_volume = Item_combination_try.get_volume() + volume_item
                if (new_weight > max_weight or new_volume > max_volume):
                    continue
                Item_combination_try.add_item(item)
                copy_all_items.remove_item(item)
            if (Item_combination_try > Item_combination_best):
                Item_combination_best = Item_combination_try
                All_items = copy_all_items
        knapsack.add_items(Item_combination_best)
        self.knapsack = knapsack

    def get_best_knapsack(self):
        return self.knapsack


def main():
    solver_random = Solver_Random(1000)
    solver_optimal_recursive = Solver_Optimal_Recursive()
    solver_optimal_iterative_deepcopy = Solver_Optimal_Iterative_Deepcopy()
    solver_optimal_iterative = Solver_Optimal_Iterative()
    solver_random_improved = Solver_Random_Improved(5000)

    knapsack_file = "knapsack_small"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file +
          "_solution_random.csv")
    solve(solver_optimal_recursive, knapsack_file + ".csv", knapsack_file +
          "_solution_optimal_recursive.csv")
    solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    solve(solver_optimal_iterative, knapsack_file + ".csv", knapsack_file +
          "_solution_optimal_iterative.csv")
    solve(solver_random_improved, knapsack_file + ".csv", knapsack_file +
          "_solution_random_improved.csv")

    knapsack_file = "knapsack_medium"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file +
          "_solution_random.csv")
    solve(solver_optimal_recursive, knapsack_file + ".csv", knapsack_file +
          "_solution_optimal_recursive.csv")
    solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    solve(solver_optimal_iterative, knapsack_file + ".csv", knapsack_file +
          "_solution_optimal_iterative.csv")
    solve(solver_random_improved, knapsack_file + ".csv", knapsack_file +
          "_solution_random_improved.csv")

    knapsack_file = "knapsack_large"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file +
          "_solution_random.csv")
    solve(solver_random_improved, knapsack_file + ".csv", knapsack_file +
          "_solution_random_improved.csv")


def solve(solver, knapsack_file, solution_file):
    """ Uses 'solver' to solve the knapsack problem in file
    'knapsack_file' and writes the best solution to 'solution_file'.
    """
    knapsack, items = load_knapsack(knapsack_file)
    solver.solve(knapsack, items)
    knapsack = solver.get_best_knapsack()
    print(f"saving solution with {knapsack.get_points()} points to \
          '{solution_file}'")
    knapsack.save(solution_file)


if __name__ == "__main__":  # keep this at the bottom of the file
    main()
