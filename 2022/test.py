def get_input():
    with open("input18.txt", "r") as f:
        file = f.read().splitlines()

    coords = set()
    for line in file:
        coords.add(tuple(map(int, line.split(","))))

    return coords


def get_edges(point):
    edges = []
    edges.append((point[0]+1, point[1], point[2]))
    edges.append((point[0], point[1]+1, point[2]))
    edges.append((point[0], point[1], point[2]+1))
    edges.append((point[0]-1, point[1], point[2]))
    edges.append((point[0], point[1]-1, point[2]))
    edges.append((point[0], point[1], point[2]-1))
    return edges


def part_one(coords):
    # I don't really understand why sides can't be a se and why that makes it too small
    sides = []
    for coord in coords:
        sides.extend(get_edges(coord))

    to_del = []
    for side in sides:
        if side in coords:
            to_del.append(side)

    for d in to_del:
        sides.remove(d)

    return len(sides)


def part_two(coords):
    sides = []
    for coord in coords:
        sides.append((coord[0]+1, coord[1], coord[2]))
        sides.append((coord[0], coord[1]+1, coord[2]))
        sides.append((coord[0], coord[1], coord[2]+1))
        sides.append((coord[0]-1, coord[1], coord[2]))
        sides.append((coord[0], coord[1]-1, coord[2]))
        sides.append((coord[0], coord[1], coord[2]-1))

    to_del = []
    for side in sides:
        if side in coords:
            to_del.append(side)

    for d in to_del:
        sides.remove(d)
    discovered = []

    maxes = (max(x for x in [side[0] for side in sides]) + 1, max(y for y in [side[1]
             for side in sides]) + 1, max(z for z in [side[2] for side in sides]) + 1)
    mins = (min(x for x in [side[0] for side in sides]) - 1, min(y for y in [side[1]
            for side in sides]) - 1, min(z for z in [side[2] for side in sides]) - 1)
    stack = []
    stack.append(maxes)

    count_exposed = 0
    for point in sides:
        x_plus = range(point[0], maxes[0])
        x_minus = range(mins[0], point[0])
        y_plus = range(point[1], maxes[1])
        y_minus = range(mins[1], point[1])
        z_plus = range(point[2], maxes[2])
        z_minus = range(mins[2], point[2])

        exterior = any([
            all((x, point[1], point[2]) not in coords for x in x_plus),
            all((x, point[1], point[2]) not in coords for x in x_minus),
            all((point[0], y, point[2]) not in coords for y in y_plus),
            all((point[0], y, point[2]) not in coords for y in y_minus),
            all((point[0], point[1], z) not in coords for z in z_plus),
            all((point[0], point[1], z) not in coords for z in z_minus),
        ])
        count_exposed += exterior

    return count_exposed


print(part_one(get_input()))
print(part_two(get_input()))
