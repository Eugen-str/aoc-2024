def get_input(filename):
    lines = []
    split = -1
    with open(filename, "r") as fp:
        for i, line in enumerate(fp):
            line = line.strip()
            if line == "":
                split = i
            lines.append(line)

    rules = []
    for rule in lines[:split]:
        x, y = rule.split('|')
        rules.append((int(x), int(y)))

    updates = []
    for rule in lines[split+1:]:
        tmp = rule.split(',')
        updates.append(list(map(int, tmp)))

    return rules, updates

def is_valid(update, rules):
    for i, page in enumerate(update):
        before = update[:i]
        after = update[i+1:]

        for b in before:
            ok_loop = False
            for (x, y) in rules:
                if b == x and y == page:
                    ok_loop = True
                    break
            if not ok_loop:
                return False

        for a in after:
            ok_loop = False
            for (x, y) in rules:
                if page == x and y == a:
                    ok_loop = True
                    break
            if not ok_loop:
                return False
    return True

def solve_part1(rules, updates):
    res = 0
    for update in updates:
        if is_valid(update, rules):
            res += update[len(update) // 2]
    return res

def solve_part2(rules, updates):
    res = 0
    for update in updates:
        if not is_valid(update, rules):
            i = 0
            while i < len(update):
                curr = update[i]
                for update in updates:

                i += 1

if __name__ == "__main__":
    rules, updates = get_input("inputs/05.txt")

    print(solve_part1(rules, updates))
