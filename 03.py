def is_numeric(c):
    return c >= '0' and c <= '9'

def matching(s, i):
    if s[i:i+4] != "mul(":
        return -1, -1, False
    
    j = i + 4
    found_comma = False
    while s[j] != ')':
        if not is_numeric(s[j]):
            if s[j] == ',' and found_comma == False:
                found_comma = True
            else:
                return -1, -1, False
        j += 1
    
    nums = s[i+4:j].split(',')
    x, y = int(nums[0]), int(nums[1])

    return x,y,True
    

def solve_part1(input):
    s = 0
    for i in range(len(input)):
        x,y,f = matching(input, i)
        if(f):
            s += x*y

    return s

def solve_part2(input):
    s = 0
    enabled = True
    for i in range(len(input)):
        x,y,f = matching(input, i)
        if(f and enabled):
            s += x*y
        if(input[i:i+4] == "do()"):
            enabled = True
        if(input[i:i+7] == "don't()"):
            enabled = False

    return s

if __name__ == "__main__":
    s = ""
    with open("inputs/03.txt", "r") as fp:
        for line in fp:
            s += line.rstrip()
    
    print(solve_part1(s))
    print(solve_part2(s))