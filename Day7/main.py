#!/usr/bin/env python3
from collections import defaultdict

input = map(str.split, open('input.txt').read().splitlines())

path = []
dirs = defaultdict(int)

for line in input:
    
    if line[0] == "$":
        if line[1] == "cd":
            if line[2] == "..":
                path.pop()
                # print(path)
            else:
                path.append(line[2])
                # print(path)
    
    elif line[0] != "dir":
        for dir in range(len(path)):
            dirs[tuple(path[: dir + 1])] += int(line[0])
# Part 1
print(sum(size for size in dirs.values() if size <= 100000))

# part 2
required = 30000000 - (70000000 - dirs[('/',)])

print(min(size for size in dirs.values() if size >= required))