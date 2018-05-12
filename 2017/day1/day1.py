with open("./input", "r") as number:
    num = number.read()
    solution = sum(int(num[i]) for i in range(len(num))
                   if num[i] == num[(i+1) % len(num)])
    print(solution)

    solution = sum(int(num[i]) for i in range(len(num))
                   if num[i] == num[(i+(int(len(num)/2))) % len(num)])
    print(solution)
