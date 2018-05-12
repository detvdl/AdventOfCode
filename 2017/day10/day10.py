from functools import reduce

with open("input", "r") as file:
    lengths = file.read()

    # Puzzle 1
    lengths1 = [int(length) for length in lengths.split(",")]
    nums = list(range(256))
    cur_pos = 0
    skip = 0
    for length in lengths1:
        if (cur_pos + length) > len(nums):
            remainder = (cur_pos + length) % len(nums)
            to_rev = nums[cur_pos:] + nums[0:remainder]
            to_rev = list(reversed(to_rev))
            fst = to_rev[-remainder:]
            snd = nums[remainder:cur_pos]
            lst = to_rev[:-remainder]
        else:
            fst = nums[:cur_pos]
            snd = list(reversed(nums[cur_pos:cur_pos+length]))
            lst = nums[cur_pos+length:]

        nums = fst + snd + lst
        cur_pos = (cur_pos + length + skip) % len(nums)
        skip = skip + 1
    print(f'Multiplying the first two numbers gives: {nums[0] * nums[1]}')

    # Puzzle 2
    lengths2 = [ord(length) for length in lengths] + [17, 31, 73, 47, 23]
    nums = list(range(256))
    cur_pos = 0
    skip = 0
    for i in range(64):
        for length in lengths2:
            if (cur_pos + length) > len(nums):
                remainder = (cur_pos + length) % len(nums)
                to_rev = list(reversed(nums[cur_pos:] + nums[0:remainder]))
                fst = to_rev[-remainder:]
                snd = nums[remainder:cur_pos]
                lst = to_rev[:-remainder]
            else:
                fst = nums[:cur_pos]
                snd = list(reversed(nums[cur_pos:cur_pos+length]))
                lst = nums[cur_pos+length:]

            nums = fst + snd + lst
            cur_pos = (cur_pos + length + skip) % len(nums)
            skip = skip + 1
    dense = [reduce((lambda x, y: x ^ y), nums[i:i+16]) for i in range(0, 255, 16)]
    knothash = ''.join(format(num, '02x') for num in dense)
    print(f'The final Knot Hash is: {knothash}')
