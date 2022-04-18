

def sum (acc, start, finish):
    if start > finish: return acc
    else: return sum (acc+start, start+1, finish)

print (sum (0, 1, 998))