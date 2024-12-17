program = [2,4,1,2,7,5,4,1,1,3,5,5,0,3,3,0]


def step(A):
    """Run a single loop."""
    B = A % 8
    B = B ^ 2
    C = A // (2**B)
    B = B ^ 7
    B = B ^ C
    return B % 8


def find(A, col=0):
    if step(A) != program[-(col + 1)]:
        return

    if col == len(program) - 1:
        As.append(A)
    else:
        for B in range(8):
            find(A * 8 + B, col + 1)


As = []
for a in range(8):
    find(a)
print(As[0])