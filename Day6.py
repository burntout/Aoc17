#!/usr/bin/env python

def redist(memBankStart):
    mem = list(memBankStart)
    bankSize = len(mem)
    toReDist = max(mem)
    reDistFrom = mem.index(toReDist)
    mem[reDistFrom] = 0
    for i in range(1,toReDist + 1 ):
        newLoc = (reDistFrom + i) % bankSize
        mem[newLoc] = mem[newLoc] + 1
    return mem

def cycles(memBankStart):
    memState = list(memBankStart)
    states = []
    cnt = 0
    while tuple(memState) not in states:
        states.append(tuple(memState))
        cnt += 1 
        memState = redist(memState)
    return cnt, cnt - states.index(tuple(memState))

testmem = [0,2,7,0]
print cycles(testmem)
testmem  = [ 11,11,13,7,0,15,5,5,4,4,1,1,7,1,15,11]
print cycles(testmem)

