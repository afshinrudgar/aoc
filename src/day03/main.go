package main

import (
	"errors"
	"fmt"
	"math"
	"os"
	"slices"
)

type Battery uint8
type Bank []Battery

func (b Bank) leftMax() []Battery {
	maxFromLeft := make([]Battery, len(b)-1)
	maxFromLeft[0] = b[0]
	for i := 1; i < len(b)-1; i++ {
		maxFromLeft[i] = max(maxFromLeft[i-1], b[i])
	}
	return maxFromLeft
}

func (b Bank) rightMax() []Battery {
	maxFromRight := make([]Battery, len(b)-1)
	maxFromRight[len(b)-2] = b[len(b)-1]
	for i := len(b) - 3; i >= 0; i-- {
		maxFromRight[i] = max(maxFromRight[i+1], b[i+1])
	}
	return maxFromRight
}

func readBank(s string) Bank {
	var res Bank
	for _, c := range s {
		res = append(res, Battery(c-'0'))
	}
	return res
}

func readInput(f *os.File) ([]Bank, error) {
	var res []Bank
	var s string
	for {
		n, err := fmt.Fscanf(f, "%s\n", &s)
		if n == 0 {
			break
		}
		if err != nil {
			return nil, err
		}
		res = append(res, readBank(s))
	}
	return res, nil
}

// PART 1 - selecting 2 batteries in the bank
func (b Bank) findMaxJoltage() uint8 {
	maxFromLeft := b.leftMax()
	maxFromRight := b.rightMax()

	res := uint8(0)
	for i := 0; i < len(b)-1; i++ {
		joltage := uint8(maxFromLeft[i]*10 + maxFromRight[i])
		res = max(res, joltage)
	}
	return res
}

// PART 2 - selecting 12 batteries in the bank

func (b Bank) maxJoltage(numParts int, idx int, cache [][]uint64) (uint64, error) {
	if len(b[idx:]) < numParts || numParts < 1 {
		return 0, errors.New("not enough batteries")
	}

	if numParts == 1 {
		return uint64(slices.Max(b[idx:])), nil
	}

	if cache[numParts][idx] != 0 {
		return cache[numParts][idx], nil
	}

	res := uint64(0)
	mx := uint64(b[idx])
	coef := uint64(math.Pow10(numParts - 1))
	for i := idx; i < len(b)-1; i++ {
		mx = max(mx, uint64(b[i]))
		rest, err := b.maxJoltage(numParts-1, i+1, cache)
		if err != nil {
			break
		}
		res = max(res, mx*coef+rest)
	}

	cache[numParts][idx] = res
	return res, nil
}

func (b Bank) findMaxJoltage2() uint64 {
	var LEN int = 12
	cache := make([][]uint64, LEN+1)
	for i := range LEN + 1 {
		cache[i] = make([]uint64, len(b))
	}

	res, err := b.maxJoltage(LEN, 0, cache)
	if err != nil {
		panic(err)
	}
	return res
}

func solve1(banks []Bank) uint64 {
	sum := uint64(0)
	for _, bank := range banks {
		sum += uint64(bank.findMaxJoltage())
	}
	return sum
}

func solve2(banks []Bank) uint64 {
	sum := uint64(0)
	for _, bank := range banks {
		sum += bank.findMaxJoltage2()
	}
	return sum
}

func main() {
	var f *os.File
	var err error
	if len(os.Args) > 1 {
		f, err = os.Open(os.Args[1])
		if err != nil {
			panic(err)
		}
		defer f.Close()
	} else {
		f = os.Stdin
	}

	banks, err := readInput(f)
	if err != nil {
		panic(err)
	}

	fmt.Println("PART 1:", solve1(banks))
	fmt.Println("PART 2:", solve2(banks))
}
