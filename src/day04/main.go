package main

import (
	"errors"
	"fmt"
	"os"
)

type RollsOfPaperMap [][]bool

const NEIGHBORS_LIMIT = 4

// part 1
func (r RollsOfPaperMap) isAccessible(i int, j int) bool {
	return r[i][j] && r.countNeighbors(i, j) <= NEIGHBORS_LIMIT
}

func (r RollsOfPaperMap) countAccessibleRollsOfPaper() uint {
	count := uint(0)
	for i := range r {
		for j := range r[i] {
			if r.isAccessible(i, j) {
				count++
			}
		}
	}
	return count
}

// part 2
type RollsOfPaperNeighborhood struct {
	neighborhood [][]uint
	rollsOfPaper RollsOfPaperMap
}

func (r RollsOfPaperMap) countNeighbors(i int, j int) uint {
	neighbors := uint(0)
	for _, di := range []int{-1, 0, 1} {
		for _, dj := range []int{-1, 0, 1} {
			ni := i + di
			nj := j + dj
			if ni >= 0 && ni < len(r) && nj >= 0 && nj < len(r[i]) && r[ni][nj] {
				neighbors++
			}
		}
	}
	return neighbors
}

func (r RollsOfPaperMap) buildNeighborhood() RollsOfPaperNeighborhood {
	neighborhoodMap := make([][]uint, len(r))
	for i := range r {
		neighborhoodMap[i] = make([]uint, len(r[i]))
		for j := range r[i] {
			neighborhoodMap[i][j] = r.countNeighbors(i, j)
		}
	}
	return RollsOfPaperNeighborhood{neighborhoodMap, r}
}

func (r RollsOfPaperNeighborhood) isAccessible(i, j int) bool {
	return r.rollsOfPaper[i][j] && r.neighborhood[i][j] <= NEIGHBORS_LIMIT
}

func (r RollsOfPaperNeighborhood) isPossibleToLift() bool {
	for i := range r.neighborhood {
		for j := range r.neighborhood[i] {
			if r.isAccessible(i, j) {
				return true
			}
		}
	}
	return false
}

func (r RollsOfPaperNeighborhood) lift(i, j int) error {
	if !r.isAccessible(i, j) {
		return errors.New("not accessible")
	}
	r.rollsOfPaper[i][j] = false
	for _, di := range []int{-1, 0, 1} {
		for _, dj := range []int{-1, 0, 1} {
			ni := i + di
			nj := j + dj
			if ni >= 0 && ni < len(r.neighborhood) && nj >= 0 && nj < len(r.neighborhood[i]) {
				r.neighborhood[ni][nj]--
			}
		}
	}
	return nil
}

type Point struct {
	x int
	y int
}

func (r RollsOfPaperNeighborhood) liftContinuously() []Point {
	res := []Point{}
	for r.isPossibleToLift() {
		for i := range r.neighborhood {
			for j := range r.neighborhood[i] {
				if r.isAccessible(i, j) {
					err := r.lift(i, j)
					if err != nil {
						panic(err)
					}
					res = append(res, Point{i, j})
				}
			}
		}
	}
	return res
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

	rollsOfPaperMap := readInput(f)
	fmt.Println("PART 1:", solve1(rollsOfPaperMap))
	fmt.Println("PART 2:", solve2(rollsOfPaperMap))
}

func readInput(f *os.File) RollsOfPaperMap {
	var res RollsOfPaperMap
	var s string
	for {
		var line []bool
		n, err := fmt.Fscanf(f, "%s\n", &s)
		if n == 0 {
			break
		}
		if err != nil {
			panic(err)
		}
		for _, c := range s {
			line = append(line, c == '@')
		}
		res = append(res, line)
	}
	return res
}

func solve1(rollsOfPaperMap RollsOfPaperMap) uint {
	return rollsOfPaperMap.countAccessibleRollsOfPaper()
}

func solve2(rollsOfPaperMap RollsOfPaperMap) uint {
	neighborhood := rollsOfPaperMap.buildNeighborhood()
	return uint(len(neighborhood.liftContinuously()))
}
