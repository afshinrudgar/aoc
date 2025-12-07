package main

import (
	"fmt"
	"os"
	"slices"
)

type IngredientId uint64
type FreshIngredientRange struct {
	from IngredientId
	to   IngredientId
}

func (f FreshIngredientRange) isFresh(id IngredientId) bool {
	return id >= f.from && id <= f.to
}

type FreshIngredientsDB struct {
	store []FreshIngredientRange
}

func NewFreshIngredientsDB() FreshIngredientsDB {
	return FreshIngredientsDB{store: make([]FreshIngredientRange, 0)}
}

func (f *FreshIngredientsDB) insert(ingredient FreshIngredientRange) {
	f.store = append(f.store, ingredient)
}

func (f FreshIngredientsDB) isFresh(id IngredientId) bool {
	for _, ingredient := range f.store {
		if ingredient.isFresh(id) {
			return true
		}
	}
	return false
}

func (f *FreshIngredientsDB) compress() {
	slices.SortFunc(f.store, func(a, b FreshIngredientRange) int {
		return int(a.from - b.from)
	})
	newStore := make([]FreshIngredientRange, 0)
	prev := f.store[0]
	for i := 1; i < len(f.store); i++ {
		curr := f.store[i]
		if curr.from <= prev.to {
			prev.to = max(prev.to, curr.to)
		} else {
			newStore = append(newStore, prev)
			prev = curr
		}
	}
	newStore = append(newStore, prev)
	f.store = newStore
}

func (f FreshIngredientsDB) countFreshIngredients() uint64 {
	f.compress()
	count := uint64(0)
	for _, r := range f.store {
		count += uint64(r.to - r.from + 1)
	}
	return count
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

	db, availableIds := readInput(f)
	fmt.Println("PART 1:", solve1(db, availableIds))
	fmt.Println("PART 2:", solve2(db))
}

func readInput(f *os.File) (FreshIngredientsDB, []IngredientId) {
	db := NewFreshIngredientsDB()
	for {
		val := FreshIngredientRange{}
		_, err := fmt.Fscanf(f, "%d-%d\n", &val.from, &val.to)
		if err != nil {
			break
		}
		db.insert(val)
	}

	var availableIds []IngredientId
	for {
		var id IngredientId
		n, err := fmt.Fscanln(f, &id)
		if n == 0 {
			break
		}
		if err != nil {
			panic(err)
		}
		availableIds = append(availableIds, id)
	}

	return db, availableIds
}

func solve1(db FreshIngredientsDB, availableIds []IngredientId) int {
	count := 0
	for _, id := range availableIds {
		if db.isFresh(id) {
			count++
		}
	}
	return count
}

func solve2(db FreshIngredientsDB) uint64 {
	return db.countFreshIngredients()
}
