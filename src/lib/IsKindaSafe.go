package lib

import "fmt"

func closeEnough(a, b int) bool {
	diff := absInt(a - b)
	return 0 < diff && diff < 4
}

func findProblem(report Report) int {
	isAllIncreasing := allIncreasing(report)
	isAllDecreasing := allDecreasing(report)

	fmt.Printf("Increasing: %v, Decreasing: %v\n", isAllIncreasing, isAllDecreasing)
	if isAllIncreasing || isAllDecreasing {
		// problem is in the closeness

		if !closeEnough(report[0], report[1]) {
			return 0
		}

		for i := 1; i < len(report)-1; i++ {
			if !closeEnough(report[i], report[i+1]) {
				return i + 1
			}
		}

		return -1
	}

	// problem is in the order
	increments := 0
	decrements := 0
	for i := 1; i < len(report); i++ {
		if report[i] < report[i-1] {
			decrements++
		} else if report[i] > report[i-1] {
			increments++
		}
	}

	if increments > decrements {
		for i := 1; i < len(report); i++ {
			if report[i] <= report[i-1] {
				return i
			}
		}
	} else {
		for i := 1; i < len(report); i++ {
			if report[i] >= report[i-1] {
				return i
			}
		}
	}

	return -1
}

func dampenProblem(report Report, problem int) Report {
	new_report := make(Report, len(report)-1)
	for i := 0; i < len(report); i++ {
		if i < problem {
			new_report[i] = report[i]
		} else if i > problem {
			new_report[i-1] = report[i]
		}
	}
	return new_report
}

func IsReportKindaSafe(report Report) bool {
	fmt.Printf("*** Checking report %v\n", report)
	prob := findProblem(report)
	fmt.Printf("Problem %d in %v\n", prob, report)
	if prob == -1 {
		return true
	}
	new_report := dampenProblem(report, prob)
	fmt.Printf("Dampened report %v\n", new_report)
	return IsReportSafe(new_report)
}
