package lib

func allIncreasing(report Report) bool {
	for i := 1; i < len(report); i++ {
		if report[i] <= report[i-1] {
			return false
		}
	}
	return true
}

func allDecreasing(report Report) bool {
	for i := 1; i < len(report); i++ {
		if report[i] >= report[i-1] {
			return false
		}
	}
	return true
}
func absInt(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func allClose(report Report) bool {
	for i := 1; i < len(report); i++ {
		diff := absInt(report[i] - report[i-1])
		if diff < 1 || diff > 3 {
			return false
		}
	}
	return true
}

func IsReportSafe(report Report) bool {
	return (allIncreasing(report) || allDecreasing(report)) && allClose(report)
}
