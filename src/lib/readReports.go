package lib

import (
	"strconv"
	"strings"
)

type Report []int

func ReadReports(input string) []Report {
	var res []Report
	for _, report := range strings.Split(input, "\n") {
		if report == "" {
			continue
		}

		var r Report
		for _, num := range strings.Fields(report) {
			n, err := strconv.Atoi(num)
			if err != nil {
				panic(err)
			}
			r = append(r, n)
		}
		res = append(res, r)
	}

	return res
}
