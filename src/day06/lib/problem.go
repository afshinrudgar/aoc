package lib

type Problem struct {
	operands []int64
	op       string
}

func New(operands []int64, op string) Problem {
	return Problem{operands: operands, op: op}
}

func (p Problem) calculate() int64 {
	switch p.op {
	case "+":
		sum := int64(0)
		for _, operand := range p.operands {
			sum += operand
		}
		return sum
	case "*":
		prod := int64(1)
		for _, operand := range p.operands {
			prod *= operand
		}
		return prod
	default:
		panic("invalid operator")
	}
}

func GrandTotal(problems []Problem) int64 {
	total := int64(0)
	for _, p := range problems {
		total += p.calculate()
	}
	return total
}
