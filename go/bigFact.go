import (
	"math/big"
	"fmt"
	"math"
	"runtime"
	"strconv"
	"time"
)

func main() {

	runtime.GOMAXPROCS(runtime.NumCPU() * 2)
	start := time.Now()

	a, b := int64(3000000000000), int64(10000000)

	x := numOfFactDigits(a)
	fmt.Printf("numberOfDigits(%v!) = %v\n", a, x)
	xLen := len(strconv.FormatInt(x, 10))

	y := func() *big.Int {
		fmt.Printf("%v! is being calculated...\n", b)
		result := compute(b)
		l := numOfFactDigits(b)
		return big.NewInt(0).Div(result, big.NewInt(0).Exp(big.NewInt(10), big.NewInt(l-int64(xLen)), nil))
	}()
	fmt.Printf("selectDigits(%v!, 0, %v) = %v\n", b, xLen, y)

	z := func() int {
		digits := big.NewInt(0).Add(big.NewInt(x), y).String()
		sum := 0
		for _, c := range digits {
			sum += int(c - '0')
		}
		return sum
	}()
	fmt.Printf("sumOfDigits(%v + %v) = %v\n", x, y, z)

	elapsed := time.Now()
	fmt.Println("time =", elapsed.Sub(start))
}

func compute(n int64) *big.Int {

	if n < 0 {
		panic(fmt.Sprintf("%v < 0", n))
	}
	if n < 2 {
		return big.NewInt(1)
	}

	return rangeProduct(1, n)
}

func rangeProduct(start, end int64) *big.Int {

	if end-start == 1 {
		return mulInt64(start, end)
	}
	if end-start == 2 {
		return mulBigInt(mulInt64(start, start+1), big.NewInt(end))
	}

	mid := (start + end) / 2
	p := make(chan *big.Int, 2)
	go func() { p <- rangeProduct(start, mid) }()
	go func() { p <- rangeProduct(mid+1, end) }()

	return mulBigInt(<-p, <-p)
}

func mulInt64(x, y int64) *big.Int {
	return mulBigInt(big.NewInt(x), big.NewInt(y))
}

func mulBigInt(x, y *big.Int) *big.Int {
	return big.NewInt(0).Mul(x, y)
}

// too slow for huge numbers
func numOfDigits(n *big.Int) int64 {
	c := int64(math.Log(2)/math.Log(10)*float64(n.BitLen())) + 1
	r := big.NewInt(0).Exp(big.NewInt(10), big.NewInt(c-1), nil).Cmp(n)
	if r > 0 {
		return c - 1
	}
	return c
}

func numOfFactDigits(n int64) int64 {
	return int64(math.Floor(logGamma(float64(n+1))/math.Log(10)) + 1)
}


// GammaLn implementation

// Lanczos coefficients
var lanczos = [...]float64{0.99999999999999709182,
	57.156235665862923517, -59.597960355475491248,
	14.136097974741747174, -0.49191381609762019978,
	0.33994649984811888699e-4, 0.46523628927048575665e-4,
	-0.98374475304879564677e-4, 0.15808870322491248884e-3,
	-0.21026444172410488319e-3, 0.21743961811521264320e-3,
	-0.16431810653676389022e-3, 0.84418223983852743293e-4,
	-0.26190838401581408670e-4, 0.36899182659531622704e-5,
}

// value of g constant in the Lanczos approximation
const g = float64(607) / float64(128)

func logGamma(n float64) float64 {

	if math.IsNaN(n) || n <= 0 {
		return math.NaN()
	}

	sum := float64(0)
	for i, v := range lanczos {
		if i == 0 {
			sum += v
		} else {
			sum += v / (n + float64(i))
		}
	}

	a, b := n+0.5, n+g+0.5
	halfLn2pi := 0.5 * math.Log(2*math.Pi)

	return a*math.Log(b) - b + halfLn2pi + math.Log(sum/n)
}

