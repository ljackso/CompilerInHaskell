
var globalOne int = 1;
var globalVar int = 2;

func main() {
    var luke int = fac( add(addOne(globalVar), 5));
    show (luke);
}; 

func fac(n int) int {
	if (n == 0) {
		return globalOne;
	} else {
		return n * fac(n-globalOne);
	};
};

func addOne(n int) int {
    return n + globalOne;
};

func add(n int, m int) int {
    return n + m;
};  

