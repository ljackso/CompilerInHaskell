
var globalOne int = 1;
var globalTwo int = 20;

func main() {
    var luke int = fac( add(addOne(2), 5));
    Show (luke);
}; 

func fac(n int) int {
    Show(n);
    if (n == 0) {
        return 1;
    } else {
        return n * fac(n-1);
    };
};

func addOne(n int) int {
    return n + globalOne;
};

func add(n int, m int) int {
    return n + m;
};  

