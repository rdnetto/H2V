int fib(int n){
    return _fib(0, 1, n);
}

int _fib(int x0, int x1, int n){
    if(n == 0)
        return x0;
    else
        return _fib(x1, (x0 + x1), (n - 1));
}
