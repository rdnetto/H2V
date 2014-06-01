int f2(int a, int b, int c){
    int f2a(int x){
        return x + a + d;
    }
    int f2b(int x){
        return x + b;
    }
    int f2c(int x){
        return f2a(a) + f2b(d) - x;
    }

    int d = a + b;
    return f2c(c);
}
