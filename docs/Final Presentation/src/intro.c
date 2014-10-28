int main(int a){
    int x = foo(a);
    int y = bar(a);     //might have side-effects
    int z = baz(a);
    return x + z;
}
