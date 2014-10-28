void foldl(int* array, int N, int (*f)(int), int x0){
    int res = x0;
    for(int i = 0; i < N; i++)
        res = f(res, array[i]);
    return res;
}
