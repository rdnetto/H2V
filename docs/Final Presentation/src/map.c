int* xorArray(int mask, const int* input, int* output, int N){
    for(int i = 0; i < N; i++){
        output[i] = input[i] ^ mask;
    } //end for

    return output;
}
