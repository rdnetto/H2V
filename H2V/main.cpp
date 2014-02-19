#include <stdio.h>
#include <QString>


int main(int argc, char *argv[]){
    if(argc != 2){
        printf("USAGE: H2V FILE\n");
        return 1;
    }else{
        QString file = argv[1];
        printf("Reading from: %s\n", argv[1]);
    } //end if

    return 0;
}
