int ai;
int bi = 2;
int ci = 2.0;

float af;
float bf = 2;
float cf = 2.0;

string as = "Compiler";

bool ab = true;

int sqrt(int a){
    return a*a;
}

int main(){
    int ai1 = 4+bi;
    int ai2 = sqrt(ci);
    int ai3 = sqrt(ci)- 5;

    float af1 = 4.5 + bi;
    float af2 = sqrt(ci);
    float af3 = sqrt(ci) - 5;

    /* Raise semantic error! Undeclared variable */
    /* b is a undeclared varaible */
    //int a = b + 5;

    /* Raise semantic error! Redeclared variable */
    //int a1 = 6;
    /* a is a redeclared varaible */
    //int a1 = bi + 5;
    
    return;
}
