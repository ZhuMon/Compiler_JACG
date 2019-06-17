int out = 2;

int main(){
    float b = 5.2;
    // a should be 27
    int a = 1 + b * 5; 
    print(a);

    
    a += 2;
    a -= 3;
    a *= 6;
    a /= 4;
    a %= 20;

    // a should be 19
    print(a);

    int c = a++;
    print(c); //19
    print(a); //20

    c = ++a;
    print(c); //21
    print(a); //21

    c = --a;
    print(c); //20
    print(a); //20

    c = a--;
    print(c); //20
    print(a); //19

    c++;
    c--;
    ++c;
    --c;
    print(c); //20

    out++;
    out--;
    ++out;
    --out;


    int a1 = 25 % 5 - 4 / 2;
    float a2 = 25 % 5;

    print(a1); //-2

    return;
}
