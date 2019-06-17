/*
 * 2019 Spring Compiler Course Assignment 2 
 */

float c = 1.5;

bool loop(int n, int m) {
    while (n > m) {
        n--;
    }
    return true;
}


/* Re-declared error! declare for second time */
int fun1(int a);
int fun1(int a);

/* Raise semantic error
* the type of formal parameter is not the same
* error message: function formal parameter is not the same */
void fun2(int a);
void fun2(float b){

}

/* Raise semantic error
* the number of formal parameter is not the same
* error message: function formal parameter is not the same */
void fun3(int a);
void fun3(int a, float b){

}

/* Raise semantic error
* the return type is not the same
* error message: function return type is not the same 
*/
void fun4(int a);
int fun4(int a){

}

/* Raise semantic error
 * the return type and the formal parameter is not the same 
 * error message:
 * 1. function return type is not the same
 * 2. function formal parameter is not the same
 */
void d (int a);
int d (int a, int c) { 

}

int main() {
    // Declaration
    int x;
    int i;
    int a = 5;

    // if condition
    if (a > 10) {
        x += a;
        // Undeclared variable
        print(b);
    } else {
        x = a % 10 + 10 * 7; /* Arithmetic */
    }
    print("Hello World");

    // Redeclare variable
    float a = 1.0;

    /* Raise semantic error! Undeclared variable */
    /* b is a undeclared varaible */
    int a = b + 5;

    /* The followings are invalid, 
     * dump a semantic error message 
     */
    int a1 = 25.5 % 5;
    int a2 = 25 % 5.2; 
    int a3 = 25.5 % 5.2;

    /* Raise divide by zero semantic error */
    int a4 = 10 / 0;
    int b = 0;
    a / b;


    /* Raise semantic error!
     * error message: function formal parameter is not the same
     */
    loop(3);

    /* Raise semantic error! Undeclared functions */
    foo1();

    return 0; 

}
