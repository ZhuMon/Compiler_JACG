
int main(){
    int a = 8;
    int b = 4;
    
    if(a != b){
        print(1);
    }

    if(a == b){
        print(1);
    } else {
        print(2.0);
    }

    if(a == b){
        print(b);
    } else if(a >= b){
        if(a != b){
            print("3");
        }
    } else if(a < b){
        print(b);
    } else {
        print(b);
    }
    
    if(a > b){
        if(a != b){
            print(b);
        } else {
            print(b);
        }
    } else if(a != b){
        print(b);
    }
    return;
}
