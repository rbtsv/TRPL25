#include<iostream>
#include<vector>
#include<algorithm>

using namespace std;


vector <int> create_vector(){
    vector<int> v{1,2,3};
    for(auto x: v)
        cout << x << " ";
    return v;    
}


vector <int> * create_pvector(){
    vector<int> v{1,2,3};
    for(auto x: v)
        cout << x << " ";
    return &v;    
}

vector <int> * copy_vector_pointer(vector<int> * pv){
    //vector<int> v{1,2,3};
    for(auto x: *pv)
        cout << x << " ";
    return pv;    
}
    

int main (int argc, char const *argv[]){
	int x, y, *px, * py;
    int &lx = x;
    const int &ly = y;
    
    x = 7;
    y = -1;
    
    cout << y << " " << ly << endl;
    x = 6;
    cout << x << " " << lx << endl;
    y = 5;
    cout << y << " " << ly << endl;
    lx = 4;
    cout << x << " " << lx << endl;
    
    cout << "====" << endl;
    
    cout << "&x" << &x << endl;
    for(size_t i = 0; i < 5; ++i){
        int x = y + i;
        cout << x << " " << lx << endl;
        int y = 1;
        int z = 2;
        cout << "&x: " << &x << endl << "&y: " << &y  << endl << "&z: " << &z << endl;
    }
    cout << "----" << endl;
    int z = 2;
    cout << "&x: " << &x << endl << "&y: " << &y  << endl << "&z: " << &z << endl;
    
    cout << x << " " << lx << endl;
    
    cout << "====" << endl;
    
    auto vec = create_pvector();
    cout << "\n";
    for(auto x: *vec)
        cout << x << " ";
    
    vector<int> v{4,5, 6};
    auto pvec = copy_vector_pointer(& v);
    
    for(auto x: *pvec)
        cout << x << " ";
    
    long long ll = 1000000000;
    cout << endl << std::max(ll, static_cast<long long>(x));
    
    
    return 0;		
}    