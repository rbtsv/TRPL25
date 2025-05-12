#include <iostream>
#include <string>
#include <cassert>
#include <vector>
#include <chrono>
#include <unistd.h>  // для sleep()









template<typename Functor> auto time_count(Functor f, const std::string & name){
    std::cout << name << std::endl;
    auto start = std::chrono::high_resolution_clock::now();
    f();
    auto stop = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
    auto duration_count = duration.count();
    std::cout << "%%% Time: " << duration_count << "ms" << std::endl;
    return duration_count;
}



int main(int argc, char* argv[]) {
    std::vector<int> v{1,2,3}, w{0,5,6};
    
    std::cout << "&v: " << &v << std::endl;
    std::cout << "&w: " << &w << std::endl;
    
    
    size_t measured_time = time_count(
    [&v, w]() { 
        std::cout << "v[1]: " << v[1] << std::endl;
        sleep(2);
        std::cout << "&v: " << &v << std::endl;
        std::cout << "&w: " << &w << std::endl;
        
    }, "Time Measurement");
        
    
    std::cout << measured_time << std::endl;
    
    return 0;
}
