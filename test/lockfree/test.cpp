#include <thread>

#include "unistd.h"

#include "lockfree_queue.h"

using namespace std;

lockfree::queue<size_t> q;

const size_t n = 1'000'000'000;

void push(size_t t){
  for(size_t i = t * n; i < (t + 1) * n; ++i){
    q.push(i);
  }
}

void pull(){
  for(;;){
    size_t i;
    q.pop(i);
  }
}

int main(int argc, char** argv){
  for(size_t i = 0; i < 4; ++i){
    auto t = new thread(push, i);
  }

  for(size_t i = 0; i < 4; ++i){
    auto t = new thread(pull);
  }

  sleep(1000);

  return 0;
}
