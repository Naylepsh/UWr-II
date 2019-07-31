#include <stdio.h>

#define repeat do
#define until(exp) while(!(exp))

const int MAXSIZE = 100000;

class point{
public:
    int i;
    int j;
public:
    point();
    point(int i, int j);
    bool operator<(point) const;
};

point::point() {
    this->i = 0;
    this->j = 0;
}

point::point(int i, int j) {
    this->i = i;
    this->j = j;
}

bool point::operator<(const point p) const{
    long long t = this->i;
    long long s = this->j;
    long long a = t * s;
    t = p.i;
    s = p.j;
    long long b = t * s;
    return a < b;
}

class priority_queue{
public:
    point elems[MAXSIZE+1];
    int n = 0;
public:
    int parent(int i) const;
    int left_child(int i) const;
    int right_child(int i) const;
    void move_down(int i);
    void move_up(int i);
    void swap(int i, int k);
    void push(point p);
    point pop();
};

int priority_queue::parent(int i) const {
    return i/2;
}

int priority_queue::left_child(int i) const {
    return 2*i;
}

int priority_queue::right_child(int i) const {
    return 2*i+1;
}

void priority_queue::move_down(int i) {
    int k = i;
    int j;
    repeat{
        j = k;
        if (left_child(j) <= n &&  elems[k] < elems[left_child(j)]){
            k = left_child(j);
        }
        if (right_child(j) <= n && elems[k] < elems[right_child(j)]){
            k = right_child(j);
        }
        swap(j, k);
    } until(j == k);
}

void priority_queue::swap(int i, int k) {
    point temp = elems[i];
    elems[i] = elems[k];
    elems[k] = temp;
}

void priority_queue::move_up(int i) {
    int k = i;
    int j;
    repeat {
        j = k;
        if (j > 1 && elems[parent(j)] < elems[k])
            k = parent(j);
        swap(j, k);
    } until(j == k);
}

void priority_queue::push(point p) {
    elems[n+1] = p;
    ++n;
    move_up(n);
}

point priority_queue::pop() {
    point temp = elems[1];
    elems[1] = elems[n];
    --n;
    move_down(1);
    return temp;
}



int main() {
    int M;
    int k;
    scanf("%d", &M);
    scanf("%d", &k);

    priority_queue candidates;
    long long previous = 0;
    int printed = 0;

    candidates.push(point(M,M));
    long long i1;
    long long j1;
    long long product;

    while (printed < k){
        point p = candidates.pop();
        i1 = p.i;
        j1 = p.j;
        product = i1*j1;
        //printf("now: i=%d, j=%d\n", p.i, p.j);
        if (previous != product){
            printf("%lld\n", product);
            previous = product;
            printed++;
        }

        if (p.i == M){
            if (p.j > 1){
                // push point on the left
                candidates.push(point(p.i, p.j-1));
            }
            if (p.i-1 >= p.j && p.i > 1){
                // push point above
                candidates.push(point(p.i-1, p.j));
            }
        }
        else{
            if (p.i > 1){
                // push point above
                candidates.push(point(p.i-1, p.j));
            }
        }

        if (candidates.n == MAXSIZE){
            point p1 = candidates.pop();
            i1 = p1.i;
            j1 = p1.j;
            product = i1*j1;
            if (previous != product){
                printf("%lld\n", product);
                previous = product;
                printed++;
            }
        }
    }
    return 0;
}