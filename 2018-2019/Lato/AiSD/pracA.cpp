#include <iostream>
#include <algorithm>

struct data {
    long long totalWeight;
    long long pathWeight;
    int i;
    int j;
};

long long weightSum(const unsigned int vertices[], int n);
void moveJ(data & d, const unsigned int vertices[], int n);

long long currentBest = 0;

int main() {
    int n;
    scanf("%d", &n);
    unsigned int vertices[n];
    for (int i = 0; i < n; i++) {
        scanf("%u", &vertices[i]);
    }

    data d = {weightSum(vertices, n), 0, 0, 0};
    for (; d.i < n; d.i++){
        moveJ(d, vertices, n);
        d.pathWeight -= vertices[d.i];
        if (std::min(d.pathWeight, d.totalWeight - d.pathWeight) > currentBest)
            currentBest = std::min(d.pathWeight, d.totalWeight - d.pathWeight);
    }

    printf("%lld", currentBest);
    return 0;
}

void moveJ(data & d, const unsigned int vertices[], int n) {
    while (true) {
        if (d.pathWeight >= d.totalWeight - d.pathWeight) {
            break;
        } else {
            d.pathWeight = d.pathWeight + vertices[d.j];
            d.j = d.j + 1 > n - 1 ? 0 : d.j + 1;
            if (std::min(d.pathWeight, d.totalWeight - d.pathWeight) > currentBest)
                currentBest = std::min(d.pathWeight, d.totalWeight - d.pathWeight);
        }
    }
}

long long weightSum(const unsigned int vertices[], int n){
    long long sum = 0;
    for (int i = 0; i < n; i++){
        sum += vertices[i];
    }
    return sum;
}